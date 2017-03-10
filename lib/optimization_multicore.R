# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Multicore-функции для оптимизации стратегий:
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
###
#' Функция BF оптимизации движка стратегии (multicore)
#' 
#' @param var_df Оптимизационная матрица параметров стратегии
#' @param FUN.StrategyGear Функция стратегии
#' @param fast Упрощенный/полный perfomance анализ
#' @param ohlc_args Лист с параметрами котировок
#' @param str_args Лист с параметрами стратегии
#'
#' @return result DF с perfomance'ами по всем итерациям цикла 
#'
#' @export
BruteForceOptimizer.mc <- function(var_df, 
                                   FUN.StrategyGear, 
                                   fast = FALSE,
                                   ohlc_args, trade_args) {
    #
    require(doParallel)
    if (getDoParWorkers() == 1) {
        workers <- detectCores() - 1    
        registerDoParallel(cores = workers)
    } else {
        workers <- getDoParWorkers()
    }
    # вычисления
    result <- 
        foreach(i = 1:workers, .combine = data.frame) %dopar% {
            BruteForceOptimizer(
                var_df = Delegate(i, nrow(var_df), p = workers) %>% var_df[., ], 
                FUN.StrategyGear = match.fun(FUN.StrategyGear), 
                fast = fast,
                ohlc_args, trade_args
            ) %>%
            data.frame(.)
        }
    #
    return(result)
}
#
###
#' Roller функция обучающей оптимизации движка стратегии (multicore)
#' 
#' @param slice_index Временные интервалы оптимизационных окон (индексы start/end)
#' @param var_df Оптимизационная матрица параметров стратегии
#' @param FUN.StrategyGear Функция стратегии
#' @param win_size Период обучения (нужно для более точной кластеризации)
#' @param ohlc_args Лист с параметрами котировок
#' @param trade_args Лист с параметрами стратегии
#' @param 
#'
#' @return result DF с perfomance'ами по всем итерациям цикла 
#'
#' @export                             
RollerOptimizer.learning <- function(slice_index, 
                                     var_df,
                                     FUN.StrategyGear,
                                     win_size,
                                     ohlc_args, trade_args) {
    require(doParallel)
    #.CurrentEnv <- environment()                                                    
    if (getDoParWorkers() == 1) {
        workers <- detectCores() - 1    
        registerDoParallel(cores = workers)
    } else {
        workers <- getDoParWorkers()
    }
    
    ## Вычисление оптимизаций на обучающих периодах
    # выбор оптимальной стратегии распараллеливания
    if (length(slice_index$widthSlice) >= workers) {
        bf_data <- foreach(i = 1:workers) %dopar% {
            map_data <- Delegate(i, length(slice_index$widthSlice), p = workers)
            foreach(x = 1:length(map_data)) %do% {
                ohlc_args %>%
                {
                    .$from_date <- slice_index$widthSlice[[map_data[x]]][1, ]
                    .$to_date <- slice_index$widthSlice[[map_data[x]]][2, ]
                    return(.)
                } %>%    
                BruteForceOptimizer(var_df = var_df,
                    FUN.StrategyGear = match.fun(FUN.StrategyGear), 
                    fast = TRUE,
                    ohlc_args = ., trade_args)    
            } %>%
            unlist(., recursive = FALSE)
        }
    } else {
        bf_data <- foreach(i = 1:length(slice_index$widthSlice)) %do% {
            ohlc_args %>%
            {
                .$from_date <- slice_index$widthSlice[[x]][1, ]
                .$to_date <- slice_index$widthSlice[[x]][2, ]
                return(.)
            } %>%
            BruteForceOptimizer.mc(var_df = var_df,
                FUN.StrategyGear = match.fun(FUN.StrategyGear), 
                fast = TRUE,
                ohlc_args = ., trade_args)
        }
    }

    ## КА
    cluster_data <- lapply(1:length(bf_data),
        function(x) {
            ## Подготовка к КА
            data_for_cluster <- 
                bf_data[[x]] %>%
                {
                    CalcKmean.preparation(data = ., 
                        n.mouth = win_size, 
                        hi = TRUE, q.hi = 0.5, 
                        only_profitable = TRUE)
                }
            data_for_cluster$profit <- NULL
            data_for_cluster$draw <- NULL
            ## Вычисление кластеров
            clustFull.data <- 
                CalcKmean.parameters(data = data_for_cluster, 
                    iter.max = 100, 
                    plusplus = FALSE, 
                    test.range = 30) %>%
                .[[2]] %>%
                CalcKmean(data = data_for_cluster, 
                    n.opt = ., 
                    plusplus = FALSE, 
                    var.digits = 3)
            ## Округление центров до значений точек пространства    
            clustFull.data[[2]] %<>%
                {
                    for (x in 1:ncol(.[, !(colnames(.) %in% c('k_mm', 'profit.norm'))])) {
                        .[, x] <- .[, x] - .[, x] %% 5
                    }
                    return(.)        
                }    
            return(clustFull.data)
        }) 
    #
    return(cluster_data)
}