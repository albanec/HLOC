# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Multicore-функции для оптимизации стратегий:
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
###
#' Функция BF оптимизации движка стратегии (multicore)
#' 
#' @param var.df DF с данными для перебора
#' @param ohlc_source XTS с полными котировками
#' @param from_date Начало торговли
#' @param to_date Конец торговли
#' @param lookback Обучающее окно (перед началом торговли)
#' @param FUN Функция анализа (OneThreadRun ветка функций)
#' @param linker_file Путь к linker файлу
#' @param balance_start Стартовый баланс
#' @param slips Слипы (вектор)
#' @param commissions Комиссии (вектор)
#' @param expiration Вектор с датами экспирации
#' @param ticker Тикер инструмента
#' @param return_type Тип доходности
#' @param fast Упрощенный/полный perfomance анализ
#' @param eval_string Сторока с описанием переменных внутри lapply-цикла
#' @param 
#'
#' @return result DF с perfomance'ами по всем итерациям цикла 
#'
#' @export
BruteForceOpt_mc <- function(var.df, ohlc_source,
                                      from_date, to_date, lookback = FALSE,
                                      FUN, 
                                      linker_file = 'bots/test/linker.R',
                                      balance_start, slips, commissions,
                                      expiration, ticker, return_type = 'ret',
                                      fast = FALSE, 
                                      eval_string) {
    #
    #.CurrentEnv <- environment()
    #
    require(doParallel)
    FUN <- match.fun(FUN)
    
    workers <- detectCores() - 1    
    if (fast == FALSE) {    
        registerDoParallel(cores = workers)    
    }
     
    n_vars <- nrow(var.df)

    result <- foreach(i = 1:workers) %dopar% {
        FUN <- match.fun(FUN)
        map_range <- Delegate(i, n_vars, p = workers)
        x <- var.df[map_range, ]
        df <- BruteForceOpt(DATA = x, 
            ohlc_source = ohlc_source, 
            from_date, to_date, lookback,
            FUN = FUN, 
            fast = fast,
            balance_start = balance_start, slips = slips, commissions = commissions,
            expiration = expiration, ticker = ticker, return_type = return_type,
            eval_string = eval_string)
        return(df)
    }
    # объединение результатов
    result %<>%    
        {
            .[!is.null(.)]
        } %>%
        MergeData_inList.byRow(.)
 
    return(result)
}
#
###
#' Roller функция обучающей оптимизации движка стратегии (multicore)
#' 
#' @param slice_index Временные интервалы оптимизационных окон (индексы start/end)
#' @param ohlc_source XTS с исходными котировками
#' @param var.df DF с данными для перебора
#' @param FUN Функция анализа (OneThreadRun ветка функций)
#' @param win_size Период обучения (нужно для более точной кластеризации)
#' @param linker_file Путь к linker файлу
#' @param balance_start Стартовый баланс
#' @param slips Слипы (вектор)
#' @param commissions Комиссии (вектор)
#' @param expiration Вектор с датами экспирации
#' @param ticker Тикер инструмента
#' @param return_type Тип доходности
#' @param eval_string Сторока с описанием переменных внутри lapply-цикла
#' @param 
#'
#' @return result DF с perfomance'ами по всем итерациям цикла 
#'
#' @export                             
RollerOpt_learning_mc <- function(slice_index, ohlc_source,
                                  var.df,
                                  FUN, win_size,
                                  linker_file = 'bots/test/linker.R',
                                  balance_start, slips, commissions,
                                  expiration, ticker, return_type = 'ret',
                                  eval_string) {
    #
    #.CurrentEnv <- environment()                                                    
    #
    #require(doParallel)
    FUN <- match.fun(FUN)
    
    workers <- detectCores() - 1
    registerDoParallel(cores = workers)  
    
    ## Вычисление оптимизаций на обучающих периодах
    n_vars <- nrow(var.df)
    bf_data.list <- lapply(1:length(slice_index$widthSlice),
        function(x) {
            BruteForceOpt_mc(var.df = var.df, 
                ohlc_source = ohlc_source,
                from_date = slice_index$widthSlice[[x]][1, ], 
                to_date = slice_index$widthSlice[[x]][2, ], 
                lookback = TRUE,
                FUN = FUN, 
                linker_file = 'bots/test/linker.R',
                balance_start = balance_start, slips = slips, commissions = commissions,
                expiration = expiration, ticker = ticker, return_type = return_type,
                fast = TRUE, 
                eval_string = eval_string)   
        })

    ## КА
    cluster_data.list <- lapply(1:length(bf_data.list),
        function(x) {
            ## Подготовка к КА
            data_for_cluster <- 
                bf_data.list[[x]] %>%
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
            #
            return(clustFull.data)
        }) 
    #
    return(cluster_data.list)
}