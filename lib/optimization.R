# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Функции для оптимизации стратегий:
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
    FUN.StrategyGear <- match.fun(FUN.StrategyGear)
    
    # вычисления
    result <- foreach(i = 1:workers, .inorder = FALSE, .combine = rbind) %dopar% {
        BruteForceOptimizer(
            var_df = 
                Delegate(i, nrow(var_df), p = workers) %>% 
                var_df[., ], 
            FUN.StrategyGear = FUN.StrategyGear, 
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
#' Функция BF оптимизации параметров стратегии 
#' 
#' @param var_df Оптимизационная матрица параметров стратегии
#' @param FUN.StrategyGear Функция стратегии
#' @param fast Упрощенный/полный perfomance анализ
#' @param ohlc_args Лист с параметрами котировок
#' @param str_args Лист с параметрами стратегии
#'
#' @return df data.frame с perfomance'ами по всем итерациям цикла 
#'
#' @export
BruteForceOptimizer <- function(var_df, 
                                FUN.StrategyGear,
                                fast = FALSE,
                                ohlc_args, trade_args) { 
    #
    FUN.StrategyGear <- match.fun(FUN.StrategyGear)
    df <- 
        lapply(1:nrow(var_df),
            function(x) {
                OneThreadRun(FUN.StrategyGear,
                    fast = fast, 
                    dd_data_output = FALSE,
                    ohlc_args, 
                    trade_args, 
                    do.call(list, var_df[x, ]))
            }) %>%
        {
            .[!is.null(.)]
        } %>%
        MergeData_inList.byRow(.)    
    #
    return(df)
}
#
###
#' Функция одного прогона вычислений движка стратегии (на готовых данных)
#' 
#' @param data Данные для КА
#' @param cluster_args Лист с параметрами кластеризации (accuracy, 
#'  iteration_max, max, plusplus, round_type, win_size)
#'
#' @return clustFull.data Данные КА
#'
#' @export
BruteForceOptimizer.cluster_analysis <- function(data, cluster_args) {
    ## Подготовка к КА
    data_for_cluster <- ClusterAnalysis.preparation(data, 
        n.mouth = cluster_args$win_size, 
        hi = TRUE, q.hi = 0.5, 
        only_profitable = cluster_args$only_profitable)
    data_for_cluster$profit <- NULL
    data_for_cluster$draw <- NULL
    if (nrow(data_for_cluster) < 50) {
        cluster_args$only_profitable <- FALSE
        clustFull.data <- BruteForceOptimizer.cluster_analysis(data, cluster_args)
        return(clustFull.data)
    } 
    ## Вычисление кластеров
    if (cluster_args$method %in% c('kmeans','plusplus')) {
        clustFull.data <- 
            ClusterAnalysis.parameters(data = data_for_cluster, 
                method = cluster_args$method,
                k.max = cluster_args$k.max,
                iter.max = cluster_args$iter.max,
                nstart =  cluster_args$nstart) %>%
            .[[2]] %>%
            ClusterAnalysis(data = data_for_cluster, 
                method = cluster_args$method,
                n.opt = ., 
                var.digits = 3,
                iter.max = cluster_args$iter.max,
                nstart =  cluster_args$nstart)
        ## Округление центров до значений точек пространства    
        if (!is.null(cluster_args$round_type)) {
            clustFull.data[[2]] %<>% {
                for (x in 1:ncol(.[, !(colnames(.) %in% c('k_mm', 'profit.norm'))])) {
                    if (cluster_args$round_type == 'space') {
                        .[, x] <- .[, x] - .[, x] %% 5    
                    }
                    if (cluster_args$round_type == 'integer') {
                        .[, x] <- as.integer(.[, x])    
                    }                
                    if (cluster_args$round_type == 'round') {
                        .[, x] <- round(.[, x])    
                    }
                }
                return(.)        
            }    
        }
    }
    if (cluster_args$method == 'pam') {
        clustFull.data <- 
            ClusterAnalysis.parameters(data = data_for_cluster, 
                method = cluster_args$method,
                k.max = cluster_args$k.max) %>%
            .[[2]] %>%
            ClusterAnalysis(data = data_for_cluster, 
                method = cluster_args$method,
                n.opt = ., 
                var.digits = 3)
    }
    if (cluster_args$method == 'clara') {
        clustFull.data <- 
            ClusterAnalysis.parameters(data = data_for_cluster, 
                method = cluster_args$method,
                k.max = cluster_args$k.max,
                samples = cluster_args$samples) %>%
            .[[2]] %>%
            ClusterAnalysis(data = data_for_cluster, 
                method = cluster_args$method,
                n.opt = ., 
                var.digits = 3,
                samples = cluster_args$samples)
    }
    return(clustFull.data)
}
#
###
#' Функция одного прогона вычислений движка стратегии (на готовых данных)
#' 
#' @param FUN.StrategyGear Функция движка стратегии
#' @param fast Упрощенный/полный perfomance анализ
#' @param dd_data_output Вывод данных по dd (T/F)
#' @param ohlc_args
#' @param trade_args
#' @param str_args
#'
#' @return perfomanceTable Строка с perfomance-данными прогона робота
#'
#' @export
OneThreadRun <- function(FUN.StrategyGear, 
                         fast = FALSE, 
                         dd_data_output = FALSE,
                         ohlc_args, trade_args, str_args) {
    #
    FUN.StrategyGear <- match.fun(FUN.StrategyGear) 
    ### отработка робота
    DATA <- FUN.StrategyGear(ohlc_args, trade_args, str_args)
    # для стратегий, у которых нет сделок
    if (is.null(DATA[[2]])) {
        return(NULL)
    } 
    # чистим от лишних записей
    DATA[[2]] <- StateTable.clean(DATA[[2]])
    
    ### Анализ perfomanc'ов
    # формирование таблицы сделок
    if (fast == TRUE) {
        # оценка perfomance-параметров
        perfomanceTable <- PerfomanceTable(DATA, 
            trade_table = 0,
            balance = trade_args$balance_start, 
            ret_type = 0, 
            fast = TRUE)    
    } else {
        # лист с данными по сделкам (по тикерам и за всю корзину)
        tradeTable <- TradeTable.calc(DATA[[2]], basket = FALSE, convert = TRUE)
        if (length(ohlc_args$ticker) == 1) {
            tradeTable[[1]]$TradeReturnPercent <- tradeTable[[1]]$TradeReturn * 100 / trade_args$balance_start
        }
        # оценка perfomance-параметров
        perfomanceTable <- PerfomanceTable(DATA, 
            trade_table = tradeTable,
            balance_start = trade_args$balance_start, 
            ret_type = trade_args$return_type,
            dd_data_output = dd_data_output)
        if (dd_data_output == TRUE) {
            dd_data_output.list <- perfomanceTable$dd.list
            perfomanceTable <- perfomanceTable$perfomance_table
        }
    }
    # добавление использованных параметров
    var_names <- names(str_args)[grep('_', names(str_args))]
    for (i in 1:length(var_names)) {
        temp_text <- paste0(
            'perfomanceTable %<>% cbind.data.frame(.,',var_names[i],' = ',str_args[[var_names[i]]],')'
        )
        eval(parse(text = temp_text))
    }
    #
    if (dd_data_output == TRUE) {
        return(list(perfomanceTable = perfomance_table, dd.list = dd_data_output.list))
    }
    #
    return(perfomanceTable)
}
#
