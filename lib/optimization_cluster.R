# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# SOCK-кластер функции для оптимизации стратегий:
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
###
#' Функция BF оптимизации движка стратегии (на SOCK-кластерах)
#' 
#' @param var.df DF с данными для перебора
#' @param ohlc_data XTS с полными котировками
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
#' @param export_varlist Вектор имён переменных, экспортируемых из .GlobalEnv в кластер (необязательно)
#' @param export_libs Вектор имён библиотек, экспортируемых в кластер
#' @param fast Упрощенный/полный perfomance анализ
#' @param eval_string Сторока с описанием переменных внутри lapply-цикла
#' @param 
#'
#' @return result DF с perfomance'ами по всем итерациям цикла 
#'
#' @export
BruteForceOpt_cl <- function(var.df, ohlc_data,
                             from_date, to_date, lookback = FALSE,
                             FUN, 
                             linker_file = 'bots/test/linker.R',
                             balance_start, slips, commissions,
                             expiration, ticker, return_type = 'ret',
                             export_varlist = NULL, 
                             export_libs = c('quantmod', 'xts', 'magrittr', 'tidyr', 
                                 'PerformanceAnalytics', 'lubridate'),
                             fast = FALSE, 
                             eval_string) {
    #
    .CurrentEnv <- environment()
    #
    require(parallel)
    FUN <- match.fun(FUN)

    # запуск кластера
    parallel_cluster <- 
        detectCores() %>%
        makeCluster(.)#, type = 'PSOCK')
    ## Подгрузка данных в кластер
    clusterExport(parallel_cluster, envir = .CurrentEnv, 
        varlist = c('ohlc_data', 'from_date', 'to_date', 'lookback',
            'FUN', 'linker_file', 
            'balance_start', 'slips', 'commissions', 'expiration', 
            'ticker', 'return_type', 'fast', 
            'export_varlist', 'export_libs', 'eval_string'))
    # подгрузка библиотек
    clusterEvalQ(parallel_cluster, 
        { 
            sapply(export_libs, library, character.only = TRUE)
            source(linker_file) 
        })
    # подгрузка дополнительных переменных напрямую из .GlobalEnv (если нужно)
    if (!is.null(export_varlist)) {
        clusterExport(parallel_cluster, envir = .GlobalEnv, 
            varlist = export_varlist) 
    }
    #
    temp_text <- paste0(
        'result <- 
            var.df %>%
            parRapply(parallel_cluster, 
                .,    
                function(x) {
                    FUN <- match.fun(FUN)
                    df <- FUN(data.xts = ohlc_data,
                        from_date, to_date, lookback,
                        fast = fast, 
                        balance_start = balance_start, slips = slips, commissions = commissions,
                        expiration = expiration, ticker = ticker, return_type = return_type, ', 
                        eval_string,')
                    return(df)
                })'
    )
    eval(parse(text = temp_text))
    rm(temp_text)
    # остановка кластера
    stopCluster(parallel_cluster)
    parallel_cluster <- c()    
    
    # объединение результатов
    result %<>%    
        {
            .[!is.null(.)]
        } %>%
        MergeData_inList.byRow(.)
    
    # проверка остановки кластера
    if(!is.null(parallel_cluster)) {
        stopCluster(parallel_cluster)
        parallel_cluster <- c()
    }
    #    
    return(result)
}
#
###
#' Roller функция обучающей оптимизации движка стратегии (на SOCK кластерах)
#' 
#' @param slice_index Временные интервалы оптимизационных окон (индексы start/end)
#' @param ohlc_data XTS с исходными котировками
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
#' @param export_varlist Вектор имён переменных, экспортируемых из .GlobalEnv в кластер (необязательно)
#' @param export_libs Вектор имён библиотек, экспортируемых в кластер
#' @param fast Упрощенный/полный perfomance анализ
#' @param eval_string Сторока с описанием переменных внутри lapply-цикла
#' @param 
#'
#' @return result DF с perfomance'ами по всем итерациям цикла 
#'
#' @export                             
RollerOpt_learning_cl <- function(slice_index, ohlc_data,
                                  var.df,
                                  FUN, win_size,
                                  linker_file = 'bots/test/linker.R',
                                  balance_start, slips, commissions,
                                  expiration, ticker, return_type = 'ret',
                                  export_varlist = NULL,
                                  export_libs = c('quantmod', 'xts', 'magrittr', 'tidyr', 
                                      'PerformanceAnalytics', 'lubridate'),
                                  fast = FALSE, 
                                  eval_string) {
    #
    .CurrentEnv <- environment()                                                    
    #
    require(parallel)
    FUN <- match.fun(FUN)
    
     # запуск кластера
    parallel_cluster <- 
        detectCores() %>%
        makeCluster(.)#, type = 'PSOCK')
    ## Подгрузка данных в кластер
    clusterExport(parallel_cluster, envir = .CurrentEnv, 
        varlist = c('FUN', 'linker_file', 
            'balance_start', 'slips', 'commissions', 'expiration', 
            'ticker', 'return_type', 'fast', 
            'export_varlist', 'export_libs', 'eval_string'))
    # подгрузка библиотек
    clusterEvalQ(parallel_cluster,
        { 
            sapply(export_libs, library, character.only = TRUE)
            source(linker_file) 
        })
    # подгрузка дополнительных переменных напрямую из .GlobalEnv (если нужно)
    if (!is.null(export_varlist)) {
        clusterExport(parallel_cluster, envir = .GlobalEnv, 
            varlist = export_varlist) 
    }
    # Вычисление оптимизаций на обучающих периодах
    bf_data.list <- lapply(slice_index$widthSlice, 
        function(x) {
            temp_slice_index <- x 
            temp_text <- paste0(
                'result <- 
                    var.df %>%
                    parRapply(parallel_cluster,
                        ., 
                        function(x) {
                            FUN <- match.fun(FUN)
                            df <- FUN(ohlc_data = ohlc_data,
                                from_date = temp_slice_index[1, ], 
                                to_date = temp_slice_index[2, ], 
                                lookback = TRUE,
                                fast = fast, 
                                balance_start = balance_start, 
                                slips = slips, commissions = commissions,
                                expiration = expiration, ticker = ticker, 
                                return_type = return_type, ',eval_string,')
                            return(df)
                        })')
            eval(parse(text = temp_text))
            rm(temp_text)
            result %<>%
                {
                    .[!is.null(.)]
                } %>%
                MergeData_inList.byRow(.)
            return(result)
        })
    # остановка кластера
    stopCluster(parallel_cluster)
    parallel_cluster <- c()    
    # КА
    cluster_data.list <- lapply(bf_data.list,
        function(x) {
            ## Подготовка к КА
            data_for_cluster <- CalcKmean.preparation(data = x, 
                n.mouth = win_size, 
                hi = TRUE, q.hi = 0.5, 
                only_profitable = TRUE)
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
                    var.digits = 0)
            ## округление центров до значений точек пространства    
            clustFull.data[[2]] %<>%
                {
                    for (i in 1:ncol(.[, !(colnames(.) %in% c('k_mm', 'profit.norm'))])) {
                        .[, i] <- .[, i] - .[, i] %% 5
                    }
                    return(.)        
                }
            return(clustFull.data)    
        })    
    # проверка остановки кластера
    if (!is.null(parallel_cluster)) {
        stopCluster(parallel_cluster)
        parallel_cluster <- c()
    }
    return(cluster_data.list)
}
#