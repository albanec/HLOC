# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# SOCK-кластер функции для оптимизации стратегий:
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
###
#' Функция BF оптимизации движка стратегии (на SOCK-кластерах)
#' 
#' @param var_df Оптимизационная матрица параметров стратегии
#' @param FUN.StrategyGear Функция стратегии
#' @param fast Упрощенный/полный perfomance анализ
#' @param linker_file Путь к linker файлу
#' @param export_varlist Вектор имён переменных, экспортируемых из .GlobalEnv в кластер (необязательно)
#' @param export_libs Вектор имён библиотек, экспортируемых в кластер
#' @param eval_string Сторока с описанием переменных внутри lapply-цикла
#' @param ohlc_args Лист с параметрами котировок
#' @param str_args Лист с параметрами стратегии
#'
#' @return result DF с perfomance'ами по всем итерациям цикла 
#'
#' @export
BruteForceOptimizer.psock <- function(var_df, 
                                      FUN.StrategyGear
                                      fast = FALSE,
                                      linker_file = 'bots/test/linker.R',
                                      export_varlist = NULL, 
                                      export_libs = c('quantmod', 'xts', 'magrittr', 'tidyr', 
                                          'PerformanceAnalytics', 'lubridate'),
                                      ohlc_args, str_args) {
    #
    require(parallel)
    .CurrentEnv <- environment()

    ## Запуск кластера
    workers <- detectCores() - 1
    parallel_cluster <- makeCluster(workers)#, type = 'PSOCK')
    # подгрузка данных в кластер
    clusterExport(parallel_cluster, envir = .CurrentEnv, 
        varlist = c('var_df', 'ohlc_args', 'str_args'))
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
    
    # Вычисления
    result <- 
        parRapply(parallel_cluster, 
            1:workers,    
            function(x) {
                df <- BruteForceOptimizer(
                    var_df = Delegate(i, nrow(var_df), p = workers) %>% var_df[., ], 
                    FUN.StrategyGear = match.fun(FUN.StrategyGear), 
                    fast = fast,
                    ohlc_args, trade_args)
            }) %>%
        # объединение результатов
        {
            .[!is.null(.)]
        } %>%
        MergeData_inList.byRow(.)
    
    # остановка кластера
    stopCluster(parallel_cluster)
    parallel_cluster <- c()    
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
# ' Roller функция обучающей оптимизации движка стратегии (на SOCK кластерах)
# ' 
# ' @param slice_index Временные интервалы оптимизационных окон (индексы start/end)
# ' @param var_df Оптимизационная матрица параметров стратегии
# ' @param FUN.StrategyGear Функция стратегии
# ' @param win_size Период обучения (нужно для более точной кластеризации)
# ' @param linker_file Путь к linker файлу
# ' @param export_varlist Вектор имён переменных, экспортируемых из .GlobalEnv в кластер (необязательно)
# ' @param ohlc_args Лист с параметрами котировок
# ' @param str_args Лист с параметрами стратегии
# ' @param 
# '
# ' @return result DF с perfomance'ами по всем итерациям цикла 
# '
# ' @export                             

