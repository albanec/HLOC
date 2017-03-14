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
        foreach(i = 1:workers, .combine = rbind) %dopar% {
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