# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Функции для оптимизации стратегий:
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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
    #FUN.StrategyGear <- match.fun(FUN.StrategyGear)
    df <- 
        lapply(1:nrow(var_df),
            function(x) {
                OneThreadRun(match.fun(FUN.StrategyGear),
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
    for (i in 1:length(str_args)) {
        temp_text <- paste0(
            'perfomanceTable %<>% cbind.data.frame(.,',names(str_args)[i],' = ',str_args[[names(str_args)[i]]],')'
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