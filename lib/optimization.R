# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Функции для оптимизации стратегий:
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
###
#' Функция BF оптимизации параметров стратегии 
#' 
#' @param DATA data.frame с данными для перебора
#' @param ohlc_source XTS с полными котировками
#' @param from_date Начало торговли
#' @param to_date Конец торговли
#' @param lookback Обучающее окно (перед началом торговли)
#' @param FUN Функция анализа (OneThreadRun ветка функций)
#' @param rolling_opt Упрощенный/полный perfomance анализ
#' @param balance_start Стартовый баланс
#' @param slips Слипы (вектор)
#' @param commissions Комиссии (вектор)
#' @param expiration Вектор с датами экспирации
#' @param ticker Тикер инструмента
#' @param return_type Тип доходности
#' @param eval_string Сторока с описанием координат переменных внутри lapply-цикла
#'
#' @return df data.frame с perfomance'ами по всем итерациям цикла 
#'
#' @export
BruteForceOpt <- function(DATA, ohlc_source, 
                          from_date, to_date, lookback = FALSE,
                          FUN, 
                          rolling_opt = FALSE,
                          balance_start, slips, commissions,
                          expiration, ticker, return_type = 'ret',
                          eval_string) { 
    #
    FUN <- match.fun(FUN)
    
    temp_text <- paste0(
        'df <- lapply(1:nrow(DATA),
            function(x) {
                FUN(ohlc_source = ohlc_source,
                    from_date, to_date, lookback,
                    rolling_opt = rolling_opt, 
                    balance_start = balance_start, slips = slips, commissions = commissions,
                    expiration = expiration, ticker = ticker, return_type = return_type, ',
                    eval_string,')
            })'
    )

    eval(parse(text = temp_text))
    rm(temp_text)
    df %<>%
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
#' @param data.xts XTS с котировками
#' @param FUN Функция движка стратегии
#' @param rolling_opt Упрощенный/полный perfomance анализ
#' @param var_names Вектор имен переменных стратегии
#' @param balance_start Стартовый баланс
#' @param slips Слипы (вектор)
#' @param commissions Комиссии (вектор)
#' @param expiration Вектор с датами экспирации
#' @param ticker Тикер инструмента
#' @param ... Другие данные, нужные для вычислений робота
#'
#' @return perfomanceTable Строка с perfomance-данными прогона робота
#'
#' @export
OneThreadRun <- function(data.xts, FUN,
                         rolling_opt = FALSE, var_names = NULL,
                         balance_start, slips, commissions,
                         expiration, ticker, return_type = 'ret',
                         dd_data_output = FALSE,
                         ...) {
    #
    FUN <- match.fun(FUN) 
    
    ## отработка робота
    data_strategy.list <- FUN(data_source = data.xts, 
        balance_start = balance_start, slips = slips, commiss = commissions,
        exp.vector = expiration, ticker, return_type = return_type, 
        dd_data_output = FALSE,
        ...)
    
    ## Анализ perfomanc'ов
    # для стратегий, у которых нет сделок
    if (is.null(data_strategy.list$states)) {
        return(NULL)
    } else {
        ### Формирование таблицы сделок
        ## чистим от лишних записей
        data_strategy.list[[2]] <- StatesTable.clean(data_strategy.list[[2]])
        if (rolling_opt == TRUE) {
            ### оценка perfomance-параметров
            perfomance_table <- PerfomanceTable(DATA = data_strategy.list[[1]], 
                STATES = 0,
                TRADES = 0,
                balance = balance_start, ret_type = 0, 
                fast = TRUE)    
        } else {
            ## лист с данными по сделкам (по тикерам и за всю корзину)
            trades_table.list <- TradesTable.calc(STATES = data_strategy.list[[2]], basket = FALSE, convert = TRUE)
            if (length(ticker) == 1) {
                trades_table.list[[1]]$TradeReturnPercent <- trades_table.list[[1]]$TradeReturn * 100 / balance_start
            }
            ### оценка perfomance-параметров
            perfomance_table <- PerfomanceTable(DATA = data_strategy.list[[1]], 
                STATES = data_strategy.list[[2]],
                TRADES = trades_table.list,
                balance_start = balance_start, 
                ret_type = return_type,
                dd_data_output = dd_data_output)
            if (dd_data_output == TRUE) {
                dd_data_output.list <- perfomance_table$dd.list
                perfomance_table <- perfomance_table$perfomance_table
            }
        }
    }
    # добавление использованных параметров
    if (!is.null(var_names)) {
        for (i in 1:length(var_names)) {
            dots <- list(...)
            temp_text <- paste0(
                'perfomance_table %<>% cbind.data.frame(.,',var_names[i],' = ',dots[[var_names[i]]],')'
            )
            eval(parse(text = temp_text))
        }
    }
    #
    if (dd_data_output == TRUE) {
        return(list(perfomance_table = perfomance_table, dd.list = dd_data_output.list))
    }
    return(perfomance_table)
}