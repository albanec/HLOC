# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Функции расчета метрик доходности:
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
###
#' Расчёт итоговой таблицы с данными по доходностям 
#'
#' Функция вычисляет параметры по доходностям (дней и сделок) и формирует итоговый DF 
#' 
#' @param asset Анализируемые данные баланса (balance + im.balance или eqiuty)
#' @param trade_table Данные таблицы сделок
#' @param balance_start Стартовый баланс
#' @param by Расчет по дням, трейдам или всё ('days', 'trade', 'both')
#' @param ...
#'
#' @return profitTable DF с данными по profit'у
#'
#' @export
ProfitTable <- function(asset, trade_table, balance_start, by = 'both',
                        ...) {
    ### расчёт итоговой доходности 
    # здесь для анализа используется equty, чтобы лишний раз не считать разницу
    full_return <- 
        xts::last(asset) %>%
        as.numeric(.)
    full_return_percent <- full_return * 100 / balance_start
    ## доходность в год
    full_return_annual <- 
        index(asset) %>%
        ndays(.) %>%
        {
            full_return * 250 / .
        }        
    ## доходность в месяц
    full_return_monthly <-
        index(asset) %>%
        ndays(.) %>%
        {
            full_return * 20 / .
        }
 
    ### расчёт метрик по дням
    if (by %in% c('days', 'both')) {
        profit_table_byDays <- ProfitTable.byDays(asset)
    }
    ### расчёт метрик по сделкам для корзины
    if (by %in% c('trade', 'both')) {
        profit_table_byTrade <-    
            ProfitTable.byTrade(trade_table[[1]]) %>%
            {
                .[[1]]
            }
    }
    ### формирование итогового DF
    profit_table <- 
        data.frame(
            Profit = full_return,
            ProfitPercent = full_return_percent,
            ProfitAnnual = full_return_annual,
            ProfitMonthly = full_return_monthly,
            row.names = NULL
        ) %>%
        {
            x <- .
            args <- list(...) 
            args_names <- names(args)
            if (('nbar' %in% args_names) && ('nbar.trade' %in% args_names) == TRUE ) {
                full_return_bar <- full_return / args$nbar
                full_return_nbar_trade <- full_return / args$nbar.trade
                x <- cbind(x, ProfitBars = full_return_bar, ProfitBarsIn = full_return_nbar_trade)
            }
            return(x)
        }
    if (by %in% 'days') {
        profit_table %<>% cbind(., profit_table_byDays)    
    }
    if (by %in% 'trade') {
        profit_table %<>% cbind(., profit_table_byTrade)    
    }
    if (by %in% 'both') {
        profit_table %<>% cbind(., profit_table_byDays, profit_table_byTrade)
    }
    #
    return(profit_table)
}
#
