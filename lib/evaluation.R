# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Функции для расчёта perfomance-метрик 
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
###
#' Расчет perfomance-метрик (полный набор)
#'
#' Расчитывает perfomance-метрики (статистика по временным метрикам (datesTable) + доходности (profit_table) + 
#' просадкам (DrawdownsTable) + коэффициентам продуктивности (ratioTable)) 
#' по данным отработки стратегии (data_strategy.list[[1]]) 
#' 
#' @param DATA Лист данных стратегии
#' @param trade_table Таблица сделок
#' @param asset_cols Имена колонок с asset-параметром (balance + im.balance)
#' @param balance_start Стартовый баланс
#' @param ret_type Тип return'а
#' @param fast Быстрый расчёт (считаются только х-ки для КА)
#' @param dd_data_output Вывод данных по dd (T/F)
#' @param trade_stats Расчёт статистики по сделкам (T/F)
#'    
#' @return perfomance_table Итоговая perfomance-таблица (list)
#'
#' @export
PerfomanceTable <- function(DATA,
                            trade_table,
                            asset_cols = c('balance', 'im.balance'),
                            balance_start, 
                            ret_type, 
                            fast = FALSE, 
                            dd_data_output = FALSE,
                            trade_stats = TRUE) {
    #
    asset <-    
        DATA[[1]][, which(colnames(DATA[[1]]) %in% asset_cols)] %>% 
        rowSums() %>%
        xts(., order.by = index(DATA[[1]]))
    colnames(asset) <- c('asset')

    # Если расчёт в fast режиме (нужно для rolling оптимизации и кластеризации) 
    if (fast == TRUE) {
        rm(trade_table, ret_type)
        # вычисление максимальной просадки (в процентах)
        drawdowns <- Drawdowns(data = asset, fullData = TRUE)
        # max.drawdown <- 
        #     min(drawdowns[[2]]$Depth) %>%
        #     as.numeric(.)
        max_drawdown_percent <-
            min(drawdowns[[2]]$DepthPercent) %>%
            as.numeric(.) 
        remove(drawdowns)
        # вычисление итоговой доходности (в процентах)
        full_return <- as.numeric(xts::last(asset)) - as.numeric(xts::first(asset))
        full_return_percent <- full_return * 100 / balance_start
        #
        # sharp.data <-    
        #     SharpeRatio.annualized(DATA[[1]]$perfReturn, scale = 1, geometric = TF) %>%
        #     Ratio.transformMetric(., metric.name = 'SharpRatio')
        #
        perfomance_table <- data.frame(ProfitPercent = full_return_percent, DrawdownMaxPercent = max_drawdown_percent)
        #
        return(perfomance_table)
    }
    
    # Если расчёт полных метрик:
    cat('INFO(PerfomanceTable):    Calc PerfomanceMetrics ... Start', '\n')
    ### простые временные метрики
    cat('INFO(PerfomanceTable):    Calc DatesMetrics', '\n', sep = '    ')
    dates_table <- DatesTable(data = asset, states = DATA[[2]])        
    ### расчёт drawdown'ов
    cat('INFO(PerfomanceTable):    Calc DrawdownsTable', '\n')
    drawdowns_table <- DrawdownsTable(data_balance = asset, dd_data_output = dd_data_output)
    if (dd_data_output == TRUE) {
        drawdowns_data_output.list <- drawdowns_table$dd.list 
        drawdowns_table <- drawdowns_table$drawdown_table
    }
    ### profit метрики
    cat('INFO(PerfomanceTable):    Calc ProfitTable', '\n')
    profit_table <- ProfitTable(asset, 
        trade_table = trade_table, 
        #DrawdownsTable = drawdowns_table,
        balance_start = balance_start, 
        by = ifelse(trade_stats == TRUE, 'both', 'days'),
        nbar = dates_table$BarsNum,
        nbar.trade = dates_table$BarsNumIn)
    ### расчёт коэффициентов
    cat('INFO(PerfomanceTable):    Calc RatioTable', '\n')
    ratio_table <- RatioTable(DATA[[1]]$perfReturn, ret_type = ret_type)
    ### фактор восстановления
    rf <-
        profit_table$Profit / drawdowns_table$DrawdownMax %>%
        abs(.) %>%
        data.frame(RatioRecoveryFactor = .)
    ### коэф. выигрыша
    if (trade_stats == TRUE) {
        payoff_ratio <- 
            profit_table$TradeWinAverageProfit / profit_table$TradeLossAverageLoss %>%
            abs(.) %>%
            data.frame(RatioPayoff = .)
    } else {
        payoff_ratio <- data.frame(RatioPayoff = NA)
    }
    ### итоговая таблица
    cat('INFO(PerfomanceTable):    Build PerfomanceTable', '\n')
    perfomance_table <- cbind(dates_table, 
        ratio_table, 
        rf, 
        payoff_ratio, 
        drawdowns_table, 
        profit_table)
    cat('INFO(PerfomanceTable):    Calc PerfomanceMetrics ... OK', '\n')
    
    # если нужно вытащить данные просадке
    if (dd_data_output == TRUE) {
        return(list(perfomance_table = perfomance_table, dd.list = drawdowns_data_output.list))
    }
    #
    return(perfomance_table)
}


                        