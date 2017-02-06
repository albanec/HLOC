# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Функции для расчёта perfomance-метрик 
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
###
#' Расчет perfomance-метрик (полный набор)
#'
#' Расчитывает perfomance-метрики (статистика по временным метрикам (datesTable) + доходности (profit_table) + 
#' просадкам (DrawdownsTable) + коэффициентам продуктивности (ratioTable)) 
#' по данным отработки стратегии (data_strategy.list[[1]]) 
#' 
#' @param data Входной xts данных отработки стратегии (data_strategy.list[[1]])
#' @param  balance Стартовый баланс
#'  
#' @return perfomanceTable.list Итоговая perfomance-таблица (list)
#'
#' @export
PerfomanceTable <- function(data = data_strategy.list[[1]], states = data_strategy.list[[2]], 
                            trades_table = trades_table.list,
                            balance_start, ret_type, fast = FALSE, dd_data_output = FALSE) {
  #
  ## Если расчёт в fast режиме (нужно для rolling оптимизации и кластеризации) 
  if (fast == TRUE) {
    # вычисление максимальной просадки (в процентах)
    drawdowns <- Drawdowns.dd_data(data = data$balance + data$im.balance, fullData = TRUE)
    # max.drawdown <- 
    #   min(drawdowns[[2]]$Depth) %>%
    #   as.numeric(.)
    max_drawdown_percent <-
      min(drawdowns[[2]]$DepthPercent) %>%
      as.numeric(.) 
    remove(drawdowns)
    # вычисление итоговой доходности (в процентах)
    full_return <- 
      xts::last(data$equity) %>%
      as.numeric(.)
    full_return_percent <- full_return * 100 / balance_start   
    #
    # sharp.data <-  
    #   SharpeRatio.annualized(returns, scale = 1, geometric = TF) %>%
    #   Ratio.transformMetric(., metric.name = 'SharpRatio')
    #
    perfomance_table <- data.frame(ProfitPercent = full_return_percent, DrawdownMaxPercent = max_drawdown_percent)
    #
    return(perfomance_table)
  }
  ### Есди расчёт полных метрик:
  cat('INFO(PerfomanceTable):  Calc PerfomanceMetrics ... Start', '\n')
  ## простые временные метрики
  cat('INFO(PerfomanceTable):  Calc DatesMetrics', '\n', sep = '  ')
  dates_table <- DatesTable(data = data, states = states)    
  ## расчёт drawdown'ов
  cat('INFO(PerfomanceTable):  Calc DrawdownsTable', '\n')
  drawdowns_table <- DrawdownsTable(data_balance = data$balance + data$im.balance, dd_data_output = dd_data_output)
  if (dd_data_output == TRUE) {
    drawdowns_data_output.list <- drawdowns_table$dd.list 
    drawdowns_table <- drawdowns_table$drawdown_table
  }
  ## profit метрики
  cat('INFO(PerfomanceTable):  Calc ProfitTable', '\n')
  profit_table <- ProfitTable(data = data, trades_table = trades_table, #DrawdownsTable = drawdowns_table,
                              balance_start = balance_start, 
                              nbar = dates_table$BarsNum, nbar.trade = dates_table$BarsNumIn)
  ## расчёт коэффициентов
  cat('INFO(PerfomanceTable):  Calc RatioTable', '\n')
  ratio_table <- RatioTable(returns = data$perfReturn, ret_type = ret_type)
  # фактор восстановления
  rf <-
    profit_table$Profit / drawdowns_table$DrawdownMax %>%
    abs(.) %>%
    data.frame(RatioRecoveryFactor = .)
  # коэф. выигрыша
  payoff_ratio <- 
    profit_table$TradesWinAverageProfit / profit_table$TradesLossAverageLoss %>%
    abs(.) %>%
    data.frame(RatioPayoff = .)
  #
  ### итоговая таблица
  cat('INFO(PerfomanceTable):  Build PerfomanceTable', '\n')
  perfomance_table <- cbind(dates_table, ratio_table, 
                           rf, payoff_ratio, 
                           drawdowns_table, profit_table)
 #
  cat('INFO(PerfomanceTable):  Calc PerfomanceMetrics ... OK', '\n')
  #
  if (dd_data_output == TRUE) {
    return(list(perfomance_table = perfomance_table, dd.list = drawdowns_data_output.list))
  }
  return(perfomance_table)
}


            