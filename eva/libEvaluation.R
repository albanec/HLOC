# source("libEva_Dates.R")
# source("libEva_Deals.R")
# source("libEva_Drawdown.R")
# source("libEva_Profit.R")
# source("libEva_Ratio.R")
#
###
#' Расчет perfomance-метрик 
#'
#' Расчитывает perfomance-метрики (статистика по временным метрикам (datesTable) + доходности (profitTable) + 
#' просадкам (drawdownTable) + коэффициентам продуктивности (ratioTable)) 
#' по данным отработки стратегии (data.strategy.list[[1]]) 
#' 
#' @param data Входной xts данных отработки стратегии 
#' @param  balance Стартовый баланс
#'  
#' @return perfomanceTable Итоговая perfomance-таблица
#'
#' @export
CalcPerfomanceTable <- function(data, data.state, balance, ret.type, ...) {
  #
  ### Расчёт метрик
  cat("INFO(CalcPerfomanceTable):  Calc PerfomanceMetrics ... Start ", "\n", sep = "  ")
  # простые временные метрики
  cat("INFO(CalcPerfomanceTable):  Calc DatesMetrics", "\n", sep = "  ")
  datesTable <- DatesTable(data = data, data.state = data.state)
  # profit метрики
  cat("INFO(CalcPerfomanceTable):  Calc ProfitTable", "\n", sep = "  ")
  profitTable <- ProfitTable(data = data, balance = balance.start, 
                             nbar = datesTable$NumBars, nbar.trade = datesTable$NumBarsTrade)
  # расчёт drawdown'ов
  cat("INFO(CalcPerfomanceTable):  Calc DrawdownTable", "\n", sep = "  ")
  drawdownTable <- DrawdownTable(data.balance = data$balance)
  # расчёт коэффициентов
  cat("INFO(CalcPerfomanceTable):  Calc RatioTable", "\n", sep = "  ")
  ratioTable <- RatioTable(returns = data$perfReturn, ret.type)
  #
  ### итоговая таблица
  cat("INFO(CalcPerfomanceTable):  Build PerfomanceTable", "\n", sep = "  ")
  perfomanceTable <- cbind.data.frame(datesTable, ratioTable, drawdownTable, profitTable)
  #
  cat("INFO(CalcPerfomanceTable):  Calc PerfomanceMetrics ... OK", "\n", sep = "  ")
  #
  return(perfomanceTable)
}
