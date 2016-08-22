# source("libEva_Dates.R")
# source("libEva_Deals.R")
# source("libEva_Drawdown.R")
# source("libEva_Profit.R")
# source("libEva_ProfitDays.R")
# source("libEva_ProfitDeals.R")
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
#' @return perfomanceTable.list Итоговая perfomance-таблица (list)
#'
#' @export
CalcPerfomanceTable <- function(data, data.state, dealsTable,
                                balance, ret.type, ...) {
  #
  ### Расчёт метрик
  cat("INFO(CalcPerfomanceTable):  Calc PerfomanceMetrics ... Start", "\n")
  ## простые временные метрики
  cat("INFO(CalcPerfomanceTable):  Calc DatesMetrics", "\n", sep = "  ")
  datesTable <- DatesTable(data = data, data.state = data.state)    
  ## расчёт drawdown'ов
  cat("INFO(CalcPerfomanceTable):  Calc DrawdownTable", "\n")
  drawdownTable <- DrawdownTable(data.balance = data$balance)
  ## profit метрики
  cat("INFO(CalcPerfomanceTable):  Calc ProfitTable", "\n")
  profitTable <- ProfitTable(data = data, dealsTable = dealsTable, drawdownTable = drawdownTable,
                                  balance = balance.start, 
                                  nbar = datesTable$NumBars, nbar.trade = datesTable$NumBarsTrade)
  ## расчёт коэффициентов
  cat("INFO(CalcPerfomanceTable):  Calc RatioTable", "\n")
  ratioTable <- RatioTable(returns = data$perfReturn, ret.type)
  # фактор восстановления
  rf <- 
    profitTable$Return / drawdownTable$MaxDrawdown %>%
    abs(.) %>%
    data.frame(RecoveryFactor = .)
  # коэф. выигрыша
  win.ratio <- 
    profitTable$MeanGoodDealReturn / profitTable$MeanBadDealReturn %>%
    abs(.) %>%
    data.frame(WinRatio = .)
  #
  ### итоговая таблица
  cat("INFO(CalcPerfomanceTable):  Build PerfomanceTable", "\n")
  perfomanceTable <- cbind(datesTable, ratioTable, 
                           rf, win.ratio, 
                           drawdownTable, profitTable)
 #
  cat("INFO(CalcPerfomanceTable):  Calc PerfomanceMetrics ... OK", "\n")
  #
  return(perfomanceTable)
}


            