source("libEva_Ratio.R")
source("libEva_Drawdown.R")
source("libEva_DealsTable.R")
source("libEva_Dates.R")
#
CalcPerfomanceTable <- function(data, returns, 
                                from.date, to.date, period) {
  # простые временные параметры
  datesTable <- DateTable(data, from.date, to.date, period)
  # расчёт коэффициентов
  ratioTable <- RatioTable(returns, ret.type)
  # расчёт drawdown'ов
  drawdownTable <- DrawdownTable(returns, ret.type, period = period)
  #
  # итоговая таблица
  perfomanceTable <- cbind.data.frame(datesTable, ratioTable, drawdownTable)
  return(perfomanceTable)
}
