# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Функции для расчёта perfomance-метрик 
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
### Загрузка дочерних библиотек
source("lib/evaluation/evaluation_dates.R")
source("lib/evaluation/evaluation_deals.R")
source("lib/evaluation/evaluation_drawdown.R")
source("lib/evaluation/evaluation_profit.R")
source("lib/evaluation/evaluation_ratio.R")
#
###
#' Расчет perfomance-метрик (полный набор)
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
                                balance, ret.type, fast = FALSE, ...) {
  #
  ## Если расчёт в fast режиме (нужно для rolling оптимизации и кластеризации) 
  if (fast == TRUE) {
    # вычисление максимальной просадки (в процентах)
    drawdowns <- CalcDrawdownDataSet(data = data$balance, fullData = TRUE)
    # max.drawdown <- 
    #   min(drawdowns[[2]]$Depth) %>%
    #   as.numeric(.)
    max.drawdown.percent <-
      min(drawdowns[[2]]$DepthPercent) %>%
      as.numeric(.)  
    remove(drawdowns)
    # вычисление итоговой доходности (в процентах)
    fullReturn <- 
      last(data$equity) %>%
      as.numeric(.)
    fullReturn.percent <- fullReturn * 100 / balance   
    #
    # sharp.data <-  
    #   SharpeRatio.annualized(returns, scale = 1, geometric = TF) %>%
    #   TransformMetric(., metric.name = "SharpRatio")
    #
    perfomanceTable <- data.frame(Return = fullReturn.percent, MaxDrawdown = max.drawdown.percent)
    return(perfomanceTable)
  }
  ### Есди расчёт полных метрик:
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


            