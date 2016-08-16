# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Функции для анализа временных параметров бектеста
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
###
#' Таблица временных параметров работы стратегии
#'
#' Формирует таблицу временных параметров работы стратегии
#' 
#' @param data xts с данными отработки стратегии
#' @param from.date Дата начала торговли
#' @param to.date Дата окончания торговли
#' @param period Период свечей
#'  
#' @return datesTable Таблица с временными параметрами
#'
#' @export
DateTable <- function(data) {
  # Зависимости:
  require(PerformanceAnalytics)
  # ----------
  #
  # cat("INFO(DateTable):  Calc Date Metrics", "\n", sep = "  ")
  trading.days <- 
    index(data) %>%
    CalcTradingDays(x = .)
  # начало торговли
  from.date <- 
    first(data) %>%
    index(.)
  # конец торговли
  to.date <-
    last(data) %>%
    index(.)
  # периодичность входных данных
  period <- 
    periodicity(data) %>%
    {
      paste(.[[2]], "mins")
    }
  ### таблица временных метрик
  datesTable <- cbind.data.frame(from.date, to.date, period, trading.days)
  colnames(datesTable) <- c("StartDate", "EndDate", "Period", "NumTradeDays")
  #
  return(datesTable)
}
#
###
#' Вычисление торговых дней
#'
#' Возвращает количество торговых дней за период  
#' 
#' @param x Временной ряд жля анализа
#' @param fullDays Полный/неполный день
#'  
#' @return tradingDays Число торговых дней
#'
#' @export
CalcTradingDays <- function(x, fullDays = FALSE) {
  #
  tradingDays <- ndays(x)
  #
  if (fullDays == TRUE) {
    tradingDays <- tradingDays - 1
  }
  #
  return(tradingDays)
}
#
