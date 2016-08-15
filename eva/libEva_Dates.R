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
  trading.days <- CalcTradingDays(data)
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
#' @param data xts с данными отработки стратегии
#'  
#' @return tradingDays Число торговых дней
#'
#' @export
CalcTradingDays <- function(data) {
  #
  tradingDays <- 
    index(data) %>%
    ndays(.)
  #
  return(tradingDays)
}
#
