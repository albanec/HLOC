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
DateTable <- function(data, period) {
  # Зависимости:
  require(PerformanceAnalytics)
  # ----------
  #
  cat("INFO(DateTable):  Build Date Metrics", "\n", sep = "  ")
  trading.days <- CalcTradingDays(data)
  from.date <- 
    first(data) %>%
    index(.)
  to.date <-
    last(data) %>%
    index(.)
  datesTable <- cbind.data.frame(from.date, to.date, period, trading.days)
  colnames(datesTable) <- c("Начальная дата", "Конечная дата", "Период", "Число торговых дней")
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
  return(tradingDays)
}
#