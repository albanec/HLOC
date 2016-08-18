# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Функции для анализа временных параметров бектеста
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
###
#' Таблица временных параметров работы стратегии
#'
#' Формирует таблицу временных параметров работы стратегии
#' 
#' @param data XTS с данными отработки стратегии
#' @param data.state XTS с данными состояний стратегии
#' @param from.date Дата начала торговли
#' @param to.date Дата окончания торговли
#' @param period Период свечей
#'  
#' @return datesTable Таблица с временными параметрами
#'
#' @export
DatesTable <- function(data, data.state) {
  # Зависимости:
  require(PerformanceAnalytics)
  # ----------
  #
  # cat("INFO(DateTable):  Calc Date Metrics", "\n", sep = "  ")
  trading.days <- 
    index(data) %>%
    CalcTradingDays(x = .)
  ### начало торговли
  from.date <- 
    first(data) %>%
    index(.)
  ### конец торговли
  to.date <-
    last(data) %>%
    index(.)
  ### периодичность входных данных
  period <- 
    periodicity(data) %>%
    {
      paste(.[[2]], "mins")
    }
  ### всего баров
  nbar <- nrow(data)
  ### бары в рынке
  nbar.trade <-
    index(data.state) %>%
    {
      data.state[duplicated(.) == FALSE]
    } %>%
    {
      x <- .
      temp.index <- which(!duplicated(x$pos.num, fromLast = TRUE))    
      result <- x[temp.index]$pos.ticks
    } %>%
    sum(.)
  ### бары вне рынка
  nbar.out <- nbar - nbar.trade
  ### таблица временных метрик
  datesTable <- cbind.data.frame(from.date, to.date, period, trading.days, nbar, nbar.trade, nbar.out)
  colnames(datesTable) <- c("StartDate", "EndDate", "Period", "NumTradeDays", 
                            "NumBars", "NumBarsTrade", "NumBarsNoTrade")
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
