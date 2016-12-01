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
#' @param states XTS с данными состояний стратегии
#' @param from.date Дата начала торговли
#' @param to.date Дата окончания торговли
#' @param period Период свечей
#'  
#' @return datesTable Таблица с временными параметрами
#'
#' @export
DatesTable <- function(data, states) {
  # Зависимости:
  require(PerformanceAnalytics)
  # ----------
  #
  # cat('INFO(DateTable):  Calc Date Metrics', '\n', sep = '  ')
  trading.days <- 
    index(data) %>%
    CalcTradingDays(x = .)
  ### начало торговли
  from.date <- 
    xts::first(data) %>%
    index(.)
  ### конец торговли
  to.date <-
    xts::last(data) %>%
    index(.)
  ### периодичность входных данных
  period <- 
    periodicity(data) %>%
    {
      paste(.[[2]], 'mins')
    }
  ### всего баров
  nbar <- 
    index(data) %>%
    unique(.) %>%
    length(.)
  ### бары в рынке
  nbar.trade <-
    states[states$pos.bars != 0] %>%
    {
      which(!duplicated(.$pos.num, fromLast = TRUE))    
    } %>%
    {
      states[.]$pos.bars
    } %>%
    sum(.)
  ### бары вне рынка
  nbar.out <- nbar - nbar.trade
  ### таблица временных метрик
  datesTable <- cbind.data.frame(from.date, to.date, trading.days, period, nbar, nbar.trade, nbar.out)
  colnames(datesTable) <- c('DateStart', 'DateEnd', 'DaysNum', 'BarsPeriod',  
                            'BarsNum', 'BarsNumIn', 'BarsNumOut')
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
