# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Функции расчета метрик доходности:
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
###
#' Расчёт итоговой таблицы с данными по доходностям
#'
#' Функция вычисляет параметры по доходностям (выводит итоговые данные)
#' 
#' @param data.state Полные данные equity (после  отработки стратегии)
#' @param deals.data Таблица сделок

#'
#' @return profitTable DF с данными по просадкам
#'
#' @export
ProfitTable <- function(data.state, deals.data, balance, value = "both", ...) {
  ### расчёт итоговой доходности (2.2 абсолютная доходность)
  if (value == "both") {
    fullReturn <- 
      last(data.state$equity) %>%
      as.numeric(.) %>%
      {
        data.frame(., (. * 100 / balance))
      }
  } else {
    fullReturn <- ifelse(value == "abs",
                         #
                         last(data.state$equity) %>%
                           as.numeric(.),
                         #
                         last(data.state$equity) %>%
                           as.numeric(.) %>%
                           . * 100 / balance                  
                         )
  }
  ### доходность в годовых
  # число месяцев торговли
  n.mouth <- 
    index(data) %>%
    ndays(.) / 30
  fullReturn.annual <- fullReturn * 12 / n.mouth    
  ### разбор дней
  #
  #
  return()
}
#
###
#' Разметка профит. информации по дням
#'
#' Функция вычисляет 
#' 
#' @param data Полные данные equity (после  отработки стратегии)
#'
#' @return profitDaysTable DF с данными профита по дням
#'
#' @export
CalcTradingDaysStats <- function(data) {
  #
  # разметка номеров дней
  data$day.num <- 
    {
      data$endpoint[endpoints(data, on = "days")] <- 1
      return(data)
    } %>%
    {
      data$endpoint[!is.na(data$endpoint)]
    } %>%
    cumsum(.) 
  data$day.num <- na.locf(data$day.num, fromLast = TRUE) 
  # разбор статистики по дням 
  #
  return()
}
#
OneDayProfit_DF <- function(data) {
  #
  #
  return()
}