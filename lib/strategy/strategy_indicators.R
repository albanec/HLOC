# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Функции расчёта индикаторов
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
###
#' Функция для расчёта SMA
#' 
#' @param x XTS
#' @param per Период SMA
#' @param digits Округление до знаков после точки
#'
#' @return x XTS ряд со значениями SMA
#'
#' @export
CalcIndicator_SMA <- function(x, per, digits = 0, ...) {
  #
  x <- 
    SMA(x = x, n = per) %>%
    round(., digits = digits)
  #
  return(x)
}
#
###
#' Функция для расчёта DCI
#' 
#' @param x XTS
#' @param per Период DCI
#' @param digits Округление до знаков после точки
#'
#' @return x XTS со значениями DCI ($high, $mid, $low)
#'
#' @export
CalcIndicator_DCI <- function(x, per, digits = 0, lag = TRUE) {
  #
  x <- 
    DonchianChannel(HL = x, n = per, include.lag = lag) %>%
    round(., digits = digits)
  #
  return(x)
}
#