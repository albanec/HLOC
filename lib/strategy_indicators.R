# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Функции расчёта индикаторов
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
# ------------------------------------------------------------------------------
#' Вычисляет пересечения графиков рядов
#'
#' Вычисляются пересечения x1 'снизу-вверх' x2 (точки пробития вверх x2)
#' 
#' @param x1 xts1
#' @param x2 xts2
#'
#' @return x Ряд пересечений
#'
#' @export
CrossLine <- function(x1, x2, eq = FALSE) {
    if (eq == TRUE) {
        x <- diff(x1 >= x2)
    } else {
        x <- diff(x1 > x2)
    }
    x[1] <- 0
    x[x < 0] <- 0
    x <- sign(x)
    #
    return(x)
}
###
#' Надстройка нал CrossLine для удобства
#' @export
CrossLine.up <- function(x1, x2, eq = FALSE) {
    #
    return(CrossLine(x1, x2, eq))
}
#
#' Надстройка нал CrossLine для удобства
#' @export
CrossLine.down <- function(x1, x2, eq = FALSE) {
    #
    return(CrossLine(x2, x1, eq))
}
#
# ------------------------------------------------------------------------------
# Набор функций для расчёта индикаторов
#
#' Функция для расчёта MA
#' 
#' @param x XTS
#' @param n Период SMA
#' @param digits Округление до знаков после точки
#' @param FUN Функция для вычисления MA
#' @return x XTS ряд со значениями SMA
#'
#' @export
#CalcIndicator.SMA
CalcIndicator.MA <- function(x, n, FUN, digits = NULL) {
    FUN <- match.fun(FUN)
    x <- FUN(x, n)
    if (!is.null(digits)) {
        x %<>% round(., digits = digits)
    }
    #
    return(x)
}
#
###
#' Функция для расчёта DCI
#' 
#' @param x XTS
#' @param n Период DCI
#' @param digits Округление до знаков после точки
#'
#' @return x XTS со значениями DCI ($high, $mid, $low)
#'
#' @export
CalcIndicator.DCI <- function(x, n, digits = NULL, lag = TRUE) {
    #
    x <- DonchianChannel(HL = x, n = n, include.lag = lag)
    if (!is.null(digits)) {
        x %<>% round(., digits = digits)    
    }
    #
    return(x)
}
#
###
#' Функция для расчёта RSI
#' 
#' @param x XTS
#' @param n Период RSI
#' @param digits Округление до знаков после точки
#'
#' @return x XTS со значениями RSI
#'
#' @export
CalcIndicator.RSI <- function(x, n, digits = NULL) {
    #
    x <- RSI(price = x, n = n)
    if (!is.null(digits)) {
        x %<>% round(., digits = digits)    
    }
    #
    return(x)
}