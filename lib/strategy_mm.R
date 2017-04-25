# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Функции MM
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
###
#' Функция MM относительно ширины канала:
#' 
#' @param balance Свободные средства на счёте
#' @param row Строка с анализируемыми данными
#' @param ohlc_args Торговые параметры
#' @param trade_args Торговые параметры
#' @param str_args Параметры стратегии
#'
#' @return result Количество контрактов для покупки
#'
#' @export
CalcMM.byDCIwidth <- function(balance, row, ohlc_args, trade_args, str_args) { 
    var1 <-
        {
            as.integer(balance) * str_args$k_mm * trade_args$tick_price / as.integer(row$widthDCI)
        } %>%
        floor(.) %>%
        max(., 1)
    var2 <- 
        {
            as.integer(balance) / as.integer(ohlc_args$ohlc$IM[index.xts(row)])
        } %>%
        floor(.) %>%
        max(., 1)
    result <- min(var1, var2)
    #
    return(result)
}
#
CalcMM.simple_byIM <- function(balance, row, ohlc_args, trade_args, str_args) {
    row.ind <- index(row)
    result <- balance %/% (str_args$k_mm * ohlc_args$ohlc$IM[row.ind]) %>% as.integer(.)
    #
    return(result)
}
