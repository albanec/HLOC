# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Функции MM
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
###
#' Функция MM относительно ширины канала:
#' 
#' @param balance Свободные средства на счёте
#' @param risk Риск
#' @param widthDCI Ширина канала / 2
#' @param IM ГО на инструмент
#' @param tick.price Цена тика 
#'
#' @return result Количество контрактов для покупки
#'
#' @export
CalcMM.byDCIwidth <- function(balance, IM, ...) { 
    #risk, widthDCI, tick.price) {
    dots <- list(...)
    var1 <-
        {
            coredata(balance) * dots$risk * dots$tick.price / coredata(dots$widthDCI)
        } %>%
        floor(.) %>%
        max(., 1)
    var2 <- 
        {
            coredata(balance) / coredata(IM)
        } %>%
        floor(.) %>%
        max(., 1)
    result <- min(var1, var2)
    cat(result)
    #
    return(result)
}
#
