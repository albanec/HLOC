# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Функции MM
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
###
#' Функция MM относительно ширины канала:
#' 
#' @param balance Свободные средства на счёте
#' @param risk Риск
#' @param channelWidth Ширина канала / 2
#' @param IM ГО на инструмент
#' @param tick.price Цена тика 
#'
#' @return result Количество контрактов для покупки
#'
#' @export
CalcMM.byDCIwidth <- function(balance, risk, channelWidth, IM, tick.price) {
  var1 <-
    {
      coredata(balance) * risk * tick.price / coredata(channelWidth)
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
  #
  return(result)
}
#
