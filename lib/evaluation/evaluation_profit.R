# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Функции расчета метрик доходности:
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
### Загрузка дочерних библиотек
source('lib/evaluation/evaluation_profit_days.R')
source('lib/evaluation/evaluation_profit_deals.R')
#
###
#' Расчёт итоговой таблицы с данными по доходностям 
#'
#' Функция вычисляет параметры по доходностям (дней и сделок) и формирует итоговый DF 
#' 
#' @param data Полные данные (после  отработки стратегии)
#' @param balance Стартовый баланс
#'
#' @return profitTable DF с данными по profit'у
#'
#' @export
ProfitTable <- function(data, dealsTable, balance, ...) {
  ### расчёт итоговой доходности 
  # здесь для анализа используется equty, чтобы лишний раз не считать разницу
  fullReturn <- 
    last(data$equity) %>%
    as.numeric(.)
  fullReturn.percent <- fullReturn * 100 / balance    
  ### доходность в год
  fullReturn.annual <- 
    index(data) %>%
    ndays(.) %>%
    {
      fullReturn * 250 / .
    }    
  ### доходность в месяц
  fullReturn.monthly <-
    index(data) %>%
    ndays(.) %>%
    {
      fullReturn * 20 / .
    }
  ### расчёт метрик по дням
  profitTable.byDays <- ProfitTable.byDays(data, balance)
  ### расчёт метрик по сделкам для корзины
  profitTable.byDeals <-  
    ProfitList_byDeals(data = dealsTable[[1]]) %>%
    {
      .[[1]]
    }
  ### формирование итогового DF
  profitTable <- 
    data.frame(Return = fullReturn,
               ReturnPercent = fullReturn.percent,
               ReturnAnnual = fullReturn.annual,
               ReturnMonthly = fullReturn.monthly,
               row.names = NULL) %>%
    {
      x <- .
      args <- list(...) 
      args.names <- names(args)
      if (('nbar' %in% args.names) && ('nbar.trade' %in% args.names) == TRUE ) {
        fullReturn.bar <- fullReturn / args$nbar
        fullReturn.nbar.trade <- fullReturn / args$nbar.trade
        x <- cbind(x, ReturnBar = fullReturn.bar, ReturnBarTrade = fullReturn.nbar.trade)
      }
      return(x)
    } %>%
    cbind(., profitTable.byDays, profitTable.byDeals)
  #
  return(profitTable)
}
#
