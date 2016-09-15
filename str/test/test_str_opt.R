#source("str/libStrategy.R")
## простой перебор с sma.per = 1:100
# system.time({
#   x <- 
#     1:2 %>%
#     lapply(., function(x){
#                 TestStr_OneTradeProbe(data.source = data.source.list[[1]],
#                                       sma.per = x, add.per, k.mm, balance.start, 
#                                       basket.weights, sleeps, commissions, ret.type)
#               }
#     ) %>%
#     {
#       .[!is.na(.)]
#     } %>%
#     MergeData_inList_byRow(.)
# })
#
###
#' Функция одного прогона вычислений движка test стратегии
#' 
#' @param data.souce Лист с котировками
#' @param sma.per Периоды SMA
#' @param add.per Период докупок
#' @param k.mm Коэффициент MM
#' @param basket.weights Веса корзины (вектор)
#' @param sleeps Слипы (вектор)
#' @param commissions Комиссии (вектор)
#' @param balance.start Стартовый баланс
#'
#' @return list(data, data.state) Лист с данными отработки и данные сделок
#'
#' @export
TestStr_OneTradeProbe <- function(data.source = data.source.list[[1]], 
                                  sma.per, add.per, k.mm, basket.weights, 
                                  sleeps, commissions,
                                  balance.start, ret.type) {
  ### 
  ## Отработка тестового робота
  data.strategy.list <- TestStr_gear(data.source, sma.per, add.per, k.mm, 
                                     basket.weights, sleeps, commissions,
                                     balance.start)
  ## Анализ perfomanc'ов
  # для стратегий, у которых нет сделок
  if (length(data.strategy.list[[1]]) == 1 && length(data.strategy.list[[2]]) == 1) {
    # perfomanceTable <- 
    #   rep(NA, 66) %>%
    #   data.frame(., row.names = NULL) %>% 
    #   t(.) %>%
    #   {
    #     colnames(.) <- c(
    #       "StartDate", "EndDate", "Period", "NumTradeDays", "NumBars", "NumBarsTrade", 
    #       "NumBarsNoTrade", "SharpRatio", "SortinoRatio", "CalmarRatio", "SterlingRatio", 
    #       "RecoveryFactor", "WinRatio", "MaxDrawdownDay", "MaxDrawdown", "MaxDrawdownPercent", 
    #       "MeanDrawdown", "MeanDrawdownPercent", "MaxDrawdownDays", "MeanDrawdownDays", 
    #       "NowDrawdownDays", "NowDrawdownPeriods", "NowDrawdown", "NowDrawdownPercent", 
    #       "Return", "ReturnPercent", "ReturnAnnual", "ReturnMonthly", "ReturnBar", 
    #       "ReturnBarTrade", "BestDay", "BestDayReturn", "BestDayReturnPercent", "MeanGoodDayReturn", 
    #       "MeanGoodDayReturnPercent", "NumGoogDay", "NumGoogDayPercent", "MaxGoodDays", 
    #       "FullGoodDayReturn", "WorstDay", "WorstDayReturn", "WorstDayReturnPercent", 
    #       "MeanBadDayReturn", "MeanBadDayReturnPercent", "NumBadDay", "NumBadDayPercent", 
    #       "MaxBadDays", "FullBadDayReturn", "ProfitFactorDaily", "DealsNum", "NumGoogDeals", 
    #       "NumBadDeals", "MaxGoodDeals", "MaxBadDeals", "FullGoodDealReturn", "FullBadDealReturn", 
    #       "MeanGoodDealReturn", "MeanGoodDealReturnPercent", "MeanBadDealReturn", 
    #       "MeanBadDealReturnPercent", "MeanDealBars", "MeanGoodDealBars", "MeanBadDealBars", 
    #       "MeanDealReturn", "MeanDealReturnPercent", "ProfitFactorDeals"
    #     )
    #     return(.)
    #   }
    return()
  } else {
    ### Формирование таблицы сделок
    ## чистим от лишних записей
    data.strategy.list[[2]] <- CleanStatesTable(data = data.strategy.list[[2]])
    ## лист с данными по сделкам (по тикерам и за всю корзину)
    dealsTable.list <- CalcDealsTables(data = data.strategy.list[[2]], convert = TRUE)
    # очистка мусора по target = "temp"
    CleanGarbage(target = "temp", env = ".GlobalEnv")
    #
    ### оценка perfomance-параметров
    perfomanceTable <- 
      CalcPerfomanceTable(data = data.strategy.list[[1]], 
                          data.state = data.strategy.list[[2]],
                          dealsTable = dealsTable.list,
                          balance = balance.start, 
                          ret.type)  
  }
  perfomanceTable %<>%
    # добавление использованных параметров
    cbind.data.frame(., sma.per_ = sma.per, add.per_ = add.per, k.mm_ = k.mm)
  #
  return(perfomanceTable)
}
