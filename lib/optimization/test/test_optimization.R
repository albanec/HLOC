# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Функции для оптимизации test стратегии:
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
###
#' Тупая функция оптимизации одного параметра движка test стратегии (multithread)
#' 
#' @param var.begin Стартовое значение оптимизации
#' @param var.end Конечное значение оптимизации
#'
#' @return result DF с perfomance'ами по всем итерациям цикла 
#'
#' @export
TestStr_BruteForceOpt_Parallel <- function(var.begin, var.end, rolling_opt = FALSE, ...) {
  #
  require(parallel)
  # запуск кластера
  parallel_cluster <- 
    parallel::detectCores() %>%
    parallel::makeCluster(.)
  ## Подгрузка данных в кластер
  # подгрузка библиотек
  clusterEvalQ(parallel_cluster, {
    library(quantmod)
    library(magrittr)
    library(tidyr)
    library(PerformanceAnalytics)
    source("main/test/linker.R")
  })
  # подгрузка переменных
  clusterExport(parallel_cluster, envir = .GlobalEnv, 
    varlist = c(
      "data.source.list", 
      "add.per", "k.mm", "balance.start", 
      "basket.weights", "sleeps", "commissions", "ret.type"
    )
  )
  #
  result <- 
    var.begin:var.end %>%
    parLapply(
      parallel_cluster,
      ., 
      function(x){
        TestStr_OneThreadRun(data.source = data.source.list[[1]],
                             sma.per = x, add.per, k.mm, balance.start, 
                             basket.weights, sleeps, commissions, ret.type,
                             rolling_opt)
      }
    ) %T>%
    {
      parallel::stopCluster(parallel_cluster)
      parallel_cluster <- c()  
    } %>%
  #result %<>%  
    {
      .[!is.na(.)]
    } %>%
    MergeData_inList_byRow(.)
  #  
  return(result)
}
#
###
#' Тупая функция оптимизации одного параметра движка test стратегии (на одном ядре)
#' 
#' @param var.begin Стартовое значение оптимизации
#' @param var.end Конечное значение оптимизации
#' @param data.souce Лист с котировками
#' @param sma.per Периоды SMA
#' @param add.per Период докупок
#' @param k.mm Коэффициент MM
#' @param basket.weights Веса корзины (вектор)
#' @param sleeps Слипы (вектор)
#' @param commissions Комиссии (вектор)
#' @param balance.start Стартовый баланс
#'
#' @return result DF с perfomance'ами по всем итерациям цикла 
#'
#' @export
TestStr_BruteForceOpt_Simple <- function(var.begin, var.end,
                                         data.source, add.per, k.mm, balance.start, 
                                         basket.weights, sleeps, commissions, ret.type,
                                         rolling_opt = FALSE) {
  #
  result <- 
    var.begin:var.end %>%
    lapply(
      ., 
      function(x){
        TestStr_OneThreadRun(data.source = data.source.list[[1]],
                             sma.per = x, add.per, k.mm, balance.start, 
                             basket.weights, sleeps, commissions, ret.type,
                             rolling_opt)
      }
    ) %>%
    {
      .[!is.na(.)]
    } %>%
    MergeData_inList_byRow(.)  
  #
  return(result)
}
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
TestStr_OneThreadRun <- function(data.source = data.source.list[[1]], 
                                 sma.per, add.per, k.mm, basket.weights, 
                                 sleeps, commissions,
                                 balance.start, ret.type, 
                                 rolling_opt = FALSE) {
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
    if (rolling_opt == TRUE) {
      ### оценка perfomance-параметров
      perfomanceTable <- 
        CalcPerfomanceTable(data = data.strategy.list[[1]], 
                            data.state = 0,
                            dealsTable = 0,
                            balance = balance.start, ret.type = 0, 
                            fast = TRUE)  
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
  }
  perfomanceTable %<>%
    # добавление использованных параметров
    cbind.data.frame(., sma.per_ = sma.per, add.per_ = add.per, k.mm_ = k.mm)
  #
  return(perfomanceTable)
}