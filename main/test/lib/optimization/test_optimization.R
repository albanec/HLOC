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
BruteForceOpt_Parallel.test_str <- function(#input_data = 'data.source.list', 
                                           sma_begin, sma_end, sma_step,
                                           # add.per_begin, add.per_end, add.per_step,
                                           rolling_opt = FALSE, ...) {
                                           #function(input_data = 'data.source.list', sma_begin, sma_end, sma_step,
                                           #         add.per_begin, add.per_end, add.per_step,
                                           #         rolling_opt = FALSE, ...) {                          
  #
  require(parallel)
  # запуск кластера
  parallel_cluster <- 
    detectCores() %>%
    makeCluster(.)
  ## Подгрузка данных в кластер
  # подгрузка библиотек
  clusterEvalQ(parallel_cluster, {
    library(quantmod)
    library(magrittr)
    library(tidyr)
    library(PerformanceAnalytics)
    source('main/test/linker.R')
  })
  # подгрузка переменных
  clusterExport(parallel_cluster, envir = .GlobalEnv, 
    varlist = c(
      'data.source.list', 
      'add.per',
      'k.mm', 'balance.start', 
      'basket.weights', 'slips', 'commissions', 'ret.type'
    )
  )
  # Формирование параметров оптимизации
  sma_vector <- seq(sma_begin, sma_end, by = sma_step)
  # add.per_vector <- seq(add.per_begin, add.per_end, by = add.per_step)
  vars <- sma_vector
  #   lapply(add.per, 
  #          function(x) {
  #            result <- data.frame(sma, x)
  #            return(result)
  #          }) %>%
  #   MergeData_inList.byRow(.) %>%
  #   {
  #     list(.[, 1], .[, 2])
  #   }
  # remove(sma_vector)
  # remove(add.per_vector)
    sma_vector
  #
  result <- 
    vars %>%
    parLapply(
      parallel_cluster,
      ., 
      function(x){
        OneThreadRun.test_str(data.xts = data.source.list[[1]],
                             sma.per = x, add.per = 10, k.mm, basket.weights,
                             slips, commissions, 
                             balance.start, ret.type,
                             rolling_opt) 
      }
    )
  stopCluster(parallel_cluster)
  parallel_cluster <- c()  
  result %<>%  
    {
      .[!is.na(.)]
    } %>%
    MergeData_inList.byRow(.)
  #
  if(!is.null(parallel_cluster)) {
    stopCluster(parallel_cluster)
    parallel_cluster <- c()
  }
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
#' @param slips Слипы (вектор)
#' @param commissions Комиссии (вектор)
#' @param balance.start Стартовый баланс
#'
#' @return result DF с perfomance'ами по всем итерациям цикла 
#'
#' @export
BruteForceOpt.test_str <- function(var.begin, var.end,
                                  data.xts, add.per, k.mm, basket.weights, 
                                  slips, commissions, 
                                  balance.start, ret.type,
                                  rolling_opt = FALSE) {
  #
  result <- 
    var.begin:var.end %>% 
    lapply(., 
           function(x){
             OneThreadRun.test_str(data.xts = data.xts,
                                  sma.per = x, add.per, k.mm, basket.weights,
                                  slips, commissions, 
                                  balance.start, ret.type,
                                  rolling_opt) 
           }
    ) %>%
    {
      .[!is.na(.)]
    } %>%
    MergeData_inList.byRow(.)  
  #
  return(result)
}
#
###
#' Функция одного прогона вычислений движка test стратегии
#' 
#' @param data.xts XTS с котировками
#' @param sma.per Периоды SMA
#' @param add.per Период докупок
#' @param k.mm Коэффициент MM
#' @param basket.weights Веса корзины (вектор)
#' @param slips Слипы (вектор)
#' @param commissions Комиссии (вектор)
#' @param balance.start Стартовый баланс
#'
#' @return list(data, data.state) Лист с данными отработки и данные сделок
#'
#' @export
OneThreadRun.test_str <- function(data.xts, 
                                 sma.per, add.per, k.mm, basket.weights, 
                                 slips, commissions,
                                 balance.start, ret.type, 
                                 rolling_opt = FALSE) {
  ### 
  ## Отработка тестового робота
  data.strategy.list <- TestStr.gear(data.source = data.xts,
                                     sma.per, add.per, k.mm, balance.start, 
                                     basket.weights, slips, commissions)
  ## Анализ perfomanc'ов
  # для стратегий, у которых нет сделок
  if (length(data.strategy.list[[1]]) == 1 && length(data.strategy.list[[2]]) == 1) {
    return()
  } else {
    ### Формирование таблицы сделок
    ## чистим от лишних записей
    data.strategy.list[[2]] <- StatesTable.clean(data = data.strategy.list[[2]])
    if (rolling_opt == TRUE) {
      ### оценка perfomance-параметров
      perfomanceTable <- PerfomanceTable(data = data.strategy.list[[1]], 
                                             data.state = 0,
                                             tradesTable = 0,
                                             balance = balance.start, ret.type = 0, 
                                             fast = TRUE)  
    } else {
      ## лист с данными по сделкам (по тикерам и за всю корзину)
      basket <- TRUE
      tradesTable.list <- TradesTable.calc(data = data.strategy.list[[2]], basket = basket, convert = TRUE)
      ### оценка perfomance-параметров
      perfomanceTable <- PerfomanceTable(data = data.strategy.list[[1]], 
                                             data.state = data.strategy.list[[2]],
                                             tradesTable = tradesTable.list,
                                             balance = balance.start, 
                                             ret.type)  
    }
  }
  perfomanceTable %<>%
    # добавление использованных параметров
    cbind.data.frame(., sma.per_ = sma.per, add.per_ = add.per) #, k.mm_ = k.mm)
  #
  return(perfomanceTable)
}