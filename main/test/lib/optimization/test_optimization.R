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
BruteForceOpt_parallel_cl.test_str <- function(#input_data = 'data_source.list', 
                                               sma_begin, sma_end, sma_step,
                                               # add_perbegin, add_perend, add_perstep,
                                               rolling_opt = FALSE, ...) {
                                               #function(input_data = 'data_source.list', sma_begin, sma_end, sma_step,
                                               #         add_perbegin, add_perend, add_perstep,
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
      'data_source.list', 
      'add_per',
      'k_mm', 'balance_start', 
      'basket_weights', 'slips', 'commissions', 'ret_type'
    )
  )
  # Формирование параметров оптимизации
  sma_vector <- seq(sma_begin, sma_end, by = sma_step)
  # add_pervector <- seq(add_perbegin, add_perend, by = add_perstep)
  vars <- sma_vector
  #   lapply(add_per, 
  #          function(x) {
  #            result <- data.frame(sma, x)
  #            return(result)
  #          }) %>%
  #   MergeData_inList.byRow(.) %>%
  #   {
  #     list(.[, 1], .[, 2])
  #   }
  # remove(sma_vector)
  # remove(add_pervector)
    sma_vector
  #
  result <- 
    vars %>%
    parLapply(
      parallel_cluster,
      ., 
      function(x){
        OneThreadRun.test_str(data.xts = data_source.list[[1]],
                              sma_per = x, add_per = 10, k_mm, basket_weights,
                              slips, commissions, 
                              balance_start, ret_type,
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
#' @param sma_per Периоды SMA
#' @param add_per Период докупок
#' @param k_mm Коэффициент MM
#' @param basket_weights Веса корзины (вектор)
#' @param slips Слипы (вектор)
#' @param commissions Комиссии (вектор)
#' @param balance_start Стартовый баланс
#'
#' @return result DF с perfomance'ами по всем итерациям цикла 
#'
#' @export
BruteForceOpt.test_str <- function(var.begin, var.end,
                                   data.xts, add_per, k_mm, basket_weights, 
                                   slips, commissions, 
                                   balance_start, ret_type,
                                   rolling_opt = FALSE) {
  #
  result <- 
    var.begin:var.end %>% 
    lapply(., 
           function(x){
             OneThreadRun.test_str(data.xts = data.xts,
                                   sma_per = x, add_per, k_mm, basket_weights,
                                   slips, commissions, 
                                   balance_start, ret_type,
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
#' @param sma_per Периоды SMA
#' @param add_per Период докупок
#' @param k_mm Коэффициент MM
#' @param basket_weights Веса корзины (вектор)
#' @param slips Слипы (вектор)
#' @param commissions Комиссии (вектор)
#' @param balance_start Стартовый баланс
#'
#' @return list(data, states) Лист с данными отработки и данные сделок
#'
#' @export
OneThreadRun.test_str <- function(data.xts, 
                                  sma_per, add_per, k_mm, basket_weights, 
                                  slips, commissions,
                                  balance_start, ret_type, 
                                  rolling_opt = FALSE) {
  ### 
  ## Отработка тестового робота
  data_strategy.list <- TestStr.gear(data_source = data.xts,
                                     sma_per, add_per, k_mm, balance_start, 
                                     basket_weights, slips, commissions)
  ## Анализ perfomanc'ов
  # для стратегий, у которых нет сделок
  if (length(data_strategy.list[[1]]) == 1 && length(data_strategy.list[[2]]) == 1) {
    return()
  } else {
    ### Формирование таблицы сделок
    ## чистим от лишних записей
    data_strategy.list[[2]] <- StatesTable.clean(data = data_strategy.list[[2]])
    if (rolling_opt == TRUE) {
      ### оценка perfomance-параметров
      perfomance_table <- PerfomanceTable(data = data_strategy.list[[1]], 
                                          states = 0,
                                          trades_table = 0,
                                          balance = balance_start, ret_type = 0, 
                                          fast = TRUE)  
    } else {
      ## лист с данными по сделкам (по тикерам и за всю корзину)
      basket <- TRUE
      trades_table.list <- TradesTable.calc(data = data_strategy.list[[2]], basket = basket, convert = TRUE)
      ### оценка perfomance-параметров
      perfomanceTable <- PerfomanceTable(data = data_strategy.list[[1]], 
                                         states = data_strategy.list[[2]],
                                         trades_table = trades_table.list,
                                         balance = balance_start, 
                                         ret_type)  
    }
  }
  perfomanceTable %<>%
    # добавление использованных параметров
    cbind.data.frame(., sma_per = sma_per, add_per = add_per) #, k_mm = k_mm)
  #
  return(perfomanceTable)
}