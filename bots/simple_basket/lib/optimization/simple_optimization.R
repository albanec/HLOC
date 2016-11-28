# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Функции для оптимизации simple стратегии:
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
###
#' Тупая функция оптимизации одного параметра движка simple стратегии (multithread)
#' 
#' @param var.begin Стартовое значение оптимизации
#' @param var.end Конечное значение оптимизации
#'
#' @return result DF с perfomance'ами по всем итерациям цикла 
#'
#' @export
SimpleStr_BruteForceOpt.Parallel <- function(var.begin, var.end, ...) {
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
    source('bots/simple/linker.R')
  })
  # подгрузка переменных
  clusterExport(parallel_cluster, envir = .GlobalEnv, 
    varlist = c(
      'data_source.list', 
      'add_per', 'k_mm', 'balance_start', 
      'basket_weights', 'slips', 'commissions', 'ret_type'
    )
  )
  #
  result <- 
    var.begin:var.end %>%
    parLapply(
      parallel_cluster,
      ., 
      function(x){
        SimpleStr_OneThreadRun(data_source = data_source.list[[1]],
                               sma_per = x, add_per, k_mm, balance_start, 
                               basket_weights, slips, commissions, ret_type)
      }
    ) 
  #
  parallel::stopCluster(parallel_cluster)
  parallel_cluster <- c()
  #
  result %<>% 
    {
      .[!is.na(.)]
    } %>%
    MergeData_inList.byRow(.)
  #
  if(!is.null(parallel_cluster)) {
    parallel::stopCluster(parallel_cluster)
    parallel_cluster <- c()
  }
  #  
  return(result)
}
###
#' Тупая функция оптимизации одного параметра движка simple стратегии (на одном ядре)
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
#' @return result Лист с perfomance'ами по всем итерациям цикла 
#'
#' @export
SimpleStr_BruteForceOpt <- function(var.begin, var.end,
                                    data_source, add_per, k_mm, balance_start, 
                                    basket_weights, slips, commissions, ret_type) {
  #
  result <- 
    var.begin:var.end %>%
    lapply(
      ., 
      function(x){
        SimpleStr_OneThreadRun(data_source = data_source.list[[1]],
                               sma_per = x, add_per, k_mm, balance_start, 
                               basket_weights, slips, commissions, ret_type)
      }
    ) %>%
    {
      .[!is.na(.)]
    } #%>%
    #MergeData_inList.byRow(.)
  #
  return(result)
}
#
###
#' Функция одного прогона вычислений движка simple стратегии
#' 
#' @param data.souce Лист с котировками
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
SimpleStr_OneThreadRun <- function(data_source = data_source.list[[1]], 
                                   sma_per, add_per, k_mm, basket_weights, 
                                   slips, commissions, 
                                   balance_start, ret_type,
                                   rolling_opt = FALSE) {
  ### отработка тестового робота
  data_strategy.list <- SimpleStr.gear(data_source,
                                       sma_per, add_per, k_mm, 
                                       basket_weights, slips, commissions,
                                       balance_start)
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
      perfomanceTable <- PerfomanceTable(data = data_strategy.list[[1]], 
                                         states = 0,
                                         trades_table = 0,
                                         balance = balance_start, ret_type = 0, 
                                         fast = TRUE) 
    } else {
      ## лист с данными по сделкам (по тикерам и за всю корзину)
      trades_table.list <- TradesTable.calc(data = data_strategy.list[[2]], basket = TRUE, convert = TRUE)
      # очистка мусора по target = 'temp'
      CleanGarbage(target = 'temp', env = '.GlobalEnv')
      # 
      ### оценка perfomance-параметров
      perfomanceTable <- PerfomanceTable(data = data_strategy.list[[1]], 
                                         states = data_strategy.list[[2]],
                                         trades_table = trades_table.list,
                                         balance = balance_start, 
                                         ret_type = ret_type)  
    }
  }
  perfomanceTable %<>% 
    # добавление использованных параметров
    cbind.data.frame(., sma_per = sma_per, add_per = add_per, k_mm = k_mm)
  #
  return(perfomanceTable)
}