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
    source('main/simple/linker.R')
  })
  # подгрузка переменных
  clusterExport(parallel_cluster, envir = .GlobalEnv, 
    varlist = c(
      'data.source.list', 
      'add.per', 'k.mm', 'balance.start', 
      'basket.weights', 'slips', 'commissions', 'ret.type'
    )
  )
  #
  result <- 
    var.begin:var.end %>%
    parLapply(
      parallel_cluster,
      ., 
      function(x){
        SimpleStr_OneThreadRun(data.source = data.source.list[[1]],
                               sma.per = x, add.per, k.mm, balance.start, 
                               basket.weights, slips, commissions, ret.type)
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
#' @param sma.per Периоды SMA
#' @param add.per Период докупок
#' @param k.mm Коэффициент MM
#' @param basket.weights Веса корзины (вектор)
#' @param slips Слипы (вектор)
#' @param commissions Комиссии (вектор)
#' @param balance.start Стартовый баланс
#'
#' @return result Лист с perfomance'ами по всем итерациям цикла 
#'
#' @export
SimpleStr_BruteForceOpt <- function(var.begin, var.end,
                                    data.source, add.per, k.mm, balance.start, 
                                    basket.weights, slips, commissions, ret.type) {
  #
  result <- 
    var.begin:var.end %>%
    lapply(
      ., 
      function(x){
        SimpleStr_OneThreadRun(data.source = data.source.list[[1]],
                               sma.per = x, add.per, k.mm, balance.start, 
                               basket.weights, slips, commissions, ret.type)
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
SimpleStr_OneThreadRun <- function(data.source = data.source.list[[1]], 
                                   sma.per, add.per, k.mm, basket.weights, 
                                   slips, commissions, 
                                   balance.start, ret.type,
                                   rolling_opt = FALSE) {
  ### отработка тестового робота
  data.strategy.list <- SimpleStr.gear(data.source,
                                       sma.per, add.per, k.mm, 
                                       basket.weights, slips, commissions,
                                       balance.start)
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
      perfomanceTable <- 
        PerfomanceTable(data = data.strategy.list[[1]], 
                            data.state = 0,
                            tradesTable = 0,
                            balance = balance.start, ret.type = 0, 
                            fast = TRUE) 
    } else {
      ## лист с данными по сделкам (по тикерам и за всю корзину)
      tradesTable.list <- TradesTable.calc(data = data.strategy.list[[2]], basket = TRUE, convert = TRUE)
      # очистка мусора по target = 'temp'
      CleanGarbage(target = 'temp', env = '.GlobalEnv')
      # 
      ### оценка perfomance-параметров
      perfomanceTable <- 
        PerfomanceTable(data = data.strategy.list[[1]], 
                            data.state = data.strategy.list[[2]],
                            tradesTable = tradesTable.list,
                            balance = balance.start, 
                            ret.type = ret.type)  
    }
  }
  perfomanceTable %<>% 
    # добавление использованных параметров
    cbind.data.frame(., sma.per_ = sma.per, add.per_ = add.per, k.mm_ = k.mm)
  #
  return(perfomanceTable)
}