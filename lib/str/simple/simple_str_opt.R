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
SimpleStr_Parallel_BruteForceOpt <- function(var.begin, var.end, ...) {
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
    source("str/libStrategy.R")
    source("str/test/test_str_gen.R")
    source("str/test/test_str_eva.R")
    source("str/test/test_str_opt.R")
    source("str/test/test_str_gear.R")
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
        SimpleStr_OneThreadRun(data.source = data.source.list[[1]],
                             sma.per = x, add.per, k.mm, balance.start, 
                             basket.weights, sleeps, commissions, ret.type)
      }
    ) %>% 
    {
      .[!is.na(.)]
    } %>%
    MergeData_inList_byRow(.)
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
#' @param sleeps Слипы (вектор)
#' @param commissions Комиссии (вектор)
#' @param balance.start Стартовый баланс
#'
#' @return result DF с perfomance'ами по всем итерациям цикла 
#'
#' @export
SimpleStr_BruteForceOpt <- function(var.begin, var.end,
                                    data.source, add.per, k.mm, balance.start, 
                                    basket.weights, sleeps, commissions, ret.type) {
  #
  result <- 
    var.begin:var.end %>%
    lapply(
      ., 
      function(x){
        SimpleStr_OneThreadRun(data.source = data.source.list[[1]],
                               sma.per = x, add.per, k.mm, balance.start, 
                               basket.weights, sleeps, commissions, ret.type)
      }
    ) %>%
    {
      .[!is.na(.)]
    } %>%
    MergeData_inList_byRow(.)
  })  
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
#' @param sleeps Слипы (вектор)
#' @param commissions Комиссии (вектор)
#' @param balance.start Стартовый баланс
#'
#' @return list(data, data.state) Лист с данными отработки и данные сделок
#'
#' @export
SimpleStr_OneThreadRun <- function(data.source = data.source.list[[1]], 
                                   sma.per, add.per, k.mm, basket.weights, 
                                   sleeps, commissions, 
                                   balance.start, ret.type) {
  ### отработка тестового робота
  data.strategy.list <- SimpleStr_gear(data.source,
                                            sma.per, add.per, k.mm, 
                                            basket.weights, sleeps, commissions,
                                            balance.start)
  ## Анализ perfomanc'ов
  # для стратегий, у которых нет сделок
  if (length(data.strategy.list[[1]]) == 1 && length(data.strategy.list[[2]]) == 1) {
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
                          ret.type = ret.type)
  }
  perfomanceTable %<>% 
    # добавление использованных параметров
    cbind.data.frame(., sma.per_ = sma.per, add.per_ = add.per, k.mm_ = k.mm)
  #
  return(perfomanceTable)
}
