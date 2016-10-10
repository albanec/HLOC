TestStr_RollerOpt <- function(date_slices, #input_data = 'data.source.list', 
                              sma_begin, sma_end, sma_step,
                              # add.per_begin, add.per_end, add.per_step,
                              rolling_opt = FALSE, ...) {
                              #function(input_data = 'data.source.list', sma_begin, sma_end, sma_step,
                              #         add.per_begin, add.per_end, add.per_step,
                              #         rolling_opt = FALSE, ...) {
  #
  require(parallel)
  # Формирование параметров оптимизации
  sma_vector <- seq(sma_begin, sma_end, by = sma_step)
  # add.per_vector <- seq(add.per_begin, add.per_end, by = add.per_step)
  vars <- sma_vector
  #   lapply(add.per, 
  #          function(x) {
  #            result <- data.frame(sma, x)
  #            return(result)
  #          }) %>%
  #   MergeData_inList_byRow(.) %>%
  #   {
  #     list(.[, 1], .[, 2])
  #   }
  # remove(sma_vector)
  # remove(add.per_vector)
  #
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
      'add.per',
      'k.mm', 'balance.start', 
      'basket.weights', 'sleeps', 'commissions', 'ret.type'
    )
  )
  bf_data <-
    lapply(
      date_slices$widthSlice, 
      function(x) {
        temp_slice <<- x 
        clusterExport(parallel_cluster, envir = .GlobalEnv, 
          varlist = c('temp_slice')
        )
        result <- 
          vars %>%
          parLapply(
            parallel_cluster,
            ., 
            function(x){
              TestStr_OneThreadRun(data.source = temp_slice,
                                   sma.per = x, add.per = 10, k.mm, balance.start, 
                                   basket.weights, sleeps, commissions, ret.type,
                                   rolling_opt)
            }
          ) 
        result %<>%
          {
            .[!is.na(.)]
          } %>%
          MergeData_inList_byRow(.)
        return(result)
      }
    )
  stopCluster(parallel_cluster)
  parallel_cluster <- c()  
  #
  cluster_data <- lapply(
    bf_data,
    function(x) {
      ## Подготовка к КА
      data_for_cluster <- CalcKmean_DataPreparation(data = x, n.mouth = 6, 
                                                    hi = TRUE, q.hi = 0.5, 
                                                    one.scale = FALSE)
      data_for_cluster$profit <- NULL
      data_for_cluster$draw <- NULL 
      ## Вычисление параметров кластеризации 
      clustPar.data <- CalcKmean_Parameters(data = data_for_cluster, iter.max = 100, 
                                            plusplus = FALSE, test.range = 30)
      ## Вычисление самох кластеров
      clustFull.data <- CalcKmean(data = data_for_cluster, clustPar.data[[2]], 
                                    plusplus = FALSE, var.digits = 2)
    } 
  )  
  #
  if(!is.null(parallel_cluster)) {
    stopCluster(parallel_cluster)
    parallel_cluster <- c()
  }
  #  
  return(result)
}