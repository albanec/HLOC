TestStr_RollerOpt_learningTime <- function(data_slices, #input_data = 'data.source.list', 
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
    library(xts)
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
      'basket.weights', 'slips', 'commissions', 'ret.type'
    )
  )
  bf_data.list <-
    lapply(
      data_slices$widthSlice, 
      function(x) {
        data_slice <- x 
        #clusterExport(parallel_cluster, envir = .GlobalEnv, 
        #  varlist = c('temp_slice')
        #)
        result <- 
          vars %>%
          parLapply(
            parallel_cluster,
            ., 
            function(x){
              TestStr_OneThreadRun(data.xts = data_slice,
                                   sma.per = x, add.per = 10, k.mm, basket.weights,
                                   slips, commissions,
                                   balance.start, ret.type,
                                   rolling_opt = TRUE)
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
  cluster_data.list <- lapply(
    bf_data.list,
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
                                  plusplus = FALSE, var.digits = 0)
    } 
  )  
  #
  if(!is.null(parallel_cluster)) {
    stopCluster(parallel_cluster)
    parallel_cluster <- c()
  }
  result <- cluster_data.list
  #  
  return(result)
}