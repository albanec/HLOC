TestStr_RollerOpt_cl <- function(data_slices, #input_data = 'ohlc.list', 
                                 sma_begin, sma_end, sma_step,
                                 # add_perbegin, add_perend, add_perstep,
                                 fast = FALSE, ...) {
                                 #function(input_data = 'ohlc.list', sma_begin, sma_end, sma_step,
                                 #         add_perbegin, add_perend, add_perstep,
                                 #         fast = FALSE, ...) {
  #
  require(parallel)
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
    source('bots/test/linker.R')
  })
  # подгрузка переменных
  clusterExport(parallel_cluster, envir = .GlobalEnv, 
    varlist = c(
      'add_per',
      'k_mm', 'balance_start', 
      'basket_weights', 'slips', 'commissions', 'ret_type'
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
              OneThreadRun.test_str(data.xts = data_slice,
                                    sma_per = x, add_per = 10, k_mm, basket_weights,
                                    slips, commissions,
                                    balance_start, ret_type,
                                    fast = TRUE)
            }
          ) 
        result %<>%
          {
            .[!is.na(.)]
          } %>%
          MergeData_inList.byRow(.)
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
      data_for_cluster <- CalcKmean.preparation(data = x, n.mouth = 6, 
                                                    hi = TRUE, q.hi = 0.5, 
                                                    one.scale = FALSE)
      data_for_cluster$profit <- NULL
      data_for_cluster$draw <- NULL 
      ## Вычисление параметров кластеризации 
      clustPar.data <- CalcKmean.parameters(data = data_for_cluster, iter.max = 100, 
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