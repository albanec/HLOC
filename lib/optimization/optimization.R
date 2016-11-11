RollerOpt_learning <- function(data_slices, #input_data = 'data.source.list', 
                               varlist,
                               FUN,
                               linker_file = 'main/test/linker.R',
                               export_varlist = c('add.per', 'k.mm', 'balance.start', 
                                                  'basket.weights', 'slips', 'commissions', 'ret.type'),
                               export_libs = c('quantmod', 'xts', 'magrittr', 'tidyr', 'PerformanceAnalytics',
                                               'lubridate')
                               # add.per_begin, add.per_end, add.per_step,
                               rolling_opt = TRUE, ...) {
                               #function(input_data = 'data.source.list', sma_begin, sma_end, sma_step,
                               #         add.per_begin, add.per_end, add.per_step,
                               #         rolling_opt = FALSE, ...) {
  FUN <- match.fun(FUN)
  #
  require(parallel)
  
  # запуск кластера
  parallel_cluster <- 
    detectCores() %>%
    makeCluster(.)
  
  ## Подгрузка данных в кластер
  # подгрузка библиотек
  clusterEvalQ(parallel_cluster, {
    # library(quantmod)
    # library(xts)
    # library(magrittr)
    # library(tidyr)
    # library(PerformanceAnalytics)
    # library(lubridate)
    library(export_libs)
    source(linker_file)
  })
  # подгрузка переменных
  clusterExport(parallel_cluster, 
                envir = .GlobalEnv, 
                varlist = export_varlist)

  bf_data.list <-
    lapply(
      data_slices$widthSlice, 
      function(x) {
        data_slice <- x 
        #clusterExport(parallel_cluster, envir = .GlobalEnv, 
        #  varlist = c('temp_slice')
        #)
        result <- 
          varlist %>%
          parLapply(
            parallel_cluster,
            ., 
            function(x){
              FUN(x, ...)
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
  # остановка кластера
  stopCluster(parallel_cluster)
  parallel_cluster <- c()  
  
  # КА
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
  
  # проверка остановки кластера
  if (!is.null(parallel_cluster)) {
    stopCluster(parallel_cluster)
    parallel_cluster <- c()
  }
  result <- cluster_data.list
  #  
  return(result)
}