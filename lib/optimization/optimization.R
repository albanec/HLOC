RollerOpt.learning <- function(data_slices,
                               var_opt.list,
                               FUN,
                               linker_file = 'main/test/linker.R',
                               export_varlist = c('add.per', 'k.mm', 'balance.start', 
                                                  'basket.weights', 'slips', 'commissions', 'ret.type'),
                               export_libs = c('quantmod', 'xts', 'magrittr', 'tidyr', 'PerformanceAnalytics',
                                               'lubridate')
                               rolling_opt = TRUE, ...) {
                            
  FUN <- match.fun(FUN)
  #
  require(parallel)
  
  # запуск кластера
  parallel_cluster <- 
    detectCores() %>%
    makeCluster(.)
  
  ## Подгрузка данных в кластер
  # подгрузка библиотек
  clusterEvalQ(parallel_cluster, 
               {
                 # library(quantmod)
                 # library(xts)
                 # library(magrittr)
                 # library(tidyr)
                 # library(PerformanceAnalytics)
                 # library(lubridate)
                 sapply(export_libs, library, character.only = TRUE)
                 source(linker_file)
               })
  # подгрузка переменных
  clusterExport(parallel_cluster, envir = .GlobalEnv, 
                varlist = export_varlist)

  # Вычисление оптимизаций на обучающих периодах
  bf_data.list <- lapply(data_slices$widthSlice, 
                         function(x) {
                           data_slice <- x 
                           result <- 
                             var_opt.list %>%
                             parLapply(parallel_cluster,
                                       ., 
                                       function(x){
                                         FUN(x, ...)
                                       }) 
                           result %<>%
                             {
                               .[!is.na(.)]
                             } %>%
                             MergeData_inList.byRow(.)
                           return(result)
                         })
  
  # остановка кластера
  stopCluster(parallel_cluster)
  parallel_cluster <- c()  
  
  # КА
  cluster_data.list <- lapply(bf_data.list,
                              function(x) {
                                ## Подготовка к КА
                                data_for_cluster <- CalcKmean.preparation(data = x, 
                                                                          n.mouth = 6, 
                                                                          hi = TRUE, q.hi = 0.5, 
                                                                          one.scale = FALSE)
                                data_for_cluster$profit <- NULL
                                data_for_cluster$draw <- NULL 
                                ## Вычисление параметров кластеризации 
                                clustPar.data <- CalcKmean.parameters(data = data_for_cluster, 
                                                                      iter.max = 100, 
                                                                      plusplus = FALSE, 
                                                                      test.range = 30)
                                ## Вычисление самох кластеров
                                clustFull.data <- CalcKmean(data = data_for_cluster, 
                                                            clustPar.data[[2]], 
                                                            plusplus = FALSE, 
                                                            var.digits = 0)
                              })  
  
  # проверка остановки кластера
  if (!is.null(parallel_cluster)) {
    stopCluster(parallel_cluster)
    parallel_cluster <- c()
  }
  result <- cluster_data.list
  #  
  return(result)
}

###
#' Тупая функция оптимизации одного параметра движка test стратегии (multithread)
#' 
#' @param var.list Лист с данными для перебора
#' @param FUN Функция анализа (OneThreadRun функции)
#' @param linker_file Путь к linker файлу
#' @param export_varlist Вектор имён переменных, экспортируемых из .GlobalEnv в кластер
#' @param export_libs Вектор имён библиотек, экспортируемых в кластер
#' @param rolling_opt Упрощенный/полный perfomance анализ
#' @param var_names Вектор имен переменных стратегии
#' @param var.end Конечное значение оптимизации
#'
#' @return result DF с perfomance'ами по всем итерациям цикла 
#'
#' @export
BruteForceOpt_Parallel <- function(var.list, FUN, 
                                   linker_file = 'main/test/linker.R',
                                   export_varlist = c('add.per', 'k.mm', 'balance.start', 
                                                      'basket.weights', 'slips', 'commissions', 'ret.type'),
                                   export_libs = c('quantmod', 'xts', 'magrittr', 'tidyr', 'PerformanceAnalytics',
                                                   'lubridate'),
                                   rolling_opt = FALSE, ...) {
  #
  require(parallel)
  FUN <- match.fun(FUN)

  # запуск кластера
  parallel_cluster <- 
    detectCores() %>%
    makeCluster(.)

  ## Подгрузка данных в кластер
  # подгрузка библиотек
  clusterEvalQ(parallel_cluster, 
               {
                 sapply(export_libs, library, character.only = TRUE)
                 source(linker_file)
               })
  # подгрузка переменных
  clusterExport(parallel_cluster, envir = .GlobalEnv, 
                varlist = export_varlist)
ё
  result <- 
    var.list %>%
    parLapply(parallel_cluster,
              ., 
              function(x) {
                FUN(x, ...)
              })
  
  stopCluster(parallel_cluster)
  parallel_cluster <- c()  
  
  result %<>%  
    {
      .[!is.na(.)]
    } %>%
    MergeData_inList.byRow(.)
  
  if(!is.null(parallel_cluster)) {
    stopCluster(parallel_cluster)
    parallel_cluster <- c()
  }
  #  
  return(result)
}
###
#' Тупая функция оптимизации 
#' 
#' @param var.list Лист с данными для перебора
#' @param data.xts XTS с котировками
#' @param FUN Функция анализа (OneThreadRun функции)
#' @param rolling_opt Упрощенный/полный perfomance анализ
#' @param var_names Вектор имен переменных стратегии
#' @param balance_start Стартовый баланс
#' @param slips Слипы (вектор)
#' @param commissions Комиссии (вектор)
#' @param expiration Вектор с датами экспирации
#' @param ticker Тикер инструмента
#' @param return_type Тип доходности
#' @param eval_string Строка с куском скрипта для аргументов стратегии внутри apply цикла 
#'
#' @return result DF с perfomance'ами по всем итерациям цикла 
#'
#' @export
BruteForceOpt <- function(var.df, data.xts, 
                          FUN, 
                          rolling_opt = FALSE,
                          balance_start, slips, commissions,
                          expiration, ticker, return_type = 'ret',
                          eval_string) {
  #
  FUN <- match.fun(FUN)

  result <- 
    var.df %>% 
    apply(., 1, 
          function(x){
            #(x)
            temp.text <- 
              paste('FUN(data.xts = data.xts,
                         rolling_opt = rolling_opt, 
                         balance_start = balance_start, slips = slips, commissions = commissions,
                         expiration = expiration, ticker = ticker, ',#return_type = return_type, 
                         eval_string,')',
                         sep = '')
              eval(parse(text = temp.text))
              rm(temp.text)
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
#' Функция одного прогона вычислений движка стратегии
#' 
#' @param data.xts XTS с котировками
#' @param FUN Функция движка стратегии
#' @param rolling_opt Упрощенный/полный perfomance анализ
#' @param var_names Вектор имен переменных стратегии
#' @param balance_start Стартовый баланс
#' @param slips Слипы (вектор)
#' @param commissions Комиссии (вектор)
#' @param expiration Вектор с датами экспирации
#' @param ticker Тикер инструмента
#' @param ... Другие данные, нужные для вычислений робота
#'
#' @return perfomanceTable Строка с perfomance-данными прогона робота
#'
#' @export
OneThreadRun <- function(data.xts, FUN,
                         rolling_opt = FALSE, var_names,
                         balance_start, slips, commissions,
                         expiration, ticker, return_type = 'ret',
                         ...) {
  #
  FUN <- match.fun(FUN) 
  
  ## отработка робота
  data.strategy.list <- FUN(data.source = data.xts, 
                            balance_start = balance_start, slips = slips, commiss = commissions,
                            exp.vector = expiration, ticker, return_type = return_type, ...)
  
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
                                         dealsTable = 0,
                                         balance = balance_start, ret.type = 0, 
                                         fast = TRUE)  
    } else {
      ## лист с данными по сделкам (по тикерам и за всю корзину)
      dealsTable.list <- DealsTables.calc(data = data.strategy.list[[2]], basket = FALSE, convert = TRUE)
      ### оценка perfomance-параметров
      perfomanceTable <- PerfomanceTable(data = data.strategy.list[[1]], 
                                         data.state = data.strategy.list[[2]],
                                         dealsTable = dealsTable.list,
                                         balance_start = balance_start, 
                                         ret.type = ret_type)
    }
  }
  # добавление использованных параметров
  for (i in 1:length(var_names)) {
    temp.text <- paste('perfomanceTable %<>% cbind.data.frame(.,',var_names[i],' = ',var_names[i],')', 
                       sep = '')
    eval(parse(text = temp.text))
  }
  #
  return(perfomanceTable)
}