# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Функции для оптимизации стратегий:
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
###
#' Roller функция оптимизации движка стратегии (multithread)
#' 
#' @param data_slices Временные интервалы оптимизационных окон
#' @param var.df DF с данными для перебора
#' @param FUN Функция анализа (OneThreadRun ветка функций)
#' @param linker_file Путь к linker файлу
#' @param balance_start Стартовый баланс
#' @param slips Слипы (вектор)
#' @param commissions Комиссии (вектор)
#' @param expiration Вектор с датами экспирации
#' @param ticker Тикер инструмента
#' @param return_type Тип доходности
#' @param export_varlist Вектор имён переменных, экспортируемых из .GlobalEnv в кластер (необязательно)
#' @param export_libs Вектор имён библиотек, экспортируемых в кластер
#' @param rolling_opt Упрощенный/полный perfomance анализ
#' @param eval_string Сторока с описанием переменных внутри lapply-цикла
#' @param 
#'
#' @return result DF с perfomance'ами по всем итерациям цикла 
#'
#' @export               
RollerOpt_learning_cl <- function(data_slices,
                                  var.df,
                                  FUN, win_size,
                                  linker_file = 'bots/test/linker.R',
                                  balance_start, slips, commissions,
                                  expiration, ticker, return_type = 'ret',
                                  export_varlist = NULL,
                                  export_libs = c('quantmod', 'xts', 'magrittr', 'tidyr', 
                                                  'PerformanceAnalytics', 'lubridate'),
                                  rolling_opt = FALSE, 
                                  eval_string) {
  #
  .CurrentEnv <- environment()                          
  #
  require(parallel)
  FUN <- match.fun(FUN)
  
   # запуск кластера
  parallel_cluster <- 
    detectCores() %>%
    makeCluster(.)#, type = 'PSOCK')
  ## Подгрузка данных в кластер
  clusterExport(parallel_cluster, envir = .CurrentEnv, 
                varlist = c('FUN', 'linker_file', 
                            'balance_start', 'slips', 'commissions', 'expiration', 
                            'ticker', 'return_type', 'rolling_opt', 
                            'export_varlist', 'export_libs', 'eval_string'))
  # подгрузка библиотек
  clusterEvalQ(parallel_cluster,
               { 
                 sapply(export_libs, library, character.only = TRUE)
                 source(linker_file) 
               })
  # подгрузка дополнительных переменных напрямую из .GlobalEnv (если нужно)
  if (!is.null(export_varlist)) {
    clusterExport(parallel_cluster, envir = .GlobalEnv, 
                  varlist = export_varlist) 
  }

  # Вычисление оптимизаций на обучающих периодах
  bf_data.list <- lapply(data_slices$widthSlice, 
                         function(x) {
                           data_slice <- x 
                           result <- 
                             var.df %>%
                             parRapply(parallel_cluster,
                                       ., 
                                       function(x) {
                                         #FUN(x, ...)
                                         FUN <- match.fun(FUN)
                                         temp_text <- paste0(
                                           'df <- FUN(data.xts = data_slice,
                                                      rolling_opt = rolling_opt, 
                                                      balance_start = balance_start, 
                                                      slips = slips, commissions = commissions,
                                                      expiration = expiration, ticker = ticker, 
                                                      return_type = return_type, ',eval_string,')')
                                         eval(parse(text = temp_text))
                                         rm(temp_text)
                                         return(df)
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
                                                                          n.mouth = win_size, 
                                                                          hi = TRUE, q.hi = 0.5, 
                                                                          one.scale = FALSE)
                                data_for_cluster$profit <- NULL
                                data_for_cluster$draw <- NULL 
                                ## Вычисление кластеров 
                                clustFull.data <- 
                                  CalcKmean.parameters(data = data_for_cluster, 
                                                       iter.max = 100, 
                                                       plusplus = FALSE, 
                                                       test.range = 30) %>%
                                  .[[2]] %>%
                                  CalcKmean(data = data_for_cluster, 
                                            n.opt = ., 
                                            plusplus = FALSE, 
                                            var.digits = 0)
                              })  
  
  # проверка остановки кластера
  if (!is.null(parallel_cluster)) {
    stopCluster(parallel_cluster)
    parallel_cluster <- c()
  }
  
  return(cluster_data.list)
}
#
RollerOpt_learning_mc <- function(data_slices,
                                  var.df,
                                  FUN, win_size,
                                  linker_file = 'bots/test/linker.R',
                                  balance_start, slips, commissions,
                                  expiration, ticker, return_type = 'ret',
                                  export_varlist = NULL,
                                  export_libs = c('quantmod', 'xts', 'magrittr', 'tidyr', 
                                                  'PerformanceAnalytics', 'lubridate'),
                                  rolling_opt = FALSE, 
                                  eval_string) {
  #
  #.CurrentEnv <- environment()                          
  #
  #require(doParallel)
  FUN <- match.fun(FUN)
  
  # Вычисление оптимизаций на обучающих периодах
  n_vars <- nrow(var.df)
  bf_data.list <- 
    foreach(i = 1:length(data_slices$widthSlice)) %do% {
      temp_slice <- data_slices$widthSlice[[i]]
      df <- BruteForceOpt_parallel_mc(var.df = var.df, data.xts = temp_slice,
                                      FUN = FUN, 
                                      linker_file = 'bots/test/linker.R',
                                      balance_start = balance_start, slips = slips, commissions = commissions,
                                      expiration = expiration, ticker = ticker, return_type = return_type,
                                      export_varlist = NULL, 
                                      export_libs = c('quantmod', 'xts', 'magrittr', 'tidyr', 
                                                      'PerformanceAnalytics', 'lubridate'),
                                      rolling_opt = rolling_opt, 
                                      eval_string = eval_string)
      return(df)
    }
  # КА
  cluster_data.list <- 
    foreach(i = 1:length(bf_data.list)) %do% {
      x <- bf_data.list[[i]]
      ## Подготовка к КА
      data_for_cluster <- CalcKmean.preparation(data = x, 
                                                n.mouth = win_size, 
                                                hi = TRUE, q.hi = 0.5, 
                                                one.scale = FALSE)
      data_for_cluster$profit <- NULL
      data_for_cluster$draw <- NULL
      ## Вычисление самих кластеров
      clustFull.data <- 
        CalcKmean.parameters(data = data_for_cluster, 
                             iter.max = 100, 
                             plusplus = FALSE, 
                             test.range = 30) %>%
        .[[2]] %>%
        CalcKmean(data = data_for_cluster, 
                  n.opt = ., 
                  plusplus = FALSE, 
                  var.digits = 0)
      return(clustFull.data)
    }
  #
  return(cluster_data.list)
}
#
BruteForceOpt_parallel_mc <- function(var.df, data.xts,
                                      FUN, 
                                      linker_file = 'bots/test/linker.R',
                                      balance_start, slips, commissions,
                                      expiration, ticker, return_type = 'ret',
                                      export_varlist = NULL, 
                                      export_libs = c('quantmod', 'xts', 'magrittr', 'tidyr', 
                                                      'PerformanceAnalytics', 'lubridate'),
                                      rolling_opt = FALSE, 
                                      eval_string) {
  #
  #.CurrentEnv <- environment()
  #
  require(doParallel)
  FUN <- match.fun(FUN)

  workers <- detectCores() - 1  
  registerDoParallel(cores = workers) 
  
  n_vars <- nrow(var.df)

  result <- 
    foreach(i = 1:workers) %dopar% {
      FUN <- match.fun(FUN)
      map_range <- Delegate_mcore(i, n_vars, p = workers)
      x <- var.df[map_range, ]
      df <- BruteForceOpt(x = x, data.xts = data.xts, 
                          FUN = FUN, 
                          rolling_opt = rolling_opt,
                          balance_start = balance_start, slips = slips, commissions = commissions,
                          expiration = expiration, ticker = ticker, return_type = return_type,
                          eval_string = eval_string)
      return(df)
    }
  # объединение результатов
  result %<>%  
    {
      .[!is.na(.)]
    } %>%
    MergeData_inList.byRow(.)
 
  return(result)
}
#
###
#' Функция BF оптимизации движка стратегии (PSOCK-кластер)
#' 
#' @param var.df DF с данными для перебора
#' @param data.xts XTS с котировками
#' @param FUN Функция анализа (OneThreadRun ветка функций)
#' @param linker_file Путь к linker файлу
#' @param balance_start Стартовый баланс
#' @param slips Слипы (вектор)
#' @param commissions Комиссии (вектор)
#' @param expiration Вектор с датами экспирации
#' @param ticker Тикер инструмента
#' @param return_type Тип доходности
#' @param export_varlist Вектор имён переменных, экспортируемых из .GlobalEnv в кластер (необязательно)
#' @param export_libs Вектор имён библиотек, экспортируемых в кластер
#' @param rolling_opt Упрощенный/полный perfomance анализ
#' @param eval_string Сторока с описанием переменных внутри lapply-цикла
#' @param 
#'
#' @return result DF с perfomance'ами по всем итерациям цикла 
#'
#' @export
BruteForceOpt_parallel_cl <- function(var.df, data.xts,
                                         FUN, 
                                         linker_file = 'bots/test/linker.R',
                                         balance_start, slips, commissions,
                                         expiration, ticker, return_type = 'ret',
                                         export_varlist = NULL, 
                                         export_libs = c('quantmod', 'xts', 'magrittr', 'tidyr', 
                                                         'PerformanceAnalytics', 'lubridate'),
                                         rolling_opt = FALSE, 
                                         eval_string) {
  #
  .CurrentEnv <- environment()
  #
  require(parallel)
  FUN <- match.fun(FUN)

  # запуск кластера
  parallel_cluster <- 
    detectCores() %>%
    makeCluster(.)#, type = 'PSOCK')
  ## Подгрузка данных в кластер
  clusterExport(parallel_cluster, envir = .CurrentEnv, 
                varlist = c('data.xts', 'FUN', 'linker_file', 
                            'balance_start', 'slips', 'commissions', 'expiration', 
                            'ticker', 'return_type', 'rolling_opt', 
                            'export_varlist', 'export_libs', 'eval_string'))
  # подгрузка библиотек
  clusterEvalQ(parallel_cluster, 
               { 
                 sapply(export_libs, library, character.only = TRUE)
                 source(linker_file) 
               })
  # подгрузка дополнительных переменных напрямую из .GlobalEnv (если нужно)
  if (!is.null(export_varlist)) {
    clusterExport(parallel_cluster, envir = .GlobalEnv, 
                  varlist = export_varlist) 
  }
  #
  result <- 
    var.df %>%
    parRapply(parallel_cluster,
              ., #1, 
              function(x) {
                #FUN(x, ...)
                FUN <- match.fun(FUN)
                temp_text <- paste0(
                  'df <- FUN(data.xts = data.xts,
                             rolling_opt = rolling_opt, 
                             balance_start = balance_start, slips = slips, commissions = commissions,
                             expiration = expiration, ticker = ticker, return_type = return_type, ', 
                             eval_string,')')
                eval(parse(text = temp_text))
                rm(temp_text)
                return(df)
             })
  # остановка кластера
  stopCluster(parallel_cluster)
  parallel_cluster <- c()  
  
  # объединение результатов
  result %<>%  
    {
      .[!is.na(.)]
    } %>%
    MergeData_inList.byRow(.)
  
  # проверка остановки кластера
  if(!is.null(parallel_cluster)) {
    stopCluster(parallel_cluster)
    parallel_cluster <- c()
  }
  #  
  return(result)
}
###
#' Функция BF оптимизации параметров стратегии 
#' 
#' @param x DF с данными для перебора
#' @param data.xts XTS с котировками
#' @param FUN Функция анализа (OneThreadRun ветка функций)
#' @param rolling_opt Упрощенный/полный perfomance анализ
#' @param balance_start Стартовый баланс
#' @param slips Слипы (вектор)
#' @param commissions Комиссии (вектор)
#' @param expiration Вектор с датами экспирации
#' @param ticker Тикер инструмента
#' @param return_type Тип доходности
#' @param eval_string Сторока с описанием координат переменных внутри foreach-цикла
#'
#' @return df DF с perfomance'ами по всем итерациям цикла 
#'
#' @export
BruteForceOpt <- function(x, data.xts, 
                          FUN, 
                          rolling_opt = FALSE,
                          balance_start, slips, commissions,
                          expiration, ticker, return_type = 'ret',
                          eval_string) { 
  #
  FUN <- match.fun(FUN)
  
  temp_text <- paste0(
    'df <-  
      foreach(i = 1:nrow(x)) %do% {
        FUN(data.xts = data.xts,
            rolling_opt = rolling_opt, 
            balance_start = balance_start, slips = slips, commissions = commissions,
            expiration = expiration, ticker = ticker, return_type = return_type, ',
            eval_string,')
      }')
  eval(parse(text = temp_text))
  rm(temp_text)
  
  df %<>%
    {
      .[!is.na(.)]
    } %>%
    MergeData_inList.byRow(.)  
  #
  return(df)
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
                         rolling_opt = FALSE, var_names = NULL,
                         balance_start, slips, commissions,
                         expiration, ticker, return_type = 'ret',
                         ...) {
  #
  FUN <- match.fun(FUN) 
  
  ## отработка робота
  data_strategy.list <- FUN(data_source = data.xts, 
                            balance_start = balance_start, slips = slips, commiss = commissions,
                            exp.vector = expiration, ticker, return_type = return_type, ...)
  
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
      trades_table.list <- TradesTable.calc(data = data_strategy.list[[2]], basket = FALSE, convert = TRUE)
      if (length(ticker) == 1) {
        trades_table.list[[1]]$TradeReturnPercent <- trades_table.list[[1]]$TradeReturn * 100 / balance_start
      }
      ### оценка perfomance-параметров
      perfomance_table <- PerfomanceTable(data = data_strategy.list[[1]], 
                                          states = data_strategy.list[[2]],
                                          trades_table = trades_table.list,
                                          balance_start = balance_start, 
                                          ret_type = return_type)
    }
  }
  # добавление использованных параметров
  if (!is.null(var_names)) {
    for (i in 1:length(var_names)) {
      dots <- list(...)
      temp_text <- paste0(
        'perfomance_table %<>% cbind.data.frame(.,',var_names[i],' = ',dots[[var_names[i]]],')')
      eval(parse(text = temp_text))
    }
  }
  #
  return(perfomance_table)
}