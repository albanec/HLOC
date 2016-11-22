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
RollerOpt.learning <- function(data_slices,
                               var.df,
                               FUN,
                               linker_file = 'main/test/linker.R',
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
    makeCluster(.)
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
                                         temp.text <- paste(
                                           'df <- FUN(data.xts = data.xts,
                                                      rolling_opt = rolling_opt, 
                                                      balance_start = balance_start, 
                                                      slips = slips, commissions = commissions,
                                                      expiration = expiration, ticker = ticker, 
                                                      return_type = return_type, ',eval_string,')', 
                                           sep = '')
                                         eval(parse(text = temp.text))
                                         rm(temp.text)
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
#
###
#' Функция BF оптимизации движка стратегии (multithread)
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
BruteForceOpt_parallel_mnode <- function(var.df, data.xts,
                                         FUN, 
                                         linker_file = 'main/test/linker.R',
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
    makeCluster(.)
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
                temp.text <- paste(
                  'df <- FUN(data.xts = data.xts,
                             rolling_opt = rolling_opt, 
                             balance_start = balance_start, slips = slips, commissions = commissions,
                             expiration = expiration, ticker = ticker, return_type = return_type, ', 
                             eval_string,')', 
                  sep = '')
                eval(parse(text = temp.text))
                rm(temp.text)
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
#' @param var.df DF с данными для перебора
#' @param data.xts XTS с котировками
#' @param FUN Функция анализа (OneThreadRun ветка функций)
#' @param rolling_opt Упрощенный/полный perfomance анализ
#' @param balance_start Стартовый баланс
#' @param slips Слипы (вектор)
#' @param commissions Комиссии (вектор)
#' @param expiration Вектор с датами экспирации
#' @param ticker Тикер инструмента
#' @param return_type Тип доходности
#' @param eval_string Сторока с описанием переменных внутри lapply-цикла
#'
#' @return result DF с perfomance'ами по всем итерациям цикла 
#'
#' @export
BruteForceOpt <- function(var.df, data.xts, 
                          FUN, 
                          rolling_opt = FALSE,
                          balance_start, slips, commissions,
                          expiration, ticker, return_type = 'ret',
                          k_mm = NULL,
                          eval_string) { 
  #
  FUN <- match.fun(FUN)

  result <- 
    var.df %>% 
    apply(., 1, 
          function(x){
            temp.text <- paste(
              'df <- FUN(data.xts = data.xts,
                         rolling_opt = rolling_opt, 
                         balance_start = balance_start, slips = slips, commissions = commissions,
                         expiration = expiration, ticker = ticker, return_type = return_type, ',
                         eval_string,')',
              sep = '')
            eval(parse(text = temp.text))
            rm(temp.text)
            return(df)
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
      temp_text <- paste('perfomance_table %<>% cbind.data.frame(.,',var_names[i],' = ',dots[[var_names[i]]],')', 
                         sep = '')
      eval(parse(text = temp_text))
    }
  }
  #
  return(perfomance_table)
}