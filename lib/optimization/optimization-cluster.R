# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# SOCK-кластер функции для оптимизации стратегий:
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
###
#' Roller функция обучающей оптимизации движка стратегии (на SOCK кластерах)
#' 
#' @param data_slices Временные интервалы оптимизационных окон
#' @param var.df DF с данными для перебора
#' @param FUN Функция анализа (OneThreadRun ветка функций)
#' @param win_size Период обучения (нужно для более точной кластеризации)
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
###
#' Функция BF оптимизации движка стратегии (на SOCK-кластерах)
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
