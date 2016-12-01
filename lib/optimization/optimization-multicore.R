# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Multicore-функции для оптимизации стратегий:
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
###
#' Roller функция обучающей оптимизации движка стратегии (multicore)
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
#' @param rolling_opt Упрощенный/полный perfomance анализ
#' @param eval_string Сторока с описанием переменных внутри lapply-цикла
#' @param 
#'
#' @return result DF с perfomance'ами по всем итерациям цикла 
#'
#' @export               
RollerOpt_learning_mc <- function(data_slices,
                                  var.df,
                                  FUN, win_size,
                                  linker_file = 'bots/test/linker.R',
                                  balance_start, slips, commissions,
                                  expiration, ticker, return_type = 'ret',
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
      ## Округление центров до значений точек пространства  
      clustFull.data[[2]] %<>%
        {
          for (i = 1:ncol(.[, !(colnames(.) %in% c('k_mm', 'profit.norm'))])) {
            .[, i] <- .[, i] - .[, i] %% 5
          }
          return(.)    
        }  
      #
      return(clustFull.data)
    }
  #
  return(cluster_data.list)
}
#
###
#' Функция BF оптимизации движка стратегии (multicore)
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
#' @param rolling_opt Упрощенный/полный perfomance анализ
#' @param eval_string Сторока с описанием переменных внутри lapply-цикла
#' @param 
#'
#' @return result DF с perfomance'ами по всем итерациям цикла 
#'
#' @export
BruteForceOpt_parallel_mc <- function(var.df, data.xts,
                                      FUN, 
                                      linker_file = 'bots/test/linker.R',
                                      balance_start, slips, commissions,
                                      expiration, ticker, return_type = 'ret',
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
