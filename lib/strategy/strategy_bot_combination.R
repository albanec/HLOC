# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Функции для комбинации нескольких ботов:
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
#
###
#' Рачёт "сырых" торговых данных по пачке ботов
#' 
#' @param ohlc.xts
#' @param bot.list Лист с набором ботов (каждый лист содержит df по конкретному боту) 
#' каждый элемент листа - один бот с названием == тип бота, 
#' внутри - data.frame с параметрами и числом строк == числу ботов данного типа в корзине
#' @param balance_start
#' @param slips
#' @param commissions
#' @param return_type
#' @param expiration
#' @param ticker
#'
#' @export
BotCombination.raw_data <- function(ohlc.xts, bot.list,
                                    balance_start, slips, 
                                    commissions, return_type, expiration, ticker) {
  require(doParallel)
  
  # определение нужных окружений
  .CurrentEnv <- environment()
  .ParentEnv <- parent.frame(2)
  # регистрация ядер
  workers <- detectCores() - 1  
  registerDoParallel(cores = workers)
  
  # типы ботов, участвующие в торговле
  bot_names <- 
    foreach(i = 1:length(bot.list), .combine = c) %do% {
      bot.list[[i]]$name
    } %>%
    unique(.)
  # всего ботов
  n_bots <- length(bot.list)
  # проверка наличия и подгрузка gear-функций для нужных ботов
  env_list <- ls(name = .ParentEnv)
  strGearFUN_names <- 
    grep(pattern = 'StrGear_', x = env_list) %>%
    {
      env_list[.] %>%
      .[-grep(pattern = '.', x = ., fixed = TRUE)]
    }
  for (i in 1:length(bot_names)) {
    FUN_name <- paste0('StrGear_', bot_names[i])
    if (any(strGearFUN_names %in% FUN_name == TRUE)) {
      FUN <- get(as.character(FUN_name), mode = "function", envir = .ParentEnv)
      assign(paste0(FUN_name), FUN, envir = .CurrentEnv)
    } else {
      stop(paste0('ERROR(testBotCombination): Сan\'t find ',FUN_name,' function !!!'))
    }
  }

  # расчёт сырых данных по пакету ботов (на выходе - листы по каждому боту)
  raw_tradesTables.list <- 
    foreach(i = 1:workers) %dopar% {
      # распределение ботов по потоку
      map_range <- Delegate_mcore(i, n_bots, p = workers)
      # проверка на наличие задания для worker'а
      if (is.null(map_range)) {
        return(NA)
      } 
      # вычисления
      map_data <- bot.list[map_range]  
      # расчёт ботов
      result <- 
        foreach(i = 1:length(map_data)) %do% {
          x <- map_data[[i]]
          if (!is.data.frame(x)) {
            warning('WARNING(testBotCombination): strategies data wrong type', '\n')
          }
          # имя бота      
          bot_name <- x$name
          
          # подготовка eval-строки
          x <- x[, grep('_', names(x))]
          var_names <- names(x)
          eval_str <- 
            foreach(i = 1:length(var_names), .combine = c) %do% {
              paste0(var_names[i],'=',x[1, i])
            } %>%
            paste(., collapse = ",")
          # лист с параметрами бота (типичные параметры gear-функиций + специфичные переменные)
          temp_text <- paste0(
            'var.list <- list(data_source = ohlc.xts,
                              balance_start = balance_start, 
                              slips = slips, commiss = commissions, 
                              return_type = return_type,
                              exp.vector = expiration, 
                              ticker = ticker,
                              basket_handler = TRUE,',
                              eval_str,')')
          eval(parse(text = temp_text))
          rm(temp_text)
          # запуск gear-функции
          FUN_name <- paste0('StrGear_', bot_name) 
          one_bot_tradesTables <- do.call(FUN_name, var.list, envir = .CurrentEnv)  
          return(one_bot_tradesTables)
        }
      return(result)
    } %>%
    {
      .[!is.na(.)]
    } %>%
    unlist(., recursive = FALSE)
  #
  return(raw_tradesTables.list)
}


