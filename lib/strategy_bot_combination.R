# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Функции для комбинации нескольких ботов:
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
#
###
#' Рачёт "сырых" торговых данных по пачке ботов
#' 
#' @param ohlc_source XTS с полными котировками
#' @param from_date Начало торговли
#' @param to_date Конец торговли
#' @param lookback Обучающее окно (перед началом торговли)
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
BotCombination.raw_data <- function(ohlc_source,
                                    from_date, to_date, lookback = FALSE,
                                    bot.list,
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
            result <- lapply(1:length(map_data),
                function(i) {
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
                    # выделение нужных котировок
                    if (lookback == TRUE) {
                        temp_text <- paste0('lookback <- max(',eval_str,')')
                        eval(parse(text = temp_text))
                        rm(temp_text)
                        ohlc_source <- Subset_TradeOHLC(ohlc_source = ohlc_source, 
                            from_date, to_date, 
                            lookback = lookback)
                    } else {
                        ohlc_source <- Subset_TradeOHLC(ohlc_source = ohlc_source, from_date, to_date, lookback = NULL)
                    }
                    # лист с параметрами бота (типичные параметры gear-функиций + специфичные переменные)
                    temp_text <- paste0(
                        'var.list <- list(data_source = ohlc_source,
                            balance_start = balance_start, 
                            slips = slips, commiss = commissions, 
                            return_type = return_type,
                            exp.vector = expiration, 
                            ticker = ticker,
                            basket_handler = TRUE,',
                            eval_str,')'
                    )
                    eval(parse(text = temp_text))
                    rm(temp_text)
                    # запуск gear-функции
                    FUN_name <- paste0('StrGear_', bot_name) 
                    one_bot_tradesTables <- do.call(FUN_name, var.list, envir = .CurrentEnv)    
                    comment(one_bot_tradesTables) <- bot_name
                    return(one_bot_tradesTables)
                })
            return(result)
        } %>%
        {
            .[!is.na(.)]
        } %>%
        unlist(., recursive = FALSE)
    #
    return(raw_tradesTables.list)
}
#
#' Пакетный обработчик сделок
#'
#' @param ohlc.xts Котировки инструмента
#' @param DATA Данные торговли по ботам
#' @param bot.list Лист с параметрами ботов на данном временном интервале
#' @param balance_start Стартовый баланс
#' @param commiss Коммиссия по инструменту
#' @param ... 
BotCombination.handler <- function(ohlc.xts,
                                   DATA,
                                   bot.list = bot.list[[i]],
                                   balance_start,
                                   commiss,
                                   ...) {
    # определение нужных окружений
    .CurrentEnv <- environment()
    .ParentEnv <- parent.frame(2)
    # всего ботов
    n_bots <- length(bot.list)

    ### нормирование временных шкал
    # формирование нормирующей последовательности и дополнительных столблцов
    index_norm <- 
        lapply(1:n_bots,
            function(x) {
                DATA[[x]][[2]]$pos
            }) %>%
        do.call(merge, .) %>%
        .$pos %>%
        index(.)
  
    ### доп. столбцы по каждому боту
    DATA <- lapply(1:n_bots,
        function(x) {
            #cat(x, '\n')  
            # нормирование временных индексов
            DATA[[x]][[2]] <-
                merge(DATA[[x]][[2]], temp = xts(NULL, order.by = index_norm)) %>%
                {
                    .$temp <- NULL
                    return(.)
                }
            # начальный баланс
            DATA[[x]][[2]]$balance <- NA
            #DATA[[x]][[2]]$balance[1] <- balance_start #* bot.list[[x]]$weight[1]
            # начальное число синтетических контрактов корзины
            DATA[[x]][[2]]$n <- NA
            #states$n <- 0
            # прочее
            DATA[[x]][[2]]$diff.n <- NA
            DATA[[x]][[2]]$diff.n[1] <- 0
            DATA[[x]][[2]]$margin <- NA
            DATA[[x]][[2]]$commiss <- NA
            DATA[[x]][[2]]$equity <- NA
            DATA[[x]][[2]]$im.balance <- NA
            DATA[[x]][[2]]$im.balance[1] <- 0
            # вес бота в корзине
             #DATA[[x]][[2]]$weight <- 1
        
            ### корректировка данных (для тех строк, где данный бот без action, но возможны action других ботов)
            # заполнение price 
            temp_ind <-
                which(is.na(DATA[[x]][[2]]$Price)) %>%
                DATA[[x]][[2]]$Price[.] %>%
                index(.)
            if (length(temp_ind) > 0) {
                DATA[[x]][[2]]$Price[temp_ind] <- ohlc.xts[temp_ind, paste0(bot.list[[x]]$ticker,'.Open')]
            }
            rm(temp_ind)
            # заполнение pos
            temp_ind <-
                which(is.na(DATA[[x]][[2]]$pos)) %>%
                DATA[[x]][[2]]$pos[.] %>%
                index(.)
            if (length(temp_ind) != 0) {
                # удаление индексов, которых нет в full-данных по боту
                temp_ind <- temp_ind[temp_ind %in% index(DATA[[x]][[1]]$pos)]
                # перенос pos
                DATA[[x]][[2]]$pos[temp_ind] <- DATA[[x]][[1]]$pos[temp_ind]
                DATA[[x]][[2]]$pos[is.na(DATA[[x]][[2]]$pos)] <- 0
            }
            rm(temp_ind)
            # заполнение pos.bars
            temp_ind <-
                which(is.na(DATA[[x]][[2]]$pos.bars)) %>%
                DATA[[x]][[2]]$pos.bars[.] %>%
                index(.)
            if (length(temp_ind) != 0) {
                # удаление индексов, которых нет в full-данных по боту
                temp_ind <- temp_ind[temp_ind %in% index(DATA[[x]][[1]]$pos.bars)]
                # перенос pos.bars
                DATA[[x]][[2]]$pos.bars[temp_ind] <- DATA[[x]][[1]]$pos.bars[temp_ind]
                DATA[[x]][[2]]$pos.bars[is.na(DATA[[x]][[2]]$pos.bars)] <- 0
            }
            rm(temp_ind)
            # заполнение action
            DATA[[x]][[2]]$action[is.na(DATA[[x]][[2]]$action)] <- 0
            # заполение pos.open
            DATA[[x]][[2]]$pos.open[is.na(DATA[[x]][[2]]$pos.open)] <- 0
            # заполение pos.close
            DATA[[x]][[2]]$pos.close[is.na(DATA[[x]][[2]]$pos.close)] <- 0
            # перерасчёт return'ов
            #
            return(DATA[[x]])    
        }) 
    ### выгрузка функций для расчета данных по боту
    # типы ботов, участвующие в торговле
    bot_names <-
        sapply(1:n_bots,
            function(x) {
                bot.list[[x]]$name
            }) %>%
        unique(.)
    # проверка наличия и подгрузка CalcTrades_inStates_one_trade-функций (посделочные обработчики) для нужных ботов 
    env_list <- ls(name = .ParentEnv)
    # вектор имен one_trade функций в .ParentEnv
    FUN_names <- 
        grep(pattern = 'CalcTrades_inStates_one_trade.', env_list) %>%
        env_list[.]
    for (i in 1:length(bot_names)) {
        temp.name <- paste0('CalcTrades_inStates_one_trade.', bot_names[i])
        if (any(FUN_names %in% temp.name == TRUE)) {
            FUN <- get(as.character(temp.name), mode = "function", envir = .ParentEnv)
            assign(paste0(temp.name), FUN, envir = .CurrentEnv)
        } else {
            stop(paste0('ERROR(BotCombination): Сan\'t find ',temp.name,' function !!!'))
        }
    }
    rm(temp.name)
    # перезадаем вектор функций для нужных ботов
    FUN_names <- paste0('CalcTrades_inStates_one_trade.', bot_names)

    ### создание cache для ботов и для портфеля
    # окружение для кэша
    .CacheEnv <- new.env()
    # кэш для ботов
    target_col <- c('n', 'diff.n', 'balance', 'im', 'im.balance', 'commiss',
        'margin', 'perfReturn', 'equity')
    temp.cache <- lapply(1:n_bots,
        function(x) {
            DATA[[x]][[2]][, target_col] %>%
            as.data.frame(., row.names=NULL)  
        })
    assign('bot.cache', temp.cache, envir = .CacheEnv)  
    # кэш для портфеля
    temp.cache <- 
        lapply(1:length(target_col[!target_col %in% 'im']),
            function(x) {
                xts(rep(NA, length(index_norm)), order.by = index_norm)
            }) %>%
        do.call(merge, .)
    names(temp.cache) <- target_col[!target_col %in% 'im']
    temp.cache$balance[1] <- balance_start
    assign('portfolio.cache', temp.cache, envir = .CacheEnv)
    rm(temp.cache)

# ///current
    ### перебор расчёта сделок по единой шкале индексов
        # (данные по каждому боту корректируются единым обработчиком + формируются данные в целом по портфелю)
    lapply(1:length(index_norm),
        function(row_num) {
            # индекс строки
            temp.index <- index_norm[row_num]
      
            ### определение развесовки портфеля на индексе
                #! на данный момент развесовка зависит только от количества ботов
            # open_pos <- 
            #     lapply(1:n_bots,
            #         function(x) {
            #             temp.pos <- ifelse(!is.na(DATA[[x]][[2]]$pos[[row_num]]),
            #                 DATA[[x]][[2]]$pos[[row_num]],
            #                 0) 
            #             temp.pos_bars <- ifelse(!is.na(DATA[[x]][[2]]$pos.bars[[row_num]]),
            #                 DATA[[x]][[2]]$pos.bars[[row_num]],
            #                 0)
            #             out <- ifelse(temp.pos != 0 & temp.pos_bars == 0,
            #                 1,
            #                 0)
            #             return(out)
            #         }) %>%
            #     do.call(sum, .)
            # определение баланса на индексе
            temp.portfolio.cache <- get('portfolio.cache', envir = .CacheEnv)
            # было: баланс может делится относительно ботов, входящих в позы (тогда надо раскомментить код для open_pos)
            #available_balance <- temp.portfolio.cache$balance[row_num - 1] / open_pos
            # стало: баланс делится в зависимости от количества ботов 
            available_balance <- temp.portfolio.cache$balance[row_num - 1] / n_bots
            #rm(open_pos)
    
            ### перебор по каждому боту
            # подгрузка кэша по ботам
            temp.cache <- get('bot.cache', envir = .CacheEnv)
            for (bot_num in 1:n_bots) {
                ## вызов функции-посделочного обработчика
                # лист с переменными для расчёта строки
                var.list <- list(
                    cache = temp.cache[[bot_num]], row_ind = row_num,
                    pos = DATA[[bot_num]][[2]]$pos[row_num],
                    pos_bars = DATA[[bot_num]][[2]]$pos.bars[row_num],
                    # !!! в дальнейшем применить ГО по инструменту, а не корзине
                    IM = ohlc_source$IM[temp.index],
                    cret = DATA[[bot_num]][[2]]$cret[row_num],
                    balance_start = 0,
                    commiss = commiss,
                    external_balance = available_balance,
                    ...
                )
                temp.cache[[bot_num]] <- do.call(FUN_names[bot_num], var.list, envir = .CurrentEnv)
                rm(var.list)
            }
            assign('bot.cache', temp.cache, envir = .CacheEnv)

            ### расчёт общих данных по корзине 
                # на первом индексе большинство параметров по нулям, так что
            if (row_num != 1) {
                # количество контрактов по портфолио
                temp.portfolio.cahce$n[row_num] <-
                    lapply(1:n_bots,
                        function(x) {
                            temp.cache[[x]]$n[row_num]
                        }) %>%
                    do.call(sum, .)
                # изменение контрактов на индексе
                temp.portfolio.cahce$diff.n[row_num] <-
                    lapply(1:n_bots,
                        function(x) {
                            bot.cache[[x]]$diff.n[row_num]  
                        }) %>%
                    do.call(sum, .)
                # суммарное equity по портфолио  
                temp.portfolio.cahce$equity[row_num] <-
                    lapply(1:n_bots,
                        function(x) {
                            bot.cache[[x]]$equity[row_num]
                        }) %>%
                    do.call(sum, .)
                # суммарный perfReturn на индексе
                temp.portfolio.cahce$perfReturn[row_num] <-
                    lapply(1:n_bots,
                        function(x) {
                            bot.cache[[x]]$perfReturn[row_num]  
                        }) %>%
                    do.call(sum, .)
                # суммарная вариационка на такте
                temp.portfolio.cahce$margin[row_num] <-
                    lapply(1:n_bots,
                        function(x) {
                            bot.cache[[x]]$margin[row_num]  
                        }) %>%
                    do.call(sum, .)
                # суммарная комиссия на такте
                temp.portfolio.cahce$commiss[row_num] <-
                    lapply(1:n_bots, 
                        function(x) {
                            bot.cache[[x]]$commiss[row_num]
                        }) %>%
                    do.call(sum, .)
                # суммарное ГО на такте
                temp.portfolio.cahce$im.balance[row_num] <-
                    lapply(1:n_bots,
                        function(x) {
                            bot.cache[[x]]$im.balance[row_num]  
                        }) %>%
                    do.call(sum, .)
                # баланс портфолио на индексе
                temp.portfolio.cahce$balance[row_num] <- 
                balance_start + temp.portfolio.cahce$equity[row_num] - temp.portfolio.cahce$im.balance[row_num]
            } else {
                temp.portfolio.cahce$n[row_num] <- 0
                temp.portfolio.cahce$diff.n[row_num] <- 0
                temp.portfolio.cahce$equity[row_num] <- 0
                temp.portfolio.cahce$perfReturn[row_num] <- 0
                temp.portfolio.cahce$margin[row_num] <- 0
                temp.portfolio.cahce$commiss[row_num] <- 0 
                temp.portfolio.cahce$im.balance[row_num] <- 0
                temp.portfolio.cahce$balance[row_num] <- balance_start
            }
            # запись данных по портфолио в кэш
            assign('portfolio.cache', temp.portfolio.cache, envir = .CacheEnv)
            rm(temp.portfolio.cache, temp.cache)
        }
    )

    ### рузультаты
    get('bot.cache', envir = .CacheEnv) %>% 
    foreach(i = 1:n_bots) %do% {
        DATA[[i]][[2]][, target_col] <- .[[i]][, target_col]
    }
    portfolio.result <- get('portfolio.cache', envir = .CacheEnv)
    #
    rm(.CacheEnv)
    #
    return(list(DATA, portfolio.result))
}
#