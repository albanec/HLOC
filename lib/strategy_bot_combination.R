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
                                    commissions, return_type, expiration, ticker,
                                    add_benchmark = FALSE) {
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
    FUN_name_vector <- 
        grep(pattern = 'StrGear_', x = env_list) %>%
        {
            env_list[.] %>%
            {
                .[-grep(pattern = '.', x = ., fixed = TRUE)]
            }
        }
    for (i in 1:length(bot_names)) {
        FUN_name <- paste0('StrGear_', bot_names[i])
        if (any(FUN_name_vector %in% FUN_name == TRUE)) {
            FUN <- get(as.character(FUN_name), mode = "function", envir = .ParentEnv)
            assign(paste0(FUN_name), FUN, envir = .CurrentEnv)
        } else {
            stop(paste0('ERROR(testBotCombination): Сan\'t find ',FUN_name,' function !!!'))
        }
    }

    # расчёт сырых данных по пакету ботов (на выходе - листы по каждому боту)
    TRADE_TABLE <- 
        foreach(i = 1:workers) %dopar% {
            # распределение ботов по потоку
            map_range <- Delegate_mcore(i, n_bots, p = workers)
            # проверка на наличие задания для worker'а
            if (is.null(map_range)) {
                return(NA)
            } 
            ### вычисления
            map_data <- bot.list[map_range]    
            # расчёт ботов
            result <- lapply(1:length(map_data),
                function(x) {
                    if (!is.data.frame(map_data[[x]])) {
                        warning('WARNING(testBotCombination): strategies data wrong type', '\n')
                    }
                    # имя бота            
                    bot_name <- map_data[[x]]$name
                    # подготовка eval-строки
                    var <- map_data[[x]][, grep('_', names(map_data[[x]]))]
                    var_names <- names(var)
                    eval_str <- 
                        foreach(i = 1:length(var_names), .combine = c) %do% {
                            paste0(var_names[i],'=',var[1, i])
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
                            balance_start = 0, 
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
                    tradeTable <- do.call(FUN_name, var.list, envir = .CurrentEnv)    
                    comment(tradeTable) <- bot_name
                    return(tradeTable)
                })
            return(result)
        } %>%
        {
            .[!is.na(.)]
        } %>%
        unlist(., recursive = FALSE)
    #
    if (add_benchmark == TRUE) {
        # проверка наличия и подгрузка handler-функций для нужных ботов
            FUN_name_vector <- 
                grep(pattern = '.trades_handler', x = env_list) %>%
                env_list[.]
        for (i in 1:length(bot_names)) {
            FUN_name <- paste0('StrGear_',bot_names[i],'.trades_handler')
            if (any(FUN_name_vector %in% FUN_name == TRUE)) {
                FUN <- get(as.character(FUN_name), mode = "function", envir = .ParentEnv)
                assign(paste0(FUN_name), FUN, envir = .CurrentEnv)
            } else {
                stop(paste0('ERROR(testBotCombination): Сan\'t find ',FUN_name,' function !!!'))
            }
        }
        # расчёт benchmark-данных по каждому боту (на выходе - листы по каждому боту)
        BENCHMARK_TABLE <- 
            foreach(i = 1:workers) %dopar% {
                # распределение ботов по потоку
                map_range <- Delegate_mcore(i, n_bots, p = workers)
                # проверка на наличие задания для worker'а
                if (is.null(map_range)) {
                    return(NA)
                } 
                ### вычисления
                map_data <- bot.list[map_range]    
                tradeTable <- TRADE_TABLE[map_range] 
                # расчёт ботов
                tradeTable <- lapply(1:length(map_data),
                    function(x) {
                        if (!is.data.frame(map_data[[x]])) {
                            warning('WARNING(testBotCombination): strategies data wrong type', '\n')
                        }
                        # имя бота            
                        bot_name <- map_data[[x]]$name
                        
                        # подготовка eval-строки
                        k_mm <- coredata(
                            map_data[[x]][, grep('k_mm', names(map_data[[x]]))]
                        )
                        eval_str <- 
                            paste0(
                                'k_mm =',k_mm#,'tick_price = 1'
                            )#%>%
                            #paste(., collapse = ",")
                        # лист с параметрами бота (типичные параметры gear-функиций + специфичные переменные)
                        temp_text <- paste0(
                            'var.list <- list(data = tradeTable[[x]][[1]],
                                states = tradeTable[[x]][[2]], 
                                ohlc_source = ohlc_source,
                                commiss = commissions,
                                balance_start = balance_start / n_bots,',
                                eval_str,')'
                        )
                        eval(parse(text = temp_text))
                        rm(temp_text)
                        
                        ## запуск handler-функции
                        FUN_name <- paste0('StrGear_',bot_name,'.trades_handler') 
                        tradeTable[[x]] <- do.call(FUN_name, var.list, envir = .CurrentEnv)    
                        comment(tradeTable[[x]]) <- bot_name
                        
                        ## добавление нужных на следующих этапах столбцов
                        temp.text <- paste0(
                            'tradeTable[[x]][[1]]$',ticker,'.n <- tradeTable[[x]][[1]]$n;',
                            'tradeTable[[x]][[1]]$',ticker,'.diff.n <- tradeTable[[x]][[1]]$diff.n;',
                            'tradeTable[[x]][[1]]$',ticker,'.commiss <- tradeTable[[x]][[1]]$commiss;',
                            'tradeTable[[x]][[1]]$',ticker,'.equity <- tradeTable[[x]][[1]]$equity;',
                            'tradeTable[[x]][[1]]$',ticker,'.perfReturn <- tradeTable[[x]][[1]]$perfReturn;',
                            #
                            'tradeTable[[x]][[2]]$',ticker,'.n <- tradeTable[[x]][[2]]$n;',
                            'tradeTable[[x]][[2]]$',ticker,'.diff.n <- tradeTable[[x]][[2]]$diff.n;',
                            'tradeTable[[x]][[2]]$',ticker,'.commiss <- tradeTable[[x]][[2]]$commiss;',
                            'tradeTable[[x]][[2]]$',ticker,'.equity <- tradeTable[[x]][[2]]$equity;',
                            'tradeTable[[x]][[2]]$',ticker,'.perfReturn <- tradeTable[[x]][[2]]$perfReturn;'
                        )
                        eval(parse(text = temp.text))
                        # names(tradeTable[[x]][[1]])[names(tradeTable[[x]][[1]]) == 'n'] <- paste0(ticker,'.n')
                        # names(tradeTable[[x]][[1]])[names(tradeTable[[x]][[1]]) == 'diff.n'] <- paste0(ticker,'.diff.n')
                        names(tradeTable[[x]][[1]])[names(tradeTable[[x]][[1]]) == 'Price'] <- paste0(ticker,'.Price')
                        # names(tradeTable[[x]][[1]])[names(tradeTable[[x]][[1]]) == 'commiss'] <- paste0(ticker,'.commiss')
                        # names(tradeTable[[x]][[1]])[names(tradeTable[[x]][[1]]) == 'equity'] <- paste0(ticker,'.equity')
                        # names(tradeTable[[x]][[1]])[names(tradeTable[[x]][[1]]) == 'perfReturn'] <- paste0(ticker,'.equity')
                        #
                        # names(tradeTable[[x]][[2]])[names(tradeTable[[x]][[2]]) == 'n'] <- paste0(ticker,'.n')
                        # names(tradeTable[[x]][[2]])[names(tradeTable[[x]][[2]]) == 'diff.n'] <- paste0(ticker,'.diff.n')
                        names(tradeTable[[x]][[2]])[names(tradeTable[[x]][[2]]) == 'Price'] <- paste0(ticker,'.Price')
                        # names(tradeTable[[x]][[2]])[names(tradeTable[[x]][[2]]) == 'commiss'] <- paste0(ticker,'.commiss')
                        # names(tradeTable[[x]][[2]])[names(tradeTable[[x]][[2]]) == 'equity'] <- paste0(ticker,'.equity')
                        # names(tradeTable[[x]][[2]])[names(tradeTable[[x]][[2]]) == 'perfReturn'] <- paste0(ticker,'.equity')
                        names(tradeTable[[x]]) <- c('full', 'states')
                        #
                        return(tradeTable[[x]])
                    })
                return(tradeTable)
            } %>%
            {
                .[!is.na(.)]
            } %>%
            unlist(., recursive = FALSE)
        #
        return(list(TRADE_TABLE, BENCHMARK_TABLE))
    }
    #
    return(TRADE_TABLE)
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
                                   benchmark = FALSE,
                                   bot.list = bot.list[[i]],
                                   balance_start,
                                   commiss,
                                   ##dots параметы
                                   k_mm, tick_price ) {
    # определение нужных окружений
    .CurrentEnv <- environment()
    .ParentEnv <- parent.frame(2)
    # всего ботов
    n_bots <- length(bot.list)

    ### нормирование временных шкал для листов сделок
    # формирование нормирующей последовательности и дополнительных столблцов
    index_norm <- 
        lapply(1:n_bots,
            function(x) {
                DATA[[x]][[2]]$pos
            }) %>%
        do.call(merge, .) %>%
        .$pos %>%
        index(.)

    ### формирование листа данных по портфелю
        # так же, как ми ботами - лист с посвечным xts и лист с данными на сделках
    PORTFOLIO <- list(
        xts(NULL, order.by = index(DATA[[1]][[1]])),
        xts(NULL, order.by = index_norm)
    ) 
  
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
            DATA[[x]][[2]]$perfReturn <- NA
            DATA[[x]][[2]]$equity <- NA
            DATA[[x]][[2]]$im.balance <- NA
            DATA[[x]][[2]]$im.balance[1] <- 0
            # вес бота в корзине
             #DATA[[x]][[2]]$weight <- 1
        
            ### корректировка данных (для тех строк, где данный бот без action, но возможны action других ботов)
            # выделение индексов, на которых у бота нет action
            temp_ind <-
                which(is.na(DATA[[x]][[2]]$pos)) %>%
                DATA[[x]][[2]]$pos[.] %>%
                index(.)
            # удаление индексов, которых нет в full-данных по боту
            temp_ind <- temp_ind[temp_ind %in% index(DATA[[x]][[1]]$pos)]
            # заполнение данных
            if (length(temp_ind) != 0) {
                # заполнение $price 
                DATA[[x]][[2]]$Price[temp_ind] <- ohlc.xts[temp_ind, paste0(bot.list[[x]]$ticker,'.Open')]
                # перенос $pos
                DATA[[x]][[2]]$pos[temp_ind] <- DATA[[x]][[1]]$pos[temp_ind]
                DATA[[x]][[2]]$pos[is.na(DATA[[x]][[2]]$pos)] <- 0
                # перенос $pos.bars
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
            # перерасчёт $ret
            DATA[[x]][[2]]$ret <- CalcReturn(DATA[[x]][[2]]$Price, type = 'ret') * stats::lag(DATA[[x]][[2]]$pos)
            DATA[[x]][[2]]$ret[is.na(DATA[[x]][[2]]$ret)] <- 0
            # перерасчёт $cret
            #/// для не-Si инструментов надо переписать расчет $cret (b $Price, возможно)
            DATA[[x]][[2]]$cret <- DATA[[x]][[2]]$ret
            #DATA[[x]][[2]]$cret[is.na(DATA[[x]][[2]]$cret)] <- 0
            #
            return(DATA[[x]])    
        }) 
    
    ### выгрузка функций для расчета данных по боту
    # типы ботов, участвующие в торговле
    bot_names <-
        sapply(1:n_bots,
            function(x) {
                bot.list[[x]]$name
            }) #%>%
        #unique(.)
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
    target_col <- c('n', 'diff.n', 'balance', 'im', 
        'im.balance', 'commiss',
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
                    cache = temp.cache[[bot_num]], 
                    row_ind = row_num,
                    pos = DATA[[bot_num]][[2]]$pos[row_num],
                    pos_bars = DATA[[bot_num]][[2]]$pos.bars[row_num],
                    # !!! в дальнейшем применить ГО по инструменту, а не корзине
                    IM = ohlc_source$IM[temp.index],
                    cret = DATA[[bot_num]][[2]]$cret[row_num],
                    balance_start = 0,
                    commiss = commiss,
                    external_balance = available_balance,
                    #dots парамеры
                    k_mm = k_mm, 
                    widthDCI = DATA[[bot_num]][[2]]$widthDCI[row_num], 
                    tick_price = tick_price
                )
                temp.cache[[bot_num]] <- do.call(FUN_names[bot_num], var.list, envir = .CurrentEnv)
                rm(var.list)
            }
            assign('bot.cache', temp.cache, envir = .CacheEnv)

            ### расчёт общих данных по корзине 
                # на первом индексе большинство параметров по нулям, так что
            if (row_num != 1) {
                # количество контрактов по портфолио
                temp.portfolio.cache$n[row_num] <-
                    lapply(1:n_bots,
                        function(x) {
                            temp.cache[[x]]$n[row_num]
                        }) %>%
                    do.call(sum, .)
                # изменение контрактов на индексе
                temp.portfolio.cache$diff.n[row_num] <-
                    lapply(1:n_bots,
                        function(x) {
                            temp.cache[[x]]$diff.n[row_num]  
                        }) %>%
                    do.call(sum, .)
                # суммарное equity по портфолио  
                temp.portfolio.cache$equity[row_num] <-
                    lapply(1:n_bots,
                        function(x) {
                            temp.cache[[x]]$equity[row_num]
                        }) %>%
                    do.call(sum, .)
                # суммарный perfReturn на такте
                temp.portfolio.cache$perfReturn[row_num] <-
                    lapply(1:n_bots,
                        function(x) {
                            temp.cache[[x]]$perfReturn[row_num]  
                        }) %>%
                    do.call(sum, .)
                # суммарная вариационка на такте
                temp.portfolio.cache$margin[row_num] <-
                    lapply(1:n_bots,
                        function(x) {
                            temp.cache[[x]]$margin[row_num]  
                        }) %>%
                    do.call(sum, .)
                # суммарная комиссия на такте
                temp.portfolio.cache$commiss[row_num] <-
                    lapply(1:n_bots, 
                        function(x) {
                            temp.cache[[x]]$commiss[row_num]
                        }) %>%
                    do.call(sum, .)
                # суммарное ГО на такте
                temp.portfolio.cache$im.balance[row_num] <-
                    lapply(1:n_bots,
                        function(x) {
                            temp.cache[[x]]$im.balance[row_num]  
                        }) %>%
                    do.call(sum, .)
                # баланс портфолио на индексе
                temp.portfolio.cache$balance[row_num] <- 
                    balance_start + temp.portfolio.cache$equity[row_num] - temp.portfolio.cache$im.balance[row_num]
            } else {
                temp.portfolio.cache$n[row_num] <- 0
                temp.portfolio.cache$diff.n[row_num] <- 0
                temp.portfolio.cache$equity[row_num] <- 0
                temp.portfolio.cache$perfReturn[row_num] <- 0
                temp.portfolio.cache$margin[row_num] <- 0
                temp.portfolio.cache$commiss[row_num] <- 0 
                temp.portfolio.cache$im.balance[row_num] <- 0
                temp.portfolio.cache$balance[row_num] <- balance_start
            }
            # запись данных по портфолио в кэш
            assign('portfolio.cache', temp.portfolio.cache, envir = .CacheEnv)
            rm(temp.portfolio.cache, temp.cache)
        }
    )

    ### результаты
    ### данные по ботам
    DATA <- 
        get('bot.cache', envir = .CacheEnv) %>% 
        {
            foreach(i = 1:n_bots) %do% {
               
                ## Расчёт состояний
                # Изменение контрактов на такте
                DATA[[i]][[2]]$n <- .[[i]]$n
                DATA[[i]][[2]]$diff.n <- .[[i]]$diff.n 
                # Расчёт баланса, заблокированного на ГО
                DATA[[i]][[2]]$im.balance <- .[[i]]$im.balance
                # Расчёт комиссии на такте
                DATA[[i]][[2]]$commiss <- .[[i]]$commiss
                # Расчёт вариационки
                DATA[[i]][[2]]$margin <- .[[i]]$margin
                # расчёт equity по корзине в states
                DATA[[i]][[2]]$perfReturn <- .[[i]]$perfReturn
                DATA[[i]][[2]]$equity <- .[[i]]$equity
                # Расчёт баланса
                DATA[[i]][[2]]$balance <- .[[i]]$balance
                     
                ## Перенос данных из state в full таблицу
                # перенос данных по количеству контрактов корзины
                DATA[[i]][[1]]$n <-
                    merge(DATA[[i]][[1]], DATA[[i]][[2]]$n) %>%
                    {
                        t <- .
                        t$n <- na.locf(t$n)
                        return(t$n)
                    }
                # перенос данных по изменению контрактов корзины
                DATA[[i]][[1]]$diff.n <-
                    merge(DATA[[i]][[1]], DATA[[i]][[2]]$diff.n) %>%
                    {
                        t <- .
                        t$diff.n[is.na(t$diff.n)] <- 0
                        return(t$diff.n)
                    }
                # перенос данных по комиссии корзины
                DATA[[i]][[1]]$commiss <-
                    merge(DATA[[i]][[1]], DATA[[i]][[2]]$commiss) %>%
                    {
                        t <- .
                        t$commiss[is.na(t$commiss)] <- 0
                        return(t$commiss)
                    }
                # перенос данных по суммарному ГО
                DATA[[i]][[1]]$im.balance <-
                    merge(DATA[[i]][[1]], DATA[[i]][[2]]$im.balance) %>%
                    {
                        t <- .
                        t$im.balance <- na.locf(t$im.balance)
                        return(t$im.balance)
                    }
                
                ## Расчёт показателей в full данных
                # расчёт вариационки в data
                DATA[[i]][[1]]$margin <- stats::lag(DATA[[i]][[1]]$n) * DATA[[i]][[1]]$cret
                DATA[[i]][[1]]$margin[1] <- 0
                # расчёт equity по корзине в DATA[[i]][[1]]
                DATA[[i]][[1]]$perfReturn <- DATA[[i]][[1]]$margin - DATA[[i]][[1]]$commiss
                DATA[[i]][[1]]$equity <- cumsum(DATA[[i]][[1]]$perfReturn)
                
                ## добавление нужных на следующих этапах столбцов
                temp.text <- paste0(
                    'DATA[[i]][[1]]$',ticker,'.n <- DATA[[i]][[1]]$n;',
                    'DATA[[i]][[1]]$',ticker,'.diff.n <- DATA[[i]][[1]]$diff.n;',
                    'DATA[[i]][[1]]$',ticker,'.commiss <- DATA[[i]][[1]]$commiss;',
                    'DATA[[i]][[1]]$',ticker,'.equity <- DATA[[i]][[1]]$equity;',
                    'DATA[[i]][[1]]$',ticker,'.perfReturn <- DATA[[i]][[1]]$perfReturn;',
                    #
                    'DATA[[i]][[2]]$',ticker,'.n <- DATA[[i]][[2]]$n;',
                    'DATA[[i]][[2]]$',ticker,'.diff.n <- DATA[[i]][[2]]$diff.n;',
                    'DATA[[i]][[2]]$',ticker,'.commiss <- DATA[[i]][[2]]$commiss;',
                    'DATA[[i]][[2]]$',ticker,'.equity <- DATA[[i]][[2]]$equity;',
                    'DATA[[i]][[2]]$',ticker,'.perfReturn <- DATA[[i]][[2]]$perfReturn;'
                )
                eval(parse(text = temp.text))
                #
                # names(DATA[[i]][[1]])[names(DATA[[i]][[1]]) == 'n'] <- paste0(ticker,'.n')
                # names(DATA[[i]][[1]])[names(DATA[[i]][[1]]) == 'diff.n'] <- paste0(ticker,'.diff.n')
                names(DATA[[i]][[1]])[names(DATA[[i]][[1]]) == 'Price'] <- paste0(ticker,'.Price')
                # names(DATA[[i]][[1]])[names(DATA[[i]][[1]]) == 'commiss'] <- paste0(ticker,'.commiss')
                # names(DATA[[i]][[1]])[names(DATA[[i]][[1]]) == 'equity'] <- paste0(ticker,'.equity')
                # names(DATA[[i]][[1]])[names(DATA[[i]][[1]]) == 'perfReturn'] <- paste0(ticker,'.equity')
                #
                # names(DATA[[i]][[2]])[names(DATA[[i]][[2]]) == 'n'] <- paste0(ticker,'.n')
                # names(DATA[[i]][[2]])[names(DATA[[i]][[2]]) == 'diff.n'] <- paste0(ticker,'.diff.n')
                names(DATA[[i]][[2]])[names(DATA[[i]][[2]]) == 'Price'] <- paste0(ticker,'.Price')
                # names(DATA[[i]][[2]])[names(DATA[[i]][[2]]) == 'commiss'] <- paste0(ticker,'.commiss')
                # names(DATA[[i]][[2]])[names(DATA[[i]][[2]]) == 'equity'] <- paste0(ticker,'.equity')
                # names(DATA[[i]][[2]])[names(DATA[[i]][[2]]) == 'perfReturn'] <- paste0(ticker,'.equity')
                names(DATA[[i]]) <- c('full', 'states')
                #
                return(DATA[[i]])
            }
        }
    
    ###выгрузка данных по портфелю
    ## сделочные данные
    PORTFOLIO[[2]] <- get('portfolio.cache', envir = .CacheEnv) 

    # удаление кэша (он больше не нужен)
    rm(.CacheEnv)
    
    ## посвечные данные
    # количество контрактов 
    PORTFOLIO[[1]]$n <- 
        lapply(1:n_bots,
            function(x) {
                DATA[[x]][[1]]$n
            }) %>%
        do.call(merge, .) %>%
        rowSums(.)
    # изменение контрактов
    PORTFOLIO[[1]]$diff.n <-
        lapply(1:n_bots,
            function(x) {
                DATA[[x]][[1]]$diff.n  
            }) %>%
        do.call(merge, .) %>%
        rowSums(.)
    # суммарное equity   
    PORTFOLIO[[1]]$equity <-
        lapply(1:n_bots,
            function(x) {
                DATA[[x]][[1]]$equity
            }) %>%
        do.call(merge, .) %>%
        rowSums(.)
    # суммарный perfReturn
    PORTFOLIO[[1]]$perfReturn <-
        lapply(1:n_bots,
            function(x) {
                DATA[[x]][[1]]$perfReturn  
            }) %>%
        do.call(merge, .) %>%
        rowSums(.)
    # суммарная вариационка
    PORTFOLIO[[1]]$margin <-
        lapply(1:n_bots,
            function(x) {
                DATA[[x]][[1]]$margin  
            }) %>%
        do.call(merge, .) %>%
        rowSums(.)
    # суммарная комиссия
    PORTFOLIO[[1]]$commiss <-
        lapply(1:n_bots, 
            function(x) {
                DATA[[x]][[1]]$commiss
            }) %>%
        do.call(merge, .) %>%
        rowSums(.)
    # суммарное ГО
    PORTFOLIO[[1]]$im.balance <-
        lapply(1:n_bots,
            function(x) {
                DATA[[x]][[1]]$im.balance
            }) %>%
        do.call(merge, .) %>%
        rowSums(.)
    # баланс портфолио
    PORTFOLIO[[1]]$balance <- balance_start + PORTFOLIO[[1]]$equity - PORTFOLIO[[1]]$im.balance
    
    # добавление нужных на следующих этапах столбцов
    temp.text <- paste0(
        'PORTFOLIO[[1]]$',ticker,'.n <- PORTFOLIO[[1]]$n;',
        'PORTFOLIO[[1]]$',ticker,'.diff.n <- PORTFOLIO[[1]]$diff.n;',
        'PORTFOLIO[[1]]$',ticker,'.commiss <- PORTFOLIO[[1]]$commiss;',
        'PORTFOLIO[[1]]$',ticker,'.equity <- PORTFOLIO[[1]]$equity;',
        'PORTFOLIO[[1]]$',ticker,'.perfReturn <- PORTFOLIO[[1]]$perfReturn;',
        #
        'PORTFOLIO[[2]]$',ticker,'.n <- PORTFOLIO[[2]]$n;',
        'PORTFOLIO[[2]]$',ticker,'.diff.n <- PORTFOLIO[[2]]$diff.n;',
        'PORTFOLIO[[2]]$',ticker,'.commiss <- PORTFOLIO[[2]]$commiss;',
        'PORTFOLIO[[2]]$',ticker,'.equity <- PORTFOLIO[[2]]$equity;',
        'PORTFOLIO[[2]]$',ticker,'.perfReturn <- PORTFOLIO[[2]]$perfReturn;'
    )
    eval(parse(text = temp.text))
    rm(temp.text)
    #
    # names(PORTFOLIO[[1]])[names(PORTFOLIO[[1]]) == 'n'] <- paste0(ticker,'.n')
    # names(PORTFOLIO[[1]])[names(PORTFOLIO[[1]]) == 'diff.n'] <- paste0(ticker,'.diff.n')
    #names(PORTFOLIO[[1]])[names(PORTFOLIO[[1]]) == 'Price'] <- paste0(ticker,'.Price')
    # names(PORTFOLIO[[1]])[names(PORTFOLIO[[1]]) == 'commiss'] <- paste0(ticker,'.commiss')
    # names(PORTFOLIO[[1]])[names(PORTFOLIO[[1]]) == 'equity'] <- paste0(ticker,'.equity')
    # names(PORTFOLIO[[1]])[names(PORTFOLIO[[1]]) == 'perfReturn'] <- paste0(ticker,'.equity')
    #
    # names(PORTFOLIO[[2]])[names(PORTFOLIO[[2]]) == 'n'] <- paste0(ticker,'.n')
    # names(PORTFOLIO[[2]])[names(PORTFOLIO[[2]]) == 'diff.n'] <- paste0(ticker,'.diff.n')
    #names(PORTFOLIO[[2]])[names(PORTFOLIO[[2]]) == 'Price'] <- paste0(ticker,'.Price')
    # names(PORTFOLIO[[2]])[names(PORTFOLIO[[2]]) == 'commiss'] <- paste0(ticker,'.commiss')
    # names(PORTFOLIO[[2]])[names(PORTFOLIO[[2]]) == 'equity'] <- paste0(ticker,'.equity')
    # names(PORTFOLIO[[2]])[names(PORTFOLIO[[2]]) == 'perfReturn'] <- paste0(ticker,'.equity')
    names(PORTFOLIO) <- c('full', 'states')
    #
    return(list(DATA, PORTFOLIO))
}
#