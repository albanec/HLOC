# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Функции для комбинации нескольких ботов:
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
#' Пакетный обработчик сделок
#'
#' @param bot.list Лист с параметрами ботов на данном временном интервале
#' @param data Данные торговли по ботам (без расчёта сделок)
#' @param FUN.CalcOneTrade Функция расчёта контрактов в строке state
#' @param FUN.MM Функция MM
#' @param var_pattern Паттерн имен переменных
#' @param ohlc_args Лист с данными котировок
#' @param trade_args Лист с параметрами торговли
#'
#' @return
BotPortfolio.trade_handler <- function(bot.list,
                                       data,
                                       FUN.CalcOneTrade,
                                       FUN.MM,
                                       var_pattern = '_',
                                       ohlc_args, trade_args) {
    # определение нужных окружений
    .CurrentEnv <- environment()
    .ParentEnv <- globalenv()
    .CacheEnv <- new.env()
    # подгрузка обработчиков
    #FUN.CalcTrade <- match.fun(FUN.CalcTrade)
    FUN.CalcOneTrade <- match.fun(FUN.CalcOneTrade)
    FUN.MM <- match.fun(FUN.MM)
    #
    # всего ботов
    n_bots <- length(bot.list)

    ### Подготовка данных
    # нормирование временных шкал для state-данных (листов сделок)
    index_norm <- 
        lapply(1:n_bots,
            function(x) {
                data[[x]][[2]]$pos
            }) %>%
        do.call(merge, .) %>%
        .$pos %>%
        index(.)
    # формирование листа данных по портфелю
        # так же, как с ботами - лист с посвечным xts и лист с данными на сделках
    portfolio_data <- list(
        xts(NULL, order.by = index(data[[1]][[1]])),
        xts(NULL, order.by = index_norm)
    ) 
    # доп. столбцы по каждому боту
    data <- lapply(1:n_bots,
        function(x) {
            # нормирование временных индексов
            data[[x]][[2]] <-
                merge(data[[x]][[2]], temp = xts(NULL, order.by = index_norm)) %>%
                {
                    .$temp <- NULL
                    return(.)
                }
            # начальный баланс
            data[[x]][[2]]$balance <- NA
            data[[x]][[2]]$balance[1] <- trade_args$balance_start %/% n_bots #* bot.list[[x]]$weight[1]
            # начальное контрактов бота
            data[[x]][[2]]$n <- NA
            #states$n <- 0
            # прочее
            data[[x]][[2]]$diff.n <- NA
            data[[x]][[2]]$diff.n[1] <- 0
            data[[x]][[2]]$margin <- NA
            data[[x]][[2]]$commiss <- NA
            data[[x]][[2]]$perfReturn <- NA
            data[[x]][[2]]$equity <- NA
            data[[x]][[2]]$im.balance <- NA
            data[[x]][[2]]$im.balance[1] <- 0
            # вес бота в корзине
             #data[[x]][[2]]$weight <- 1
        
            ## корректировка данных (для тех строк, где данный бот без action, но возможны action других ботов)
            # выделение индексов, на которых у бота нет action
            temp_ind <-
                which(is.na(data[[x]][[2]]$pos)) %>%
                data[[x]][[2]]$pos[.] %>%
                index(.)
            # удаление индексов, которых нет в full-данных по боту
            temp_ind <- temp_ind[temp_ind %in% index(data[[x]][[1]]$pos)]
            # заполнение данных
            if (length(temp_ind) != 0) {
                # заполнение $price 
                data[[x]][[2]]$Price[temp_ind] <- ohlc_args$ohlc[temp_ind, paste0(bot.list[[x]]$ticker,'.Open')]
                # перенос $pos
                data[[x]][[2]]$pos[temp_ind] <- data[[x]][[1]]$pos[temp_ind]
                data[[x]][[2]]$pos[is.na(data[[x]][[2]]$pos)] <- 0
                # перенос $pos.bars
                data[[x]][[2]]$pos.bars[temp_ind] <- data[[x]][[1]]$pos.bars[temp_ind]
                data[[x]][[2]]$pos.bars[is.na(data[[x]][[2]]$pos.bars)] <- 0
                # перенос $pos.num
                data[[x]][[2]]$pos.num[temp_ind] <- data[[x]][[1]]$pos.num[temp_ind]
                data[[x]][[2]]$pos.num[is.na(data[[x]][[2]]$pos.num)] <- 0
            }
            rm(temp_ind)
            # заполнение action
            data[[x]][[2]]$action[is.na(data[[x]][[2]]$action)] <- 0
            # заполение pos.open
            data[[x]][[2]]$pos.open[is.na(data[[x]][[2]]$pos.open)] <- 0
            # заполение pos.close
            data[[x]][[2]]$pos.close[is.na(data[[x]][[2]]$pos.close)] <- 0
            # перерасчёт $ret
            data[[x]][[2]]$ret <- CalcReturn(data[[x]][[2]]$Price, 
                type = trade_args$return_type) * stats::lag(data[[x]][[2]]$pos)
            data[[x]][[2]]$ret[is.na(data[[x]][[2]]$ret)] <- 0
            # перерасчёт $cret
            #/// для не-Si инструментов надо переписать расчет $cret (b $Price, возможно)
            data[[x]][[2]]$cret <- data[[x]][[2]]$ret
            #data[[x]][[2]]$cret[is.na(data[[x]][[2]]$cret)] <- 0
            #
            return(data[[x]])    
        }) 
    
    ### выгрузка функций для расчета данных по боту
    # типы ботов, участвующие в торговле
    # bot_names <-sapply(1:n_bots,
    #     function(x) {
    #         bot.list[[x]]$name
    #     })
    # BindToEnv(obj_pattern = 'CalcTrades_inStates_one_trade.', .TargetEnv = .CurrentEnv, .ParentEnv = .ParentEnv)
    # FUN_names <- paste0('CalcTrades_inStates_one_trade.', bot_names)
    # if (!any(FUN_names %in% ls(.CurrentEnv) == TRUE)) { 
    #     stop(
    #         which(FUN_names %in% ls(.CurrentEnv)) %>%
    #         paste0('ERROR(BotCombiner): Сan\'t find ',FUN_names[.],' function !!!', sep = '\n')
    #     )
    # }
    
    ### создание cache для ботов и для портфеля
    # кэш для ботов
    target_col <- c('n', 'diff.n', 'balance', 'im', 'im.balance', 'commiss',
        'margin', 'perfReturn', 'equity')
    assign('cache_bot', 
        lapply(1:n_bots,
            function(x) {
                data[[x]][[2]][, target_col] %>% as.data.frame(., row.names=NULL)  
            }), 
        envir = .CacheEnv)  
    # кэш для портфеля
    assign('cache_portfolio',
        lapply(1:length(target_col[!target_col %in% 'im']),
            function(x) {
                xts(rep(NA, length(index_norm)), order.by = index_norm)
                #xts(NULL, order.by = index_norm)
            }) %>%
        do.call(merge, .) %>%
        {
            names(.) <- target_col[!target_col %in% 'im']
            .$balance[1] <- trade_args$balance_start
            return(.)
        },
        envir = .CacheEnv)
    
    ### перебор расчёта сделок по единой шкале индексов
        # (данные по каждому боту корректируются единым обработчиком + формируются данные в целом по портфелю)
    lapply(1:length(index_norm),
        function(row_num) {
            # индекс строки
            temp.index <- index_norm[row_num]
            ## определение развесовки портфеля на индексе
                #! на данный момент развесовка зависит только от количества ботов
            # open_pos <- 
            #     lapply(1:n_bots,
            #         function(x) {
            #             temp.pos <- ifelse(!is.na(DATA[[x]][[2]]$pos[[row_num]]),
            #                 DATA[[x]][[2]]$pos[[row_num]],
            #                 0) 
            #             temp.pos.bars <- ifelse(!is.na(DATA[[x]][[2]]$pos.bars[[row_num]]),
            #                 DATA[[x]][[2]]$pos.bars[[row_num]],
            #                 0)
            #             out <- ifelse(temp.pos != 0 & temp.pos.bars == 0,
            #                 1,
            #                 0)
            #             return(out)
            #         }) %>%
            #     do.call(sum, .)
            
            # подгрузка кэшей
            cache_portfolio <- get('cache_portfolio', envir = .CacheEnv)
            cache_bot <- get('cache_bot', envir = .CacheEnv)
            # определение баланса на индексе
            # было: баланс может делится относительно ботов, входящих в позы (тогда надо раскомментить код для open_pos)
            # available_balance <- cache_portfolio$balance[row_num - 1] / open_pos
            # стало: баланс делится в зависимости от количества ботов 
            available_balance <- ifelse(row_num == 1,
                cache_portfolio$balance[1] %/% n_bots,
                cache_portfolio$balance[row_num - 1] %/% n_bots)
            #rm(open_pos)
    
            ## перебор по каждому боту
            # обработка строки сделок
            cache_bot <- 
                lapply(1:n_bots,
                    function(bot_num) {
                        ## вызов посделочного обработчика
                        cache_bot[[bot_num]] <- 
                            list(
                                cache = cache_bot[[bot_num]], 
                                row_ind = row_num,
                                row = data[[bot_num]][[2]][row_num, ],
                                FUN.MM = FUN.MM,
                                external_balance = available_balance,
                                ohlc_args = ohlc_args, 
                                trade_args = trade_args, 
                                str_args = 
                                    if (!is.null(var_pattern)) {
                                        do.call(list, bot.list[[bot_num]][grep('_', names(bot.list[[bot_num]]))])
                                    } else {
                                        do.call(list, bot.list[[bot_num]])
                                    }
                            ) %>%
                            do.call(FUN.CalcOneTrade, ., envir = .CurrentEnv)
                    })
            assign('cache_bot', cache_bot, envir = .CacheEnv)

            ## расчёт общих данных по корзине 
                # на первом индексе большинство параметров по нулям, так что
            if (row_num != 1) {
                # количество контрактов по портфолио
                cache_portfolio$n[row_num] <-
                    lapply(1:n_bots,
                        function(x) {
                            cache_bot[[x]]$n[row_num]
                        }) %>%
                    do.call(sum, .)
                # изменение контрактов на индексе
                cache_portfolio$diff.n[row_num] <-
                    lapply(1:n_bots,
                        function(x) {
                            cache_bot[[x]]$diff.n[row_num]  
                        }) %>%
                    do.call(sum, .)
                # суммарное equity по портфолио  
                cache_portfolio$equity[row_num] <-
                    lapply(1:n_bots,
                        function(x) {
                            cache_bot[[x]]$equity[row_num]
                        }) %>%
                    do.call(sum, .)
                # суммарный perfReturn на такте
                cache_portfolio$perfReturn[row_num] <-
                    lapply(1:n_bots,
                        function(x) {
                            cache_bot[[x]]$perfReturn[row_num]  
                        }) %>%
                    do.call(sum, .)
                # суммарная вариационка на такте
                cache_portfolio$margin[row_num] <-
                    lapply(1:n_bots,
                        function(x) {
                            cache_bot[[x]]$margin[row_num]  
                        }) %>%
                    do.call(sum, .)
                # суммарная комиссия на такте
                cache_portfolio$commiss[row_num] <-
                    lapply(1:n_bots, 
                        function(x) {
                            cache_bot[[x]]$commiss[row_num]
                        }) %>%
                    do.call(sum, .)
                # суммарное ГО на такте
                cache_portfolio$im.balance[row_num] <-
                    lapply(1:n_bots,
                        function(x) {
                            cache_bot[[x]]$im.balance[row_num]  
                        }) %>%
                    do.call(sum, .)
                # баланс портфолио на индексе
                cache_portfolio$balance[row_num] <- 
                    trade_args$balance_start + cache_portfolio$equity[row_num] - cache_portfolio$im.balance[row_num]
            } else {
                cache_portfolio$n[row_num] <- 0
                cache_portfolio$diff.n[row_num] <- 0
                cache_portfolio$equity[row_num] <- 0
                cache_portfolio$perfReturn[row_num] <- 0
                cache_portfolio$margin[row_num] <- 0
                cache_portfolio$commiss[row_num] <- 0 
                cache_portfolio$im.balance[row_num] <- 0
                cache_portfolio$balance[row_num] <- trade_args$balance_start
            }
            # запись данных по портфолио в кэш
            assign('cache_portfolio', cache_portfolio, envir = .CacheEnv)
            rm(cache_portfolio, cache_bot)
        }
    )

    ### результаты
    ## данные по ботам
    data <- 
        get('cache_bot', envir = .CacheEnv) %>% 
        {
            foreach(i = 1:n_bots) %do% {
                ## Расчёт state таблиц
                # Изменение контрактов на такте
                data[[i]][[2]]$n <- .[[i]]$n
                data[[i]][[2]]$diff.n <- .[[i]]$diff.n 
                # Расчёт баланса, заблокированного на ГО
                data[[i]][[2]]$im.balance <- .[[i]]$im.balance
                # Расчёт комиссии на такте
                data[[i]][[2]]$commiss <- .[[i]]$commiss
                # Расчёт вариационки
                data[[i]][[2]]$margin <- .[[i]]$margin
                # расчёт equity по корзине в states
                data[[i]][[2]]$perfReturn <- .[[i]]$perfReturn
                data[[i]][[2]]$equity <- .[[i]]$equity
                # Расчёт баланса
                data[[i]][[2]]$balance <- .[[i]]$balance
                     
                ## Перенос данных из state в full таблицы
                # перенос данных по количеству контрактов корзины
                data[[i]][[1]]$n <-
                    merge(data[[i]][[1]], data[[i]][[2]]$n) %>%
                    {
                        .$n <- na.locf(.$n)
                        return(.$n)
                    }
                # перенос данных по изменению контрактов корзины
                data[[i]][[1]]$diff.n <-
                    merge(data[[i]][[1]], data[[i]][[2]]$diff.n) %>%
                    {
                        .$diff.n[is.na(.$diff.n)] <- 0
                        return(.$diff.n)
                    }
                # перенос данных по комиссии корзины
                data[[i]][[1]]$commiss <-
                    merge(data[[i]][[1]], data[[i]][[2]]$commiss) %>%
                    {
                        .$commiss[is.na(.$commiss)] <- 0
                        return(.$commiss)
                    }
                # перенос данных по суммарному ГО
                data[[i]][[1]]$im.balance <-
                    merge(data[[i]][[1]], data[[i]][[2]]$im.balance) %>%
                    {
                        .$im.balance <- na.locf(.$im.balance)
                        return(.$im.balance)
                    }
                
                ## Расчёт показателей в data таблицах
                # расчёт вариационки в data
                data[[i]][[1]]$margin <- stats::lag(data[[i]][[1]]$n) * data[[i]][[1]]$cret
                data[[i]][[1]]$margin[1] <- 0
                # расчёт equity по корзине в data[[i]][[1]]
                data[[i]][[1]]$perfReturn <- data[[i]][[1]]$margin - data[[i]][[1]]$commiss
                data[[i]][[1]]$equity <- cumsum(data[[i]][[1]]$perfReturn)
                
                ## добавление нужных на следующих этапах столбцов
                temp.text <- paste0(
                    'data[[i]][[1]]$',bot.list[[i]]$ticker,'.n <- data[[i]][[1]]$n;',
                    'data[[i]][[1]]$',bot.list[[i]]$ticker,'.diff.n <- data[[i]][[1]]$diff.n;',
                    'data[[i]][[1]]$',bot.list[[i]]$ticker,'.commiss <- data[[i]][[1]]$commiss;',
                    'data[[i]][[1]]$',bot.list[[i]]$ticker,'.equity <- data[[i]][[1]]$equity;',
                    'data[[i]][[1]]$',bot.list[[i]]$ticker,'.perfReturn <- data[[i]][[1]]$perfReturn;',
                    #
                    'data[[i]][[2]]$',bot.list[[i]]$ticker,'.n <- data[[i]][[2]]$n;',
                    'data[[i]][[2]]$',bot.list[[i]]$ticker,'.diff.n <- data[[i]][[2]]$diff.n;',
                    'data[[i]][[2]]$',bot.list[[i]]$ticker,'.commiss <- data[[i]][[2]]$commiss;',
                    'data[[i]][[2]]$',bot.list[[i]]$ticker,'.equity <- data[[i]][[2]]$equity;',
                    'data[[i]][[2]]$',bot.list[[i]]$ticker,'.perfReturn <- data[[i]][[2]]$perfReturn;'
                )
                eval(parse(text = temp.text))
                #
                # names(data[[i]][[1]])[names(data[[i]][[1]]) == 'n'] <- paste0(bot.list[[i]]$ticker,'.n')
                # names(data[[i]][[1]])[names(data[[i]][[1]]) == 'diff.n'] <- paste0(bot.list[[i]]$ticker,'.diff.n')
                names(data[[i]][[1]])[names(data[[i]][[1]]) == 'Price'] <- paste0(bot.list[[i]]$ticker,'.Price')
                # names(data[[i]][[1]])[names(data[[i]][[1]]) == 'commiss'] <- paste0(bot.list[[i]]$ticker,'.commiss')
                # names(data[[i]][[1]])[names(data[[i]][[1]]) == 'equity'] <- paste0(bot.list[[i]]$ticker,'.equity')
                # names(data[[i]][[1]])[names(data[[i]][[1]]) == 'perfReturn'] <- paste0(bot.list[[i]]$ticker,'.equity')
                #
                # names(data[[i]][[2]])[names(data[[i]][[2]]) == 'n'] <- paste0(bot.list[[i]]$ticker,'.n')
                # names(data[[i]][[2]])[names(data[[i]][[2]]) == 'diff.n'] <- paste0(bot.list[[i]]$ticker,'.diff.n')
                names(data[[i]][[2]])[names(data[[i]][[2]]) == 'Price'] <- paste0(bot.list[[i]]$ticker,'.Price')
                # names(data[[i]][[2]])[names(data[[i]][[2]]) == 'commiss'] <- paste0(bot.list[[i]]$ticker,'.commiss')
                # names(data[[i]][[2]])[names(data[[i]][[2]]) == 'equity'] <- paste0(bot.list[[i]]$ticker,'.equity')
                # names(data[[i]][[2]])[names(data[[i]][[2]]) == 'perfReturn'] <- paste0(bot.list[[i]]$ticker,'.equity')
                names(data[[i]]) <- c('full', 'states')
                
                # добавление столбцов для пост-обработки в EVA
                data[[i]][[1]]$pos.add <- 0
                data[[i]][[1]]$pos.drop <- 0
                data[[i]][[2]]$pos.add <- 0
                data[[i]][[2]]$pos.drop <- 0
                #
                return(data[[i]])
            }
        }
    ## выгрузка данных по портфелю
    # state данные
    portfolio_data[[2]] <- get('cache_portfolio', envir = .CacheEnv) 
    # удаление кэша (он больше не нужен)
    rm(.CacheEnv)
    # посвечные данные
    # количество контрактов 
    portfolio_data[[1]]$n <- 
        lapply(1:n_bots,
            function(x) {
                data[[x]][[1]]$n
            }) %>%
        do.call(merge, .) %>%
        rowSums(.)
    # изменение контрактов
    portfolio_data[[1]]$diff.n <-
        lapply(1:n_bots,
            function(x) {
                data[[x]][[1]]$diff.n  
            }) %>%
        do.call(merge, .) %>%
        rowSums(.)
    # суммарное equity   
    portfolio_data[[1]]$equity <-
        lapply(1:n_bots,
            function(x) {
                data[[x]][[1]]$equity
            }) %>%
        do.call(merge, .) %>%
        rowSums(.)
    # суммарный perfReturn
    portfolio_data[[1]]$perfReturn <-
        lapply(1:n_bots,
            function(x) {
                data[[x]][[1]]$perfReturn  
            }) %>%
        do.call(merge, .) %>%
        rowSums(.)
    # суммарная вариационка
    portfolio_data[[1]]$margin <-
        lapply(1:n_bots,
            function(x) {
                data[[x]][[1]]$margin  
            }) %>%
        do.call(merge, .) %>%
        rowSums(.)
    # суммарная комиссия
    portfolio_data[[1]]$commiss <-
        lapply(1:n_bots, 
            function(x) {
                data[[x]][[1]]$commiss
            }) %>%
        do.call(merge, .) %>%
        rowSums(.)
    # суммарное ГО
    portfolio_data[[1]]$im.balance <-
        lapply(1:n_bots,
            function(x) {
                data[[x]][[1]]$im.balance
            }) %>%
        do.call(merge, .) %>%
        rowSums(.)
    # баланс портфолио
    portfolio_data[[1]]$balance <- trade_args$balance_start + portfolio_data[[1]]$equity - portfolio_data[[1]]$im.balance
    
    # добавление нужных на следующих этапах столбцов
    temp.text <- paste0(
        'portfolio_data[[1]]$',ohlc_args$ticker,'.n <- portfolio_data[[1]]$n;',
        'portfolio_data[[1]]$',ohlc_args$ticker,'.diff.n <- portfolio_data[[1]]$diff.n;',
        'portfolio_data[[1]]$',ohlc_args$ticker,'.commiss <- portfolio_data[[1]]$commiss;',
        'portfolio_data[[1]]$',ohlc_args$ticker,'.equity <- portfolio_data[[1]]$equity;',
        'portfolio_data[[1]]$',ohlc_args$ticker,'.perfReturn <- portfolio_data[[1]]$perfReturn;',
        #
        'portfolio_data[[2]]$',ohlc_args$ticker,'.n <- portfolio_data[[2]]$n;',
        'portfolio_data[[2]]$',ohlc_args$ticker,'.diff.n <- portfolio_data[[2]]$diff.n;',
        'portfolio_data[[2]]$',ohlc_args$ticker,'.commiss <- portfolio_data[[2]]$commiss;',
        'portfolio_data[[2]]$',ohlc_args$ticker,'.equity <- portfolio_data[[2]]$equity;',
        'portfolio_data[[2]]$',ohlc_args$ticker,'.perfReturn <- portfolio_data[[2]]$perfReturn;'
    )
    eval(parse(text = temp.text))
    rm(temp.text)
    #
    # names(portfolio_data[[1]])[names(portfolio_data[[1]]) == 'n'] <- paste0(ohlc_args$ticker,'.n')
    # names(portfolio_data[[1]])[names(portfolio_data[[1]]) == 'diff.n'] <- paste0(ohlc_args$ticker,'.diff.n')
    # names(portfolio_data[[1]])[names(portfolio_data[[1]]) == 'Price'] <- paste0(ohlc_args$ticker,'.Price')
    # names(portfolio_data[[1]])[names(portfolio_data[[1]]) == 'commiss'] <- paste0(ohlc_args$ticker,'.commiss')
    # names(portfolio_data[[1]])[names(portfolio_data[[1]]) == 'equity'] <- paste0(ohlc_args$ticker,'.equity')
    # names(portfolio_data[[1]])[names(portfolio_data[[1]]) == 'perfReturn'] <- paste0(ohlc_args$ticker,'.equity')
    #
    # names(portfolio_data[[2]])[names(portfolio_data[[2]]) == 'n'] <- paste0(ohlc_args$ticker,'.n')
    # names(portfolio_data[[2]])[names(portfolio_data[[2]]) == 'diff.n'] <- paste0(ohlc_args$ticker,'.diff.n')
    # names(portfolio_data[[2]])[names(portfolio_data[[2]]) == 'Price'] <- paste0(ohlc_args$ticker,'.Price')
    # names(portfolio_data[[2]])[names(portfolio_data[[2]]) == 'commiss'] <- paste0(ohlc_args$ticker,'.commiss')
    # names(portfolio_data[[2]])[names(portfolio_data[[2]]) == 'equity'] <- paste0(ohlc_args$ticker,'.equity')
    # names(portfolio_data[[2]])[names(portfolio_data[[2]]) == 'perfReturn'] <- paste0(ohlc_args$ticker,'.equity')
    names(portfolio_data) <- c('full', 'states')

    # добавление столбцов для пост-обработки в EVA
    # portfolio_data[[1]]$pos.add <- 0
    # portfolio_data[[1]]$pos.drop <- 0
    # portfolio_data[[2]]$pos.add <- 0
    # portfolio_data[[2]]$pos.drop <- 0
    #
    return(list(data, portfolio_data))
}
#
BotPortfolio.mcapply <- function(bot.list,
                                 FUN_pattern = NULL, 
                                 eval_str = NULL,
                                 var_pattern = NULL,
                                 ohlc_args, trade_args,
                                 ...) {
    require(doParallel)
    dots <- list(...)
    
    # определение нужных окружений
    .CurrentEnv <- environment()
    .ParentEnv <- globalenv()
    # регистрация ядер
    if (getDoParWorkers() == 1) {
        workers <- detectCores() - 1    
        registerDoParallel(cores = workers)
    } else {
        workers <- getDoParWorkers()
    }
    # всего ботов
    n_bots <- length(bot.list) 
    # типы ботов, участвующие в торговле
    bot_names <- 
        foreach(i = 1:length(bot.list), .combine = c) %do% {
            bot.list[[i]]$name
        }
    # подгрузка gear-функиций
    BindToEnv(obj_pattern = FUN_pattern, .TargetEnv = .CurrentEnv, .ParentEnv = .ParentEnv)        
    FUN_names <- paste0(FUN_pattern, bot_names)
    if (!any(FUN_names %in% ls(.CurrentEnv) == TRUE)) { 
        stop(
            which(FUN_names %in% ls(.CurrentEnv)) %>%
            paste0('ERROR(BotCombiner): Сan\'t find ',FUN_names[.],' function !!!', sep = '\n')
        )
    }
    # расчёт сырых данных по пакету ботов (на выходе - листы по каждому боту)
    result <- 
        foreach(i = 1:workers, .combine = c) %dopar% {
            # распределение ботов по потоку
            map_range <- Delegate(i, n_bots, p = workers)
            # проверка на наличие задания для worker'а
            if (is.null(map_range)) {
                return(NA)
            } 
            # расчёт ботов
            lapply(1:length(map_range),
                function(x) {
                    if (!is.data.frame(bot.list[[map_range[x]]])) {
                        warning('WARNING(BotCombiner): strategies data wrong type', '\n')
                    }
                    if (!is.null(var_pattern)) {
                        var_args <- 
                            do.call(list, bot.list[[x]][grep('_', names(bot.list[[x]]))]) %>%
                            list(ohlc_args = ohlc_args, trade_args = trade_args, str_args = .)
                    } else {
                        var_args <- 
                            do.call(list, bot.list[[map_range[x]]]) %>%
                            list(ohlc_args = ohlc_args, trade_args = trade_args, str_args = .)
                    }
                    # запуск функции
                    if (!is.null(eval_str)) {
                        cat('here', '\n')
                        eval(parse(text = eval_str))                    
                    } else {
                        do.call(FUN_names[map_range[x]], var_args, envir = .CurrentEnv)      
                    }
                    #comment(data) <- bot.list[[map_range[x]]]$name
                })
        } %>%
        {
            .[!is.na(.)]
        }
    #
    return(result)
}
#