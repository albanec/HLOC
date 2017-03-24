# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# описания стратегий и вспомагательных функций
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
# ------------------------------------------------------------------------------
#' Переход к состояниям (фильтрация сигналов)
#'
#' @param x Ряд сигналов стратегии
#'
#' @return x$y ряд сделок (отфильтрованный ряд сигналов)
#'
#' @export
Convert.signal_to_states <- function(x) {
    x$a <-
        na.locf(x) %>%
        ifelse.fast(is.na(x$a) | is.nan(x$a) | is.infinite(x$a), 0, x$a)
    ind <- which(x$a != lag.xts(x$a))
    x$y <- rep(NA, length(x$a))
    x$y[ind] = x$a[ind]
    x$y[1] <- x$a[1]
    #
    return(x$y)
}
# ------------------------------------------------------------------------------
#' Функция для перехода к состояниям (фильтрация сигналов)
#'
#' @param x xts ряд позиций (data$pos)
#'
#' @return state Ряд состояний
#'
#' @export
CalcState <- function(x) {
    #
    x <- Replace.na(na.locf(x), 0)
    ind <- which(x != lag.xts(x))
    state <- rep(NA, length(x))
    state[ind] <- x[ind]
    state[1] <- x[1]    
    #
    return(state)
}
###
#' Функция для формирования STATE-таблицы(таблицы состояний)
#'
#' @param x DATA-таблица
#'
#' @return state STATE-таблица
#'
#' @export
CalcState.table <- function(x) {
    #
    x$state <- NA
    x$state <- CalcState(x$pos) 
    #
    return(na.omit(x))
}
AddStateTable <- function(x) {
    state <- CalcState.table(x)
    # условие закрытия сделок в конце торгового периода
    state$state[index.xts(xts::last(state$state))] <- 0
    state$pos[index.xts(xts::last(state$pos))] <- 0
    # если сделок нет, то 
    if (is.null(state)) {
        #message('WARNING(StrGear_turtles): No trades Here!!!', '\n')
        return(NULL)
    }
    #
    return(list(data = x, state = state)) 
}
###
#' Функция вычисляет return'ы по всему портфелю внутри XTS
#'
#' @param data XTS
#' @param type Тип return'a (ret/sret/lret)
#'
#' @return data XTS + return'ы по каждому инструменту
#'
#' @export
CalcReturn_inXTS <- function(data, price = 'Close', type = 'sret') {
    require(quantmod)
    # ----------
    data.names <-
       names(data)[grep('Close', names(data))] %>%
       sub('.Close', '', .)
    for (i in 1:length(data.names)) {
        temp.text <- paste0(
            'data$',data.names[i],'.',type,' <- CalcReturn(',
                'data$',data.names[i],'.',price,', type = \"',type,'\"',
            ')'
        )
        eval(parse(text = temp.text))
    }
    #
    return(data)
}
#
###
#' Функция расчета equity
#'
#' @param data Данные с доходностями и позициями
#'
#' @return data Данные + объем(w), относительной прибылью(margin), equity
#'
#' @export
CalcEquity <- function(data, s0 = 0, abs = FALSE, SR = FALSE, LR = FALSE, reinvest = TRUE, state = FALSE) {
    require(quantmod)
    # ----------
    # расчет
    if (state == FALSE) {
        data$state <- data$pos
    }
    if (abs == TRUE) {
        if (reinvest == TRUE) {
            data$w <- data$state[[1]] * s0/data$Open[[1]]
            data$w <- trunc(data$w)
            data$equity <- s0
            data$margin <- 0
            for (i in 2:nrow(data)) {
                data$margin[i] <- data$w[[i - 1]] * ( data$Open[[i]] - data$Open[[i-1]] )
                data$equity[i] <- (data$equity[[i - 1]] + data$margin[[i]])
                data$w[i] <- data$state[[i]] * data$equity[[i]] / data$Open[[i]]
                data$w[i] <- trunc(data$w[i])
            }
        } else {
            data$w <- 1
            data$margin <- lag.xts(data$state) * ( data$Open - lag.xts(data$Open) )
            data$margin[1] <- 0
            data$equity <- cumsum(data$margin)
        }
    }
    if (SR == TRUE) {
        if (reinvest == TRUE) {
          data$SR <- lag.xts(data$state) * data$SR
          data$SR[1] <- 0
        data$margin <- cumprod(data$SR + 1)
        data$margin[1] <- 0
        data$equity <- s0*data$margin
        } else {
            data$SR <- lag.xts(data$state) * data$SR
            data$SR[1] <- 0
            data$margin <- cumprod(data$SR + 1) - 1
            data$equity <- data$Open[[1]] * as.numeric(data$margin)
        }
    }
    if (LR == TRUE) {
        #if (reinvest==TRUE) {
            #
        #} else {
            data$LR <- lag.xts(data$state) * data$LR
            data$LR[1] <- 0
            data$margin <- cumsum(data$LR)
            data$equity <- data$Open[[1]] * (exp(as.numeric(xts::last(data$margin))) - 1)
        #}
    }
    #
    return(data)
}
#
# ------------------------------------------------------------------------------
#' Функция расчета профита
#'
#' Устаревшая функция
#'
#' @param data Данные с equity (в пунктах)
#' @param s0 Начальный баланс
#' @param pip Размер пункта
#'
#' @return profit Итоговый профит
#'
#' @export
CalcProfit <- function(data, s0 = 0, pip, reinvest = TRUE) {
    require(quantmod)
    # расчет итогового профита
    if (reinvest == TRUE) {
        profit <- as.numeric(xts::last(data$equity / pip) - s0)
    } else {
        profit <- as.numeric(xts::last(data$equity / pip))
    }
    #
    return(profit)
}
#
# ------------------------------------------------------------------------------
# Функции нормирования исходных данных
#
#' Функция для расчёта стоимости тиков внутри основного листа данных
#'
#' @param data XTS данные котировок (основной лист данных)
#' @param names Список тикеров для конвертирования
#' @param norm.data Нормировочные данные
#' @param outnames Название столбца для результатов
#' @param convert.to
#' @param tick.val Тиков в шаге цены
#' @param tick.price Цена тика
#'
#' @return data Основной XTS (нужные данные конвертированы к нужной валюте)
#'
#' @export
NormData_inXTS.price <- function(data, names, norm.data, outnames, convert.to, tick.val, tick.price) {
    x <- norm.data
    for (i in 1:length(names)) {
        temp.text <- paste0(
            'data$',outnames[i],' <- NormData.price(',
                'data = data$',names[i],',',
                'norm.data = x, convert.to = \"',convert.to,'\",',
                'tick.val = ',tick.val[i],',',
                'tick.price = ', tick.price[i],')'
        )
        eval(parse(text = temp.text))
    }
    #
    return(data)
}
###
#' Функция для расчёта стоимости тиков
#'
#' @param data XTS, содержащий нужные данные
#' @param norm.data Нормировочные данные
#' @param convert.to Валюта конвертирования (USD/RUB)
#' @param tick.val Тиков в шаге цены
#' @param tick.price Цена тика
#'
#' @return data XTS ряд
#'
#' @export
NormData.price <- function(data, norm.data, convert.to, tick.val, tick.price) {
    if (convert.to == 'RUB') {
        data <- (data * tick.price / tick.val) * norm.data
    }
    if (convert.to == 'USD') {
        data <- (data * tick.price / tick.val) / norm.data
    }
    #
    return(data)
}
# ------------------------------------------------------------------------------
#' Расчёт суммарного параметра (согласно весам инструмента в портфеле)
#'
#' @param data xts с данными корзины
#' @param basket_weights Веса инструментов внутри корзины
#' @param target Ключ поиска нужных столбцов
#'
#' @return data Суммированные данные (столбец)
#'
#' @export
CalcSum_inXTS_byTargetCol.basket <- function(data, basket_weights, target) {
  #require()
  #
    temp.text <-
        names(data)[grep(target, names(data))] %>%
        paste0('data$', .) %>%
        paste(., basket_weights, sep = ' * ', collapse = ' + ') %>%
        paste0('data <- ', .)
    eval(parse(text = temp.text))
    #
    return(data)
}
#
# ------------------------------------------------------------------------------
#' Функция расщепления переворотных сделок
#'
#' @param data Полные данные отработки робота
#'
#' @return data Данные с расщеплёнными позициями-переворотами
#'
#' @export
SplitSwitchPosition <- function(data) {
    ## Точки смены позиций
    data$action <- diff(data$pos)
    data$action[1] <- data$pos[1]
    # индекс строки-переворота
    temp.ind <- index.xts(data[data$action == 2 | data$action == -2])
    if (length(temp.ind) == 0) {
        cat('TestStrategy INFO: No Switch Position there', '\n')
        rm(temp.ind)
    } else {
        cat('TestStrategy INFO:  Split SwitchPosition...', '\n')
        # temp копия нужных строк (строки начала новой сделки)
        temp <-
            data[temp.ind] %>%
            {
                x <- .
                x$pos <- sign(x$action)
                # x$state <- sign(x$action)
                x$action <- abs(sign(x$action))
                return(x)
            }
        # cтроки предыдущей сделки
        data$pos[temp.ind] <- 0
        # data$state[temp.ind] <- sign(data$action[temp.ind])
        data$action[temp.ind] <- abs(sign(data$action[temp.ind]))
        data$pos.num[temp.ind] <- data$pos.num[temp.ind] - 1
        # правильное заполнение поля $pos.bars
        temp.ind.num <- data[temp.ind, which.i = TRUE]
        data$pos.bars[temp.ind] <- data$pos.bars[temp.ind.num - 1]
        data <- rbind(data, temp)
        rm(temp.ind)
    }
    #
    return(data)
}
#
# ------------------------------------------------------------------------------
#' Функция подсчёта числа баров в позициях
#'
#' @param x Ряд позиций
#'
#' @return result Ряд с числом баров в позициях
#'
#' @export
CalcPosition.bars <- function(x) {
    result <-
        # вектор, содержащий номера позиций
        unique(x) %>%
        # нумерация тиков внутри состояний сигналов
        {
            if (length(.) == 1) {
              .
            } else {
                sapply(.,
                    function(var) {
                        temp <- abs(sign(which(x == var)))
                        temp[1] <- 0
                        xts(x = cumsum(temp), order.by = index.xts(x[x == var]))
                    }) %>%
                MergeData_inList.byRow(.)
            }
        }
    #
    return(result)
}
#
###
#' Функция подсчёта позиций
#'
#' @param x Ряд позиций
#'
#' @return result Ряд номеров позиций
#'
#' @export
CalcPosition.num <- function(x) {
    action <- diff(x)
    action[1] <- x[1]
    result <-
        abs(sign(action)) %>%
        # защита от нумерации позиций 'вне рынка'
        {
            abs(sign(x)) * .
        } %>%
        cumsum(.) %>%
        # защита от нумераций пачек нулевых позиций
        {
            temp <- action
            temp[1] <- 0
            temp <- abs(sign(temp))
            x <- . * sign(temp + abs(x))
            return(x)
        }
    #
    return(result)
}
#

# CalcPosition.stop_apply <- function(pos_colname, Fun.PosStop, x, ohlc_args, trade_args, str_args) {
#     Fun.PosStop <- match.fun(Fun.PosStop)
#     # выделение ряда сигналов
#     sig <- x[, sig_colname]
#     # выделение индексов входов
#     entry_row <- which(sig != 0 & !is.na(sig))
#     entry_index <- 
#         index.xts(sig[entry_row]) %>%
#         c(., index.xts(last(sig)))
#     sig <- lapply(1:length(entry_index),
#         function(x) {
#             Fun.PosStop
#         }
#     )

# }
#
###
#' Функция расчёта позиций относительно ордеров
#'
#' @param order.xts 
#'
#' @return order.xts$pos XTS позиций
#'
#' @export
CalcPosition_byOrders <- function(order.xts) {
    #
    ind <- index.xts(order.xts)
    order.xts$pos <- NA
    order.xts$pos <- ifelse.fast(lag.xts(order.xts$pos) == 0 | is.na(lag.xts(order.xts$pos)), # бот вне рынка
        ifelse.fast(order.xts$bto != 0,
            order.xts$bto,
            order.xts$sto),
        ifelse.fast(order.xts$btc == lag.xts(order.xts$pos) | order.xts$stc == lag.xts(order.xts$pos), # бот в рынке и должен выйти
            0,
            lag.xts(order.xts$pos)))
    #
    return(order.xts$pos)
}
#
###
#' Функция очистки сигналов на датах экспирации
#'
#' @param x Данные ордеров (bto/stc/sto/btc)
#' @param exp.vector Вектор с датами экспирации
#' @param pos Расчёт по состояниям или ордерам
#'
#' @return result.list Лист с очищенными рядами сигналов
#'
#' @export
CleanSignal.expiration <- function(x, exp.vector, pos = FALSE) {
    require(lubridate)

    if (pos == FALSE) {
        # выделение данных ордеров
        col.names <- names(x)
        bto_col <- grep('bto', col.names)
        stc_col <- grep('stc', col.names)
        sto_col <- grep('sto', col.names)
        btc_col <- grep('btc', col.names)
    }
    # temp.ind <-
    #     index.xts(x) %>%
    #     strptime(., '%Y-%m-%d') %>%
    #     unique(.) %>%
    #     as.POSIXct(., origin = '1970-01-01', tz='MSK')
    temp.ind <-
            # which(temp.ind %in% exp.vector)
            which(expiration.dates %in% format(index.xts(x), '%Y-%m-%d')) %>%
        expiration.dates[.] %>%
        as.character(.)
    if (length(temp.ind) != 0) {
        if (pos == FALSE) {
            # удаление входов в дни экспирации
            x[temp.ind, bto_col] <- 0
            x[temp.ind, sto_col] <- 0
        }
        # принудительное закрытие сделок в 16:00 дня экспирации
        temp.ind <- paste(temp.ind, '16:00:00', sep = ' ')
        if (pos == FALSE) {
            x[temp.ind, stc_col] <- 1
            x[temp.ind, btc_col] <- -1
        } else {
            x[temp.ind] <- 0
        }
    }
    rm(temp.ind)
    #
    return(x)
}
#
# ------------------------------------------------------------------------------
#' Функция вычисления цены инструмента с учётом проскальзывания (на action-точках)
#'
#' @param price Данные цен на сделках
#' @param action Данные action
#' @param ohlc Данные с котировками
#' @param slips Слипы
#'
#' @return price XTS с ценами
#'
#' @export
CalcPrice.slips <- function(price, action, ohlc, slips) {
    price <- price + slips * sign(action)
    price.ind <- index.xts(price)
    low.ind <- price.ind[which(price < Lo(ohlc[price.ind]))]
    price[low.ind] <- Lo(ohlc[low.ind])
    high.ind <- price.ind[which(price > Hi(ohlc[price.ind]))]
    price[high.ind] <- Hi(ohlc[high.ind])
    #
    return(price)
}
# ------------------------------------------------------------------------------
#' Очистка таблицы состояний от пустых сделок
#'
#' Чистит таблицу сделок от строк, у которых df$pos != 0 & df$n == 0 & df$diff.n ==0
#'
#' @param x Входной xts ряд состояний
#'
#' @return data Очищенный xts ряд состояний
#'
#' @export
StateTable.clean <- function(x) {
    if ((x$n == 0 && x$diff.n ==0) != FALSE) {
        x <- x[-which(x$n == 0 & x$diff.n ==0)]
    }
    return(x)
}
#
#' Добавление цен инструмента на action-ах
#'
#' @param x xts с DATA-таблицей
#' @param FUN.AddPrice Функция расчёта цен инструмента на action'ах
#' @param ohlc_args Лист с данными котировок
#' @param trade_args Лист с торговыми данными
#'
#' @return x DATA-таблица
#'
#' @export
AddPrice <- function(x, FUN.AddPrice, ohlc_args, trade_args) {
    FUN.AddPrice <- match.fun(FUN.AddPrice)
    # настройка в STATES
    x[[2]]$Price <-
        FUN.AddPrice(x[[2]], ohlc = ohlc_args$ohlc) %>%
        # учёт проскальзываний
        CalcPrice.slips(price = ., 
            action = x[[2]]$action,
            ohlc = ohlc_args$ohlc, slips = trade_args$slips)
    # перенос котировок в DATA (в пунктах) 
    x[[1]] <- merge(x[[1]], Price = x[[2]]$Price) 
    temp.ind <- index.xts(x[[1]]$Price[is.na(x[[1]]$Price)])
    x[[1]]$Price[temp.ind] <- ohlc_args$ohlc$Open[temp.ind]    
    #
    return(x)
}
#
# ----------------------------------------------------------------------------------------------------------------------
# Функции для расчета данных по сделкам
# ----------------------------------------------------------------------------------------------------------------------
#
#' Функция-обработчик сделок
#'
#' @param data Данные отработки робота
#' @param FUN.CalcTrade Функция расчета сделок
#'
#' @return Лист с данными стратегии (data + states)
#'
#' @export
TradeHandler <- function(data,
                         FUN.CalcTrade = CalcTrade,
                         FUN.CalcOneTrade = CalcOneTrade,
                         FUN.MM,
                         ohlc_args, trade_args, str_args) {
    #
    FUN.CalcTrade <- match.fun(FUN.CalcTrade)
    # 2.1.4 Начальные параметры для расчёта сделок
    # начальный баланс
    data[[2]]$balance <- NA
    data[[2]]$balance[1] <- trade_args$balance_start
    # начальное число синтетических контрактов корзины
    data[[2]]$n <- NA
    #data[[2]]$n <- 0
    # прочее
    data[[2]]$diff.n <- NA
    data[[2]]$diff.n[1] <- 0
    data[[2]]$margin <- NA
    data[[2]]$commiss <- NA
    data[[2]]$equity <- NA
    data[[2]]$im.balance <- NA
    data[[2]]$im.balance[1] <- 0
    # т.к. обработчик не пакетный, вес бота всегда = 1
    #data[[2]]$weight <- 1
    ## 2.2 Расчёт самих сделок
    temp.df <- FUN.CalcTrade(data, 
        FUN.CalcOneTrade,
        FUN.MM, 
        ohlc_args, trade_args, str_args)
    # Изменение контрактов на такте
    data[[2]]$n <- temp.df$n
    data[[2]]$diff.n <- temp.df$diff.n
    # Расчёт баланса, заблокированного на ГО
    data[[2]]$im.balance <- temp.df$im.balance
    # Расчёт комиссии на такте
    data[[2]]$commiss <- temp.df$commiss
    # Расчёт вариационки
    data[[2]]$margin <- temp.df$margin
    # расчёт equity по корзине в state-таблице
    data[[2]]$perfReturn <- temp.df$perfReturn
    data[[2]]$equity <- temp.df$equity
    # Расчёт баланса
    data[[2]]$balance <- temp.df$balance
    rm(temp.df)
    #
    ## Перенос данных из state в data таблицу
    # перенос данных по количеству контрактов корзины
    data[[1]]$n <- 
        merge(data[[1]], data[[2]]$n)$n %>%
        na.locf(.)
    # перенос данных по комиссии корзины
    data[[1]]$commiss <- merge(data[[1]], data[[2]]$commiss)$commiss 
    data[[1]]$commiss <- Replace.na(data[[1]]$commiss, 0)
        
    # перенос данных по суммарному ГО
    data[[1]]$im.balance <- 
        merge(data[[1]], data[[2]]$im.balance)$im.balance %>%
        na.locf(.)
    
    ## Расчёт показателей в full данных
    # расчёт вариационки в data
    data[[1]]$margin <- lag.xts(data[[1]]$n) * data[[1]]$cret
    data[[1]]$margin[1] <- 0
    # расчёт equity по корзине в data
    data[[1]]$perfReturn <- data[[1]]$margin - data[[1]]$commiss
    data[[1]]$equity <- cumsum(data[[1]]$perfReturn)
    # расчёт баланса
    data[[1]]$balance <- trade_args$balance_start + data[[1]]$equity - data[[1]]$im.balance
    data[[1]]$balance[1] <- trade_args$balance_start
    #
    return(data)
}
#
#' Функция расчёта сделок
#'
#' @param data Данные стратегии
#' @param FUN.CalcOneTrade Функция обсчёта одной сделки
#' @param MM.FUN MM функция
#' @param ohlc_args
#' @param trade_args
#' @param str_args
#'
#' @return result DF с данными по сделкам
#'
#' @export
CalcTrade <- function(data, 
                      FUN.CalcOneTrade, 
                      FUN.MM,
                      ohlc_args, trade_args, str_args) {
    FUN.CalcOneTrade <- match.fun(FUN.CalcOneTrade)
    FUN.MM <- match.fun(FUN.MM) 
    .TempEnv <- new.env()

    target_col = c('n', 'diff.n', 'balance', 'im', 'im.balance', 'commiss', 
        'margin', 'perfReturn', 'equity'#, 'weight'
    )
    assign('cache', 
        data[[2]][, target_col] %>% as.data.frame(., row.names=NULL), 
        envir = .TempEnv)
    # вес бота на первой сделке
    #initial_weight <- temp.cache$weight[1]
    sapply(1:nrow(data[[2]]),
        function(x) {
            #data[[2]][x, ] <- FUN.CalcOneTrade(data[[2]], x, ...)  
            FUN.CalcOneTrade(cache = get('cache', envir = .TempEnv), 
                row_ind = x,
                row = data[[2]][x, ], 
                FUN.MM = FUN.MM,
                external_balance = NULL,
                ohlc_args, trade_args, str_args) %>%
            assign('cache', ., envir = .TempEnv)
        })
    result <- get('cache', envir = .TempEnv)
    rm(.TempEnv)
    #
    return(result)
}
#
#' Функция расчёта данных одной сделки
#'
#' @param cahce Данные кэша
#' @param row_ind Номер анализируемой строки
#' @param row Строка для анализа 
#' @param FUN.MM Функция MM
#' @param external_balance
#' @param ohlc_args
#' @param trade_args
#' @param str_args
#'
#' @return data DF с данными по сделкам
#'
#' @export
CalcOneTrade <- function(cache, 
                         row_ind, 
                         row,
                         FUN.MM,
                         external_balance = NULL,
                         ohlc_args, trade_args, str_args) {
    FUN.MM <- match.fun(FUN.MM)
    #
    cache$n[row_ind] <- ifelse.fast(as.integer(row$pos) == 0,
        0,
        ifelse.fast(as.integer(row$pos.bars) == 0,
            FUN.MM(
                balance = ifelse.fast(!is.null(external_balance),
                    external_balance,
                    ifelse.fast(row_ind != 1,
                        cache$balance[row_ind - 1],
                        trade_args$balance_start)), #* cache$weight[row_ind - 1]),
                row,
                ohlc_args, trade_args, str_args 
            ),
            cache$n[row_ind - 1])
    )
    cache$diff.n[row_ind] <- ifelse.fast(row_ind != 1,
        cache$n[row_ind] - cache$n[row_ind - 1],
        0)
    cache$commiss[row_ind] <- trade_args$commiss * abs(cache$diff.n[row_ind])
    cache$margin[row_ind] <- ifelse.fast(row_ind != 1,
        coredata(row$cret) * cache$n[row_ind - 1],
        0)
    cache$perfReturn[row_ind] <- cache$margin[row_ind] - cache$commiss[row_ind]
    cache$equity[row_ind] <- ifelse.fast(row_ind != 1,
        sum(cache$perfReturn[row_ind], cache$equity[row_ind - 1]),
        0)
    cache$im.balance[row_ind] <- ohlc_args$ohlc$IM[index.xts(row)] * cache$n[row_ind]
    if (!is.null(external_balance)) {
        # cache$balance[row_ind] <- NA
        cache$balance[row_ind] <- ifelse.fast(row_ind != 1,
            cache$balance[1] + cache$equity[row_ind] - cache$im.balance[row_ind],
            cache$balance[1])
    } else {
        cache$balance[row_ind] <- ifelse.fast(row_ind != 1,
            trade_args$balance_start + cache$equity[row_ind] - cache$im.balance[row_ind],
            cache$balance[row_ind])
    }
    #
    return(cache)
}
#