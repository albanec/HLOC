# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Движок cтратегии 'Черепахи':
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
###
#' Функция движка стратегии 'Черепахи':
#' 
#' @param ohlc_args Лист с параметрами по котировкам
    #' ohlc xts с котировкми
    #' from_date # дата начала торговли 
    #' to_date # дата окончания торговли
    #' lookback # предварительное обучение (T/F)
#' @param trade_args Лист с торговыми параметрами
    #' balance_start Стартовый баланс 
    #' slips Проскальзывания
    #' commiss Коммиссии 
    #' expiration_date Вектор с датами экспирации
    #' return_type = 'ret' Тип return'а
    #' tick_price = 1 Цена тика изменения котировки
    #' gap_filter = TRUE Фильтровать сделки на gap'ах (T/F)
    #' expiration_filter = TRUE Фильтровать сделки на экспирациях (T/F)
    #' fix_profit T/F закрывать сделки в день фиксации прибыли
    #' fix_profit.period Период фиксации прибыли ('months' и пр.)
    #' fix_profit.k k-й период фиксации 
    #' fix_profit.at Время фиксации
    #' trade_handler = 'standalone' Тип обработчика сделок (standalone/basket)
    #' reinvest T/F реинвестировать средства или торговать фикс. лотом
#' @param str_args Лист с параметрами стратегии
    #' per_DCI Период DCI 
    #' per_slowSMA Период slowSMA 
    #' per_fastSMA Период fastSMA
    #' k_mm Коэфф. ММ
#'
#' @return DATA Лист с данными отработки и данные сделок
#'
#' @export
StrategyGear.turtles  <- function(ohlc_args,
                                  trade_args,
                                  str_args) {
    ### Проверка входных данных
    if (!all(c('ohlc', 'from_date', 'to_date', 'lookback', 'ticker') %in% names(ohlc_args))) {
        stop(paste0('ERROR(StrGear.turtles): Input ohlc_args error!!!'))
    }
    if (!all(c('balance_start', 
        'slips', 'commiss', 'expiration_date',
        'return_type', 'tick_price', 'gap_filter', 'expiration_filter', 'trade_handler') %in% names(trade_args))) {
        stop(paste0('ERROR(StrGear.turtles): Input trade_args error!!!'))
    }
    if (!all(c('per_DCI', 'per_slowSMA', 'per_fastSMA', 'k_mm') %in% names(str_args))) {
        stop(paste0('ERROR(StrGear.turtles): Input str_args error!!!'))
    }
    
    ### Выделение нужного торгового интервала
    if (ohlc_args$lookback == TRUE) {
        ohlc_args$ohlc <- 
            max(str_args$per_DCI + 1, str_args$per_slowSMA, str_args$per_fastSMA) %>%
            Subset_TradeOHLC(ohlc_args$ohlc, ohlc_args$from_date, ohlc_args$to_date, lookback = .)
    } else {
        ohlc_args$ohlc <- do.call('Subset_TradeOHLC', args = ohlc_args)
    }
    if (is.null(ohlc_args$ticker)) {
        ohlc_args$ticker <- 
            grep('.Open', names(ohlc_args$ohlc)) %>%
            names(ohlc_args$ohlc)[.] %>%
            sub('.Open', '', .)     
    }
    names(ohlc_args$ohlc)[names(ohlc_args$ohlc) == paste0(ohlc_args$ticker,'.Open')] <- 'Open'
    names(ohlc_args$ohlc)[names(ohlc_args$ohlc) == paste0(ohlc_args$ticker,'.High')] <- 'High'
    names(ohlc_args$ohlc)[names(ohlc_args$ohlc) == paste0(ohlc_args$ticker,'.Low')] <- 'Low'
    names(ohlc_args$ohlc)[names(ohlc_args$ohlc) == paste0(ohlc_args$ticker,'.Close')] <- 'Close'
    names(ohlc_args$ohlc)[names(ohlc_args$ohlc) == paste0(ohlc_args$ticker,'.Volume')] <- 'Volume'
    # Здесь появится система логирования, пока так
    # cat(
    #     'TurtlesStrategy INFO:    Start TurtlesStrategy with parameters:', '\n',
    #     '    slowSMA period: ', str_args$per_slowSMA, '\n',
    #     '    fastSMA period: ', str_args$per_fastSMA, '\n',
    #     '    DCI period: ', str_args$per_DCI, '\n',
    #     '    MM kofficient: ', str_args$k_mm, '\n',
    #     '    Start Balance: ', trade_args$balance_start, '\n',
    #     '    Ticker: ', ohlc_args$ticker, '\n',
    #     'TurtlesStrategy INFO:    Start StrategyData Calculation...', '\n'
    # )
    
    ### Работа над DATA-таблицей
    # добавление индикаторов
    DATA <- xts(NULL, order.by = index.xts(ohlc_args$ohlc))
    DATA <- 
        AddIndicator.turtles(DATA, ohlc_args, str_args) %>% 
        na.omit(.)
    # добавление сигналов 
    DATA <- AddSignal.turtles(DATA, ohlc_args, trade_args, str_args)
    # выделение ордеров
    DATA <- AddPosition.turtles(DATA, ohlc_args)
    if (is.null(DATA)) {
        return(list(data = NULL, state = NULL))
    }
    
    ### STATE-таблица
    #cat('TurtlesStrategy INFO:    Build state.table...', '\n')
    DATA <- AddStateTable(DATA)
        
    # если сделок нет, то 
    if (is.null(DATA[[2]])) {
        #message('WARNING(StrGear_turtles): No trades Here!!!', '\n')
        return(list(data = NULL, state = NULL))
    }
    
    ### Расчёт цен инструментов на action'ах
    DATA <- AddPrice(DATA, AddPrice.turtles, ohlc_args, trade_args)
    
    ### Расчёт return'ов позиций и состояний
    #cat('TurtlesStrategy INFO:    CalcReturns for data & states...', '\n')             
    DATA <- AddReturn.turtles(DATA, trade_args)
    
    ### Подключение обработчика сделок
    # если бот торгует в гордом одиночестве - включается стандартный обработчик сделок
    if (trade_args$trade_handler == 'standalone') {
        #cat('TurtlesStrategy INFO:    TradeHandler Start...', '\n')
        DATA <- TradeHandler.turtles(DATA, ohlc_args, trade_args, str_args)
    } else {
        # если бот - часть пакетной торговли - то используется пакетный обработчик сделок
        #cat('TurtlesStrategy INFO:    Send DATA to basket handler...', '\n')
    }
    #
    return(DATA)
}
#
# ----------------------------------------------------------------------------------------------------------------------
# Компоненты движка cтратегии 'Черепахи':
# ----------------------------------------------------------------------------------------------------------------------
#
#' Функция для расчёта индикаторов "Черепах"
#'
#' @param x xts, в который необходимо добавить данные
#' @param ohlc_args Лист с данными котировок
#' @param str_args Лист с параметрами стратегии
#' 
#' @return x DATA-таблица
#'
#' @export
AddIndicator.turtles <- function(x, ohlc_args, str_args) {
    #
    x$slowSMA <- CalcIndicator.MA(x = Cl(ohlc_args$ohlc), n = str_args$per_slowSMA, FUN = TTR::SMA)
    #
    #cat('TurtlesStrategy INFO:    Calculate fastSMA with period:    ', per_fastSMA, '\n')
    x$fastSMA <- CalcIndicator.MA(x = Cl(ohlc_args$ohlc), n = str_args$per_fastSMA, FUN = TTR::SMA)
    #
    #cat('TurtlesStrategy INFO:    Calculate DCI with period:    ', per_DCI, '\n')
    x <- 
        CalcIndicator.DCI(x = ohlc_args$ohlc[, c('High','Low')], n = str_args$per_DCI, lag = TRUE) %>%
        {
            x$highDCI <- .$high
            x$midDCI <- .$mid
            x$lowDCI <- .$low
            x$widthDCI <- x$highDCI - x$midDCI
            return(x)
        }
    #    
    return(x)    
}
# 
#' Функция для расчёта торговых сигналов "Черепах"
#'
#' Добавляются сигналы на ордера
#'
#' @param x xts с индикаторами
#' @param ohlc_args Лист с данными котировок
#' @param str_args Лист с параметрами стратегии
#' 
#' @return x DATA-таблица
#'
#' @export
AddSignal.turtles <- function(x, ohlc_args, trade_args, str_args) {
    # выделяются только сигналы на вход
    
    # SMA
    x$sigSMA <- ifelse.fast(x$fastSMA > x$slowSMA, 
        1, 
        ifelse.fast(x$fastSMA < x$slowSMA, 
            -1, 
            0))
    x$sigSMA <- lag.xts(x$sigSMA)
    x$sigSMA[1] <- 0
    
    # GAP
    if (trade_args$gap_filter == TRUE) {
        gap_endpoint <- CalcEndpoints(x, on = 'days', k = 1, findFirst = TRUE)
        x$gap <- 0
        x$gap[gap_endpoint] <- 1
    }

    # EXP
    if (trade_args$expiration_filter == TRUE) {
        x$expiration <- 0
        expiration_index <-
            which(trade_args$expiration_date %in% format(index.xts(x), '%Y-%m-%d')) %>%
            trade_args$expiration_date[.] %>%
            as.character(.)
        if (length(expiration_index) != 0) {
            x$expiration[expiration_index] <- 1
            # индекс принудительного закрытия сделок в 16:00 дня экспирации
            expiration_index <- paste(expiration_index, '16:00:00', sep = ' ')
            x$expiration[expiration_index] <- -1
        }    
    }
    
    # UNWANTED EVENT
        # реакция на события (закрытие сделок перед событием / начало торговли с 13:00 даты события)
    if (trade_args$unwanted_event_filter == TRUE) {
        unwanted_event_index <-
            which(trade_args$unwanted_event_date %in% format(index.xts(x), '%Y-%m-%d')) %>%
            trade_args$unwanted_event_date[.] %>%
            as.character(.)
        if (length(unwanted_event_index) != 0) {
            temp_unwanted_event_index <- 
                paste(unwanted_event_index, '10:00:00', sep = ' ') %>%
                {
                    paste(., paste(unwanted_event_index, '12:45:00', sep = ' '), sep = '::')
                }
            x$expiration[temp_unwanted_event_index] <- 1
            # индекс принудительного закрытия сделок в 23:30 в день перед событием
            temp_unwanted_event_index <- 
                paste0(as.Date(unwanted_event_index) - 1) %>%
                paste(., '23:30:00', sep = ' ')
            x$expiration[temp_unwanted_event_index] <- -1
            rm(temp_unwanted_event_index)
        }
        rm(unwanted_event_index)
    }
    
    # FIX PROFIT
    if (trade_args$fix_profit == TRUE) {
        fix_profit.endpoint <- 
            CalcEndpoints(x, 
                on = trade_args$fix_profit.period, 
                k = trade_args$fix_profit.k, 
                findFirst = FALSE) %>%
            .[-1]
        # индекс принудительного закрытия сделок в trade_args$fix_profit.at день фиксации профита
        fix_profit.index <- 
            format(index.xts(x[fix_profit.endpoint, ]), '%Y-%m-%d') %>%
            as.character(.) %>%
            paste(., trade_args$fix_profit.at, sep = ' ')
        x$expiration[fix_profit.index] <- -1
        # запрет на открытие сделок до конца дня 
        fix_profit.row <- which(index.xts(x) %in% index.xts(x[fix_profit.index]))
        if (!any(fix_profit.row %in% fix_profit.endpoint)) {
            x[paste(index.xts(x[fix_profit.row + 1, ]), index.xts(x[fix_profit.endpoint, ]), sep = "::")] <- 1    
        }
    }
    
    # DCI
    x$sigDCI.open <- NA
    # сигналы для long
    x$sigDCI.open[Hi(ohlc_args$ohlc) >= x$highDCI & x$gap == 0 & x$expiration == 0] <- 1    
    # сигналы для short
    x$sigDCI.open[Lo(ohlc_args$ohlc) <= x$lowDCI & x$gap == 0 & x$expiration == 0] <- -1
    #
    return(x)
}
#
#' Функция формирования торговых правил и ряда позиций
#'
# AddPosition.turtles <- function(x, ohlc_args) {
#     # формирование ряда открытий позиции
#     x$sig_open <- NA 
#     x$raw_open <- 
#         ifelse.fast(x$sigSMA == x$sigDCI.open, x$sigDCI.open, NA) %>% 
#         Replace.na(., 0) 
#     x$sig_open <- Clean.duplicate(x$raw_open)
#     # расчёт сигналов закрытий и формирование позиций
#     x$sig_open.num <- cumsum(abs(x$sig_open))
#     nums <- unique(x$sig_open.num)
    
#     # предрасчёт закрытий
#     x$sig_close.long <- NA
#     x$sig_close.long[(Lo(ohlc_args$ohlc) <= x$midDCI & x$gap == 0) | x$expiration == -1]  <- 1
#     x$sig_close.short <- NA
#     x$sig_close.short[(Hi(ohlc_args$ohlc) >= x$midDCI & x$gap == 0) | x$expiration == -1] <- 1
#     .CacheEnv <- new.env()
#     assign('cache', 0, envir = .CacheEnv)
#     x$pos <- lapply(1:length(nums),
#         function(i) {
#             old_state <- get('cache', envir = .CacheEnv)
#             temp <- x[x$sig_open.num == nums[i], ]
#             temp_ind <- index.xts(temp)
#             sig_type <- ifelse.fast(old_state == 0, as.integer(temp$sig_open[1]), old_state)
#             #sig_type <- as.integer(temp$sig_open[1])
#             if (nums[i] == 0) {
#                 return(temp$sig_open)
#             }
#             if (sig_type == 1) {
#                 temp$sig_close.long[1] <- ifelse.fast(old_state == 0, NA, temp$sig_close.long[1])
#                 sig_close_row <- which(!is.na(temp$sig_close.long))[1]
#             } else {
#                 temp$sig_close.short[1] <- ifelse.fast(old_state == 0, NA, temp$sig_close.short[1])
#                 sig_close_row <- which(!is.na(temp$sig_close.short))[1]
#             }
#             if (!is.na(sig_close_row) & sig_close_row != 1) {
#                 temp$sig_open[1:(sig_close_row - 1)] <- sig_type
#                 temp$sig_open[sig_close_row:nrow(temp$sig_open)] <- 0    
#                 assign('cache', 0, envir = .CacheEnv)
#             } else {
#                 if (is.na(sig_close_row)) {
#                     temp$sig_open[] <- sig_type
#                     assign('cache', sig_type, envir = .CacheEnv)
#                     return(temp$sig_open)
#                 } 
#                 if (sig_close_row == 1 ) {
#                     temp$sig_open[1] <- 0
#                     if(temp$raw_open[2] != 0) {
#                         sig_type <- temp$raw_open[2]
#                         temp$sig_open[2] <- sig_type
#                         if (sig_type == 1) {
#                             temp$sig_close.long[1:2] <- NA
#                             sig_close_row <- which(!is.na(temp$sig_close.long))[1]
#                         } else {
#                             temp$sig_close.short[1:2] <- NA
#                             sig_close_row <- which(!is.na(temp$sig_close.short))[1]
#                         } 
#                         if (!is.na(sig_close_row)) {
#                             temp$sig_open[2:(sig_close_row - 1)] <- sig_type
#                             temp$sig_open[sig_close_row:nrow(temp$sig_open)] <- 0    
#                         } else {
#                             temp$sig_open[] <- sig_type
#                             if (is.na(sig_close_row)) {
#                                 assign('cache', sig_type, envir = .CacheEnv)
#                             }
#                         }
#                     } else {
#                         temp$sig_open[] <- 0
#                     }
#                 }
#             }
#             return(temp$sig_open)
#         }) %>% 
#         MergeData_inList.byRow(.)
    
#     x$raw_open
#     x$sig_open.num <- NULL
#     x$sig_close.long <- NULL
#     x$sig_close.short <- NULL
#     x$sigDCI.open <- NULL
#     #x$sigDCI.open <- Replace.na(x$sigDCI.open, 0)
    
#     x$pos <- na.locf(x$pos)
#     if (length(is.na(x$pos)) != 0) {
#         x$pos <- Replace.na(x$pos, 0)
#     }
#     # условие закрытия сделок в конце торгового периода
#     x$pos[index.xts(xts::last(x$pos))] <- 0

#     ## ПРОВЕРКА: после простановки позиций, - это должен быть ненулевой ряд
#     if (!any(x$pos != 0)) {
#         message('WARNING(TurtlesStrategy_gear): No trades Here!!!', '\n')
#         return(NULL)
#     }
#     #
#     ## Ряд номеров позиций 
#     x$pos.num <- CalcPosition.num(x$pos)
#     ## Тики в позициях
#     x$pos.bars <- CalcPosition.bars(x$pos.num)
#     x$pos.bars[x$pos.num == 0] <- 0
#     # точки смены позиций
#     x$action <- diff(x$pos)
#     x$action[1] <- x$pos[1]
#     #
#     return(x)
# } 
AddPosition.turtles <- function(x, ohlc_args) {
    # формирование ряда открытий позиции
    x$sig_open <- NA 
    x$sig_open <- 
        ifelse.fast(x$sigSMA == x$sigDCI.open, x$sigDCI.open, NA) %>% 
        Replace.na(., 0) %>% 
        Clean.duplicate(.)
    # расчёт сигналов закрытий и формирование позиций
    x$sig_open.num <- cumsum(abs(x$sig_open))
    nums <- unique(x$sig_open.num)
    
    # предрасчёт закрытий
    x$sig_close.long <- NA
    x$sig_close.long[(Lo(ohlc_args$ohlc) <= x$midDCI & x$gap == 0) | x$expiration == -1]  <- 1
    x$sig_close.short <- NA
    x$sig_close.short[(Hi(ohlc_args$ohlc) >= x$midDCI & x$gap == 0) | x$expiration == -1] <- 1

    x$pos <- lapply(1:length(nums),
        function(i) {
            temp <- x[x$sig_open.num == nums[i], ]
            temp_ind <- index.xts(temp)
            sig_type <- as.integer(temp$sig_open[1])
            if (nums[i] == 0) {
                return(temp$sig_open)
            }
            if (sig_type == 1) {
                temp$sig_close.long[1] <- NA
                sig_close_row <- which(!is.na(temp$sig_close.long))[1]
            } else {
                temp$sig_close.short[1] <- NA
                sig_close_row <- which(!is.na(temp$sig_close.short))[1]
            }
            if (!is.na(sig_close_row) & sig_close_row != 1) {
                temp$sig_open[1:(sig_close_row - 1)] <- sig_type
                temp$sig_open[sig_close_row:nrow(temp$sig_open)] <- 0    
            } else {
                temp$sig_open[] <- sig_type
            }
            return(temp$sig_open)
        }) %>% 
        MergeData_inList.byRow(.)
    
    x$sig_open.num <- NULL
    x$sig_close.long <- NULL
    x$sig_close.short <- NULL
    x$sigDCI.open <- NULL
    #x$sigDCI.open <- Replace.na(x$sigDCI.open, 0)
    
    x$pos <- na.locf(x$pos)
    if (length(is.na(x$pos)) != 0) {
        x$pos <- Replace.na(x$pos, 0)
    }
    # условие закрытия сделок в конце торгового периода
    x$pos[index.xts(xts::last(x$pos))] <- 0

    ## ПРОВЕРКА: после простановки позиций, - это должен быть ненулевой ряд
    if (!any(x$pos != 0)) {
        message('WARNING(TurtlesStrategy_gear): No trades Here!!!', '\n')
        return(NULL)
    }
    #
    ## Ряд номеров позиций 
    x$pos.num <- CalcPosition.num(x$pos)
    ## Тики в позициях
    x$pos.bars <- CalcPosition.bars(x$pos.num)
    x$pos.bars[x$pos.num == 0] <- 0
    # точки смены позиций
    x$action <- diff(x$pos)
    x$action[1] <- x$pos[1]
    #
    return(x)
} 
#
#' Расчёт цены инструмента внутри state-table "Черепах" 
#'
#' @param x STATE-данные
#' @param ohlc Котировки
#' 
#' @return x$Price Ряз с ценами на action'ах
#'
#' @export 
AddPrice.turtles <- function(x, ohlc) {
    # т.к используем стоп-заявки, переносим данные по значениям канала (с учётом проскальзывания; в пунктах)
    # на свечах после gap'ов открытие по Open
    # и с учётом позиции
    x$Price <- NA
    # индексы action'в
    action_index <- list(
        state = index.xts(x),
        open = index.xts(x[x$pos != 0]),
        close = index.xts(x[x$pos == 0]),
        neutral = index.xts(x[x$pos == 0 & x$action == 0])
    )
    # цены открытий (на сделках)
    x$Price[action_index$open] <-         
        x[action_index$open] %>%
        {
            ifelse.fast(.$pos == 1,
                ifelse.fast(Op(ohlc)[action_index$open] > .$highDCI,
                    Op(ohlc)[action_index$open],
                    .$highDCI),
                ifelse.fast(Op(ohlc)[action_index$open] < .$lowDCI,
                    Op(ohlc)[action_index$open],
                    .$lowDCI))
        } 
    # цены закрытий (на сделках)
    x$Price[action_index$close] <- 
        x[action_index$close] %>%
        {
            ifelse.fast(.$action == -1,
                ifelse.fast(Op(ohlc)[action_index$close] < .$midDCI,
                    Op(ohlc)[action_index$close],
                    .$midDCI),
                ifelse.fast(Op(ohlc)[action_index$close] > .$midDCI,
                    Op(ohlc)[action_index$close],
                    .$midDCI)) 
        }
    # цены промежуточный состояний (в сделках) (для проверки)
    if (length(action_index$neutral) != 0) {
        x$Price[action_index$neutral] <- ohlc$Open[action_index$neutral]    
    }
    coredata(x$Price) <- as.integer(x$Price)
    return(x$Price)
}
# 
#' Расчёт return'ов в пунктах и деньгах внутри gear "Черепах"
#'
#' @param x Лист DATA&STATE данных
#' @param trade_args Лист trade-аргументов
#'
#' @return x Лист DATA&STATE данных
#' 
#' @export
AddReturn.turtles <- function(x, trade_args) {
    # расчёт return'ов по сделкам (в пунктах) в state 
    x[[2]]$ret <- CalcReturn(data = x[[2]]$Price, type = trade_args$return_type) * lag.xts(x[[2]]$pos)    
    # расчёт return'ов по свечам (в пунктах) в data 
    x[[1]]$ret <- CalcReturn(data = x[[1]]$Price, type = trade_args$return_type) * lag.xts(x[[1]]$pos)    
    
    ### Расчёт cret
    # расчёт cret по инструментам в data и states
    # # для Si всё просто
    # x[[1]]$SPFB.SI.cret <- x[[1]]$SPFB.SI.ret
    # x[[2]]$SPFB.SI.cret <- x[[2]]$SPFB.SI.ret 
    # расчёт для data
    
    # добавление курса
    
    # суммарный cret в data
    x[[1]]$cret <- x[[1]]$ret
    # суммарный cret в states
    x[[2]]$cret <- x[[2]]$ret
    #
    return(x)
}
#
#' Функция-обработчик сделок для "Черепах" 
#' 
#' @param data Данные отработки робота 
#'
#' @return x Лист с данными стратегии (data + states)
#'
#' @export
TradeHandler.turtles <- function(data, 
                                 ohlc_args, 
                                 trade_args, 
                                 str_args) {
    #
    data <- TradeHandler(data, 
        FUN.CalcTrade = CalcTrade,
        FUN.CalcOneTrade = CalcOneTrade,
        FUN.MM = CalcMM.byDCIwidth, 
        ohlc_args, trade_args, str_args) 
    # добавление нужных на следующих этапах столбцов
    temp.text <- paste0(
        'data[[1]]$',ohlc_args$ticker,'.n <- data[[1]]$n;',
        'data[[1]]$',ohlc_args$ticker,'.diff.n <- data[[1]]$diff.n;',
        'data[[1]]$',ohlc_args$ticker,'.commiss <- data[[1]]$commiss;',
        'data[[1]]$',ohlc_args$ticker,'.equity <- data[[1]]$equity;',
        'data[[1]]$',ohlc_args$ticker,'.perfReturn <- data[[1]]$perfReturn;',
        #
        'data[[2]]$',ohlc_args$ticker,'.n <- data[[2]]$n;',
        'data[[2]]$',ohlc_args$ticker,'.diff.n <- data[[2]]$diff.n;',
        'data[[2]]$',ohlc_args$ticker,'.commiss <- data[[2]]$commiss;',
        'data[[2]]$',ohlc_args$ticker,'.equity <- data[[2]]$equity;',
        'data[[2]]$',ohlc_args$ticker,'.perfReturn <- data[[2]]$perfReturn;'
    )
    eval(parse(text = temp.text))

    # names(data[[1]])[names(data[[1]]) == 'n'] <- paste0(ohlc_args$ticker,'.n')
    # names(data[[1]])[names(data[[1]]) == 'diff.n'] <- paste0(ohlc_args$ticker,'.diff.n')
    names(data[[1]])[names(data[[1]]) == 'Price'] <- paste0(ohlc_args$ticker,'.Price')
    # names(data[[1]])[names(data[[1]]) == 'commiss'] <- paste0(ohlc_args$ticker,'.commiss')
    # names(data[[1]])[names(data[[1]]) == 'equity'] <- paste0(ohlc_args$ticker,'.equity')
    # names(data[[1]])[names(data[[1]]) == 'perfReturn'] <- paste0(ohlc_args$ticker,'.equity')
    #
    # names(data[[2]])[names(data[[2]]) == 'n'] <- paste0(ohlc_args$ticker,'.n')
    # names(data[[2]])[names(data[[2]]) == 'diff.n'] <- paste0(ohlc_args$ticker,'.diff.n')
    names(data[[2]])[names(data[[2]]) == 'Price'] <- paste0(ohlc_args$ticker,'.Price')
    # names(data[[2]])[names(data[[2]]) == 'commiss'] <- paste0(ohlc_args$ticker,'.commiss')
    # names(data[[2]])[names(data[[2]]) == 'equity'] <- paste0(ohlc_args$ticker,'.equity')
    # names(data[[2]])[names(data[[2]]) == 'perfReturn'] <- paste0(ohlc_args$ticker,'.equity')
    names(data) <- c('full', 'states')

    ## уборка
    # добавление столбцов для пост обработки в EVA
    data[[1]]$pos.add <- 0
    data[[1]]$pos.drop <- 0
    data[[2]]$pos.add <- 0
    data[[2]]$pos.drop <- 0
    #
    return(data)
}
#
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Функции для оптимизации стратегии "Черепахи":
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
###
#' Функция генерации DF с перебором переменных оптимизации "Черепах"
#' 
#' @param DCI Диапазон оптимизации DCI
#' @param step_DCI Шаг оптимизации DCI
#' @param slowSMA Диапазон оптимизации slowSMA
#' @param step_slowSMA Шаг оптимизации slowSMA
#' @param fastSMA Диапазон оптимизации fastSMA
#' @param step_fastSMA Шаг оптимизации fastSMA
#'
#' @return df DF с перебором параметров для оптимизации
#'
#' @export
CalcVarList.turtles <- function(DCI = 20:150, step_DCI = 5,
                                slowSMA = 30:250, step_slowSMA = 5,
                                fastSMA = 20:150, step_fastSMA = 5,
                                trend_only = TRUE) {
    #
    if (trend_only == TRUE) {
        slow <- seq.int(from = min(slowSMA), to = max(slowSMA), by = step_slowSMA)
        df <- lapply(slow, 
            function(x) {
                if (x <= max(fastSMA)) {
                    fast <- seq.int(from = min(fastSMA), to = x - 1, by = step_fastSMA)                                    
                } else {
                    fast <- seq.int(from = min(fastSMA), to = max(fastSMA), by = step_fastSMA)
                }
                dci <- seq.int(from = min(DCI), to = max(DCI), by = step_DCI)
                result <- expand.grid(x, fast, dci)
                return(result)
            }) %>%
            MergeData_inList.byRow(.)
    } else {
        var_seq <- seq.int(
            from = min(min(fastSMA), min(slowSMA)), 
            to = max(max(fastSMA), max(slowSMA)), 
            by = min(step_fastSMA, step_slowSMA)
        )
        dci <- seq.int(from = min(DCI), to = max(DCI), by = step_DCI)
        df <- expand.grid(var_seq, var_seq, dci)
    }
    colnames(df) <- c('per_slowSMA', 'per_fastSMA', 'per_DCI')
    # проверка на ошибочные строки (совпадения периодов SMA)
    temp.ind <- which(df$per_slowSMA == df$per_fastSMA)
    if (length(temp.ind) != 0) {
        df <- df[-temp.ind, ]
    } 
    # 
    return(df)
}
