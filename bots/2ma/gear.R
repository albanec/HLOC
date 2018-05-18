# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Движок тестоовй cтратегии '2ma':
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
###
#' Функция движка стратегии '2ma':
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
    #' exp.vector Вектор с датами экспирации
    #' return_type = 'ret' Тип return'а
    #' tick_price = 1 Цена тика изменения котировки
    #' gap_filter = TRUE Фильтровать сделки на gap'ах (T/F)
    #' trade_handler = 'standalone' Тип обработчика сделок (standalone/basket)
#' @param str_args Лист с параметрами стратегии
    #' per_slowMA Период slowMA 
    #' per_fastMA Период fastMA
    #' MA_type Тип MA
#'
#' @return DATA Лист с данными отработки и данные сделок
#'
#' @export
StrategyGear.two_ma  <- function(ohlc_args,
                                 trade_args,
                                 str_args) {
    ### Проверка входных данных
    if (!all(c('ohlc', 'from_date', 'to_date', 'lookback', 'ticker') %in% names(ohlc_args))) {
        stop(paste0('ERROR(StrategyGear.two_ma): Input ohlc_args error!!!'))
    }
    if (!all(c('balance_start', 
        'slips', 'commiss', 'expiration_date',
        'return_type', 'tick_price', 'gap_filter', 'trade_handler') %in% names(trade_args))) {
        stop(paste0('ERROR(StrategyGear.two_ma): Input trade_args error!!!'))
    }
    if (!all(c('per_slowMA', 'per_fastMA') %in% names(str_args))) {
        stop(paste0('ERROR(StrategyGear.two_ma): Input str_args error!!!'))
    }
    
    ### Выделение нужного торгового интервала
    if (ohlc_args$lookback == TRUE) {
        ohlc_args$ohlc <- 
            max(str_args$per_slowMA, str_args$per_fastMA) %>%
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
    #     '2maStrategy INFO:    Start 2maStrategy with parameters:', '\n',
    #     '    slowMA period: ', str_args$per_slowMA, '\n',
    #     '    fastMA period: ', str_args$per_fastMA, '\n',
    #     '    MA type: ', str_args$MA_type, '\n',
    #     '    Start Balance: ', trade_args$balance_start, '\n',
    #     '    Ticker: ', ohlc_args$ticker, '\n',
    #     '2maStrategy INFO:    Start StrategyData Calculation...', '\n'
    # )
    
    ### Работа над DATA-таблицей
    # добавление индикаторов
    DATA <- xts(NULL, order.by = index.xts(ohlc_args$ohlc))
    DATA <- 
        AddIndicator.two_ma(DATA, ohlc_args, str_args) %>% 
        na.omit(.)
    # добавление сигналов 
    DATA <- AddSignal.two_ma(DATA, ohlc_args, trade_args, str_args)
    # выделение ордеров
    DATA <- AddPosition.two_ma(DATA, ohlc_args)
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
    DATA <- AddPrice(DATA, AddPrice.byMarket, ohlc_args, trade_args)
    
    ### Расчёт return'ов позиций и состояний
    #cat('TurtlesStrategy INFO:    CalcReturns for data & states...', '\n')             
    DATA <- AddReturn.two_ma(DATA, trade_args)
    
    ### Подключение обработчика сделок
    # если бот торгует в гордом одиночестве - включается стандартный обработчик сделок
    if (trade_args$trade_handler == 'standalone') {
        #cat('TurtlesStrategy INFO:    TradeHandler Start...', '\n')
        DATA <- TradeHandler.two_ma(DATA, ohlc_args, trade_args, str_args)
    } else {
        # если бот - часть пакетной торговли - то используется пакетный обработчик сделок
        #cat('TurtlesStrategy INFO:    Send DATA to basket handler...', '\n')
    }
    #
    return(DATA)
}
#
# ----------------------------------------------------------------------------------------------------------------------
# Компоненты движка cтратегии '2ma':
# ----------------------------------------------------------------------------------------------------------------------
#
#' Функция для расчёта индикаторов "2ma"
#'
#' @param x xts, в который необходимо добавить данные
#' @param ohlc_args Лист с данными котировок
#' @param str_args Лист с параметрами стратегии
#' 
#' @return x DATA-таблица
#'
#' @export
AddIndicator.two_ma <- function(x, ohlc_args, str_args) {
    #
    x$slowMA <- CalcIndicator.MA(x = Cl(ohlc_args$ohlc), 
        n = str_args$per_slowMA,
        FUN = TTR::SMA)
    #
    #cat('TurtlesStrategy INFO:    Calculate fastSMA with period:    ', per_fastSMA, '\n')
    x$fastMA <- CalcIndicator.MA(x = Cl(ohlc_args$ohlc), 
        n = str_args$per_fastMA,
        FUN = TTR::SMA)
    #    
    return(x)    
}
# 
#' Функция для расчёта торговых сигналов "2ma"
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
AddSignal.two_ma <- function(x, ohlc_args, trade_args, str_args) {
    # GAP
    gap_endpoint <- CalcEndpoints(x, on = 'days', k = 1, findFirst = TRUE)
    x$gap <- 0
    x$gap[gap_endpoint] <- 1

    # EXP
    x$expiration <- 0
    expiration_index <-
        which(trade_args$expiration_date %in% format(index.xts(x), '%Y-%m-%d')) %>%
        trade_args$expiration_date[.] %>%
        as.character(.)
    if (length(expiration_index) != 0) {
        # запрет на вход в сделки
        x$expiration[expiration_index] <- 1
        # индекс принудительного закрытия сделок в 16:00 дня экспирации
        expiration_index <- paste(expiration_index, '16:00:00', sep = ' ')
        x$expiration[expiration_index] <- -1
    }
    
    # UNWANTED EVENT
        # реакция на события (закрытие сделок перед событием / начало торговли с 13:00 даты события)
    if (trade_args$unwanted_event_filter == TRUE) {
        unwanted_event_index <-
            which(trade_args$unwanted_event_date %in% format(index.xts(x), '%Y-%m-%d')) %>%
            trade_args$unwanted_event_date[.] %>%
            as.character(.)
        if (length(unwanted_event_index) != 0) {
            # запрет на вход в сделки
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
        }
        rm(expiration_index, unwanted_event_index, temp_unwanted_event_index)
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
    
    # MA
    x$sig <- NA
    # сигналы
    x$sig <- ifelse.fast(x$fastMA > x$slowMA,
        1,
        ifelse.fast(x$fastMA < x$slowMA,
            -1,
            0))
    #
    return(x)
}
#
#' Функция формирования торговых правил и ряда позиций
#'
AddPosition.two_ma <- function(x, ohlc_args) {
    # формирование ряда открытий позиции
    x$pos <- lag.xts(x$sig)
    x$pos[1] <- 0
    #
    x$pos[x$gap != 0]  <- NA
    x$pos[x$expiration == 1] <- NA
    x$pos[x$expiration == -1] <- 0
    #
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
    
    ## Ряд состояний 
    x$state <- CalcState(x = x$pos)
    # условие закрытия сделок в конце торгового периода
    x$state[index.xts(xts::last(x$state))] <- 0
    
    ## Тики в позициях
    x$pos.bars <- CalcPosition.bars(x$pos.num)
    x$pos.bars[x$pos.num == 0] <- 0
    
    ## Точки смены позиций
    x$action <- diff(x$pos)
    x$action[1] <- x$pos[1]

    #Расщепление переворотов в позициях (расщепление строк с $action = +/-2)
    #индекс строки-переворота
    temp.ind <- index.xts(x[abs(x$action) == 2])
    if (length(temp.ind) == 0) {
      cat('TestStrategy INFO: No Switch Position there', '\n')
    } else {
        # temp копия нужных строк (строки начала новой сделки)
        temp <- 
            x[temp.ind] %>% 
            {
                .$pos <- sign(.$action)  
                # .$state <- sign(.$action)  
                .$action <- sign(.$action)  
                return(.)
            }
        # cтроки предыдущей сделки
        x$pos[temp.ind] <- 0
        # x$state[temp.ind] <- sign(x$action[temp.ind])
        x$action[temp.ind] <- sign(x$action[temp.ind])
        x$pos.num[temp.ind] <- x$pos.num[temp.ind] - 1
        # правильное заполнение поля $pos.bars
        temp.ind.num <- x[temp.ind, which.i = TRUE]
        x$pos.bars[temp.ind] <- x$pos.bars[temp.ind.num - 1] 
        x <- rbind(x, temp)   
    }
    remove(temp.ind)
    #
    return(x)
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
AddReturn.two_ma <- function(x, trade_args) {
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
TradeHandler.two_ma <- function(data, 
                                 ohlc_args, 
                                 trade_args, 
                                 str_args) {
    #
    data <- TradeHandler(data, 
        FUN.CalcTrade = CalcTrade,
        FUN.CalcOneTrade = CalcOneTrade,
        FUN.MM = CalcMM.simple_byIM, 
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
CalcVarList.two_ma <- function(slowMA = 30:250, step_slowMA = 5,
                               fastMA = 20:150, step_fastMA = 5,
                               trend_only = TRUE) {
    #
    if (trend_only == TRUE) {
        slow <- seq.int(from = min(slowMA), to = max(slowMA), by = step_slowMA)
        df <- lapply(slow, 
            function(x) {
                if (x <= max(fastMA)) {
                    fast <- seq.int(from = min(fastMA), to = x - 1, by = step_fastMA)                                    
                } else {
                    fast <- seq.int(from = min(fastMA), to = max(fastMA), by = step_fastMA)
                }
                result <- expand.grid(x, fast)
                return(result)
            }) %>%
            MergeData_inList.byRow(.)
    } else {
        var_seq <- seq.int(
            from = min(min(fastMA), min(slowMA)), 
            to = max(max(fastMA), max(slowMA)), 
            by = min(step_fastMA, step_slowMA)
        )
        df <- expand.grid(var_seq, var_seq)
    }
    colnames(df) <- c('per_slowMA', 'per_fastMA')
    # проверка на ошибочные строки (совпадения периодов SMA)
    temp.ind <- which(df$per_slowMA == df$per_fastMA)
    if (length(temp.ind) != 0) {
        df <- df[-temp.ind, ]
    } 
    # 
    return(df)
}
