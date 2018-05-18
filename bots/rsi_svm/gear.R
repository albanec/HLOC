# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Движок cтратегии 'RMI & SVM':
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
###
#' Функция движка стратегии 'RMI & SVM':
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
    #'
#' @return DATA Лист с данными отработки и данные сделок
#'
#' @export
StrategyGear.rsi_svm  <- function(ohlc_args,
                                  trade_args,
                                  str_args) {
    ### Проверка входных данных
    if (!all(c('ohlc', 'from_date', 'to_date', 'lookback', 'ticker') %in% names(ohlc_args))) {
        stop(paste0('ERROR(StrategyGear.rsi_svm): Input ohlc_args error!!!'))
    }
    if (!all(c('balance_start', 
        'slips', 'commiss', 'expiration_date',
        'return_type', 'tick_price', 'gap_filter', 'trade_handler') %in% names(trade_args))) {
        stop(paste0('ERROR(StrategyGear.rsi_svm): Input trade_args error!!!'))
    }
    if (!all(c('per_SMA', 'per_RSI') %in% names(str_args))) {
        stop(paste0('ERROR(StrategyGear.rsi_svm): Input str_args error!!!'))
    }
    
    ### Выделение нужного торгового интервала
    if (ohlc_args$lookback == TRUE) {
        ohlc_args$ohlc <- 
            max(str_args$per_RSI, str_args$per_SMA) %>%
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
    #     'StrategyGear.rsi_svm INFO:    Start StrategyGear.rsi_svm with parameters:', '\n',
    #     '    SMA period: ', str_args$per_slowSMA, '\n',
    #     '    RSI period: ', str_args$per_DCI, '\n',
    #     '    MM kofficient: ', str_args$k_mm, '\n',
    #     '    Start Balance: ', trade_args$balance_start, '\n',
    #     '    Ticker: ', ohlc_args$ticker, '\n',
    #     'StrategyGear.rsi_svm INFO:    Start StrategyData Calculation...', '\n'
    # )
    
    ### Работа над DATA-таблицей
    # добавление индикаторов
    DATA <- xts(NULL, order.by = index.xts(ohlc_args$ohlc))
    DATA <- 
        AddIndicator.rsi_svm(DATA, ohlc_args, str_args) %>% 
        na.omit(.)
    # добавление сигналов 
    DATA <- AddSignal.rsi_svm(DATA, ohlc_args, trade_args, str_args)
    # выделение ордеров
    DATA <- AddPosition.rsi_svm(DATA, ohlc_args)
    if (is.null(DATA)) {
        return(list(data = NULL, state = NULL))
    }
    
    ### STATE-таблица
    #cat('StrategyGear.rsi_svm INFO:    Build state.table...', '\n')
    DATA <- AddStateTable(DATA)
        
    # если сделок нет, то 
    if (is.null(DATA[[2]])) {
        #message('WARNING(StrategyGear.rsi_svm): No trades Here!!!', '\n')
        return(list(data = NULL, state = NULL))
    }
    
    ### Расчёт цен инструментов на action'ах
    DATA <- AddPrice(DATA, AddPrice.rsi_svm, ohlc_args, trade_args)
    
    ### Расчёт return'ов позиций и состояний
    #cat('StrategyGear.rsi_svm INFO:    CalcReturns for data & states...', '\n')             
    DATA <- AddReturn.rsi_svm(DATA, trade_args)
    
    ### Подключение обработчика сделок
    # если бот торгует в гордом одиночестве - включается стандартный обработчик сделок
    if (trade_args$trade_handler == 'standalone') {
        #cat('StrategyGear.rsi_svm INFO:    TradeHandler Start...', '\n')
        DATA <- TradeHandler.rsi_svm(DATA, ohlc_args, trade_args, str_args)
    } else {
        # если бот - часть пакетной торговли - то используется пакетный обработчик сделок
        #cat('StrategyGear.rsi_svm INFO:    Send DATA to basket handler...', '\n')
    }
    #
    return(DATA)
}
#
# ----------------------------------------------------------------------------------------------------------------------
# Компоненты движка cтратегии 'Черепахи':
# ----------------------------------------------------------------------------------------------------------------------
#
#' Функция для расчёта индикаторов "RMI & SVM"
#'
#' @param x xts, в который необходимо добавить данные
#' @param ohlc_args Лист с данными котировок
#' @param str_args Лист с параметрами стратегии
#' 
#' @return x DATA-таблица
#'
#' @export
AddIndicator.rsi_svm <- function(x, ohlc_args, str_args) {
    #
    #cat('StrategyGear.rsi_svm INFO:    Calculate SMA with period:    ', per_SMA, '\n')
    x$SMA <- CalcIndicator.SMA(Op(ohlc_args$ohlc), str_args$per_SMA)
    #
    #cat('StrategyGear.rsi_svm INFO:    Calculate RSI with period:    ', per_RSI, '\n')
    x$RSI <- CalcIndicator.RSI(Op(ohlc_args$ohlc), str_args$per_RSI)
    #    
    return(x)    
}
#
#' Функция для расчёта торговых сигналов "RMI & SVM"
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
AddSignal.rsi_svm <- function(x, ohlc_args, trade_args, str_args) {
    # GAP
    #if 
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
        x$expiration[expiration_index] <- 1
        # индекс принудительного закрытия сделок в 16:00 дня экспирации
        expiration_ind <- paste(expiration_index, '16:00:00', sep = ' ')
        x$expiration[expiration_index] <- -1
    }
    rm(expiration_index)

    # Синтетический индикатор тренда 
    x$sigTrend <- Op(ohlc_args$ohlc[index(x)]) - x$SMA
    
    # Движение свечей
    x$sigDirection <- ifelse.fast(Cl(ohlc_args$ohlc[index(x)]) - Op(ohlc_args$ohlc[index(x)]) > 0, 1, -1)
    #
    DataSet <- x[, c('RSI', 'sigTrend', 'sigDirection')]

    #Separate the data into 60% training set to build our model, 20% test set to test the patterns we found, and 20% validation set to run our strategy over new data
    Training <- DataSet[1:8312, ]
    Test <- DataSet[8313:11082, ]
    Val <- DataSet[11083:13854, ]

    system.time({
    SVM = svm(sigDirection ~ sigRSI + sigTrend, data=Training, kernel="radial",cost=1,gamma=1/2)
    })
    TrainingPredictions = predict(SVM,Training,type="class")
    TrainingPredictions = ifelse(TrainingPredictions > 0, 1, -1)

    TrainingData = data.frame(Training,TrainingPredictions)
    
    ggplot(TrainingData, aes(x = sigTrend,y = RSI)) + 
    stat_density2d(geom = "contour", aes(color=as.factor(TrainingPredictions))) + 
    labs(title="SVM RSI3 and Trend Predictions", x = "Open - SMA", y = "RSI", color = "Training Predictions")

    ShortRange1 = Test$RSI > 55 & Test$sigTrend < 120 
    ShortRange2 = Test$RSI > 75 & Test$sigTrend > 0
    
    LongRange1 = Test$RSI < 62.5 & Test$RSI > -62.5 & Test$sigTrend > 550
    LongRange2 = Test$RSI < -62.5

    
    # Now lets see how well these patterns hold up over out test set:
    ShortTrades = Test[ShortRange1 | ShortRange2, ]
    ShortCorrect = sum(ShortTrades$sigDirection == -1)/nrow(ShortTrades)*100
    ShortCorrect

    LongTrades  = Test[LongRange1 | LongRange2, ]
    LongCorrect = sum(LongTrades$sigDirection == 1)/nrow(LongTrades)*100
    LongCorrect

    #
    return(x)
}

AddPrice.by_market <- function(x, ohlc) {
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