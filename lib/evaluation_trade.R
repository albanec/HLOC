# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Функции для вычисления таблицы сделок
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
###
#' Создание итоговой таблицы сделок
#' 
#' @param data Входной xts ряд сделок
#' @param basket Расчитывать по корзине, или нет (T/F)
#' @param ticker_names Вектор тикеров (необязательно)
#' @param convert Переносить открытия/закрытия в одну строку или нет (по умолчанию нет)
#'
#' @return list List, содержащий все сделки
#'
#' @export
TradeTable.calc <- function(data, 
                            basket = FALSE, convert = FALSE, ticker_names = NULL,
                            bto.name = 'BTO', bto_add.name = 'BTO_add',
                            sto.name = 'STO', sto_add.name = 'STO_add',
                            stc.name = 'STC', stc_drop.name = 'STC_drop',
                            btc.name = 'BTC', btc_drop.name = 'BTC_drop') {
    #
    data <- data[!is.na(data$pos.num)]
    
    if (is.null(ticker_names)) {
        ticker_names <- 
            grep('.equity', names(data)) %>%
            names(data)[.] %>%
            sub('.equity', '', .)     
    }
    pos.num.list <- 
        max(data$pos.num) %>%
            1:. %>%
        {
            names(.) <- .
            return(.)
        } %>%
        as.list(.)
    #
    ### расчёт таблицы сделок (данные по каждому тикеру)
    tradeTable_byTickers <- 
        # посделочный расчёт, на выходе лист с df по каждой сделке
        lapply(pos.num.list,
            function(x) {
                TradeSummary(data, type = 'byTicker', n = x, ticker_names = ticker_names, 
                    bto.name, bto_add.name,
                    sto.name, sto_add.name,
                    stc.name, stc_drop.name,
                    btc.name, btc_drop.name)
            }) %>%
        # объединение данных внутри листа в один df
        MergeData_inList.byRow(.)
    if (convert != FALSE) {
        tradeTable_byTickers %<>% TradeTable.convert(trade_table = ., type = 'byTicker')
    }
    ### расчёт таблицы сделок (данные по корзине)
    if (basket == TRUE) {
        tradeTable_byBasket <- 
            lapply(pos.num.list,
                function(x) {
                    TradeSummary(data, type = 'byBasket', n = x, ticker_names = ticker_names, 
                        bto.name = , bto_add.name,
                        sto.name, sto_add.name,
                        stc.name, stc_drop.name,
                        btc.name, btc_drop.name)
                }) %>%
            MergeData_inList.byRow(.)
        if (convert != FALSE) {
            tradeTable_byBasket %<>% TradeTable.convert(trade_table = ., type = 'byBasket')    
        }
        result.list <- list(tradeTable_byBasket, tradeTable_byTickers)
        names(result.list) <- c('byBasket', 'byTicker')
    } else {
        result.list <- list(tradeTable_byTickers)
        names(result.list) <- c('byTicker')
    }
    #
    return(result.list)
}
#
###
#' Конвертирование таблицы сделок в нужный вид
#' 
#' Функция производит вывод данных по открытию/закрытию позиций в одну строку
#' 
#' @param trade_table Данные таблицы сделок
#' @param type Тип данных для анализа (tickers/basket)
#'
#' @return result Изменённая таблица данных сделок
#'
#' @export
TradeTable.convert <- function(trade_table, type = 'byTicker') {
    ## Номера столбцов, касающихся закрытия 
    numtrade <- 
        # ряд номеров позиций
        xts::last(trade_table$PositionNum) %>%
        1:. 
    # Выделение нужных столбцов (касаются закрытия)
    if (type == 'byTicker') {
        colNum <-
            c('ExitSignal', 'ExitDate', 'ExitPrice', 'ExitCommiss', 'TradeReturn', #'Equity', 
                'BarsHeld') %>%
            {
                which(colnames(trade_table) %in% .) 
            }
    } else {
        colNum <-
            c('ExitSignal', 'ExitDate', 'ExitCommiss', 'TradeReturn', 'TradeReturnPercent',
                'TradeProfit', 'TradeProfitPercent', #'Equity', 
                'BarsHeld') %>%
            {
                which(colnames(trade_table) %in% .) 
            }
    }
    ### Перенос данных в нужные строки
    # индексы строк открытия
    openRowIndex <- which(
        (trade_table$PositionNum %in% numtrade) & !is.na(trade_table$EntrySignal)
    )
    # индексы строк закрытия
    closeRowIndex <- which(
        (trade_table$N == 0) & !is.na(trade_table$ExitSignal)
    )
    ## Вывод данных по открытию/закрытию позиций в одну строку
    trade_table <- 
        # инъекция данных
        {                            
            trade_table[openRowIndex, colNum] <- trade_table[closeRowIndex, colNum]
            trade_table$TradeReturn[openRowIndex] <- trade_table$TradeProfit[closeRowIndex]
            trade_table$TradeReturnPercent[openRowIndex] <- trade_table$TradeProfitPercent[closeRowIndex]
            trade_table$TradeProfit[openRowIndex] <- trade_table$TradeProfit[closeRowIndex]
            trade_table$TradeProfitPercent[openRowIndex] <- trade_table$TradeProfitPercent[closeRowIndex]
            trade_table$MarketCompliance[openRowIndex] <- trade_table$MarketCompliance[closeRowIndex] 
            #trade_table$Balance[openRowIndex] <- trade_table$Balance[closeRowIndex] 
            return(trade_table)
        } %>%
        # очистка ненужных данных (удаление лога закрытий)
        {
            trade_table <- trade_table[-closeRowIndex, ]
            #trade_table$TradeProfit <- NULL
            #trade_table$TradeProfitPercent <- NULL
            #names(trade_table)[names(trade_table) == 'TradeReturn'] <- 'TradeProfit'
            #names(trade_table)[names(trade_table) == 'TradeReturnPercent'] <- 'TradeProfitPercent'
            return(trade_table)
        }
    #
    return(trade_table)
}
#
###
#' Вычисление данных по одной сделке
#' 
#' Выборка данных для конкретного немера сделки
#' 
#' @param data Входной xts ряд сделок
#' @param type Тип анализа (byTicker/byBasket)
#' @param n Номер сделки
#' @param ticker_names Вектор тикеров
#' @type Считать по тикерам или по портфелю
#' @param bto_add.name Написание увеличения позиции long
#' @param sto.name Написание открытия short
#' @param sto_add.name Написание увеличения позиции short
#' @param stc.name Написание закрытия long
#' @param stc_drop.name Написание сброса позиции long
#' @param btc.name Написание закрытия short
#' @param btc_drop.name Написание сброса позиции short
#'
#' @return trade_table data.frame содержащий все сделки
#'
#' @export
TradeSummary <- function(data, type, n, ticker_names = NULL,
                         bto.name, bto_add.name,
                         sto.name, sto_add.name,
                         stc.name, stc_drop.name,
                         btc.name, btc_drop.name) {
    #
    if (is.null(ticker_names)) {
        ticker_names <- 
            grep('.Price', names(data)) %>%
            names(data)[.] %>%
            sub('.Price', '', .)     
    } 
    # вытаскиваем данные по сделке n
    data <- data[data$pos.num == n] 
    # лист тикеров
    ticker_names.list <- as.list(ticker_names)
    
    ## расчёт данных сделок (по тикеру или корзине)
    if (type == 'byTicker') {
        data <- 
            # данные по каждому тикеру выкидываем в отдельный подлист
            lapply(ticker_names.list, 
                function(x) {
                    # правильно прописываем названия столбцов с нужными данными (в names.set)
                    temp.text <- paste0(
                        'names.set <- c(\"pos\", \"pos.num\", \"pos.bars\", \"pos.add\", \"pos.drop\", 
                            \"balance\",\"im.balance\",\"',x,'.n\", \"',x,'.diff.n\", \"',x,'.Price\", ',
                            '\"',x,'.commiss\", \"',x,'.equity\", \"',x,'.perfReturn\") ;',
                        sep = ''
                    )
                    eval(parse(text = temp.text))
                    # вытаскиваем нужные столбцы (по names.set)
                    result <- data[, (which(colnames(data) %in% names.set))]
                    
                    # для удобства переименуем         
                    names(result)[names(result) == paste0(x,'.n', sep = '')] <- 'n'
                    names(result)[names(result) == paste0(x,'.diff.n', sep = '')] <- 'diff.n'
                    names(result)[names(result) == paste0(x,'.Price', sep = '')] <- 'Price'
                    names(result)[names(result) == paste0(x,'.commiss', sep = '')] <- 'commiss'
                    names(result)[names(result) == paste0(x,'.equity', sep = '')] <- 'equity'
                    names(result)[names(result) == paste0(x,'.perfReturn', sep = '')] <- 'trade.return'
            
                    # нумерация субсделок (x.0 - открытия/закрытия и x.1...n - для изменений внутри)
                    result$pos.num %<>% TradeSummary.pos_num(x = .)
                    #
                    return(result)
                }) %>%
            MergeData_inList.byRow(.)
    } else {
        names.set <- c('pos', 'pos.num', 'pos.bars', 'pos.add', 'pos.drop', 'balance', 'im.balance',
            'n', 'diff.n', 'commiss', 'equity', 'perfReturn') 
        data <- data[, (which(colnames(data) %in% names.set))]
        names(data)[names(data) == 'perfReturn'] <- 'trade.return' 
        data$pos.num %<>% TradeSummary.pos_num(x = .)
    }
    
    # конвертируем таблицу в DF            
    data %<>% Convert.XTStoDF(.) 
    
    ### расчёт итогового DF
    trade_summary <- TradeSummary.summary_df(data, 
        type, ticker_names,
        bto.name, bto_add.name,
        sto.name, sto_add.name,
        stc.name, stc_drop.name,
        btc.name, btc_drop.name)
    
    trade_summary <- trade_summary[, -1]
    #
    return(trade_summary)
} 
#
###
#' Вычисление номеров сделок в нужном для TradeTable виде (вспомогательная функция)
#' 
#' @param x XTS с номерами позиций из states
#'
#' @return x Ряд сделок в нужном для TradeTable виде
#'
#' @export
TradeSummary.pos_num <- function(x) {
    row.num <- nrow(x)
    x <- 
        row.num %>%
        { 
            ifelse(. < 3, 
                0,
                ifelse(. > 9,
                    0.01,
                    ifelse(. > 10,
                        0.001,
                        0.1)))
        } %>% 
        rep(., row.num) %>%
        # расчёт номеров
        {
            .[1] <- 0 
            return(.)
        } %>%
        cumsum(.) %>%
        {
            .[nrow(.)] <- 0
            x + .
            return(x)
        }
    #
    return(x)    
}
#
###
#' Вычисление итогового df по сделке (вспомогательная функция)
#' 
#' @param x Входной xts ряд одной сделки
#' @param type Тип анализа (byTicker/byBasket)
#' @param ticker_names Вектор тикеров
#' @param bto.name Написание открытия long
#' @param bto_add.name Написание увеличения позиции long
#' @param sto.name Написание открытия short
#' @param sto_add.name Написание увеличения позиции short
#' @param stc.name Написание закрытия long
#' @param stc_drop.name Написание сброса позиции long
#' @param btc.name Написание закрытия short
#' @param btc_drop.name Написание сброса позиции short
#'
#' @return summary data.frame, содержащий данные по сделке
#'
#' @export
TradeSummary.summary_df <- function(x, type, ticker_names = NULL,
                                    bto.name, bto_add.name,
                                    sto.name, sto_add.name,
                                    stc.name, stc_drop.name,
                                    btc.name, btc_drop.name) {
    # bto.name = 'ОткрПозиПоРынк',
    # bto_add.name = 'ИзменПоРынку',
    # sto.name = 'ОткрПозиПоРынк1',
    # sto_add.name = 'ИзменПоРынку1',
    # stc.name = 'ЗакрПозиПоРынк',
    # stc_drop.name = 'ЗакрПозиПоРынк',
    # btc.name = 'ЗакрПозиПоРынк1',
    # btc_drop.name = 'ЗакрПозиПоРынк1'

    if (is.null(ticker_names)) {
        ticker_names <- 
            grep('.Price', names(data)) %>%
            names(data)[.] %>%
            sub('.Price', '', .)     
    } 

    summary <- 
        nrow(x) %>%
        # форомирование скелета DF
        data.frame(PositionNum = numeric(.),
            Position = character(.),
            Ticker = character(.),
            N = numeric(.),
            diff.N = integer(.),
            EntrySignal = character(.),
            EntryDate = character(.) %>% 
                as.numeric() %>% 
                as.Date(),
            EntryPrice = integer(.),
            EntryCommiss = integer(.),
            ExitSignal = character(.),
            ExitDate = character(.) %>% 
                as.numeric() %>% 
                as.Date(),
            ExitPrice = integer(.),
            ExitCommiss = integer(.),
            TradeReturn = numeric(.),
            TradeReturnPercent = numeric(.),
            MarketCompliance = numeric(.),
            TradeProfit = numeric(.),
            TradeProfitPercent = numeric(.),
            #Equity = numeric(.),
            BarsHeld = numeric(.),
            #Balance = numeric(.),
            row.names = NULL)
    
    ## номера позиций
    summary$PositionNum <- x$pos.num
    ## тип позиции
    summary$Position <- ifelse(x$pos[1] == 1, 'Длинная', 'Короткая')
    ## имя тикера
    if (type == 'byTicker') {
        summary$Ticker <- ticker_names
    } else {
        summary$Ticker <- 'Basket'
    }
    ## количество контрактов тикера
    summary$N <- x$n
    ## изменения контрактов тикера на текущей сделке
    summary$diff.N <- x$diff.n
    ## сигнал открытия
    summary$EntrySignal <- ifelse(x$pos == 1, 
        ifelse(x$pos.add == 1, 
            bto_add.name,
            ifelse(x$pos.drop == 0, 
                bto.name,
                NA)),
        ifelse(x$pos == -1,
            ifelse(x$pos.add == 1, 
                sto_add.name,
                ifelse(x$pos.drop == 0, 
                    sto.name,
                    NA)),
            NA))
    # дата открытия позиции
    summary$EntryDate <- 
        ifelse(x$pos != 0 & x$pos.drop == 0, 
            x$INDEX, 
            NA) %>%
        as.POSIXct(., origin = '1970-01-01') 
    ## цена тикера на открытии (не используется в "basket" режиме)
    if (type == 'byTicker') {
        summary$EntryPrice <- ifelse(x$pos != 0 & x$pos.drop == 0, 
            x$Price, 
            NA)
    } else {
        summary$EntryPrice <- NA
    }
    ## комиссия на закрытии
    summary$EntryCommiss <- ifelse(x$pos != 0 & x$pos.drop == 0, 
        x$commiss, 
        NA)
    ## сигнал закрытия
    summary$ExitSignal <- ifelse(x$pos == 0,
        ifelse(x$pos[1] == 1, 
            stc.name,
            btc.name), 
        ifelse(x$pos.drop != 0, 
            ifelse(x$pos[1] == 1, 
                stc_drop.name,
                btc_drop.name), 
            NA))
    ## дата закрытия
    summary$ExitDate <- 
        ifelse(x$pos == 0 | x$pos.drop != 0, 
            x$INDEX, 
            NA) %>%
        as.POSIXct(., origin = '1970-01-01') 
    ## цена тикера на закрытии (не используется в 'byBasket' режиме)
    if (type == 'byTicker') {
        summary$ExitPrice <- ifelse(x$pos == 0 | x$pos.drop != 0, 
            x$Price, 
            NA)
    } else {
        summary$ExitPrice <- NA
    }
    ## коммиссия на закрытии
    summary$ExitCommiss <- ifelse(x$pos == 0 | x$pos.drop != 0, 
        x$commiss, 
        NA)
    ## return позиции
    summary$TradeReturn <- x$trade.return
    ## return позиции в %
    #if (type == 'byBasket') {
        summary$TradeReturnPercent <- x$trade.return * 100 / (xts::first(x$balance) + xts::first(x$im.balance))
    #} else {
    #    summary$tradeReturnPercent <- NA    
    #}
    ## equity внутри сделки
    summary$TradeProfit <- cumsum(x$trade.return)
    #if (type == 'byBasket') {
        summary$TradeProfitPercent <- summary$TradeProfit * 100 / (xts::first(x$balance) + xts::first(x$im.balance))
    #} else {
    #    summary$TradeProfitPercent <- NA
    #}
    ## Banchmark соответствия рынку
    if (type == 'byTicker') {
        summary$MarketCompliance <- sign(xts::last(summary$TradeReturn)) * 
            (xts::first(summary$EntryPrice) - xts::last(summary$ExitPrice)) * 100 / 
            xts::first(summary$EntryPrice)
    } else {
        summary$MarketCompliance <- NA
    }
    ## изменения equity тикера
    #summary$Equity <- x$equity
    ## тики позиций
    summary$BarsHeld <- x$pos.bars
    ## Баланс
    #summary$Balance <- x$balance + x$im.balance
    #
    return(summary)
}
#