# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Функции расчета посделочных метрик доходности:
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
###
#' Расчёт таблицы с данными по доходностям сделок 
#'
#' Функция вычисляет параметры по доходностям сделок и 
#' формирует итоговый лист с DF-данными (по данным всех тикеров корзины или в целом по корзине)
#' 
#' @param trade_table Таблица сделок (с данными по тикерам)
#'
#' @return result.list List с DF-данными по profit'у сделок (по тикерам корзины)
#'
#' @export
#
ProfitTable.byTrade <- function(trade_table) {
    # подготовка данных для обработки (фильтрация субсделок)
    names.set <- unique(trade_table$Ticker)
    ### Расчёт
    result.list <- lapply(names.set, 
        function(x){
            TradeStats.one_ticker(trade_table, ticker_name = x)
        })
    names(result.list) <- names.set
    #
    return(result.list)
}
###
#' Расчёт таблицы с данными по доходностям сделок (для одного тикера)
#'
#' Функция вычисляет параметры по доходностям сделок и формирует итоговый DF (по данным одного тикера)
#' 
#' @param trade_table Таблица сделок с данными по нужному тикеру 
#' @ticker_name Имя тикера
#' @param balance Стартовый баланс
#'
#' @return result DF с данными по profit'у сделок тикера
#'
#' @export
TradeStats.one_ticker <- function(trade_table, ticker_name, ...) {
    # подготовка данных для обработки (фильтрация субсделок)
    trade_table %<>%
        # выделение нужных строк
        {
            temp.ind <- which(.$Ticker == ticker_name) 
            result <- trade_table[temp.ind, ]
            return(result)
        } %>%
        # фильтрация субсделок
        {
            temp.ind <- which(.$PositionNum%%1 == 0)
            result <- trade_table[temp.ind, ]
            return(result)
        }     
    ### Всего сделок
    trade.num <- xts::last(trade_table$PositionNum)
    ### разбор статистики
    # индексы прибыльных/убыточных сделок
    goodTrade.index <- which(trade_table$TradeReturn >= 0)
    badTrade.index <- which(trade_table$TradeReturn < 0)
    ### Всего сделок в плюс
    numGoogTrade <- 
        goodTrade.index %>%
        length(.)
    ### Win rate
    winRatePercent <- numGoogTrade * 100 / trade.num
    ### Всего сделок в минус
    numBadTrade <- 
        badTrade.index %>%
        length(.)
    ### Loss rate
    lossRatePercent <- numBadTrade * 100 / trade.num
    ### Расчёт последовательностей сделок
    # подготовка данных для анализа
    tradeSeries <- 
    {
        ifelse(trade_table$TradeReturn >= 0, 1, -1)
    } %>%
    rle(.) %>%
    {
        data.frame(DayType = .[[2]], SeriesLength = .[[1]])
    }
    ### Max сделок в плюс
    maxGoodTrade <-
        which(tradeSeries$DayType == 1) %>%
        {
            max(tradeSeries$SeriesLength[.])
        }
    ### Max сделок в минус
    maxBadTrade <-
        which(tradeSeries$DayType == -1) %>%
        {
            max(tradeSeries$SeriesLength[.])
        }    
    ### Профит-фактор
    ## всего заработано (по сделкам)
    goodTrade.sum <- sum(trade_table$TradeReturn[goodTrade.index])
    ## всего слито (по сделкам)
    badTrade.sum <- sum(trade_table$TradeReturn[badTrade.index])
    ## PF
    pf.trade <- goodTrade.sum / abs(badTrade.sum)
    ### Средний доход по сделкам
    averageGoodTradeReturn <- 
        trade_table$TradeReturn[goodTrade.index] %>%
        mean(.)
    ### Средний доход по сделкам в %
    averageGoodTradeReturnPercent <- 
        trade_table$TradeReturnPercent[goodTrade.index] %>%
        mean(.)
    ### Средний минус
    averageBadTradeReturn <- 
        trade_table$TradeReturn[badTrade.index] %>%
        mean(.)
    ### Средний минус в %
    averageBadTradeReturnPercent <- 
        trade_table$TradeReturnPercent[badTrade.index] %>%
        mean(.)
    ### Среднее баров на сделку
    averageTradeBars <- 
        mean(trade_table$BarsHeld) %>%
        trunc(.)
    ### Среднее баров на прибыльную сделку
    averageGoodTradeBars <- 
        mean(trade_table$BarsHeld[goodTrade.index]) %>%
        trunc(.)
    ### Среднее баров на убыточную сделку
    averageBadTradeBars <- 
        mean(trade_table$BarsHeld[badTrade.index]) %>%
        trunc(.) %>%
        {
            ifelse(. == 0, 1, .)
        }
    ### Средний П/У на сделку
    averageTradeReturn <- mean(trade_table$TradeReturn)
    ### Средний П/У на сделку в %
    averageTradeReturnPercent <- mean(trade_table$TradeReturnPercent)
    # 
    ### Формирование итоговой таблицы
    result <- data.frame(TradeNum = trade.num,                        
        TradeAverageBarsHeld = averageTradeBars,
        TradeAverageProfit = averageTradeReturn,
        TradeAverageProfitPercent = averageTradeReturnPercent,
        TradeProfitFactor = pf.trade,
        TradeWin = numGoogTrade,
        TradeWinMax = maxGoodTrade,
        TradeWinRate = winRatePercent,
        TradeGrossProfit = goodTrade.sum,
        TradeWinAverageProfit = averageGoodTradeReturn,
        TradeWinAverageProfitPercent = averageGoodTradeReturnPercent,
        TradeWinAverageBarsHeld = averageGoodTradeBars, 
        TradeLoss = numBadTrade,
        TradeLossMax = maxBadTrade,
        TradeLossRate = lossRatePercent,
        TradeGrossLoss = badTrade.sum,
        TradeLossAverageLoss = averageBadTradeReturn,
        TradeLossAverageLossPercent = averageBadTradeReturnPercent,
        TradeLossAverageBarsHeld = averageBadTradeBars,
        row.names = NULL)             
    #
    return(result)
}
#

