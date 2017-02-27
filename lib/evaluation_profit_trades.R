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
#' @param trades_table Таблица сделок (с данными по тикерам)
#'
#' @return result.list List с DF-данными по profit'у сделок (по тикерам корзины)
#'
#' @export
#
ProfitTable.byTrades <- function(trades_table) {
    # подготовка данных для обработки (фильтрация субсделок)
    names.set <- unique(trades_table$Ticker)
    ### Расчёт
    result.list <- lapply(names.set, 
        function(x){
            TradesStats.one_ticker(trades_table, ticker_name = x)
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
#' @param trades_table Таблица сделок с данными по нужному тикеру 
#' @ticker_name Имя тикера
#' @param balance Стартовый баланс
#'
#' @return result DF с данными по profit'у сделок тикера
#'
#' @export
TradesStats.one_ticker <- function(trades_table, ticker_name, ...) {
    # подготовка данных для обработки (фильтрация субсделок)
    trades_table %<>%
        # выделение нужных строк
        {
            temp.ind <- which(.$Ticker == ticker_name) 
            result <- trades_table[temp.ind, ]
            return(result)
        } %>%
        # фильтрация субсделок
        {
            temp.ind <- which(.$PositionNum%%1 == 0)
            result <- trades_table[temp.ind, ]
            return(result)
        }     
    ### Всего сделок
    trades.num <- xts::last(trades_table$PositionNum)
    ### разбор статистики
    # индексы прибыльных/убыточных сделок
    goodTrade.index <- which(trades_table$TradeReturn >= 0)
    badTrade.index <- which(trades_table$TradeReturn < 0)
    ### Всего сделок в плюс
    numGoogTrades <- 
        goodTrade.index %>%
        length(.)
    ### Win rate
    winRatePercent <- numGoogTrades * 100 / trades.num
    ### Всего сделок в минус
    numBadTrades <- 
        badTrade.index %>%
        length(.)
    ### Loss rate
    lossRatePercent <- numBadTrades * 100 / trades.num
    ### Расчёт последовательностей сделок
    # подготовка данных для анализа
    tradesSeries <- 
    {
        ifelse(trades_table$TradeReturn >= 0, 1, -1)
    } %>%
    rle(.) %>%
    {
        data.frame(DayType = .[[2]], SeriesLength = .[[1]])
    }
    ### Max сделок в плюс
    maxGoodTrades <-
        which(tradesSeries$DayType == 1) %>%
        {
            max(tradesSeries$SeriesLength[.])
        }
    ### Max сделок в минус
    maxBadTrades <-
        which(tradesSeries$DayType == -1) %>%
        {
            max(tradesSeries$SeriesLength[.])
        }    
    ### Профит-фактор
    ## всего заработано (по сделкам)
    goodTrade.sum <- sum(trades_table$TradeReturn[goodTrade.index])
    ## всего слито (по сделкам)
    badTrade.sum <- sum(trades_table$TradeReturn[badTrade.index])
    ## PF
    pf.trades <- goodTrade.sum / abs(badTrade.sum)
    ### Средний доход по сделкам
    averageGoodTradeReturn <- 
        trades_table$TradeReturn[goodTrade.index] %>%
        mean(.)
    ### Средний доход по сделкам в %
    averageGoodTradeReturnPercent <- 
        trades_table$TradeReturnPercent[goodTrade.index] %>%
        mean(.)
    ### Средний минус
    averageBadTradeReturn <- 
        trades_table$TradeReturn[badTrade.index] %>%
        mean(.)
    ### Средний минус в %
    averageBadTradeReturnPercent <- 
        trades_table$TradeReturnPercent[badTrade.index] %>%
        mean(.)
    ### Среднее баров на сделку
    averageTradeBars <- 
        mean(trades_table$BarsHeld) %>%
        trunc(.)
    ### Среднее баров на прибыльную сделку
    averageGoodTradeBars <- 
        mean(trades_table$BarsHeld[goodTrade.index]) %>%
        trunc(.)
    ### Среднее баров на убыточную сделку
    averageBadTradeBars <- 
        mean(trades_table$BarsHeld[badTrade.index]) %>%
        trunc(.) %>%
        {
            ifelse(. == 0, 1, .)
        }
    ### Средний П/У на сделку
    averageTradeReturn <- mean(trades_table$TradeReturn)
    ### Средний П/У на сделку в %
    averageTradeReturnPercent <- mean(trades_table$TradeReturnPercent)
    # 
    ### Формирование итоговой таблицы
    result <- data.frame(TradesNum = trades.num,                        
        TradesAverageBarsHeld = averageTradeBars,
        TradesAverageProfit = averageTradeReturn,
        TradesAverageProfitPercent = averageTradeReturnPercent,
        TradesProfitFactor = pf.trades,
        TradesWin = numGoogTrades,
        TradesWinMax = maxGoodTrades,
        TradesWinRate = winRatePercent,
        TradesGrossProfit = goodTrade.sum,
        TradesWinAverageProfit = averageGoodTradeReturn,
        TradesWinAverageProfitPercent = averageGoodTradeReturnPercent,
        TradesWinAverageBarsHeld = averageGoodTradeBars, 
        TradesLoss = numBadTrades,
        TradesLossMax = maxBadTrades,
        TradesLossRate = lossRatePercent,
        TradesGrossLoss = badTrade.sum,
        TradesLossAverageLoss = averageBadTradeReturn,
        TradesLossAverageLossPercent = averageBadTradeReturnPercent,
        TradesLossAverageBarsHeld = averageBadTradeBars,
        row.names = NULL)             
    #
    return(result)
}
#

