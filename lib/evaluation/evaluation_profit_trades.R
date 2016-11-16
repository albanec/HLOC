# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Функции расчета посделочных метрик доходности:
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
###
#' Расчёт таблицы с данными по доходностям сделок 
#'
#' Функция вычисляет параметры по доходностям сделок и 
#' формирует итоговый лист с DF-данными (по данным всех тикеров корзины или в целом по корзине)
#' 
#' @param data Таблица сделок (с данными по тикерам)
#' @param balance Стартовый баланс
#'
#' @return result.list List с DF-данными по profit'у сделок (по тикерам корзины)
#'
#' @export
#
ProfitList_byTrades <- function(data, ...) {
  # подготовка данных для обработки (фильтрация субсделок)
  names.set <- unique(data$Ticker)
  ### Расчёт
  result.list <- lapply(names.set, 
                        function(x){
                          ProfitTable.byTrades.oneTicker(data = data, ticker.name = x)
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
#' @param data Таблица сделок с данными по нужному тикеру 
#' @ticker.name Имя тикера
#' @param balance Стартовый баланс
#'
#' @return result DF с данными по profit'у сделок тикера
#'
#' @export
ProfitTable.byTrades.oneTicker <- function(data, ticker.name, ...) {
  # подготовка данных для обработки (фильтрация субсделок)
  data %<>%
    # выделение нужных строк
    {
      temp.ind <- which(.$Ticker == ticker.name) 
      result <- data[temp.ind, ]
      return(result)
    } %>%
    # фильтрация субсделок
    {
      temp.ind <- which(.$PositionNum%%1 == 0)
      result <- data[temp.ind, ]
      return(result)
    }   
  ### Всего сделок
  trades.num <- last(data$PositionNum)
  ### разбор статистики
  # индексы прибыльных/убыточных сделок
  goodTrade.index <- which(data$TradeReturn >= 0)
  badTrade.index <- which(data$TradeReturn < 0)
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
    ifelse(data$TradeReturn >= 0, 
           1,
          -1)
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
  goodTrade.sum <- sum(data$TradeReturn[goodTrade.index])
  ## всего слито (по сделкам)
  badTrade.sum <- sum(data$TradeReturn[badTrade.index])
  ## PF
  pf.trades <- goodTrade.sum / abs(badTrade.sum)
  ### Средний доход по сделкам
  averageGoodTradeReturn <- 
    data$TradeReturn[goodTrade.index] %>%
    mean(.)
  ### Средний доход по сделкам в %
  averageGoodTradeReturnPercent <- 
    data$TradeReturnPercent[goodTrade.index] %>%
    mean(.)
  ### Средний минус
  averageBadTradeReturn <- 
    data$TradeReturn[badTrade.index] %>%
    mean(.)
  ### Средний минус в %
  averageBadTradeReturnPercent <- 
    data$TradeReturnPercent[badTrade.index] %>%
    mean(.)
  ### Среднее баров на сделку
  averageTradeBars <- 
    mean(data$BarsHeld) %>%
    trunc(.)
  ### Среднее баров на прибыльную сделку
  averageGoodTradeBars <- 
    mean(data$BarsHeld[goodTrade.index]) %>%
    trunc(.)
  ### Среднее баров на убыточную сделку
  averageBadTradeBars <- 
    mean(data$BarsHeld[badTrade.index]) %>%
    trunc(.) %>%
    {
      ifelse(. == 0, 1, .)
    }
  ### Средний П/У на сделку
  averageTradeReturn <- mean(data$TradeReturn)
  ### Средний П/У на сделку в %
  averageTradeReturnPercent <- mean(data$TradeReturnPercent)
  # 
  ### Формирование итоговой таблицы
  result <- data.frame(
                       TradesNum = trades.num,            
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

