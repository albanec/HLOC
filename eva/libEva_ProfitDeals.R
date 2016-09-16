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
ProfitList_byDeals <- function(data, ...) {
  # подготовка данных для обработки (фильтрация субсделок)
  names.set <- unique(data$Ticker)
  ### Расчёт
  result.list <- lapply(names.set, 
                        function(x){
                          ProfitTable_byDeals_oneTicker(data = data, ticker.name = x)
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
ProfitTable_byDeals_oneTicker <- function(data, ticker.name, ...) {
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
  deals.num <- last(data$PositionNum)
  ### разбор статистики
  # индексы прибыльных/убыточных сделок
  goodDeal.index <- which(data$DealReturn >= 0)
  badDeal.index <- which(data$DealReturn < 0)
  ### Всего сделок в плюс
  numGoogDeals <- 
    goodDeal.index %>%
    length(.)
  ### Всего сделок в минус
  numBadDeals <- 
    badDeal.index %>%
    length(.)
  ### Расчёт последовательностей сделок
  # подготовка данных для анализа
  dealsSeries <- 
  {
    ifelse(data$DealReturn >= 0, 
           1,
          -1)
  } %>%
  rle(.) %>%
  {
    data.frame(DayType = .[[2]], SeriesLength = .[[1]])
  }
  ### Max сделок в плюс
  maxGoodDeals <-
    which(dealsSeries$DayType == 1) %>%
    {
      max(dealsSeries$SeriesLength[.])
    }
  ### Max сделок в минус
  maxBadDeals <-
    which(dealsSeries$DayType == -1) %>%
    {
      max(dealsSeries$SeriesLength[.])
    }  
  ### Профит-фактор
  ## всего заработано (по сделкам)
  goodDeal.sum <- sum(data$DealReturn[goodDeal.index])
  ## всего слито (по сделкам)
  badDeal.sum <- sum(data$DealReturn[badDeal.index])
  ## PF
  pf.deals <- goodDeal.sum / abs(badDeal.sum)
  ### Средний доход по сделкам
  meanGoodDealReturn <- 
    data$DealReturn[goodDeal.index] %>%
    mean(.)
  ### Средний доход по сделкам в %
  meanGoodDealReturnPercent <- 
    data$DealReturnPercent[goodDeal.index] %>%
    mean(.)
  ### Средний минус
  meanBadDealReturn <- 
    data$DealReturn[badDeal.index] %>%
    mean(.)
  ### Средний минус в %
  meanBadDealReturnPercent <- 
    data$DealReturnPercent[badDeal.index] %>%
    mean(.)
  ### Среднее баров на сделку
  meanDealBars <- 
    mean(data$PositionBars) %>%
    trunc(.)
  ### Среднее баров на прибыльную сделку
  meanGoodDealBars <- 
    mean(data$PositionBars[goodDeal.index]) %>%
    trunc(.)
  ### Среднее баров на убыточную сделку
  meanBadDealBars <- 
    mean(data$PositionBars[badDeal.index]) %>%
    trunc(.) %>%
    {
      ifelse(. == 0, 1, .)
    }
  ### Средний П/У на сделку
  meanDealReturn <- mean(data$DealReturn)
  ### Средний П/У на сделку в %
  meanDealReturnPercent <- mean(data$DealReturnPercent)
  # 
  ### Формирование итоговой таблицы
  result <- data.frame(
                       DealsNum = deals.num,            
                       NumGoogDeals = numGoogDeals,
                       NumBadDeals = numBadDeals,
                       MaxGoodDeals = maxGoodDeals,
                       MaxBadDeals = maxBadDeals,
                       FullGoodDealReturn = goodDeal.sum,
                       FullBadDealReturn = badDeal.sum,
                       MeanGoodDealReturn = meanGoodDealReturn,
                       MeanGoodDealReturnPercent = meanGoodDealReturnPercent,
                       MeanBadDealReturn = meanBadDealReturn,
                       MeanBadDealReturnPercent = meanBadDealReturnPercent,
                       MeanDealBars = meanDealBars,
                       MeanGoodDealBars = meanGoodDealBars,
                       MeanBadDealBars = meanBadDealBars,
                       MeanDealReturn = meanDealReturn,
                       MeanDealReturnPercent = meanDealReturn,
                       ProfitFactorDeals = pf.deals,
                       #
                       row.names = NULL)       
  #
  return(result)
}
#

