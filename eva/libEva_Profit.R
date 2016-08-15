# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Функции расчета метрик доходности:
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
###
#' Расчёт итоговой таблицы с данными по доходностям
#'
#' Функция вычисляет параметры по доходностям (выводит итоговые данные)
#' 
#' @param data.state Полные данные equity (после  отработки стратегии)
#' @param deals.data Таблица сделок

#'
#' @return profitTable DF с данными по просадкам
#'
#' @export
ProfitTable <- function(data, balance, ...) {
  ### расчёт итоговой доходности 
  fullReturn <- 
    last(data$equity) %>%
    as.numeric(.)
  fullSR <- fullReturn * 100 / balance    
  ### доходность в годовых
  # число месяцев торговли
  n.mouth <- 
    index(data) %>%
    ndays(.) / 30
  fullReturn.annual <- fullReturn * 12 / n.mouth    
  ### разбор дней
  # статистика по дням
  trdaysStatsList <- CalcTradingDaysStats(data = data)
  ## разбор статистики
  # индексы лудших/худших дней
  bestDay.index  <- which.max(trdaysStatsList[[1]]$Return)
  worstDay.index  <- which.min(trdaysStatsList[[1]]$Return)
  goodDay.index <- which(trdaysStatsList[[1]]$Return >= 0)
  badDay.index <- which(trdaysStatsList[[1]]$Return < 0)
  # средний день в плюс
  meanGoodDayReturn <- 
    trdaysStatsList[[1]]$Return[goodDay.index] %>%
    mean(.)
  meanGoodDaySR <- 
    trdaysStatsList[[1]]$SR[goodDay.index] %>%
    mean(.)
  # средний день в минус
  meanBadDayReturn <- 
    trdaysStatsList[[1]]$Return[badDay.index] %>%
    mean(.)
  meanBadDaySR <- 
    trdaysStatsList[[1]]$SR[badDay.index] %>%
    mean(.)
  # всего дней в плюс
  numGoogDay <- 
    goodDay.index %>%
    length(.)
  # всего дней в минус
  numBadDay <- 
    badDay.index %>%
    length(.)  
  # max дней в плюс
  maxGoodDays <-
    which(trdaysStatsList[[2]]$DayType == 1) %>%
    {
      max(trdaysStatsList[[2]]$SeriesLength[.])
    }
  # max дней в минус
  maxBadDays <-
    which(trdaysStatsList[[2]]$DayType == -1) %>%
    {
      max(trdaysStatsList[[2]]$SeriesLength[.])
    }  
  # profit factor
  goodDay.sum <- sum(trdaysStatsList[[1]]$Return[goodDay.index])
  badDay.sum <- sum(trdaysStatsList[[1]]$Return[badDay.index])
  pf <- goodDay.sum / abs(badDay.sum)
  remove(goodDay.sum); remove(badDay.sum)
  #
  profitTable <- data.frame(Return = fullReturn,
                            ReturnAnnual = fullReturn.annual,
                            SR = fullSR,
                            BestDay = trdaysStatsList[[1]]$Date[bestDay.index], 
                            BestReturn = trdaysStatsList[[1]]$Return[bestDay.index], 
                            BestSR = trdaysStatsList[[1]]$SR[bestDay.index],
                            WorstDay = trdaysStatsList[[1]]$Date[worstDay.index], 
                            WorstReturn = trdaysStatsList[[1]]$Return[worstDay.index], 
                            WorstSR = trdaysStatsList[[1]]$SR[worstDay.index],
                            MeanGoodDayReturn = meanGoodDayReturn,
                            MeanGoodDaySR = meanGoodDaySR,
                            MeanBadDayReturn = meanBadDayReturn,
                            MeanBadDaySR = meanBadDaySR,
                            NumGoogDay = numGoogDay,
                            NumBadDay = numBadDay,
                            MaxGoodDays = maxGoodDays,
                            MaxBadDays = maxBadDays,
                            ProfitFactor = pf,
                            row.names = NULL)
  # 
  return(profitTable)
}
#
###
#' Вычисление данных по торговым дням
#'
#' Функция вычисляет статистику по дням (trdayStats) и по сериям убытка/профита (trdaySeries)
#' 
#' @param data Полные данные отработки стратегии)
#'
#' @return result Лист со статистикой по дням (внутри trdayStats и trdaySeries)
#'
#' @export
CalcTradingDaysStats <- function(data) {
  #
  data %<>%
    # очистка от строк c одинаковым индексом (если есть)
    {
      duplicated.ind <- 
        index(.) %>%
        duplicated(.) %>%
        which(.)
      result <-
      {
        .[-duplicated.ind]
      }
      return(result)
    }
  # разметка номеров дней
  data$trday.num <- 
    {
      data$endpoint[endpoints(data, on = "days")] <- 1
      return(data)
    } %>%
    {
      data$endpoint[!is.na(data$endpoint)]
    } %>%
    cumsum(.) 
  data$trday.num <- na.locf(data$trday.num, fromLast = TRUE) 
  ### раcчёт статистики по дням 
  trdayStats <-
    last(data$trday.num) %>%
    1:. %>%
    lapply(.,
           function(x) {
             OneTradingDaysStats_DF(data = data, n = x)
           }) %>%
    # объединение данных внутри листа в один df
    MergeData_inList_byRow(.) 
  trdaySeries <- 
    {
      ifelse(trdayStats$Return >= 0, 
             1,
            -1)
    } %>%
    rle(.) %>%
    {
      data.frame(DayType = .[[2]], SeriesLength = .[[1]])
    }
  result <- list(trdayStats = trdayStats, trdaySeries = trdaySeries)
  #
  return(result)
}
#
###
#' Вычисление данных по одному торговому дню
#'
#' Функция вычисляет 
#' 
#' @param data Полные данные отработки стратегии
#'
#' @return result DF с данными 
#'
#' @export
OneTradingDaysStats_DF <- function(data, n) {
  #
  ### выборка нужных столбцов
  data <- data[, c("balance", "trday.num")]
  ### расчёт
  result <- 
    # выгружаем данные по dd с номером n
    data[data$trday.num == n] %>%
    Convert_XTStoDF(.) %>%
    {
      df <-
        data.frame(Date = character(1) %>% 
                          as.numeric(.) %>% 
                          as.Date(.),
                   Num = as.numeric(1),
                   Return = as.numeric(1),
                   SR = as.numeric(1)
                   ) 
      df$Date <- 
        first(.$date) %>%
        format(., "%Y-%m-%d") %>%
        as.POSIXct(., origin = "1970-01-01") 
      df$Num <- n
        df$Return <- 
        {
          first(.$balance) - last(.$balance) 
        }
      df$SR <- 
        first(.$balance) %>%
        {
          df$Return * 100 / .
        }
      #
      return(df)
    }
  #
  return(result)
}