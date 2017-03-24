# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Функции расчета дневных метрик доходности:
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
###
#' Расчёт итоговой таблицы с данными по доходностям дней 
#'
#' Функция вычисляет параметры по доходностям дней торговли и формирует итоговый DF
#' 
#' @param asset Анализируемые данные баланса (balance + im.balance или eqiuty)
#'
#' @return profit_table_byDays DF с данными по profit'у (по дням)
#'
#' @export
ProfitTable.byDays <- function(asset) {
    # разбор дней
    ### статистика по дням
    trdays_stats.list <- TradingDaysStats(data = asset)
    ### разбор статистики
    # индексы лудших/худших дней
    best_day.index <- which.max(trdays_stats.list[[1]]$Return)
    worst_day.index <- which.min(trdays_stats.list[[1]]$Return)
    good_day.index <- which(trdays_stats.list[[1]]$Return >= 0)
    bad_day.index <- which(trdays_stats.list[[1]]$Return < 0)
    ## средний день в плюс
    meanGoodDay_return <- 
        trdays_stats.list[[1]]$Return[good_day.index] %>%
        mean(.)
    meanGoodDay_return_percent <- 
        trdays_stats.list[[1]]$Return.percent[good_day.index] %>%
        mean(.)
    ## средний день в минус
    meanBadDay_return <- 
        trdays_stats.list[[1]]$Return[bad_day.index] %>%
        mean(.)
    meanBadDay_return_percent <- 
        trdays_stats.list[[1]]$Return.percent[bad_day.index] %>%
        mean(.)
    ## всего дней в плюс
    numGoogDay <- 
        good_day.index %>%
        length(.)
    ## всего дней в минус
    numBadDay <- 
        bad_day.index %>%
        length(.)    
    ## max дней в плюс
    maxGoodDays <-
        which(trdays_stats.list[[2]]$DayType == 1) %>%
        {
            max(trdays_stats.list[[2]]$SeriesLength[.])
        }
    ## max дней в минус
    maxBadDays <-
        which(trdays_stats.list[[2]]$DayType == -1) %>%
        {
            max(trdays_stats.list[[2]]$SeriesLength[.])
        }    
    ## profit factor
    goodDay_sum <- sum(trdays_stats.list[[1]]$Return[good_day.index])
    badDay_sum <- sum(trdays_stats.list[[1]]$Return[bad_day.index])
    pf_daily <- goodDay_sum / abs(badDay_sum)
    ### Формирование итоговой таблицы
    profit_table_byDays <- data.frame(
        # прибыльные дни
        DaysBest = trdays_stats.list[[1]]$Date[best_day.index],
        DaysBestProfit = trdays_stats.list[[1]]$Return[best_day.index],
        DaysBestProfitPercent = trdays_stats.list[[1]]$Return.percent[best_day.index],
        DaysWinNum = numGoogDay,
        DaysWinMax = maxGoodDays,
        DaysWinRate = xts::last(trdays_stats.list[[1]]$Num) %>%
            {
            numGoogDay * 100 / .
            },
        DaysWinAverageProfit = meanGoodDay_return,
        DaysWinAverageProfitPercent = meanGoodDay_return_percent,
        DaysGrossProfit = goodDay_sum,
        # убыточные дни
        DaysWorst = trdays_stats.list[[1]]$Date[worst_day.index], 
        DaysWorstLoss = trdays_stats.list[[1]]$Return[worst_day.index], 
        DaysWorstLossPercent = trdays_stats.list[[1]]$Return.percent[worst_day.index],
        DaysLossNum = numBadDay,
        DaysLossMax = maxBadDays,
        DaysLossRate = xts::last(trdays_stats.list[[1]]$Num) %>%
            {
                numBadDay * 100 / .
            },
        DaysLossAverageLoss = meanBadDay_return,
        DaysLossAverageLossPercent = meanBadDay_return_percent,
        DaysGrossLoss = badDay_sum,
        # профит-фактор по дням
        DaysProfitFactor = pf_daily,
        #
        row.names = NULL
    )                    
    # 
    return(profit_table_byDays)
}
#
###
#' Вычисление данных по торговым дням
#'
#' Функция вычисляет статистику по дням (trdayStats) и по сериям убытка/профита (trdaySeries)
#' 
#' @param data Анализируемые данные баланса (balance + im.balance или eqiuty)
#'
#' @return result Лист со статистикой по дням (внутри trdayStats и trdaySeries)
#'
#' @export
TradingDaysStats <- function(data) {
    #
    data %<>%
        # очистка от строк c одинаковым индексом (если есть)
        {
            duplicated.ind <- 
                index.xts(.) %>%
                duplicated(.) %>%
                which(.)
            if (length(duplicated.ind) != 0) {
                result <-
                    {
                        .[-duplicated.ind]
                    }    
            } else {
                result <- .
            }
            return(result)
        }
    # разметка номеров дней
    data$trday.num <- 
        {
            data$endpoint[endpoints(data, on = 'days')] <- 1
            return(data)
        } %>%
        {
            data$endpoint[!is.na(data$endpoint)]
        } %>%
        cumsum(.) 
    data$trday.num <- na.locf(data$trday.num, fromLast = TRUE) 
    ### выборка нужных столбцов
    #data <- data[, c('balance', 'trday.num')]
    ### раcчёт статистики по дням 
    trdayStats <-
        xts::last(data$trday.num) %>%
        1:. %>%
        lapply(.,
            function(x) {
                TradingDaysStats.one_day_summary(data[data$trday.num == x])
            }) %>%
        # объединение данных внутри листа в один df
        MergeData_inList.byRow(.) 

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
    #
    return(list(trdayStats = trdayStats, trdaySeries = trdaySeries))
}
#
###
#' Вычисление данных по одному торговому дню
#'
#' Функция вычисляет 
#' 
#' @param data Анализируемые данные баланса (balance + im.balance или eqiuty)
#'
#' @return result DF с данными 
#'
#' @export
TradingDaysStats.one_day_summary <- function(data) {
    #
    if (!is.data.frame(data)) {
        data <- Convert.XTStoDF(data)     
    }
    df <- data.frame(
        Date = character(1) %>% 
            as.numeric(.) %>% 
            as.Date(.),
        Num = as.numeric(1),
        Return = as.numeric(1),
        Return.percent = as.numeric(1)
    )
    df$Date <- 
        xts::first(data$INDEX) %>%
        format(., '%Y-%m-%d') %>%
        as.POSIXct(., origin = '1970-01-01') 
    df$Num <- data$trday.num[1]
    df$Return <- xts::last(data[, 2]) - xts::first(data[, 2]) 
    df$Return.percent <- 
        xts::first(data[, 2]) %>%
        {
            df$Return * 100 / .
        }
    #
    return(df)
}
#