# подгрузка пакетов
library(quantmod)
library(rusquant)
library(magrittr)
library(PerformanceAnalytics)
# library(RQuantLib)
# library(plyr)
# library(dplyr)
# library(data.table)
#
### начальные параметры
source("str/libGeneric.R")
source("str/libStrategy.R")
source("str/simple/simple_str_gear.R")
source("str/simple/simple_str_eva.R")
### входные параметры
# temp.dir <- "data/temp"
from.date <- Sys.Date() - 300
to.date <- Sys.Date()
period <- "15min"
tickers <- c("SPFB.Si", "SPFB.RTS", "SPFB.BR")
im.dir <- "data/im"
ret.type <- "ret"
sma.per <- 9
add.per <- 10
basket.weights <- c(1,1,1) # количество инструментов в портфеле
balance.start <- 10000000
k.mm <- 0.02  # mm на заход в сделку
sleeps <- c(6, 20, 0.06) # в пунктах
commissions <- c(2, 2, 2)  # в рублях
#
### загрузка данных
data.source.list <- 
  {
    cat("Start Loading Data... ", "\n")
    GetData_Ticker_Set(tickers, from.date, to.date, period, dir = "data/temp", maxattempts = 5)
  } %>%
  {
    cat("Start Merging Data... ", "\n")
    MergeData_inList_byCol(.)  
  }
#
### нормализация данных
cat("Start Normalization&Improve Data... ", "\n")
data.source.list[[1]] <- 
  # удаление NA (по свечам)
  NormData_NA_inXTS(data = data.source.list[[1]], type = "full") %>%
  # добавляем ГО и данные по USDRUB
  AddData_FuturesSpecs_inXTS(data = ., from.date, to.date, dir = im.dir) %>%
  # вычисляем return'ы (в пунктах)
  CalcReturn_inXTS(data = ., price = "Open", type = ret.type)
#
### расчёт суммарных показателей портфеля 
# расчёт суммарного ГО (согласно весам инструмента в портфеле)
data.source.list[[1]]$IM <- CalcSum_Basket_TargetPar_inXTS(data = data.source.list[[1]], 
                                                               target = "IM", basket.weights)
# расчёт суммарного return'a 
# перевод return'ов в валюту
data.source.list[[1]]$SPFB.SI.cret <- data.source.list[[1]]$SPFB.SI.ret 
data.source.list[[1]] <- NormData_Price_inXTS(data = data.source.list[[1]], 
                                              norm.data = data.source.list[[1]]$USDRUB, 
                                              names = c("SPFB.RTS.ret", "SPFB.BR.ret"), 
                                              outnames = c("SPFB.RTS.cret", "SPFB.BR.cret"), 
                                              tick.val = c(10, 0.01), tick.price = c(0.02, 0.01), 
                                              convert.to = "RUB")
# суммирование
data.source.list[[1]]$cret <- CalcSum_Basket_TargetPar_inXTS(data = data.source.list[[1]], 
                                                                 target = "cret", basket.weights)
#
### отработка тестового робота
data.strategy.list <- TestStrategy_gear(data.source = data.source.list[[1]],
                                        sma.per, add.per, k.mm, balance.start, 
                                        basket.weights, sleeps, commissions)
### формирование таблицы сделок
data.strategy.list[[2]] <- CleanStatesTable(data = data.strategy.list[[2]])
dealsTable <- CalcDealsTable_DF(data = data.strategy.list[[2]])
# очистка мусора по target = "temp"
CleanGarbage(target = "temp", env = ".GlobalEnv")
#
### оценка perfomance-параметров
perfomanceTable <- CalcPerfomanceTable(data = data.strategy.list[[1]], 
                                       data.state = data.strategy.list[[2]],
                                       balance = balance.start, 
                                       ret.type = ret.type)
#
