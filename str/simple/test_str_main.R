# подгрузка пакетов
library(quantmod)
library(rusquant)
library(magrittr)
library(tidyr)
library(PerformanceAnalytics)
# library(RQuantLib)
# library(dplyr)
# library(data.table)
#
### начальные параметры
source("str/libStrategy.R")
source("str/simple/simple_str_gen.R")
source("str/simple/simple_str_eva.R")
#
# движок стратегии
source("str/simple/test_str_gear.R")
#
### входные параметры
# temp.dir <- "data/temp"
from.date <- "2016-02-01"
to.date <- "2016-02-02"
period <- "15min"
tickers <- c("SPFB.Si")
im.dir <- "data/im"
ret.type <- "ret"
sma.per <- 100
add.per <- 10
basket.weights <- c(1,0,0) # количество инструментов в портфеле
balance.start <- 1000000
k.mm <- 0.02  # mm на заход в сделку
sleeps <- c(0, 0, 0) # в пунктах
commissions <- c(10, 0, 0)  # в рублях
#
data.source <- Read_CSVtoXTS_FinamQuotes(filename = "data/temp/F_SI_08-28.07.16_1min.csv")
data.source <- data.source["2016-02-01::2016-02-28"]
data.source <- ExpandData_toPeriod(x = data.source, per = "15min")
data.source.list <- list(data.source)
colnames(data.source.list[[1]]) <- c("SPFB.SI.Open", "SPFB.SI.High", "SPFB.SI.Low","SPFB.SI.Close", "SPFB.SI.Volume")
#
data.source.list[[1]] <- 
  # удаление NA (по свечам)
  NormData_NA_inXTS(data = data.source.list[[1]], type = "full") %>%
  # добавляем ГО и данные по USDRUB
  AddData_FuturesSpecs_inXTS(data = ., from.date, to.date, dir = im.dir) %>%
  # вычисляем return'ы (в пунктах)
  CalcReturn_inXTS(data = ., price = "Open", type = ret.type)
data.source.list[[1]]$IM <- CalcSum_Basket_TargetPar_inXTS(data = data.source.list[[1]], 
                                                               target = "IM", basket.weights)
# расчёт суммарного return'a 
# перевод return'ов в валюту
data.source.list[[1]]$SPFB.SI.cret <- data.source.list[[1]]$SPFB.SI.ret 
data.source.list[[1]]$cret <- data.source.list[[1]]$SPFB.SI.cret 

### отработка тестового робота
data.strategy.list <- TestStrategy_gear(data.source = data.source.list[[1]],
                                        sma.per, add.per, k.mm, balance.start, 
                                        basket.weights, sleeps, commissions)
#
### формирование таблицы сделок
## чистим от лишних записей
data.strategy.list[[2]] <- CleanStatesTable(data = data.strategy.list[[2]])
## лист с данными по сделкам (по тикерам и за всю корзину)
dealsTable.list <- CalcDealsTables(data = data.strategy.list[[2]], convert = TRUE)
# очистка мусора по target = "temp"
CleanGarbage(target = "temp", env = ".GlobalEnv")
#
### оценка perfomance-параметров
perfomanceTable <- CalcPerfomanceTable(data = data.strategy.list[[1]], 
                                       data.state = data.strategy.list[[2]],
                                       dealsTable = dealsTable.list,
                                       balance = balance.start, 
                                       ret.type = ret.type)
#