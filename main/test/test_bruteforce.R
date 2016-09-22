# Загрузка библиотек
source("main/test/linker.R")
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
## подготовка исходных данных
# загрузка данных из .csv Финама
data.source <- Read_CSVtoXTS_FinamQuotes(filename = "data/temp/F_SI_08-28.07.16_1min.csv")
# выделение нужного периода
data.source <- data.source["2016-02-01::2016-02-28"]
# переход к нужному периоду свечей
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
# суммарное ГО по корзине 
data.source.list[[1]]$IM <- CalcSum_Basket_TargetPar_inXTS(data = data.source.list[[1]], 
                                                           target = "IM", basket.weights)
## расчёт суммарного return'a 
# перевод return'ов в валюту
data.source.list[[1]]$SPFB.SI.cret <- data.source.list[[1]]$SPFB.SI.ret 
data.source.list[[1]]$cret <- data.source.list[[1]]$SPFB.SI.cret 
#
#
### BruteForce оптимизация 
system.time(
  {
    perfamanceTable.one <- TestStr_BruteForceOpt(var.begin = 1, var.end = 100,
                                                   data.source = data.source.list[[1]], 
                                                   add.per, k.mm, balance.start, 
                                                   basket.weights, sleeps, commissions, ret.type)
  }
)
#
### Parallel BruteForce оптимизация 
system.time(
  {
    perfamanceTable.two <- TestStr_Parallel_BruteForceOpt(
      var.begin = 1, var.end = 100,
      data.source = data.source.list[[1]], 
      add.per, k.mm, balance.start, 
      basket.weights, sleeps, commissions, ret.type
    )
  }
)