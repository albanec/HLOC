# Загрузка библиотек
source("main/test/linker.R")
#
### входные параметры
# temp.dir <- "data/temp"
from.date <- "2016-03-01"
to.date <- "2016-03-31"
period <- "15min"
tickers <- c("SPFB.Si")
im.dir <- "data/im"
ret.type <- "ret"
sma.per <- 100
add.per <- 10
basket.weights <- c(1,0,0) # количество инструментов в портфеле
balance.start <- 1000000
k.mm <- 0.02  # mm на заход в сделку
slips <- c(0, 0, 0) # в пунктах
commissions <- c(10, 0, 0)  # в рублях
per_DCI <- 10 
per_slowSMA <- 20 
per_fastSMA <- 5 
## подготовка исходных данных
# загрузка данных из .csv Финама
data.source <- Read_CSV.toXTS.FinamQuotes(filename = "data/temp/si_data.csv")
# выделение нужного периода
data.source <- 
  paste(from.date,'::',to.date, sep = "") %>%
  data.source[.]
# переход к нужному периоду свечей
data.source <- ExpandData.toPeriod(x = data.source, per = "15min")
data.source.list <- list(data.source)
colnames(data.source.list[[1]]) <- c("SPFB.SI.Open", "SPFB.SI.High", "SPFB.SI.Low","SPFB.SI.Close", "SPFB.SI.Volume")
#
data.source.list[[1]] <- 
  # удаление NA (по свечам)
  NormData_inXTS.na(data = data.source.list[[1]], type = "full") %>%
  # добавляем ГО и данные по USDRUB
  AddData_inXTS.futuresSpecs(data = ., from.date, to.date, dir = im.dir) %>%
  # вычисляем return'ы (в пунктах)
  CalcReturn_inXTS(data = ., price = "Open", type = ret.type)
# суммарное ГО по корзине 
data.source.list[[1]]$IM <- CalcSum_inXTS_byTargetCol.basket(data = data.source.list[[1]], 
                                                           target = "IM", basket.weights)
## расчёт суммарного return'a 
# перевод return'ов в валюту
data.source.list[[1]]$SPFB.SI.cret <- data.source.list[[1]]$SPFB.SI.ret 
data.source.list[[1]]$cret <- data.source.list[[1]]$SPFB.SI.cret 
#
### выгрузка дат экспирации
  expiration.dates <- Read_CSV.toDF(file.path = expiration, sep = ",")
   colnames(expiration.dates) <- expiration.dates[1, ]
   expiration.dates <- 
    expiration.dates[-1, ] %>%
    as.vector(.) %>%
    ymd(x = ., tz = 'MSK')
#
