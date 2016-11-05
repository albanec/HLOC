# Загрузка библиотек
source("main/test/linker.R")
#
### входные параметры
# temp.dir <- "data/temp"
from.date <- '2016-02-01'
to.date <- '2016-02-28'
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
#
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

### один прогон вычислений 
## отработка тестового робота
data.strategy.list <- TestStr.gear(data.source = data.source.list[[1]],
                                   sma.per, add.per, k.mm, balance.start, 
                                   basket.weights, slips, commissions)
## формирование таблицы сделок
# чистим от лишних записей
data.strategy.list[[2]] <- StatesTable.clean(data = data.strategy.list[[2]])
# лист с данными по сделкам (по тикерам и за всю корзину)
dealsTable.list <- DealsTables.calc(data = data.strategy.list[[2]], basket = FALSE, convert = FALSE)#TRUE
# очистка мусора по target = "temp"
CleanGarbage(target = "temp", env = ".GlobalEnv")
gc()
## оценка perfomance-параметров
perfomanceTable <- 
  PerfomanceTable(data = data.strategy.list[[1]], 
                  data.state = data.strategy.list[[2]],
                  dealsTable = dealsTable.list,
                  balance = balance.start, 
                  ret.type = ret.type) %>%
  # добавление использованных параметров
  cbind.data.frame(., sma.per_ = sma.per, add.per_ = add.per, k.mm_ = k.mm)
## запись в файл 
if (firstTime == TRUE) {
  write.table(perfomanceTable, file = perfomanceDB.filename, sep = ",", col.names = TRUE )  
  firstTime <- FALSE
} else {
  write.table(perfomanceTable, file = perfomanceDB.filename, sep = ",", col.names = FALSE, append = TRUE )  
}
#