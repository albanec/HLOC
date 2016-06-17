source("/home/evgeni/Templates/R.libs/libGeneric.R")
source("/home/evgeni/Templates/R.libs/libStrategy.R")
# входные параметры
wd <- "/home/evgeni/Templates/temp/HLOC_simple_str/"
from.date <- Sys.Date()-30
to.date <- Sys.Date()
period <- "15min"
tickers <- c("SPFB.Si", "SPFB.RTS", "SPFB.BR")
imfiles.wd <- ""
ret.type <- "ret"
sma.per <- 9
balance <- 100000
#
setwd(wd)
# загрузка и нормализация данных
cat("Start Loading Data... ", "\n")
data.source.list <- GetData_Ticker_Set(tickers, from.date, to.date, period)
cat("Start Merging Data... ", "\n")
data.source.list <- MergeData_inList_byCol(data.source.list)
cat("Start Normalization&Improve Data... ", "\n")
# удаление NA (по свечам)
data.source.list[[1]] <- NormData_NA_forXTS(data = data.source.list[[1]], type="full")	
# добавляем ГО и данные по USDRUB
data.source.list[[1]] <- AddData_FuturesSpecs_forXTS(data = data.source.list[[1]], from.date, to.date, im.wd)
# вычисляем return'ы
data.source.list[[1]] <- STR_CalcReturn_inXTS(data=data.source.list[[1]], type = ret.type)
##
# работа стратегии
