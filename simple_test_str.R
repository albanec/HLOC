setwd("/home/evgeni/Templates/temp/HLOC_simple_str/")
source("/home/evgeni/Templates/R.libs/libGeneric.R")
# входные параметры
period <- "15min"
from.date <- Sys.Date()-30
to.date <- Sys.Date()
tickers <- c("SPFB.Si", "SPFB.RTS", "SPFB.BR")
# загрузка и нормализация данных
cat("Start Loading Data... ", "\n")
data.source.list <- GetData_Ticker_SetList(tickers, from.date, to.date, period)
cat("Start Merging Data... ", "\n")
data.source.list <- MergeData_fromAll_toOne(data.source.list)
cat("Start Normalization&Improve Data... ", "\n")
# удаление NA (по свечам)
data.source.list[[1]] <- NormData_NA(data=data.source.list[[1]], type="full")	
# добавляем ГО и данные по USDRUB
data.source.list[[1]] <- AddData_FuturesSpecs_forXTS(data=data.source.list[[1]], from.date, to.date)
##
# работа стратегии