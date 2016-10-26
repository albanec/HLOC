# Загрузка библиотек
source("main/simple/linker.R")
#
### начальные параметры
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
balance.start <- 1000000
k.mm <- 0.02  # mm на заход в сделку
slips <- c(6, 20, 0.06) # в пунктах
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
  AddData_inXTS.futuresSpecs(data = ., from.date, to.date, dir = im.dir) %>%
  # вычисляем return'ы (в пунктах)
  CalcReturn_inXTS(data = ., price = "Open", type = ret.type)
#
### расчёт суммарных показателей портфеля 
# расчёт суммарного ГО (согласно весам инструмента в портфеле)
data.source.list[[1]]$IM <- CalcSum_inXTS_byTargetCol.basket(data = data.source.list[[1]], 
                                                               target = "IM", basket.weights)
# расчёт суммарного return'a 
# перевод return'ов в валюту
data.source.list[[1]]$SPFB.SI.cret <- data.source.list[[1]]$SPFB.SI.ret 
data.source.list[[1]] <- NormData_inXTS.price(data = data.source.list[[1]], 
                                              norm.data = data.source.list[[1]]$USDRUB, 
                                              names = c("SPFB.RTS.ret", "SPFB.BR.ret"), 
                                              outnames = c("SPFB.RTS.cret", "SPFB.BR.cret"), 
                                              tick.val = c(10, 0.01), tick.price = c(0.02, 0.01), 
                                              convert.to = "RUB")
# суммирование
data.source.list[[1]]$cret <- CalcSum_inXTS_byTargetCol.basket(data = data.source.list[[1]], 
                                                                 target = "cret", basket.weights)
#
