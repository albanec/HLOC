# начальные параметры
source("str/libGeneric.R")
source("str/libStrategy.R")
#source("str/simple_str_gear.R")
# входные параметры
wd <- "data/temp/"
from.date <- Sys.Date() - 300
to.date <- Sys.Date()
period <- "15min"
tickers <- c("SPFB.Si", "SPFB.RTS", "SPFB.BR")
im.wd <- "data/im"
ret.type <- "ret"
sma.per <- 9
add.per <- 10
balance <- 100000
basket.weights <- c(1,1,1) # количество инструментов в портфеле
balance.initial <- 10000000
k.mm <- 0.02    # mm на заход в сделку
sleeps <- c(6, 20, 0.06) # в пунктах
comissions <- c(2, 2, 2)    # в рублях
#
#### загрузка и нормализация данных
cat("Start Loading Data... ", "\n")
data.source.list <- GetData_Ticker_Set(tickers, from.date, to.date, period, wd)
cat("Start Merging Data... ", "\n")
data.source.list <- MergeData_inList_byCol(data.source.list)
cat("Start Normalization&Improve Data... ", "\n")
# удаление NA (по свечам)
data.source.list[[1]] <- NormData_NA_inXTS(data = data.source.list[[1]], type = "full")	
# добавляем ГО и данные по USDRUB
data.source.list[[1]] <- AddData_FuturesSpecs_inXTS(data = data.source.list[[1]], from.date, to.date, im.wd)
# вычисляем return'ы (в пунктах)
data.source.list[[1]] <- STR_CalcReturn_inXTS(data = data.source.list[[1]], type = ret.type)
#
#### расчёт суммарных показателей портфеля
# расчёт суммарного ГО (согласно весам инструмента в портфеле)
data.source.list[[1]] <- STR_CalcSum_Basket_TargetPar_inXTS(data = data.source.list[[1]], 
                                                            target = "IM", basket.weights)
# расчёт суммарного return'a 
# перевод return'ов в валюту
data.source.list[[1]]$SPFB.SI.cret <- data.source.list[[1]]$SPFB.SI.ret 
data.source.list[[1]] <- STR_NormData_Price_inXTS(data = data.source.list[[1]], 
                                                  names = c("SPFB.RTS.ret", "SPFB.BR.ret"), 
                                                  outnames = c("SPFB.RTS.cret", "SPFB.BR.cret"), 
                                                  tick.val = c(10, 0.01), tick.price = c(0.02, 0.01), 
                                                  convert.to = "RUB")
# суммирование
data.source.list[[1]] <- STR_CalcSum_Basket_TargetPar_inXTS(data = data.source.list[[1]], 
                                                            target = "cret", basket.weights)
# расёт суммарной комиссии 
basket.comiss <- sum(basket.weights * comissions)
##
# работа стратегии


