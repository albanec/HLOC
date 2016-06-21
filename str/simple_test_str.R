source("/HLOC/str/libGeneric.R")
source("/HLOC/str/libStrategy.R")
# входные параметры
wd <- "/HLOC/temp/"
from.date <- Sys.Date()-300
to.date <- Sys.Date()
period <- "15min"
tickers <- c("SPFB.Si", "SPFB.RTS", "SPFB.BR")
im.wd <- "/home/evgeni/R.projects/EVA test/"
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
setwd(wd)
# загрузка и нормализация данных
cat("Start Loading Data... ", "\n")
data.source.list <- GetData_Ticker_Set(tickers, from.date, to.date, period)
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
# расч1т суммарных показателей портфеля
    # расчёт суммарного ГО (согласно весам инструмента в портфеле)
data.source.list[[1]] <- STR_CalcPortfolio_Basket_IM_inXTS(data = data.source.list[[1]], basket.weights)
    # расёт суммарного return'a 
data.source.list[[1]] <- STR_CalcReturn_Basket_Ret_inXTS(data = data.source.list[[1]], basket.weights)
    # расёт суммарной комиссии 
basket.comiss <- STR_CalcPortfolio_Basket_Comiss_Simple(basket.weights, comissions)

##
# работа стратегии
# конвертируем return'ы (в рублях и там, где нужно)
data.source.list[[1]] <- STR_NormData_Price_inXTS(data = data.source.list[[1]], 
                                                  names = c("SPFB.BR.ret"), convert.to = "RUB")