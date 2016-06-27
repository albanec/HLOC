# начальные параметры
source("str/libGeneric.R")
source("str/libStrategy.R")
#source("str/simple_str_gear.R")
# входные параметры
temp.dir <- "data/temp"
from.date <- Sys.Date() - 300
to.date <- Sys.Date()
period <- "15min"
tickers <- c("SPFB.Si", "SPFB.RTS", "SPFB.BR")
im.dir <- "data/im"
ret.type <- "ret"
sma.per <- 9
add.per <- 10
balance <- 100000
basket.weights <- c(1,1,1) # количество инструментов в портфеле
balance.initial <- 10000000
k.mm <- 0.02  # mm на заход в сделку
sleeps <- c(6, 20, 0.06) # в пунктах
commisions <- c(2, 2, 2)  # в рублях
#
#### загрузка и нормализация данных
data.source.list <- 
  {
    cat("Start Loading Data... ", "\n")
    GetData_Ticker_Set(tickers, from.date, to.date, period, dir = temp.dir, maxattempts = 5)
  } %>%
  {
    cat("Start Merging Data... ", "\n")
    MergeData_inList_byCol(.)  
  }
#
cat("Start Normalization&Improve Data... ", "\n")
data.source.list[[1]] <- 
  # удаление NA (по свечам)
  NormData_NA_inXTS(data = data.source.list[[1]], type = "full") %>%
  # добавляем ГО и данные по USDRUB
  AddData_FuturesSpecs_inXTS(data = ., from.date, to.date, dir = im.dir) %>%
  # вычисляем return'ы (в пунктах)
  STR_CalcReturn_inXTS(data = ., type = ret.type)
#
### расчёт суммарных показателей портфеля 
# расчёт суммарного ГО (согласно весам инструмента в портфеле)
data.source.list[[1]]$IM <- STR_CalcSum_Basket_TargetPar_inXTS(data = data.source.list[[1]], 
                                                               target = "IM", basket.weights)
#
# расчёт суммарного return'a 
# перевод return'ов в валюту
data.source.list[[1]]$SPFB.SI.cret <- data.source.list[[1]]$SPFB.SI.ret 
data.source.list[[1]] <- STR_NormData_Price_inXTS(data = data.source.list[[1]], 
                                                  names = c("SPFB.RTS.ret", "SPFB.BR.ret"), 
                                                  outnames = c("SPFB.RTS.cret", "SPFB.BR.cret"), 
                                                  tick.val = c(10, 0.01), tick.price = c(0.02, 0.01), 
                                                  convert.to = "RUB")
# суммирование
data.source.list[[1]]$cret <- STR_CalcSum_Basket_TargetPar_inXTS(data = data.source.list[[1]], 
                                                                 target = "cret", basket.weights)
# расёт суммарной комиссии 
basket.commis <- sum(basket.weights * commisions)
##
# работа стратегии


