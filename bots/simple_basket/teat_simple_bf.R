# Загрузка библиотек
source('bots/simple/linker.R')
#
### Входные параметры
# temp.dir <- 'data/temp'
from.date <- Sys.Date() - 300
to.date <- Sys.Date()
period <- '15min'
tickers <- c('SPFB.Si', 'SPFB.RTS', 'SPFB.BR')
im.dir <- 'data/im'
ret_type <- 'ret'
sma_per <- 9
add_per <- 10
basket_weights <- c(1,1,1) # количество инструментов в портфеле
balance_start <- 1000000
k_mm <- 0.02  # mm на заход в сделку
slips <- c(6, 20, 0.06) # в пунктах
commissions <- c(2, 2, 2)  # в рублях
#
### загрузка данных
ohlc_data.list <- 
  {
    cat('Start Loading Data... ', '\n')
    GetData.Tickers(tickers, from.date, to.date, period, dir = 'data/temp', maxattempts = 5)
  } %>%
  {
    cat('Start Merging Data... ', '\n')
    MergeData_inList.byCol(.)  
  }
#
### нормализация данных
cat('Start Normalization&Improve Data... ', '\n')
ohlc_data.list[[1]] <- 
  # удаление NA (по свечам)
  NormData_inXTS.na(data = ohlc_data.list[[1]], type = 'full') %>%
  # добавляем ГО и данные по USDRUB
  AddData_inXTS.futuresSpecs(data = ., from.date, to.date, dir = im.dir) %>%
  # вычисляем return'ы (в пунктах)
  CalcReturn_inXTS(data = ., price = 'Open', type = ret_type)
#
### расчёт суммарных показателей портфеля 
# расчёт суммарного ГО (согласно весам инструмента в портфеле)
ohlc_data.list[[1]]$IM <- CalcSum_inXTS_byTargetCol.basket(data = ohlc_data.list[[1]], 
                                                               target = 'IM', basket_weights)
# расчёт суммарного return'a 
# перевод return'ов в валюту
ohlc_data.list[[1]]$SPFB.SI.cret <- ohlc_data.list[[1]]$SPFB.SI.ret 
ohlc_data.list[[1]] <- NormData_inXTS.price(data = ohlc_data.list[[1]], 
                                              norm.data = ohlc_data.list[[1]]$USDRUB, 
                                              names = c('SPFB.RTS.ret', 'SPFB.BR.ret'), 
                                              outnames = c('SPFB.RTS.cret', 'SPFB.BR.cret'), 
                                              tick.val = c(10, 0.01), tick.price = c(0.02, 0.01), 
                                              convert.to = 'RUB')
# суммирование
ohlc_data.list[[1]]$cret <- CalcSum_inXTS_byTargetCol.basket(data = ohlc_data.list[[1]], 
                                                                 target = 'cret', basket_weights)
#
# ## BruteForce оптимизация 
# system.time(
#   {
#     perfomanceTable.one <- SimpleStr_BruteForceOpt(var.begin = 1, var.end = 100,
#                                                    ohlc_data = ohlc_data.list[[1]], 
#                                                    add_per, k_mm, balance_start, 
#                                                    basket_weights, slips, commissions, ret_type)
#   }
# )
#
### Parallel BruteForce оптимизация 
system.time(
  {
    perfomanceTable <- SimpleStr_BruteForceOpt.Parallel(
      var.begin = 1, var.end = 100,
      ohlc_data = ohlc_data.list[[1]], 
      add_per, k_mm, balance_start, 
      basket_weights, slips, commissions, ret_type
    )
  }
)
#
perfomanceTable <- MergeData_inList.byRow(perfomanceTable)
#