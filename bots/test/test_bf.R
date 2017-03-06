# Загрузка библиотек
source('bots/test/linker.R')
#
### входные параметры
# temp.dir <- 'data/temp'
from.date <- '2016-02-01'
to.date <- '2016-02-28'
period <- '15min'
tickers <- c('SPFB.Si')
im.dir <- 'data/im'
ret_type <- 'ret'
sma_per <- 100
add_per <- 10
basket_weights <- c(1,0,0) # количество инструментов в портфеле
balance_start <- 1000000
k_mm <- 0.02  # mm на заход в сделку
slips <- c(0, 0, 0) # в пунктах
commissions <- c(10, 0, 0)  # в рублях
#
## подготовка исходных данных
# загрузка данных из .csv Финама
ohlc_data <- ReadOHLC.FinamCSV(filename = 'data/temp/si_data.csv')
# выделение нужного периода
ohlc_data <- 
  paste0(from.date,'::',to.date) %>%
  ohlc_data[.]
# переход к нужному периоду свечей
ohlc_data <- ExpandData.toPeriod(x = ohlc_data, per = '15min')
ohlc_data.list <- list(ohlc_data)
colnames(ohlc_data.list[[1]]) <- c('SPFB.SI.Open', 'SPFB.SI.High', 'SPFB.SI.Low','SPFB.SI.Close', 'SPFB.SI.Volume')
#
ohlc_data.list[[1]] <- 
  # удаление NA (по свечам)
  NormData_inXTS.na(data = ohlc_data.list[[1]], type = 'full') %>%
  # добавляем ГО и данные по USDRUB
  AddData_inXTS.futuresSpecs(data = ., from.date, to.date, dir = im.dir) %>%
  # вычисляем return'ы (в пунктах)
  CalcReturn_inXTS(data = ., price = 'Open', type = ret_type)
# суммарное ГО по корзине 
ohlc_data.list[[1]]$IM <- CalcSum_inXTS_byTargetCol.basket(data = ohlc_data.list[[1]], 
                                                           target = 'IM', basket_weights)
## расчёт суммарного return'a 
# перевод return'ов в валюту
ohlc_data.list[[1]]$SPFB.SI.cret <- ohlc_data.list[[1]]$SPFB.SI.ret 
ohlc_data.list[[1]]$cret <- ohlc_data.list[[1]]$SPFB.SI.cret 
#
#
### BruteForce оптимизация (в один поток)
# system.time(
#   {
#     PerfomanceTable <- BruteForceOpt.test_str(var.begin = 1, var.end = 100,
#                                              data.xts = ohlc_data.list[[1]], 
#                                              add_per, k_mm, balance_start, 
#                                              basket_weights, slips, commissions, ret_type,
#                                              fast = FALSE)
#   }
# )
#
### Parallel BruteForce оптимизация 
system.time(
  {
    PerfomanceTable <- BruteForceOpt_cl.test_str(
      #input_data = 'ohlc_data.list',
      sma_begin = 10, sma_end = 100, sma_step = 1,
      fast = FALSE
      #add_per, k_mm, balance_start, 
      #basket_weights, slips, commissions, ret_type
    )
  }
)
#
PerfomanceTable <- MergeData_inList.byRow(PerfomanceTable)
### КА
## Подготовка к КА
data_for_cluster <- CalcKmean.preparation(data = PerfomanceTable, n.mouth = 12, 
    hi = TRUE, q.hi = 0.5, 
    one.scale = TRUE)
data_for_cluster$profit <- NULL
data_for_cluster$draw <- NULL
## Вычисление параметров кластеризации 
clustPar.data <- CalcKmean.parameters(data = data_for_cluster, iter.max = 100, 
    plusplus = FALSE, test.range = 30)
## Вычисление самох кластеров
clustFull.data <- CalcKmean(data = data_for_cluster, clustPar.data[[2]], 
    plusplus = FALSE, var.digits = 2)
# вывод данных
#print(clustFull.data[2])