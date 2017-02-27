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
data_source <- Read_CSV.toXTS.FinamQuotes(filename = 'data/temp/si_data.csv')
# выделение нужного периода
data_source <- 
  paste0(from.date,'::',to.date) %>%
  data_source[.]
# переход к нужному периоду свечей
data_source <- ExpandData.toPeriod(x = data_source, per = '15min')
data_source.list <- list(data_source)
colnames(data_source.list[[1]]) <- c('SPFB.SI.Open', 'SPFB.SI.High', 'SPFB.SI.Low','SPFB.SI.Close', 'SPFB.SI.Volume')
#
data_source.list[[1]] <- 
  # удаление NA (по свечам)
  NormData_inXTS.na(data = data_source.list[[1]], type = 'full') %>%
  # добавляем ГО и данные по USDRUB
  AddData_inXTS.futuresSpecs(data = ., from.date, to.date, dir = im.dir) %>%
  # вычисляем return'ы (в пунктах)
  CalcReturn_inXTS(data = ., price = 'Open', type = ret_type)
# суммарное ГО по корзине 
data_source.list[[1]]$IM <- CalcSum_inXTS_byTargetCol.basket(data = data_source.list[[1]], 
                                                           target = 'IM', basket_weights)
## расчёт суммарного return'a 
# перевод return'ов в валюту
data_source.list[[1]]$SPFB.SI.cret <- data_source.list[[1]]$SPFB.SI.ret 
data_source.list[[1]]$cret <- data_source.list[[1]]$SPFB.SI.cret 
#
#
### BruteForce оптимизация (в один поток)
# system.time(
#   {
#     PerfomanceTable <- BruteForceOpt.test_str(var.begin = 1, var.end = 100,
#                                              data.xts = data_source.list[[1]], 
#                                              add_per, k_mm, balance_start, 
#                                              basket_weights, slips, commissions, ret_type,
#                                              rolling_opt = FALSE)
#   }
# )
#
### Parallel BruteForce оптимизация 
system.time(
  {
    PerfomanceTable <- BruteForceOpt_parallel_cl.test_str(
      #input_data = 'data_source.list',
      sma_begin = 10, sma_end = 100, sma_step = 1,
      rolling_opt = FALSE
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