# Загрузка библиотек
source('main/test/linker.R')
#
### входные параметры
# temp.dir <- 'data/temp'
from.date <- '2015-01-01'
to.date <- '2016-02-28'
period <- '15min'
tickers <- c('SPFB.Si')
im.dir <- 'data/im'
ret.type <- 'ret'
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
data.source <- Read_CSV.toXTS.FinamQuotes(filename = 'data/temp/si_data.csv')
# выделение нужного периода
data.source <- 
  paste(from.date,'::',to.date, sep = '') %>%
  data.source[.]
# переход к нужному периоду свечей
data.source <- ExpandData.toPeriod(x = data.source, per = '15min')
data.source.list <- list(data.source)
colnames(data.source.list[[1]]) <- c('SPFB.SI.Open', 'SPFB.SI.High', 'SPFB.SI.Low','SPFB.SI.Close', 'SPFB.SI.Volume')
#
data.source.list[[1]] <- 
  # удаление NA (по свечам)
  NormData_inXTS.na(data = data.source.list[[1]], type = 'full') %>%
  # добавляем ГО и данные по USDRUB
  AddData_inXTS.futuresSpecs(data = ., from.date, to.date, dir = im.dir) %>%
  # вычисляем return'ы (в пунктах)
  CalcReturn_inXTS(data = ., price = 'Open', type = ret.type)
# суммарное ГО по корзине 
data.source.list[[1]]$IM <- CalcSum_inXTS_byTargetCol.basket(data = data.source.list[[1]], 
                                                           target = 'IM', basket.weights)
## расчёт суммарного return'a 
# перевод return'ов в валюту
data.source.list[[1]]$SPFB.SI.cret <- data.source.list[[1]]$SPFB.SI.ret 
data.source.list[[1]]$cret <- data.source.list[[1]]$SPFB.SI.cret 
#
#
## нарезка временных интервалов для обучения/торговли
system.time(
  {
    data_slices <- RollingSlicer(data = data.source.list[[1]], start_date = from.date, 
                                        end_date = to.date, period = 'months', 
                                        width = 6, by = 3, align = 'right',
                                        add_bySlice = TRUE)
 }
)
# ## bf-оптимизация на обучающих данных 
# system.time(
#   {
#     PerfomanceTable_learning <- lapply(data_slices$widthSlice, 
#                                        function(x) {
#                                          temp_slice <<- list(x) 
#                                          Parallel_test(input_data = 'temp_slice',
#                                                        sma_begin = 10, sma_end = 100, sma_step = 1,
#                                                        rolling_opt = TRUE)
#                                        }) 
#   }
# )
# ### КА
# system.time(
#   {
#     cluster_data <- lapply(
#       PerfomanceTable_learning,
#       function(x) {
#         ## Подготовка к КА
#         data_for_cluster <- CalcKmean_DataPreparation(data = x, n.mouth = 6, 
#                                                       hi = TRUE, q.hi = 0.5, 
#                                                       one.scale = FALSE)
#         data_for_cluster$profit <- NULL
#         data_for_cluster$draw <- NULL 
#         ## Вычисление параметров кластеризации 
#         clustPar.data <- CalcKmean_Parameters(data = data_for_cluster, iter.max = 100, 
#                                               plusplus = FALSE, test.range = 30)
#         ## Вычисление самох кластеров
#         clustFull.data <- CalcKmean(data = data_for_cluster, clustPar.data[[2]], 
#                                     plusplus = FALSE, var.digits = 2)
#       } 
#     )  
#   }
# )
system.time(
  {
    learning_data <- TestStr_RollerOpt_learningTime(data_slices, sma_begin = 10, sma_end = 100, sma_step = 1,
                                                    rolling_opt = TRUE)
  }
)
CleanGarbage(target = 'temp', env = '.GlobalEnv')
gc()
#