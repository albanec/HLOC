# Загрузка библиотек
source('main/test/linker.R')
#
### входные параметры
# temp.dir <- 'data/temp'
from_date <- '2016-03-01'
to_date <- '2016-03-31'
period <- '15min'
tickers <- c('SPFB.Si')
im_dir <- 'data/im'
ret_type <- 'ret'
balance_start <- 1000000
k_mm <- 0.02  # mm на заход в сделку
slips <- 10 # в пунктах
commissions <- 3  # в рублях
per_DCI <- 50 
per_slowSMA <- 50 
per_fastSMA <- 40 
expiration_dates.filename <- 'data/expiration/expiration_dates.csv'

## подготовка исходных данных
# загрузка данных из .csv Финама
data.source <- Read_CSV.toXTS.FinamQuotes(filename = 'data/temp/si_source.csv')
# выделение нужного периода
data.source <- 
  paste(from_date,'::',to_date, sep = '') %>%
  data.source[.]
# переход к нужному периоду свечей
data.source <- ExpandData.toPeriod(x = data.source, per = '15min')
data.source.list <- list(data.source)
rm(data.source)

colnames(data.source.list[[1]]) <- c('SPFB.SI.Open', 'SPFB.SI.High', 'SPFB.SI.Low','SPFB.SI.Close', 'SPFB.SI.Volume')
#
data.source.list[[1]] <- 
  # удаление NA (по свечам)
  NormData_inXTS.na(data = data.source.list[[1]], type = 'full') %>%
  # добавляем ГО и данные по USDRUB
  AddData_inXTS.futuresSpecs(data = ., from_date, to_date, dir = im_dir, add.USDRUB = FALSE) %>%
  # вычисляем return'ы (в пунктах)
  CalcReturn_inXTS(data = ., price = 'Open', type = ret_type)
# суммарное ГО по корзине 
data.source.list[[1]]$IM <- CalcSum_inXTS_byTargetCol.basket(data = data.source.list[[1]], 
                                                           target = 'IM', basket.weights = 1)
## расчёт суммарного return'a 
# перевод return'ов в валюту
data.source.list[[1]]$SPFB.SI.cret <- data.source.list[[1]]$SPFB.SI.ret 
data.source.list[[1]]$cret <- data.source.list[[1]]$SPFB.SI.cret 
#
colnames(data.source.list[[1]]) <- c('Open', 'High', 'Low','Close', 'Volume', 'SPFB.SI.IM', 'SPFB.SI.ret', 'IM',
                                     'SPFB.SI.cret', 'cret')
#
### выгрузка дат экспирации
expiration.dates <- Read_CSV.toDF(file.path = expiration_dates.filename, sep = ',')
colnames(expiration.dates) <- expiration.dates[1, ]
expiration.dates <- 
  expiration.dates[-1, ] %>%
  as.vector(.) %>%
  ymd(x = ., tz = 'MSK')
#
exp.vector <- expiration.dates

### один прогон вычислений 
### отработка робота
data.strategy.list <- StrategyGear.Turtles(data.source = data.source.list[[1]],
                                           per_DCI = per_DCI, per_slowSMA = per_slowSMA, per_fastSMA = per_fastSMA, 
                                           k.mm = k_mm, balance_start = balance_start, 
                                           slips = slips, commissions = commissions, 
                                           return_type = 'ret',
                                           exp.vector = exp.vector)
