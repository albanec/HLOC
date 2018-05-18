# Загрузка библиотек
source('bots/turtles/linker.R')
Sys.setenv(TZ = 'MSK')#
### входные параметры
from_date <- "2016-02-01"
to_date <- "2016-02-28"
period <- '15min'
ticker <- 'SPFB.SI'
im_dir <- 'data/im'
return_type <- 'ret'
expiration_dates.filename <- 'data/expiration_dates.csv'
unwanted_dates.filename <- 'data/unwanted_events.csv'
basket_weights <- 1
tick_value = 1 #10
tick_price = 1 #0.02

### подготовка исходных данных
# загрузка данных из .csv Финама
system.time({
ohlc <- ReadOHLC.FinamCSV(filename = 'data/temp/si_data.csv')
indexTZ(ohlc) <- 'MSK'
ohlc_usdrub <- ReadOHLC.FinamCSV(filename = 'data/temp/usdrub_data.csv')
indexTZ(ohlc_usdrub) <- 'MSK'
})
# переход к нужному периоду свечей
system.time({
ohlc <- ExpandOHLC.to_period(x = ohlc, per = '15min')
})
colnames(ohlc) <- paste0(ticker, c('.Open', '.High', '.Low','.Close', '.Volume'))
#
system.time({
ohlc <- 
    # удаление NA (по свечам)
    NormData_inXTS.na(data = ohlc, type = 'full') %>%
    # добавляем ГО и данные по USDRUB
    AddData_inXTS.futuresSpecs(data = ., from_date, to_date, dir = im_dir, add.USDRUB = FALSE) %>%
    # вычисляем return'ы (в пунктах)
    CalcReturn_inXTS(data = ., price = 'Open', type = return_type) 
})
# суммарное ГО по корзине 
ohlc$IM <- CalcSum_inXTS_byTargetCol.basket(data = ohlc, 
    target = 'IM', basket_weights = 1)

# расчёт суммарного return'a 
ohlc <- merge(ohlc, Cl(ohlc_usdrub))
names(ohlc) <- c(names(ohlc)[-ncol(ohlc)], 'USDRUB')
ohlc$USDRUB <- na.locf(ohlc$USDRUB)
ohlc$USDRUB <- na.locf(ohlc$USDRUB, fromLast = TRUE)
ohlc <- na.omit(ohlc)
# перевод return'ов в валюту
ohlc <- NormData_inXTS.price(data = ohlc, 
    exchange_rate = ohlc$USDRUB, 
    names_in = paste0(ticker, '.ret'), 
    names_out = paste0(ticker, '.cret'), 
    tick_value, tick_price, 
    convert_to = 'RUB')
ohlc$cret <- CalcSum_inXTS_byTargetCol.basket(data = ohlc, target = '.cret', basket_weights)
# выгрузка дат экспирации
expiration.dates <- Read_CSV.toDF(file.path = expiration_dates.filename, sep = ',')
colnames(expiration.dates) <- expiration.dates[1, ]
expiration.dates <- 
    expiration.dates[-1, ] %>%
    as.vector(.)
# выгрузка данных по нежелательным событиям
unwanted.dates <- Read_CSV.toDF(file.path = unwanted_dates.filename, sep = ',')
colnames(unwanted.dates) <- unwanted.dates[1, ]
unwanted.dates <- 
    unwanted.dates[-1, ] %>%
    as.vector(.)   

### Один прогон вычислений 
ohlc_args <- list(ohlc = ohlc,
    from_date = from_date, 
    to_date = to_date, 
    ticker = ticker,
    lookback = 'TRUE')
trade_args <- list(balance_start = 10000000,
    balance_operating = 10000000,
    reinvest = TRUE,
    slips = 10, 
    commiss = 3, 
    expiration_date = expiration.dates,
    unwanted_event_filter = FALSE,
    unwanted_event_date = unwanted.dates, 
    return_type = 'ret',
    tick_price = tick_price,
    tick_value = tick_value,
    gap_filter = TRUE,
    expiration_filter = TRUE,
    fix_profit = FALSE,
    fix_profit.period = 'months',
    fix_profit.k = 1,
    fix_profit.at = '23:30:00',
    trade_handler = 'standalone')
str_args <- list(per_DCI = 40, 
    per_slowSMA = 30, 
    per_fastSMA = 25, 
    k_mm = 0.02)

### отработка робота 
# Вычисление сделок
DATA <- StrategyGear.turtles(ohlc_args, trade_args, str_args)
# чистим от лишних записей
DATA[[2]] <- StateTable.clean(DATA[[2]])
# лист с данными по сделкам (по тикерам и за всю корзину)
tradeTable <- TradeTable.calc(DATA[[2]], basket = FALSE, convert = TRUE)#TRUE
# if (length(ticker) == 1) {
#     TradeTable.list[[1]]$TradeReturnPercent <- TradeTable.list[[1]]$tradeReturn * 100 / balance_start
# } 

# очистка мусора по target = 'temp'
CleanGarbage(target = 'temp', env = '.GlobalEnv')
gc()

## оценка perfomance-параметров
system.time({
perfomance_table <- 
    PerfomanceTable(DATA, 
        trade_table = tradeTable,
        asset_cols = c('balance', 'im.balance'),
        balance_start = trade_args$balance_start, 
        ret_type = trade_args$return_type,
        fast = FALSE,
        dd_data_output = FALSE) %>%
    # добавление использованных параметров
    cbind.data.frame(., 
        per_DCI = str_args$per_DCI, per_slowSMA = str_args$per_slowSMA, 
        per_fastSMA = str_args$per_fastSMA, k_mm = str_args$k_mm)
})

### All-in-one версия
system.time({
perfomance_table <- OneThreadRun(FUN.StrategyGear = StrategyGear.turtles,
    fast = FALSE, dd_data_output = FALSE,
    ohlc_args, trade_args, str_args)
})