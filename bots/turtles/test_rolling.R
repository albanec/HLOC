# Загрузка библиотек
source('bots/turtles/linker.R')
Sys.setenv(TZ = 'MSK')
### входные параметры
from_date <- "2016-01-01"
to_date <- "2016-12-31"
period <- '15min'
ticker <- c('SPFB.SI')
im_dir <- 'data/im'
return_type <- 'ret'
expiration_dates.filename <- 'data/expiration_dates.csv'
unwanted_dates.filename <- 'data/unwanted_events.csv'
basket_weights <- 1
tick_value = 1 #10
tick_price = 1 #0.02

### подготовка исходных данных
# загрузка данных из .csv Финама
ohlc <- ReadOHLC.FinamCSV(filename = 'data/temp/si_data.csv')
indexTZ(ohlc) <- 'MSK'
ohlc_usdrub <- ReadOHLC.FinamCSV(filename = 'data/temp/usdrub_data.csv')
indexTZ(ohlc_usdrub) <- 'MSK'

# переход к нужному периоду свечей
ohlc <- ExpandOHLC.to_period(x = ohlc, per = '15min')    
colnames(ohlc) <- paste0(ticker, c('.Open', '.High', '.Low','.Close', '.Volume'))
ohlc <- 
    # удаление NA (по свечам)
    NormData_inXTS.na(data = ohlc, type = 'approx') %>%
    # добавляем ГО и данные по USDRUB
    AddData_inXTS.futuresSpecs(data = ., from_date, to_date, dir = im_dir, add.USDRUB = FALSE) %>%
    # вычисляем return'ы (в пунктах)
    CalcReturn_inXTS(data = ., price = 'Open', type = return_type)

# суммарное ГО по корзине 
ohlc$IM <- CalcSum_inXTS_byTargetCol.basket(data = ohlc, 
    target = 'IM', basket_weights = 1)

## расчёт суммарного return'a 
# перевод return'ов в валюту
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

# Параметоры системы
ohlc_args <- list(ohlc = ohlc,
    from_date = from_date, 
    to_date = to_date, 
    ticker = ticker,
    lookback = TRUE)
trade_args <- list(balance_start = 10000000,
    balance_operating = 10000000,
    reinvest = FALSE,
    slips = 10, 
    commiss = 3, 
    expiration_date = expiration.dates,
    unwanted_event_filter = TRUE,
    unwanted_event_date = unwanted.dates, 
    return_type = 'ret',
    tick_price = tick_price,
    tick_value = tick_value,
    gap_filter = TRUE,
    expiration_filter = TRUE,
    fix_profit = TRUE,
    fix_profit.period = 'months',
    fix_profit.k = 1,
    fix_profit.at = '23:30:00',
    trade_handler = 'standalone')
cluster_args <- list(method = 'clara',
    k.max = 30,
    only_profitable = TRUE,
    #iter.max = 1000,
    #nstart = 100,  
    #round_type = 'round',
    samples = 50,
    win_size = 4)

# генерация матрицы оптимизируемых параметров
var_df <- 
    CalcVarList.turtles(trend_only = TRUE) %>%
    mutate(k_mm = 0.02)
# нарезка временных интервалов
system.time({
    ohlc_slices <- RollingSlicer(ohlc = ohlc_args$ohlc, 
        from_date = ohlc_args$from_date, to_date = ohlc_args$to_date, period = 'months',
        width = 4, by = 2,
        align = 'left',
        add_bySlice = TRUE, 
        justIndex = TRUE)
})

## ---------- обучение -------------------------------------------------------------------------------------------------
# формирование кластеров на обучающих периодах
workers <- detectCores() - 1    
registerDoParallel(cores = workers)

system.time({
bf_data <- RollerOptimizer.bruteforce_optimizer(slice_index = ohlc_slices$widthSlice,  
    var_df = var_df,
    FUN.StrategyGear = StrategyGear.turtles,
    ohlc_args, trade_args)
})
system.time({
clusters <- RollerOptimizer.cluster_analysis(bf_data, cluster_args)
})

# подготовка пачек ботов для торговли на торговых периодах
# (каждый период - лист, в котором листы с параметрами ботов)
bot.list <- lapply(1:length(clusters),
    function(x) {
        clusters[[x]]$cluster.centers %>%
        mutate(name = 'turtles', ticker = 'SPFB.SI') %>%
        { 
            lapply(1:nrow(.),
                function(x) {
                    return(.[x, ])  
                })
        }
    }) 
## ---------- торговля -------------------------------------------------------------------------------------------------
system.time({
trade_data <- RollerOptimizer.trade(slice_index = ohlc_slices$bySlice, 
    bot.list,
    FUN.CalcOneTrade = CalcOneTrade,
    FUN.MM = CalcMM.byDCIwidth,
    ohlc_args, 
    trade_args)
})
# склейка данных по портфелю
portfolio_asset <- foreach(i = 1:length(trade_data), .combine = rbind) %do% {
    trade_data[[i]]$portfolio$data$balance + trade_data[[i]]$portfolio$data$im.balance %>%
    xts(.)
}
#