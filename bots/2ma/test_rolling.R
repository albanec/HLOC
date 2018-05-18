# Загрузка библиотек
source('bots/2ma/linker.R')
Sys.setenv(TZ = 'MSK')#
### входные параметры
from_date <- "2016-01-01"
to_date <- "2017-02-28"
period <- '15min'
tickers <- c('SPFB.SI')
im_dir <- 'data/im'
return_type <- 'ret'
expiration_dates.filename <- 'data/expiration_dates.csv'
unwanted_dates.filename <- 'data/unwanted_events.csv'

### подготовка исходных данных
# загрузка данных из .csv Финама
system.time({
ohlc <- ReadOHLC.FinamCSV(filename = 'data/temp/si_data.csv')
})
# переход к нужному периоду свечей
system.time({
ohlc <- ExpandOHLC.to_period(x = ohlc, per = '15min')
})
colnames(ohlc) <- c('SPFB.SI.Open', 'SPFB.SI.High', 'SPFB.SI.Low','SPFB.SI.Close', 'SPFB.SI.Volume')
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
system.time({
ohlc$IM <- CalcSum_inXTS_byTargetCol.basket(data = ohlc, 
    target = 'IM', basket_weights = 1)
})
# расчёт суммарного return'a 
# перевод return'ов в валюту
ohlc$SPFB.SI.cret <- ohlc$SPFB.SI.ret 
ohlc$cret <- ohlc$SPFB.SI.cret 
indexTZ(ohlc) <- 'MSK'
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
    ticker = tickers,
    lookback = 'TRUE')
trade_args <- list(balance_start = 10000000,
    balance_operating = 10000000,
    reinvest = FALSE,
    slips = 10, 
    commiss = 3, 
    expiration_date = expiration.dates,
    unwanted_event_filter = TRUE,
    unwanted_event_date = unwanted.dates, 
    return_type = 'ret',
    tick_price = 1,
    gap_filter = TRUE,
    expiration_filter = TRUE,
    fix_profit = FALSE,
    fix_profit.period = 'months',
    fix_profit.k = 1,
    fix_profit.at = '23:30:00',
    trade_handler = 'standalone')
cluster_args <- list( method = 'clara',
    k.max = 30,
    #iter.max = 1000,
    #nstart = 100,  
    #round_type = 'round'
    samples = 50,
    win_size = 4,
    only_profitable = TRUE)

# Генерация матрицы оптимизируемых параметров
var_df <- 
    CalcVarList.two_ma() %>%
    mutate(k_mm = 2)

# нарезка временных интервалов
system.time({
    ohlc_slices <- RollingSlicer(ohlc = ohlc_args$ohlc, 
        from_date = ohlc_args$from_date, to_date = ohlc_args$to_date, period = 'months',
        width = 3, by = 1,
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
    var_df = #head(
        var_df,
        #100),
    FUN.StrategyGear = StrategyGear.two_ma,
    ohlc_args, trade_args)
})
system.time({
clusters <- RollerOptimizer.cluster_analysis(bf_data, cluster_args)
})

# load(file = 'clusters.RData')
# load(file = 'cl1.Rdata')
# load(file = 'cl2.Rdata')
# load(file = 'cl3.Rdata')
# load(file = 'cl4.Rdata')

# подготовка пачек ботов для торговли на торговых периодах
# (каждый период - лист, в котором листы с параметрами ботов)
bot.list <- lapply(1:length(clusters),
    function(x) {
        clusters[[x]]$cluster.centers %>%
        mutate(name = 'two_ma', ticker = 'SPFB.SI') %>%
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
    FUN.MM = CalcMM.simple_byIM,
    ohlc_args, 
    trade_args)
})
# склейка данных по портфелю
portfolio_asset <- foreach(i = 1:length(trade_data), .combine = rbind) %do% {
    trade_data[[i]]$portfolio$data$balance + trade_data[[i]]$portfolio$data$im.balance %>%
    xts(.)
}
#