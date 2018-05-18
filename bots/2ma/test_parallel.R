# Загрузка библиотек
source('bots/2ma/linker.R')
Sys.setenv(TZ = 'MSK')#
### входные параметры
from_date <- "2016-01-01"
to_date <- "2016-12-31"
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

# Генерация матрицы оптимизируемых параметров
var_df <- 
    CalcVarList.two_ma() %>%
    mutate(k_mm = 2)

## Запуск оптимизатора
# system.time({
# bf_data <- BruteForceOpt_cl.two_ma(var_df = head(var_df, 100),
#     FUN.StrategyGear = StrategyGear.two_ma, 
#     fast = FALSE,
#     ohlc_args, trade_args)
# })
system.time({
bf_data <- BruteForceOptimizer.mc(var_df = head(var_df, 100), 
    FUN.StrategyGear = StrategyGear.two_ma,
    fast = TRUE,
    ohlc_args, trade_args)
})

# КА
cluster_args <- list( method = 'clara',
    k.max = 30,
    #iter.max = 1000,
    #nstart = 100,  
    #round_type = 'round'
    samples = 50)

system.time({
cluster_data <- 
    bf_data %>%
    {
        ## Подготовка к КА
        data_for_cluster <- ClusterAnalysis.preparation(data = ., 
            n.mouth = 6, 
            hi = TRUE, q.hi = 0.5, 
            only_profitable = TRUE)
        data_for_cluster$profit <- NULL
        data_for_cluster$draw <- NULL 
        ## Вычисление самих кластеров
        if (cluster_args$method %in% c('kmeans','plusplus')) {
            clustFull.data <- 
                ClusterAnalysis.parameters(data = data_for_cluster, 
                    method = cluster_args$method,
                    k.max = cluster_args$k.max,
                    iter.max = cluster_args$iter.max,
                    nstart =  cluster_args$nstart) %>%
                .[[2]] %>%
                ClusterAnalysis(data = data_for_cluster, 
                    method = cluster_args$method,
                    n.opt = ., 
                    var.digits = 3,
                    iter.max = cluster_args$iter.max,
                    nstart =  cluster_args$nstart)
            ## Округление центров до значений точек пространства    
            clustFull.data[[2]] %<>% {
                for (x in 1:ncol(.[, !(colnames(.) %in% c('k_mm', 'profit.norm'))])) {
                    .[, x] <- .[, x] - .[, x] %% 5
                }
                return(.)        
            }
        }
        if (cluster_args$method == 'pam') {
            clustFull.data <- 
                ClusterAnalysis.parameters(data = data_for_cluster, 
                    method = cluster_args$method,
                    k.max = cluster_args$k.max) %>%
                .[[2]] %>%
                ClusterAnalysis(data = data_for_cluster, 
                    method = cluster_args$method,
                    n.opt = ., 
                    var.digits = 3)
        }
        if (cluster_args$method == 'clara') {
            clustFull.data <- 
                ClusterAnalysis.parameters(data = data_for_cluster, 
                    method = cluster_args$method,
                    k.max = cluster_args$k.max,
                    samples = cluster_args$samples) %>%
                .[[2]] %>%
                ClusterAnalysis(data = data_for_cluster, 
                    method = cluster_args$method,
                    n.opt = ., 
                    var.digits = 3,
                    samples = cluster_args$samples)
        }
        return(clustFull.data)
    }
})
