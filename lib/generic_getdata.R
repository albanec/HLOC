# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Функции работы с серверами данных:
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
###
#' Функция загрузки списка котировок за период from/to_date + сохранения в файлы
#' 
#' @param tickers CSV или вектор с тикерами
#' @param from_date Дата начала (даты в формате '2015-01-01')
#' @param to_date Дата конца (даты в формате '2015-01-01')
#' @param period Вектор периодов (или единичное значение; из вектора будет выбрано min значение)
#' @param maxattempts Количество попыток загрузки (для каждого тикера)
#'
#' @return data.list Лист с XTS по котировкам (+ все котировки сохраняются в csv файлы)
#'
#' @export
GetOHLC <- function(tickers = 'TickerList.csv', 
                    from_date, to_date, period, 
                    maxattempts = 5, rename = FALSE, dir, local = TRUE) {
    # Зависимости:
    require(rusquant)
    # ----------
    # 
    # установка пути в temp папку
    old.dir <- getwd()
    setwd(dir)
    cat('INFO(GetOHLC):    Current work.dir:', getwd(), '\n')    
    # проверка, tickers путь к .csv или нет
    if (all(grepl('.csv', tickers)) == TRUE) {
        # если путь, то выгружаем из .csv данные тикеров
        cat('INFO(GetOHLC):    Loading Tickers: ', tickers, '\n')
        tickers <- 
            read.csv(tickers, header = F, stringsAsFactors = F) %>%
            .[, 1]     
        cat('INFO(GetOHLC):    Loading Tickers: OK', '\n') 
    } 
    #
    n.ticker <- length(tickers)
    n.period <- length(period)
    # если фреймы - вектор, то выгружаем min (остальное расширит expand'ер)
    period.min <- period[1]
    FirstTime <- TRUE 
    # цикл загрузки котировок тикеров
    for (i in 1:n.ticker) {
        # количество попыток загрузки ограничено пер. maxattempts
        for (t in 1:maxattempts) {
            cat(
                'INFO(GetOHLC):    (',i,'/',n.ticker,')', 
                'Downloading: ',tickers[i],'    Attempt: ',t,'/',maxattempts,'\n'
            )
            # загрузка данных
            data <- GetOHLC.one_ticker(ticker = tickers[i], from_date = from_date, to_date = to_date, 
                period = period.min, rename)
            if (exists('data')) {
                cat('INFO(GetOHLC):    (',i,'/',n.ticker,')','Downloading ',tickers[i],'    complete','\n')
                break
            }
        }
        data <- na.omit(data)
        # сохранение данных в .csv
        data.name <- 
            tickers[i] %>%
            as.character(.)
        Save_XTS.toCSV(data = data, filename = data.name, period = period.min)
        # формирование уникального имени (по тикеру и периоду)
        assign(paste(data.name, period.min, sep='.'), data)
        # удаление лишнего
        remove(data)
        remove(data.name)
    }
    # объединение данных в выходной лист
    temp.text <- 
        sapply(tickers, 
            function(x) { 
                paste(x, period.min, sep = '.') 
            }) %>%
        as.vector(.) %>%
        paste(., collapse = ', ') %>%
        paste0('data.list <- list(',.,')')
    eval(parse(text = temp.text))
    names(data.list) <- tickers
    # возвращение в исходную директорию
    setwd(old.dir)
    #
    return(data.list)
}
#
###
#' Функция загрузки тикера с Финам + (если нужно) переименовывает столбцы
#' 
#' @param ticker Нужный тикер
#' @param from_date Дата-старт (даты в формате '2015-01-01')
#' @param to_date Дата-стоп (даты в формате '2015-01-01')
#' @param period Период свечей
#' @param rename Нужно ли переименовывать (T/F)
#'    
#' @return data XTS массив
#'
#' @export
GetOHLC.one_ticker <- function(ticker, period = '15min', 
                              from_date, to_date = Sys.Date(), 
                              rename = FALSE) {
    # Зависимости:
    require(rusquant)     
    # ----------
    #
    cat('INFO(GetOHLC.one_ticker):    ', 'Download Source Data...', '\n')
    # загрузка данных
    data <- getSymbols(ticker, from = from_date, to = to_date, period = period, src = 'Finam', 
        auto.assign = FALSE, warning = FALSE)
    # проверка на правильность загруженных данных
    if (is.xts(data) !=    TRUE) {
        stop(paste0('ERROR(GetOHLC.one_ticker):    ticker ',ticker,' not present!!!'))
    }
    # нужно ли переименовывать данные к обезличенному OHLCV виду
    # если rename == FALSE, данные будут вида 'ticker_name.OHLCV'
    if (rename == TRUE) {
        names(data) <- c('Open' , 'High' , 'Low' , 'Close' , 'Volume')    
    }
    return(data)
}
#
###
#' Функция чтения XTS рядов из .csv файлов (выгрузки из Finam)
#' 
#' @param filename Название файла (без расширения .csv)
#' @param period Указать в название период свечей
#' @param tframe Указать в названии номер тайм-фрейма во FrameList'е
#' @param sep Тип разделителя
#'
#' @return data XTS ряд, полученный из файла
#'
#' @export
ReadOHLC.FinamCSV <- function(filename) {
    require(tidyr)
    require(foreach)
    require(lubridate)

    ## считывание .csv
    cat('INFO(ReadOHLC.FinamCSV):    Load file ... ',filename, '\n', sep = '')
    data <- Read_CSV.toDF(file.path = filename, sep = ',')
    ## проверка полей-заголовков
    colNames.temp <- data[1, ]
    #
    if ('<TICKER>' %in% colNames.temp) {
        ticker_name <- 
            which(colNames.temp %in% '<TICKER>') %>%
            data[2, .]
    } else {
        ticker_name <- 'Unkown ticker'
    }
    cat('INFO(ReadOHLC.FinamCSV):    Export ticker ... \"',ticker_name,'\" data', '\n', sep = '')
    #
    if ('<PER>' %in% colNames.temp) {
        per <-    
            which(colNames.temp %in% '<PER>') %>%
            data[2, .]
        if (per <= 10 && per != 0) {    
            per <- 
                c('tick', '1min', '5min', '10min', '15min', '30min', 'hour', 'day', 'week', 'month') %>%
                .[per]
        } else {
            per <- 'Unkown period'
        }
    } else {
        per <- 'Unkown period'
    }
    cat('INFO(ReadOHLC.FinamCSV):    Data period ... \"',per,'\"', '\n', sep = '')
    ### формирование XTS
    ## выделяем полезные данные
    data <- 
        c('<DATE>', '<TIME>', '<OPEN>', '<HIGH>', '<LOW>', '<CLOSE>', '<VOL>') %>%
        {
            which(colNames.temp %in% .) 
        } %>%    
        data[, .] 
    ## обработка
    colNames.temp <- data[1, ]
    data <- data[-1, ]
    ## выделение и обработка котировок
    quotes <- 
        c('<OPEN>', '<HIGH>', '<LOW>', '<CLOSE>', '<VOL>') %>%
        {
            which(colNames.temp %in% .) 
        } %>%
        data[, .] %>%
        # конвертирование котировок в numeric
        {
            foreach(i = 1:ncol(.), .combine = cbind) %do% {
                as.numeric(.[, i]) %>%
                data.frame(.)
            } 
        } %>%
        # переименование столбцов
        {
            colnames(.) <- c('Open', 'High', 'Low', 'Close', 'Volume')
            return(.) 
        }
    ## формирование временного ряда
    ts <-
        c('<DATE>', '<TIME>') %>%
        {
            which(colNames.temp %in% .) 
        } %>%
        data[, .] %>%
        ## конвертирование в integer
        {
            foreach(i = 1:ncol(.), .combine = cbind) %do% {
                as.character(.[, i]) %>%
                data.frame(.)
            }
        } %>%
        {
            colnames(.) <- c('date', 'time')
            return(.)
        } %>%
        # конвертация в DF и объеденение столбцов даты и времени в один 
        unite(., 'trueTime', date, time, sep = ' ')        
    ## результирующий XTS
    data <- xts(quotes, 
        order.by = lubridate::fast_strptime(ts$trueTime, '%Y%m%d %H%M%S') %>% as.POSIXct(.), 
        src = 'finam', 
        updated = Sys.time())
    rm(quotes)
    rm(ts)
    #
    return(data)
}
#
###
#' Функция выделения данных по tf и временному интервалу
#'
#' @param data.list Лист с котировками в XTS
#' @param frames .csv (или вектор), сожержащий список нужных временных интервалов (в виде '2014-12-01::2014-12-31')
#' @param period Вектор, содержащий нужные периоды свечей (в порядке возрастания); или один период
#' 
#' @return 
#'
#' @export
ExpandOHLC <- function(data.list, frames, period) {
    # 
    ## проверка, в файле данные по периоду или нет
    if (any(grepl('.csv', frames)) == TRUE) {
        cat('INFO(ExpandOHLC):    Loading FrameList: ', frames, '\n')
        frames <- read.csv(frames, header = F, stringsAsFactors = F)
        frames <- frames[, 1]        
        cat('(ExpandOHLC):    Loading FrameList: OK', '\n')
    } 
    # количество нужных временных интервалов
    n.frame <- length(frames)
    # количество тикеров в исходных данных
    n.ticker <- length(data.list) 
    # количество нужных свечных периодов
    n.period <- length(period)
    period.min <- period[1]
    ## цикл по тикерам внутри листа
    for (i in 1:n.ticker) {
        # выгрузка нужного xts
        data <- data.list[[i]]
        # имя тикера
        data.name <- names(data)[grep('Close', names(data))]
        data.name <- sub('.Close', '', data.name)
        cat( 'INFO(ExpandOHLC):    Processing Data:    ', data.name, '\n')
        ## выделение данных по нужным временным периодам
        for (n in 1:n.frame) {
            cat ('INFO(ExpandOHLC):    Expand...    ', data.name, 'for TimeFrame ', frames[n], '\n')
            window <- frames[n] 
            ## для каждого временного периода выделяются по нужным периодам свечей
            for (t in 1:n.period) {
                p <- period[t]
                cat ('INFO(ExpandOHLC):    Expand...    ', data.name, 'for Period ', p, '\n')
                data.temp <- 
                    # выделение данных по временному периоду
                    data[window] %>%
                    # выделение данных по периоду свечи
                    ExpandOHLC.to_period(., per = p)
                # сохранение данных
                Save_XTS.toCSV(data = data.temp, filename = data.name, period = p, tframe = n)
            }
            # удаление temp данных
            remove(data.temp)
            remove(data)
        }
    }
}
#
###
#' Функция выделения данных по периодам свечей
#'
#' @param x XTS с данными
#' @param per Период свечей ('5min' '10min' '15min' '30min' '1hour' '1day')
#' 
#' @return x XTS с данными (с новым периодом)
#'
#' @export
ExpandOHLC.to_period <- function(x, per) {
    # подготовка данных по периоду
    if (per == '5min') { 
        p1 <- 'mins'
        k <- 5
    }
    if (per == '10min') {
        p1 <- 'mins'
        k <- 10
    }
    if (per == '15min') {
        p1 <- 'mins'
        k <- 15
    }
     if (per == '30min') {
        p1 <- 'mins'
        k <- 30
    }
    if (per == '1hour') {
        p1 <- 'hours'
        k <- 1
    }
    if (per == '1day') {
        p1 <- 'days'
        k <- 1
    }
    #
    colNames <- colnames(x)
    ## Выборка нужных индексов для свечей
    ind <- 
        # расстановка endpoint'ов по периоду свечи 
        endpoints(x = x, on = p1, k = k) %>%
        # модификация
        {
            x <- .
            x <- 
                x[-length(x)] %>%
                {
                    . + 1 
                }
            return(x)
        } %>%
        # вычисление индексов
        x[., ] %>%
        index.xts(.)
    ## subset 
    x <- 
        # расчёт свечей
        to.period(x = x, period = p1, k = k) %>%
        # замена индексов на нужные
        {
            if (nrow(.) == length(ind)) {
                index(.) <- ind 
            } else {
                stop('ERROR(ExpandOHLC.to_period): ')
            }
            return(.)
        }
    colnames(x) <- colNames
    #
    return(x)
}
#