# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Функции работы с серверами данных:
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
###
#' Функция загрузки списка котировок за период from/to.date + сохранения в файлы
#' 
#' @param tickers CSV или вектор с тикерами
#' @param from.date Дата начала (даты в формате "2015-01-01")
#' @param to.date Дата конца (даты в формате "2015-01-01")
#' @param period Вектор периодов (или единичное значение; из вектора будет выбрано min значение)
#' @param maxattempts Количество попыток загрузки (для каждого тикера)
#'
#' @return data.list Лист с XTS по котировкам (+ все котировки сохраняются в csv файлы)
#'
#' @export
GetData_Ticker_Set <- function(tickers = "TickerList.csv", from.date, to.date, period, 
                               maxattempts = 5, rename = FALSE, dir, local = TRUE) {
  # Зависимости:
  require(rusquant)
  # ----------
  # 
  # установка пути в temp папку
  old.dir <- getwd()
  setwd(dir)
  cat("INFO(GetData_Ticker_Set):  Current work.dir:", getwd(), "\n")  
  # проверка, tickers путь к .csv или нет
  if (all(grepl(".csv", tickers)) == TRUE) {
    # если путь, то выгружаем из .csv данные тикеров
    cat("INFO(GetData_Ticker_Set):  Loading Tickers: ", tickers, "\n")
    tickers <- 
      read.csv(tickers, header = F, stringsAsFactors = F) %>%
      .[, 1]   
    cat("INFO(GetData_Ticker_Set):  Loading Tickers: OK", "\n") 
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
      cat("INFO(GetData_Ticker_Set):  (",i,"/",n.ticker,")", 
          "Downloading: ",tickers[i],"  Attempt: ",t,"/",maxattempts,"\n")
      # загрузка данных
      data <- GetData_Ticker_One(ticker = tickers[i], from.date = from.date, to.date = to.date, 
                                 period = period.min, rename)
      if (exists("data")) {
        cat("INFO(GetData_Ticker_Set):  (",i,"/",n.ticker,")","Downloading ",tickers[i],"  complete","\n")
        break
      }
    }
    data <- na.omit(data)
    # сохранение данных в .csv
    data.name <- 
      tickers[i] %>%
      as.character(.)
    Save_XTStoCSV(data = data, filename = data.name, period = period.min)
    # формирование уникального имени (по тикеру и периоду)
    assign(paste(data.name, period.min, sep="."), data)
    # удаление лишнего
    remove(data)
    remove(data.name)
  }
  # объединение данных в выходной лист
  temp.text <- 
    sapply(tickers, 
           function(x) { 
             paste(x, period.min, sep = ".") 
           }) %>%
    as.vector(.) %>%
    paste(., collapse = ", ") %>%
    paste("data.list <- list(",.,")",  
           sep = "")
  eval(parse(text = temp.text))
  names(data.list) <-  tickers
  # возвращение в исходную директорию
  setwd(old.dir)
  #
  return(data.list)
}
#
###
#' Функция выделения данных по tf и временному интервалу
#'
#' @param data.list Лист с котировками в XTS
#' @param frames .csv (или вектор), сожержащий список нужных временных интервалов (в виде '2014-12-01/2014-12-31')
#' @param period Вектор, содержащий нужные периоды свечей (в порядке возрастания); или один период
#' 
#' @return 
#'
#' @export
ExpandData_toPeriod <- function(data.list, frames, period) {
  # 
  ## проверка, в файле данные по периоду или нет
  if (any(grepl(".csv", frames)) == TRUE) {
    cat("INFO(ExpandData_toPeriod):  Loading FrameList: ", frames, "\n")
    frames <- read.csv(frames, header = F, stringsAsFactors = F)
    frames <- frames[, 1]    
    cat("(ExpandData_toPeriod):  Loading FrameList: OK", "\n")
  } 
  # количество нужных временных интервалов
  n.frame <- length(frames)
  # количество тикеров в исходных данных
  n.ticker <- length(data.list) 
  # количество нужных свечных периодов
  n.period <- length(period)
  period.min <- period[1]
  ## цикл по тикерам
  for (i in 1:n.ticker) {
    # выгрузка нужного xts
    data <- data.list[[i]]
    # имя тикера
    data.name <- names(data)[grep("Close", names(data))]
    data.name <- sub(".Close", "", data.name)
    cat( "INFO(ExpandData_toPeriod):  Processing Data:  ", data.name, "\n")
    ## выделение данных по нужным временным периодам
    for (n in 1:n.frame) {
      cat ("INFO(ExpandData_toPeriod):  Expand...  ", data.name, "for TimeFrame ", frames[n], "\n")
      window <- frames[n] 
      ## для каждого временного периода выделяются по нужным периодам свечей
      for (t in 1:n.period) {
        p <- period[t]
        cat ("INFO(ExpandData_toPeriod):  Expand...  ", data.name, "for Period ", p, "\n")
        if (p == "5min") { 
          p1 <- "mins"
          k <- 5
        }
        if (p == "10min") {
          p1 <- "mins"
          k <- 10
        }
        if (p == "15min") {
          p1 <- "mins"
          k <- 15
        }
        if (p == "30min") {
          p1 <- "mins"
          k <- 30
        }
        if (p == "1hour") {
          p1 <- "hours"
          k <- 1
        }
        if (p == "1day") {
          p1 <- "days"
          k <- 1
        }
        data.temp <- 
          # выделение данных по временному периоду
          data[window] %>%
          {
            # расстановка endpoint'ов по периоду свечи 
            ends <- endpoints(., p1, k)
            # выделение данных с нужным периодом свечи
            result <- .[ends]
            return(result)
          }
        # сохранение данных
        Save_XTStoCSV(data = data.temp, filename = data.name, period = p, tframe = n)
      }
      # удаление temp данных
      remove(data.temp)
      remove(data)
    }
  }
}
#
###
#' Функция загрузки тикера с Финам + (если нужно) переименовывает столбцы
#' 
#' @param ticker Нужный тикер
#' @param from.date Дата-старт (даты в формате "2015-01-01")
#' @param to.date Дата-стоп (даты в формате "2015-01-01")
#' @param period Период свечей
#' @param rename Нужно ли переименовывать (T/F)
#'  
#' @return data XTS массив
#'
#' @export
GetData_Ticker_One <- function(ticker, period = "15min", loadFrom = "Finam",
                               from.date, to.date = Sys.Date(), 
                               rename = FALSE) {
  # Зависимости:
  require(rusquant)   
  # ----------
  #
  cat("INFO(GetData_Ticker_One):  ", "Download Source Data...", "\n")
  # загрузка данных
  data <- getSymbols(ticker, from = from.date, to = to.date, period = period, src = loadFrom, auto.assign = FALSE)
  # проверка на правильность загруженных данных
  if (is.xts(data) !=  TRUE) {
    stop(paste("ERROR(GetData_Ticker_One):  ticker ",ticker," not present!!!", sep = ""))
  }
  # нужно ли переименовывать данные к обезличенному OHLCV виду
  # если rename == FALSE, данные будут вида "ticker_name.OHLCV"
  if (rename == TRUE) {
    names(data) <- c("Open" , "High" , "Low" , "Close" , "Volume")  
  }
  return(data)
}
#
