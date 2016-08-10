#
Convert_XTStoDF <- function(x) {
  # ----------
  # Общее описание:
  #   функция конвертирования XTS в DF
  # Входные данные:
  #  x: XTS ряд
  # Выходные данные:
  #  x: конвертированные в df данные
  # Зависимости:
  require(quantmod)   
  # ----------
  if (is.xts(x) != TRUE) {
    stop(paste("ERROR(Convert_XTStoDF):  Input Data wrong type!!!", sep = ""))
  } else {
    x <- data.frame(date = index(x), x, row.names = NULL)  
  }
  return(x)
}
#
Convert_DFtoXTS <- function(x) {
  # ----------
  # Общее описание:
  #   функция конвертирования DF в XTS
  # Входные данные:
  #  x: DF 
  # Выходные данные:
  #  x: конвертированные в XTS данные
  # Зависимости:
  require(quantmod)   
  # ----------
  if (is.data.frame(x) != TRUE) {
    stop(paste("ERROR(Convert_DFtoXTS):  Input Data wrong type!!!", sep = ""))
  } else {
    x <- xts(x[, -1], order.by = as.POSIXct(x$date))
  }
  return(x)
}
#
Save_XTStoCSV <- function(data, filename, period = FALSE, tframe = FALSE) {
  # ----------
  # Общее описание:
  #   функция записи XTS рядов в .csv файл   
  # Входные данные:
  #   data - нужный xts
  #  filename - название файла (без расширения .csv)
  #  period - указать в название период свечей
  #  tframe - указать в названии номер тайм-фрейма во Framelist'e
  # Выходные данные:
  #  сохраненный .csv файл
  # Зависимости:
  require(zoo)
  # ----------  
  #
  if (period !=  FALSE) {
    filename <- paste(filename, period, sep = ".")
  }
  if (tframe !=  FALSE) {
    filename <- paste(filename, tframe, sep = ".")
  }
  filename <- paste(filename, ".csv", sep = "")
  write.zoo(data, file = filename, sep = ",")
  cat("Save OK :  ", file.path(getwd(), filename), "\n")
}
#
Read_CSVtoXTS <- function(filename, period = FALSE, tframe = FALSE) {
  # ----------
  # Общее описание:
  #   функция чтения XTS рядов из .csv файлов
  # Входные данные:
  #  filename - название файла (без расширения .csv)
  #  period - указать в название период свечей
  #  tframe - указать в названии номер тайм-фрейма во FrameList'е
  # Выходные данные:
  #  xts ряд, полученный из файла
  # Зависимости:
    require(xts)
  # ----------
  # 
  if (period !=  FALSE) {
    filename <- paste(filename, period, sep = ".")
  }
  if (tframe !=  FALSE) {
    filename <- paste(filename, tframe, sep = ".")
  }
  filename <- paste(filename, "csv", sep = ".")
  data <- read.csv(file = filename)
  data <- xts(data[,-1], order.by = as.POSIXct(data$Index))
  cat("Read OK :  ", file.path(getwd(), filename), "\n")
  return(data)
}
#
Read_CSVtoDF <- function(file.path, sep = ";") {
  # ----------
  # Общее описание:
  # функция считывания простых .csv
  # Входные данные:
  # file.path: путь к файлу
  # sep: тип разделителя
  # Выходные данные:
  # file: считанный файл
  # ----------
  #
  file <- read.table(file=file.path, header=F, sep = ";", as.is=T) 
  return(file)
}
#
GetData_Ticker_One <- function(ticker, period = "15min", 
                               from.date, to.date = Sys.Date(), rename = FALSE) {
  # ----------
  # Общее описание:
  #   функция загрузки тикера с Финам + (если нужно) переименовывает столбцы
  # Входные данные:
  #   ticker: нужный тикер
  #  from.date: дата-старт / to.date: дата-стоп  (даты в формате "2015-01-01")
  #  period: период свечей
  # rename: (T/F) нужно ли переименовывать
  # Выходные данные:
  #  xts массив "data"
  # Зависимости:
  require(rusquant)   
  # ----------
  #
  cat("INFO(GetData_Ticker_One):  ", "Download Source Data...", "\n")
  data <- getSymbols(ticker, from = from.date, to = to.date, period = period, src = "Finam", auto.assign = FALSE)
  if (is.xts(data) !=  TRUE) {
    stop(paste("ERROR(GetData_Ticker_One):  ticker ", ticker, " not present!!!", sep = ""))
  }
  if (rename == TRUE) {
    names(data) <- c("Open" , "High" , "Low" , "Close" , "Volume")  
  }
  return(data)
}
#
GetData_Ticker_Set <- function(tickers = "TickerList.csv", from.date, to.date, period, 
                               maxattempts = 5, rename = FALSE, dir) {
  # ----------
  # Общее описание:
  #   функция загрузки списка котировок за период from/to.date + сохранения в файлы
  # Входные данные:
  # tickers: .csv или вектор с тикерами
  # from.date/to.date: даты начала/конца (даты в формате "2015-01-01")
  # period: вектор периодов (или единичное значение; из вектора будет выбрано min значение)
  # maxattempts: количество попыток загрузки 
  # Выходные данные:
  #  .csv файлы
  # data.list: лист с XTS по котировкам
  # Зависимости:
  require(rusquant)
  # ----------
  #
  old.dir <- getwd()
  setwd(dir)
  cat("INFO(GetData_Ticker_Set):  Current work.dir:", getwd(), "\n")  
  if (all(grepl(".csv", tickers)) == TRUE) {
    cat("INFO(GetData_Ticker_Set):  Loading Tickers: ", tickers, "\n")
    tickers <- 
      read.csv(tickers, header = F, stringsAsFactors = F) %>%
      .[, 1]   
    cat("INFO(GetData_Ticker_Set):  Loading Tickers: OK", "\n") 
  } 
  n.ticker <- length(tickers)
  n.period <- length(period)
  # если фреймы - вектор, то 
  period.min <- period[1]
  FirstTime <- TRUE 
  for (i in 1:n.ticker) {
    # цикл загрузки с max количеством попыток
    for (t in 1:maxattempts) {
      cat("INFO(GetData_Ticker_Set):  (", i ,"/", n.ticker, ")", 
          "Downloading: ", tickers[i], "  Attempt: ", t ,"/", maxattempts, "\n")
      data <- GetData_Ticker_One(ticker = tickers[i], from.date = from.date, to.date = to.date, 
                                 period = period.min, rename)
      if (exists("data")) {
         cat("INFO(GetData_Ticker_Set):  (", i ,"/", n.ticker, ")", 
             "Downloading ", tickers[i], "  complete", "\n")
         break
      }
    }
    data <- na.omit(data)
    data.name <- as.character(tickers[i])
    Save_XTStoCSV(data = data, filename = data.name, period = period.min)
    assign(paste(data.name, period.min, sep="."), data)
    remove(data); remove(data.name)
  }
  tickers <- sapply(tickers, function(x) { paste(x, period.min, sep = ".") })
  tickers.temp <- paste(tickers, collapse = ",")
  temp.text <- paste("data.list <- list(", tickers.temp, ") ;",
             "names(data.list) <-  tickers",  
             sep = "")
  eval(parse(text = temp.text))
  setwd(old.dir)
  return(data.list)
}
#
CalcReturn <- function(data, type = "sret") {
  # ----------
  # Общее описание:
  #   функция вычисляет return'ы
  # Входные данные:
  # data: $ряд 
  # type: тип return'a (ret/sret/lret)
  # Выходные данные:
  #  data: $ряд с return'ами 
  # Зависимости:
  require(quantmod)
  # ----------
  if (type == "ret") {
    data <- data - lag(data)
  }
  if (type == "sret") {
    data <- Delt(data, type = "arithmetic")
  } 
  if (type == "lret") {
    data <- Delt(data, type = "log")    
  }
  data[1] <- 0
  return(data)
}
#
MergeData_inList_byCol <- function(data.list, col.name = FALSE) {
  # ----------
  # Общее описание:
  #   функция объединения данных в один XTS 
  # Входные данные:
  #  data.list: лист, сожержащий XTS нужных тикеров 
  # col.name: если нужно объединить опред. столбцы, то присвоить название
  # Выходные данные:
  #  list(merged.data) - xts ряд объединенных значений (по всем тикерам)
  # ----------
  # 
  n.ticker <- length(data.list) 
  FirstTime <- TRUE
  #  чтение и объединение данных
  for (i in 1:n.ticker) {
    data <- data.list[[i]]
    data.name <- 
      names(data)[grep("Close", names(data))] %>%
      sub(".Close", "", .)
    cat("INFO(MergeData_fromAll_toOne):  Processing StocksData:  ", data.name, "\n")
    if (col.name != FALSE) {
      temp.text <-
        paste(data.name, col.name, sep = ".") %>%
        paste("data <- data$", ., sep = "")
      eval(parse(text = temp.text))
    }
    if (FirstTime == TRUE) {
      FirstTime <- FALSE
      merged.data <- data
    } else {
      merged.data <- merge(merged.data, data)
    }
  }  
  merged.data <- list(merged.data)
  names(merged.data) <- c("merged.data")
  return(merged.data)
}
#
MergeData_inList_byRow <- function(data.list) {
  while(length(data.list) > 1) {
    idxdata.list <- seq(from=1, to=length(data.list), by=2)
    data.list <- lapply(idxdata.list, 
                        function(i) {
                          if(i == length(data.list)) { 
                            return(data.list[[i]]) 
                          }
                          return(rbind(data.list[[i]], data.list[[i+1]]))
                        })
  }
  return(data.list[[1]])
}
#
NormData_NA_inXTS <- function(data, type="full", filename = FALSE) {
  # ----------
  # Общее описание:
  # функция удаления NA из XTS
  # Входные данные:
  # data: XTS, сожержащий нужные данные 
  # type: (full/locf/approx) способ удаления NA
  # filename: если нужэно сохранить, то определить название файла
  # Выходные данные:
  #  data: XTS ряд, очищенный от NA (по всем тикерам)
  # ----------
  # 
  if (is.xts(data) != TRUE) {
    stop("INFO(NormData_NA): Error in source data: DataType != .xts !!!")
  }
  if (any(is.na(data)) != TRUE) {
    cat("INFO(NormData_NA): No NA rows in data", "\n")
  } else {
    # нормализация NA-значений
    if (type == "locf") {
      # нормализация с пом-ю na.locf
      data <- na.locf(data)
    } 
    if (type == "full") {
      # нормализация по уровням свечей 
      data.names <- 
        names(data)[grep("Close", names(data))] %>%
        sub(".Close", "", .)
      for (i in 1:length(data.names)) {
        temp.text <- paste(
          "if (any(is.na(data$",data.names[i],".Close))!= TRUE) {",
            "cat(\"INFO(NormData_NA): No NA in\"",",data.names[i]", ",\"\\n\")",
          "} else {",
            "data$",data.names[i],".temp <- data$",data.names[i],".Open ; ",        
            "data$",data.names[i],".Open[is.na(data$",data.names[i],".Open)] <- ",
            "na.locf(coredata(data$",data.names[i],".Close))[is.na(data$",data.names[i],".Open)] ; ",
            "",
            "data$",data.names[i],".Close[is.na(data$",data.names[i],".Close)] <- ",
            "rev(na.locf(rev(coredata(data$",data.names[i],".temp))))[is.na(data$",data.names[i],".Close)] ; ",
            "",
            "data$",data.names[i],".High[is.na(data$",data.names[i],".High)] <- ",
              "ifelse(data$",data.names[i],".Close[is.na(data$",data.names[i],".High)] > ",
                "data$",data.names[i],".Open[is.na(data$",data.names[i],".High)],",
                "data$",data.names[i],".Close[is.na(data$",data.names[i],".High)],",
                "data$",data.names[i],".Open[is.na(data$",data.names[i],".High)]) ; ",
            "",
            "data$",data.names[i],".Low[is.na(data$",data.names[i],".Low)] <- ",
              "ifelse(data$",data.names[i],".Close[is.na(data$",data.names[i],".Low)] >",
                "data$",data.names[i],".Open[is.na(data$",data.names[i],".Low)],",
                "data$",data.names[i],".Open[is.na(data$",data.names[i],".Low)],",
                "data$",data.names[i],".Close[is.na(data$",data.names[i],".Low)]) ; ",
            "",
            "data$",data.names[i],".Volume[is.na(data$",data.names[i],".Volume)] <- 0 ; ",
            "",
            "data$",data.names[i],".temp <- NULL ; ",
            "cat(\"INFO(NormData_NA): All NA remove in\"",",data.names[i]", ",\"\\n\")", 
          "}",
          sep = "")
        eval(parse(text = temp.text))
      }
    }
    if (type == "approx") {
      # аппроксимация NA
      data <- na.approx(data)
    }
    data <- na.omit(data)
  }
  if (filename != FALSE) {
    Save_XTStoCSV(data = merged.data, filename = filename)  
  }
  return(data)
}
#
SubsetColumn_inXTS <- function(data, target) {
  # выделение столбцов с именами, содержащими target параметр
  data <-
    colnames(data) %>%
    grep(target, .) %>%
    {
      data[, .]
    }
  return(data)
}
#
CleanGarbage <- function(target = "temp", env = ".GlobalEnv") {
  cat("INFO(CleanTempData): Removing TempData..  Start", "\n")
  removeVector <- 
    ls(env) %>%
    {
      grep(target, ., value = TRUE) 
    } 
  rm(list = removeVector, envir = as.environment(env))
  cat("INFO(CleanTempData): Removing TempData..  OK", "\n")
}
#
CleanGarbage_inCols <- function(x, target = "temp") {
  x <- 
    colnames(x) %>%
    grep(target, .) %>%
    {
      x[, -.]
    }
  return(x)
}