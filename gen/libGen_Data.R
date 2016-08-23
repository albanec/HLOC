# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Функции работы с файлами и типами данных:
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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
Read_CSVtoXTS_FinamQuotes <- function(filename, ...) {
  #
  ## считывание .csv
  data <- Read_CSVtoDF(file.path = filename, sep = ",")
  #
  if (length(data) != 0) {
    cat("INFO(Read_CSVtoXTS_FinamQuotes):  Load file ... ",filename, "\n", sep = "")
  } 
  ## проверка полей-заголовков
  colNames.temp <- data[1, ]
  data <- data[-1, ]
  #
  if ("<TICKER>" %in% colNames.temp) {
    ticker.name <- 
      which(colNames.temp %in% "<TICKER>") %>%
      data[1, .]
  } else {
    ticker.name <- "Unkown ticker"
  }
  cat("INFO(Read_CSVtoXTS_FinamQuotes):  Export ticker ... \"",ticker.name,"\" data", "\n", sep = "")
  #
  if ("<PER>" %in% colNames.temp) {
    per <-  
      which(colNames.temp %in% "<TICKER>") %>%
      data[1, .]
    if (per <= 10 && per != 0) {  
      per <- 
        c("tick", "1min", "5min", "10min", "15min", "30min", "hour", "day", "week", "month") %>%
        .[per]
    } else {
      per <- "Unkown period"
    }
  } else {
    per <- "Unkown period"
  }
  cat("INFO(Read_CSVtoXTS_FinamQuotes):  Data period ... \"",per,"\"", "\n", sep = "")
  # выделение данных
  data <- 
    c("<DATE>","<TIME>","<OPEN>","<HIGH>","<LOW>","<CLOSE>","<VOL>") %>%
    {
      which(colNames.temp %in% .) 
    } %>%
    { 
      x <- 
        data[, .] %>%
        {
          apply(as.matrix(.), 2, as.numeric)
        } %>%
        as.data.frame(.)
      #regular.names <- c("date", "time", "Open", "High", "Low", "Close", "Volume")
      #colnames(x) <- regular.names[.]
      colnames(x) <- c("date", "time", "Open", "High", "Low", "Close", "Volume")
      return(x)
    } %>%
    unite(., "Date", date, time, sep = " ")
  # преобразование в XTS
  data %<>% 
    {
      xts(.[, -1], 
          as.POSIXct(strptime(.$Date, "%Y%m%d %H%M%S")), 
          src = "finam", 
          updated = Sys.time())
    } 
  #
  return(data)
}
#
###
#' Функция считывания простых .csv
#' 
#' @param file.path Путь к файлу
#' @param sep Тип разделителя
#'
#' @return file Считанный файл
#'
#' @export
Read_CSVtoDF <- function(file.path, sep = ";") {
  #
  file <- read.table(file=file.path, header=F, sep = sep, as.is=T) 
  #
  return(file)
}
#
###
#' Функция чтения XTS рядов из .csv файлов
#' 
#' @param filename Название файла (без расширения .csv)
#' @param period В названии исходного файла период свечей
#' @param tframe В названии исходного файла номер тайм-фрейма во FrameList'е
#' @param sep Тип разделителя
#'
#' @return data XTS ряд, полученный из файла
#'
#' @export
Read_CSVtoXTS <- function(filename, period = FALSE, tframe = FALSE, sep = ",") {
  # ----------
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
  #
  data <- read.csv(file = filename, sep = sep)
  data <- xts(data[,-1], order.by = as.POSIXct(data$Index))
  cat("Read OK :  ", file.path(getwd(), filename), "\n")
  #
  return(data)
}
###
#' Функция записи XTS рядов из .csv файлов
#' 
#' @param data Нужный xts
#' @param filename Название файла (без расширения .csv)
#' @param period Указать в название период свечей
#' @param tframe Указать в названии номер тайм-фрейма во FrameList'е
#' @param sep Тип разделителя
#'
#' @return 
#'
#' @export
Save_XTStoCSV <- function(data, filename, period = FALSE, tframe = FALSE, sep = ",") {
  # ----------
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
  write.zoo(data, file = filename, sep = sep)
  cat("Save OK :  ", file.path(getwd(), filename), "\n")
}
#
###
#' Функция конвертирования XTS в DF
#' 
#' @param x XTS ряд
#'
#' @return x Конвертированные в df данные
#'
#' @export
Convert_XTStoDF <- function(x) {
  # ----------
  require(quantmod)   
  # ----------
  #
  if (is.xts(x) != TRUE) {
    stop(paste("ERROR(Convert_XTStoDF):  Input Data wrong type!!!", sep = ""))
  } else {
    x <- data.frame(date = index(x), x, row.names = NULL)  
  }
  #
  return(x)
}
#
###
#' Функция конвертирования DF в XTS
#' 
#' @param x DF
#'
#' @return x Конвертированные в XTS данные
#'
#' @export
Convert_DFtoXTS <- function(x) {
  # ----------
  require(quantmod)   
  # ----------
  if (is.data.frame(x) != TRUE) {
    stop(paste("ERROR(Convert_DFtoXTS):  Input Data wrong type!!!", sep = ""))
  } else {
    x <- xts(x[, -1], order.by = as.POSIXct(x$date))
  }
  #
  return(x)
}