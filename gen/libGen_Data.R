# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Функции работы с файлами и типами данных:
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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
  file <- read.table(file=file.path, header=F, sep = ";", as.is=T) 
  #
  return(file)
}
#
###
#' Функция чтения XTS рядов из .csv файлов
#' 
#' @param filename Название файла (без расширения .csv)
#' @param period Указать в название период свечей
#' @param tframe Указать в названии номер тайм-фрейма во FrameList'е
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