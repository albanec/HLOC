# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Функции работы с файлами и типами данных:
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
###
#' Функция считывания простых .csv
#' 
#' @param file.path Путь к файлу
#' @param fast Чтение через fread (T/F)
#' @param sep Тип разделителя
#'
#' @return file Считанный файл
#'
#' @export
Read_CSV.toDF <- function(file.path, fast = TRUE, sep = ';') {
    #
    if (fast == TRUE) {
        require(data.table)
        require(magrittr)
        file <- 
            fread(input = file.path, sep = sep, header = F, stringsAsFactors = F) %>%
            as.data.frame(.)
    } else {
        file <- read.table(file = file.path, header = F, stringsAsFactors = F, sep = sep, as.is = T)     
    }
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
Read_CSV.toXTS <- function(filename, period = FALSE, tframe = FALSE, sep = ',', fast = FALSE) {
    # ----------
    require(xts)
    # ----------
    # 
    if (period !=    FALSE) {
    filename <- paste(filename, period, sep = '.')
    }
    if (tframe !=    FALSE) {
    filename <- paste(filename, tframe, sep = '.')
    }
    filename <- paste(filename, 'csv', sep = '.')
    #
    if (fast == TRUE) {
        require(data.table)
        require(magrittr)
        file <- 
            fread(input = filename, sep = sep, header = F, stringsAsFactors = F) %>%
            as.data.frame(.)
    } else {
        data <- read.csv(file = filename, sep = sep)
    }
    data <- xts(data[,-1], order.by = as.POSIXct(data[, 1]))
    cat('Read OK :    ', file.path(getwd(), filename), '\n')
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
Save_XTS.toCSV <- function(data, filename, period = FALSE, tframe = FALSE, sep = ',') {
    # ----------
    require(zoo)
    # ----------    
    #
    if (period !=    FALSE) {
    filename <- paste(filename, period, sep = '.')
    }
    if (tframe !=    FALSE) {
    filename <- paste(filename, tframe, sep = '.')
    }
    filename <- paste0(filename, '.csv')
    write.zoo(data, file = filename, sep = sep)
    cat('Save OK :    ', file.path(getwd(), filename), '\n')
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
Convert.XTStoDF <- function(x) {
    # ----------
    require(quantmod)     
    # ----------
    #
    if (is.xts(x) != TRUE) {
        stop(paste0('ERROR(Convert.XTStoDF):    Input Data wrong type!!!'))
    } else {
        x <- data.frame(INDEX = index.xts(x), x, row.names = NULL)    
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
Convert.DFtoXTS <- function(x) {
    # ----------
    require(quantmod)     
    # ----------
    if (is.data.frame(x) != TRUE) {
        stop(paste0('ERROR(Convert.DFtoXTS):    Input Data wrong type!!!'))
    } else {
        x <- xts(x[, -1], order.by = as.POSIXct(x$INDEX))
    }
    #
    return(x)
}