# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Generic функции для работы с котировками: 
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
###
#' Функция умного удаления NA из XTS
#'
#' @param data Исходный XTS
#' @param type способ удаления NA (full/locf/approx)
#' @param filename Если нужэно сохранить, то определить название файла
#'
#' @return data XTS ряд, очищенный от NA (по всем тикерам)
#'
#' @export
NormData_inXTS.na <- function(data, type = 'full', filename = FALSE) {
  # 
  if (is.xts(data) != TRUE) {
  stop('INFO(NormData_NA): Error in source data: DataType != .xts !!!')
  }
  if (any(is.na(data)) != TRUE) {
  cat('INFO(NormData_NA): No NA rows in data', '\n')
  } else {
  # нормализация NA-значений
  if (type == 'locf') {
    # нормализация с пом-ю na.locf
    data <- na.locf(data)
  } 
  if (type == 'full') {
    # нормализация по уровням свечей 
    data.names <- 
    names(data)[grep('.Close', names(data))] %>%
    sub('.Close', '', .)
    if (is.null(data.names))
    #
    for (i in 1:length(data.names)) {
      temp.text <- paste0(
        'if (any(is.na(data$',data.names[i],'.Close))!= TRUE) {',
        'cat(\"INFO(NormData_NA): No NA in\"',',data.names[i]', ',\"\\n\")',
        '} else {',
        'data$',data.names[i],'.temp <- data$',data.names[i],'.Open ; ',    
        'data$',data.names[i],'.Open[is.na(data$',data.names[i],'.Open)] <- ',
        'na.locf(coredata(data$',data.names[i],'.Close))[is.na(data$',data.names[i],'.Open)] ; ',
        '',
        'data$',data.names[i],'.Close[is.na(data$',data.names[i],'.Close)] <- ',
        'rev(na.locf(rev(coredata(data$',data.names[i],'.temp))))[is.na(data$',data.names[i],'.Close)] ; ',
        '',
        'data$',data.names[i],'.High[is.na(data$',data.names[i],'.High)] <- ',
          'ifelse(data$',data.names[i],'.Close[is.na(data$',data.names[i],'.High)] > ',
          'data$',data.names[i],'.Open[is.na(data$',data.names[i],'.High)],',
          'data$',data.names[i],'.Close[is.na(data$',data.names[i],'.High)],',
          'data$',data.names[i],'.Open[is.na(data$',data.names[i],'.High)]) ; ',
        '',
        'data$',data.names[i],'.Low[is.na(data$',data.names[i],'.Low)] <- ',
          'ifelse(data$',data.names[i],'.Close[is.na(data$',data.names[i],'.Low)] >',
          'data$',data.names[i],'.Open[is.na(data$',data.names[i],'.Low)],',
          'data$',data.names[i],'.Open[is.na(data$',data.names[i],'.Low)],',
          'data$',data.names[i],'.Close[is.na(data$',data.names[i],'.Low)]) ; ',
        '',
        'data$',data.names[i],'.Volume[is.na(data$',data.names[i],'.Volume)] <- 0 ; ',
        '',
        'data$',data.names[i],'.temp <- NULL ; ',
        'cat(\"INFO(NormData_NA): All NA remove in\"',',data.names[i]', ',\"\\n\")', 
        '}')
      eval(parse(text = temp.text))
    }
  }
  if (type == 'approx') {
    # аппроксимация NA
    data <- na.approx(data)
  }
  data <- na.omit(data)
  }
  if (filename != FALSE) {
  Save_XTS.toCSV(data = merged.data, filename = filename)  
  }
  #
  return(data)
}
#
###
#' Функция для расчёта стоимости тиков внутри основного листа данных
#' 
#' @param data Данные котировок
#' @names Список тикеров для конвертирования 
#' @param norm.data Данные USDRUB_TOM
#' @convert.to Валюта конвертации
#' @tick.val Шаг тика
#' @tick.price Цена тика
#' @outnames Имя столбца, в который будут направлены данные
#'
#' @return data Основной xts
#'
#' @export
NormData_inXTS.price <- function(data, names, norm.data, outnames, convert.to, tick.val, tick.price) {
  # ----------
  x <- norm.data
  for (i in 1:length(names)) {
    temp.text <- paste0('data$',outnames[i],' <- ',
                       'NormData.price(data = data$',names[i],',',
                                                 'norm.data = x, convert.to = \"',convert.to,'\",',
                                                 'tick.val = ',tick.val[i],',',
                                                 'tick.price = ', tick.price[i],')')
    eval(parse(text = temp.text))  
  }
  return(data)  
}
#
###
#' Функция для расчёта стоимости тиков
#' 
#' @param data Данные котировок
#' @param norm.data Данные USDRUB_TOM
#' @convert.to Валюта конвертации
#' @tick.val Шаг тика
#' @tick.price Цена тика
#'
#' @return data Основной xts
#'
#' @export
NormData.price <- function(data, norm.data, convert.to, tick.val, tick.price) {
  # ----------
  if (convert.to == 'RUB') {
    data <- (data * tick.price / tick.val) * norm.data
  }
  if (convert.to == 'USD') {
    data <- (data * tick.price / tick.val) / norm.data  
  }
  #
  return(data)
}
#
###
#' Функция добавляет параметры инструментов 
#' (для фьючерсов: размеры ГО и курс USDRUB для пересчёта к RUB)
#' 
#' @param data XTS, сожержащий нужные данные 
#' @param from.date Дата начала
#' @param to.date Дата конца
#' @param im.wd Папка с данными по ГО
#'
#' @return data XTS ряд, с добавленными параметрами
#'
#' @export
AddData_inXTS.futuresSpecs <- function(data, from.date, to.date, dir, add.USDRUB = TRUE) {
  # 
  old.dir <- getwd()
  setwd(dir) 
  # загрузка ГО
  data.names <- names(data)[grep('Close', names(data))]
  data.names <- sub('.Close', '', data.names)
  temp.data <- xts()
  for (i in 1:length(data.names)) {
    temp.text <- paste0('temp.data <- Read_CSV.toXTS(filename = \"',data.names[i],'.IM\") ; ',
                        'data$',data.names[i],'.IM <- temp.data ; ',
                        'remove(temp.data) ; ',
                        'data$',data.names[i],'.IM <- na.locf(data$',data.names[i],'.IM) ; ')
    eval(parse(text = temp.text))
  }
  remove(temp.text)
  remove(data.names)
  if (add.USDRUB == TRUE) {
    # загрузка котировок USDRUB_TOM
    data.USDRUB <- GetData.OneTicker(ticker = 'USD000UTSTOM', from.date, to.date, period = 'day', rename = TRUE)
    data$USDRUB <- data.USDRUB$Close
    remove(data.USDRUB)
    data$USDRUB <- na.locf(data$USDRUB)  
  } 
  # очистка от NA (на данном этапе na.omit полезным данным не навредит)
  data <- na.omit(data)
  setwd(old.dir)
  #
  return(data)
}
#
###
#' Функция вычисляет return'ы 
#' 
#' @param data $ряд 
#' @param type Тип return'a (ret/sret/lret)
#'
#' @return data $ряд с return'ами 
#'
#' @export
CalcReturn <- function(data, type = 'sret') {
  # ----------
  require(quantmod)
  # ----------
  if (type == 'ret') {
  data <- data - lag(data)
  }
  if (type == 'sret') {
  data <- Delt(data, type = 'arithmetic')
  } 
  if (type == 'lret') {
  data <- Delt(data, type = 'log')  
  }
  data[1] <- 0
  #
  return(data)
}