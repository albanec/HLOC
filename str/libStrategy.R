#
STR_Convert_SigToState <- function(x) {
  # ----------
  # Общее описание:
  #  функция для перехода к состояниям (фильтрация сигналов)
  # Входные данные:
  #  x - ряд сигналов стратегии
  # Выходные данные:
  #  ряд сделок (отфильтрованный ряд сигналов)
  # ----------
  x$a <- 
    na.locf(x) %>%
    ifelse(is.na(x$a) | is.nan(x$a) | is.infinite(x$a), 0, x$a)
  ind <- which(x$a != lag(x$a))
  x$y <- rep(NA, length(x$a))
  x$y[ind] = x$a[ind]
  x$y[1] <- x$a[1]
  return (x$y)
}
#
STR_CalcState_Data <- function(data) {
  # ----------
  # Общее описание:
  #  функция для перехода к состояниям (фильтрация сигналов)
  # Входные данные:
  #  data: ряд позиций (data$pos)
  # Выходные данные:
  #  data$state: ряд состояний  
  # Зависимости:
  require(quantmod) 
  # ----------
  data$pos <-
    na.locf(data$pos) %>%
    ifelse(is.na(.) | is.nan(.) | is.infinite(.), 0, .)
  ind <- which(data$pos != lag(data$pos))
  data$state <- rep(NA, length(data$pos))
  data$state[ind] <- data$pos[ind]
  data$state[1] <- data$pos[1]
  return(data$state)
}
#
STR_CalcState_Table <- function(data) {
  # ----------
  # Общее описание:
  # генерирует таблицу сделок
  # Входные данные:
  # data: данные
  # Выходные данные:
  # state.data: данные с рядом состояний  
  # Зависимости:
  require(quantmod) 
  # ----------
  state.data <- 
    data %>% STR_CalcState_Data(.) %>%
    merge(data, .) %>%
    na.omit(.)
  return(state.data)
}
#
STR_CalcReturn_inXTS <- function(data, price = "Close", type = "sret") {
  # ----------
  # Общее описание:
  #  функция вычисляет return'ы по всему портфелю внутри XTS
  # Входные данные:
  #  data: XTS 
  #  type: тип return'a (ret/sret/lret)
  # Выходные данные:
  #  data: XTS + return'ы по каждому инструменту 
  # Зависимости:
  require(quantmod)
  # ----------
  data.names <- 
    names(data)[grep("Close", names(data))] %>%
    sub(".Close", "", .)
  for (i in 1:length(data.names)) {
    temp.text <- paste("data$",data.names[i],".",type," <- ",
                       "CalcReturn(data$",data.names[i],".",price,", type = \"",type,"\")", 
                       sep="")
    eval(parse(text = temp.text))
  }
  return(data)
}
#
STR_NormData_Price_inXTS <- function(data, names, norm.data, outnames, convert.to, tick.val, tick.price) {
  # ----------
  # Общее описание:
  # Функция для расчёта стоимости тиков внутри основного листа данных
  # Входные данные:
  # data: XTS данные котировок (основной лист данных)
  # names: список тикеров для конвертирования 
  # Выходные данные:
  # data: основной XTS (нужные данные конвертированы к нужной валюте)
  # ----------
  x <- norm.data
  for (i in 1:length(names)) {
    temp.text <- paste("data$",outnames[i]," <- ",
                       "STR_NormData_Price_byCol(data = data$",names[i],",",
                                                 "norm.data = x, convert.to = \"",convert.to,"\",",
                                                 "tick.val = ",tick.val[i],",",
                                                 "tick.price = ", tick.price[i],")",
                       sep = "")
    eval(parse(text = temp.text))  
  }
  return(data)  
}
#
STR_CalcSum_Basket_TargetPar_inXTS <- function(data, basket.weights, target) {
  #require()
  # расчёт суммарного параметра (согласно весам инструмента в портфеле)
  temp.text <- 
    names(data)[grep(target, names(data))] %>% 
    paste("data$", ., sep = "") %>%
    paste(., basket.weights, sep = " * ", collapse = " + ") %>%
    paste("data <- ", ., sep = "") 
  eval(parse(text = temp.text))
  return(data)
}
#
STR_AddData_FuturesSpecs_inXTS <- function(data, from.date, to.date, dir) {
  # ----------
  # Общее описание:
  # функция добавляет параметры инструментов (для фьючерсов: размеры ГО и курс USDRUB для пересчёта к RUB)
  # Входные данные:
  # data: XTS, сожержащий нужные данные 
  # from.date / to.date
  # Выходные данные:
  #  data: XTS ряд, с добавленными параметрами
  # ----------
  # 
  old.dir <- getwd()
  setwd(dir) 
  # загрузка ГО
  data.names <- names(data)[grep("Close", names(data))]
  data.names <- sub(".Close", "", data.names)
  temp.data <- xts()
  for (i in 1:length(data.names)) {
    temp.text <- paste("temp.data <- Read_CSVtoXTS(filename = \"",data.names[i],".IM\") ; ",
               "data$",data.names[i],".IM <- temp.data ; ",
               "remove(temp.data) ; ",
               "data$",data.names[i],".IM <- na.locf(data$",data.names[i],".IM) ; ",
               sep="")
    eval(parse(text = temp.text))
  }
  remove(temp.text); remove(data.names); 
  # загрузка котировок USDRUB_TOM
  data.USDRUB <- GetData_Ticker_One(ticker = "USD000UTSTOM", from.date, to.date, period = "day", rename = TRUE)
  data$USDRUB <- data.USDRUB$Close
  remove(data.USDRUB)
  data$USDRUB <- na.locf(data$USDRUB)
  # очистка от NA (на данном этапе na.omit полезным данным не навредит)
  data <- na.omit(data)
  setwd(old.dir)
  return(data)
}
#
STR_NormData_Price_byCol <- function(data, norm.data, convert.to, tick.val, tick.price) {
  # ----------
  # Общее описание:
  # Функция для расчёта стоимости тиков
  # Входные данные:
  # data: данные котировок
  # norm.data: данные USDRUB_TOM 
  # Выходные данные:
  # data: основной xts 
  # ----------
  if (convert.to == "RUB") {
    data <- (data * tick.price / tick.val) * norm.data
  }
  if (convert.to == "USD") {
    data <- (data * tick.price / tick.val) / norm.data  
  }
  return(data)
}