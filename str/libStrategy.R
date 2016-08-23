#
Convert_SigToState <- function(x) {
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
CalcState_Data <- function(data) {
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
CalcState_Table <- function(data) {
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
    data %>% CalcState_Data(.) %>%
    merge(data, .) %>%
    na.omit(.)
  return(state.data)
}
#
CalcReturn_inXTS <- function(data, price = "Close", type = "sret") {
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
CalcSum_Basket_TargetPar_inXTS <- function(data, basket.weights, target) {
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

#
