# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# описания стратегий и вспомагательных функций
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
### Загрузка дочерних библиотек
source("lib/strategy/strategy_indicators.R")
###
#' Переход к состояниям (фильтрация сигналов)
#' 
#' @param x Ряд сигналов стратегии
#'
#' @return x$y ряд сделок (отфильтрованный ряд сигналов)
#'
#' @export
Convert.signal_to_states <- function(x) {
  x$a <- 
    na.locf(x) %>%
    ifelse(is.na(x$a) | is.nan(x$a) | is.infinite(x$a), 0, x$a)
  ind <- which(x$a != lag(x$a))
  x$y <- rep(NA, length(x$a))
  x$y[ind] = x$a[ind]
  x$y[1] <- x$a[1]
  #
  return(x$y)
}
#
###
#' Вычисляет пересечения графиков рядов
#'
#' Вычисляются пересечения x1 "снизу-вверх" x2 (точки пробития вверх x2)
#' 
#' @param x1 xts1
#' @param x2 xts2
#'
#' @return x Ряд пересечений
#'
#' @export
CrossLine <- function(x1, x2, eq = FALSE) {
  if (eq == TRUE) {
    x <- diff(x1 >= x2)
  } else {
    x <- diff(x1 > x2)
  }
  x[1] <- 0
  x[x < 0] <- 0
  x <- sign(x)
  #
  return(x)
}
#
#' Надстройка нал CrossLine для удобства
#' @export
CrossLine.up <- function(x1, x2, eq = FALSE) {
  result <- CrossLine(x1, x2, eq)
  #
  return(result)
}
#
#' Надстройка нал CrossLine для удобства
#' @export
CrossLine.down <- function(x1, x2, eq = FALSE) {
  result <- CrossLine(x2, x1, eq)
  #
  return(result)
}
#
###
#' Функция для перехода к состояниям (фильтрация сигналов)
#' 
#' @param data Ряд позиций (data$pos)
#'
#' @return data$state Ряд состояний
#'
#' @export
CalcStates.inData <- function(x) {
  require(quantmod) 
  # ----------
  x <-
    na.locf(x) %>%
    {
      ifelse(is.na(.) | is.nan(.) | is.infinite(.), 0, .)
    } %>%
    xts(., order.by = index(x))
  ind <- which(x != lag(x))
  result <- rep(NA, length(x))
  result[ind] <- x[ind]
  result[1] <- x[1]
  #
  return(result)
}
#
###
#' Генерирует таблицу сделок
#' 
#' @param data Данные
#'
#' @return state.data Данные с рядом состояний  
#'
#' @export
CalcStates.buildStatesTable <- function(data) {
  require(quantmod) 
  # ----------
  state.data <- 
    data %>% CalcStates.inData(.) %>%
    merge(data, .) %>%
    na.omit(.)
  #
  return(state.data)
}
#
###
#' Функция вычисляет return'ы по всему портфелю внутри XTS
#' 
#' @param data XTS 
#' @param type Тип return'a (ret/sret/lret)
#'
#' @return data XTS + return'ы по каждому инструменту 
#'
#' @export
CalcReturn_inXTS <- function(data, price = "Close", type = "sret") {
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
  #
  return(data)
}
#
###
#' Функция расчета equity
#' 
#' @param data Данные с доходностями и позициями
#'
#' @return data Данные + объем(w), относительной прибылью(margin), equity 
#'
#' @export
CalcEquity <- function(data, s0 = 0, abs = FALSE, SR = FALSE, LR = FALSE, reinvest = TRUE, state = FALSE) {
  require(quantmod) 
  # ----------
  # расчет 
  if (state == FALSE) {
      data$state <- data$pos
  }
  if (abs == TRUE) {     
    if (reinvest == TRUE) {
      data$w <- data$state[[1]] * s0/data$Open[[1]]
      data$w <- trunc(data$w)
      data$equity <- s0
      data$margin <- 0
      for (i in 2:nrow(data)) { 
        data$margin[i] <- data$w[[i-1]] * ( data$Open[[i]] - data$Open[[i-1]] )
        data$equity[i] <- (data$equity[[i-1]] + data$margin[[i]])
        data$w[i] <- data$state[[i]] * data$equity[[i]] / data$Open[[i]]
        data$w[i] <- trunc(data$w[i])
      } 
    } else {
      data$w <- 1 
      data$margin <- lag(data$state) * ( data$Open-lag(data$Open) )
      data$margin[1] <- 0
      data$equity <- cumsum(data$margin)
    }
  }
  if (SR == TRUE) {
    if (reinvest == TRUE) {
      data$SR <- lag(data$state) * data$SR
      data$SR[1] <- 0
      data$margin <- cumprod(data$SR + 1) 
      data$margin[1] <- 0
      data$equity <- s0*data$margin
    } else {
      data$SR <- lag(data$state) * data$SR
      data$SR[1] <- 0
      data$margin <- cumprod(data$SR + 1) - 1
      data$equity <- data$Open[[1]] * as.numeric(data$margin)
    }
  }
  if (LR == TRUE) {
    #if (reinvest==TRUE) {
      #
    #} else {
      data$LR <- lag(data$state) * data$LR
      data$LR[1] <- 0
      data$margin <- cumsum(data$LR)
      data$equity <- data$Open[[1]] * (exp(as.numeric(last(data$margin))) - 1)
    #}
  }
  #
  return(data)
}
#
###
#' Функция расчета профита
#'
#' Устаревшая функция 
#' 
#' @param data Данные с equity (в пунктах)
#' @param s0 Начальный баланс
#' @param pip Размер пункта
#'
#' @return profit Итоговый профит
#'
#' @export
CalcProfit <- function(data, s0 = 0, pip, reinvest = TRUE) {
  require(quantmod) 
  # расчет итогового профита
  if (reinvest == TRUE) {
    profit <- as.numeric(last(data$equity / pip) - s0)        
  } else {
    profit <- as.numeric(last(data$equity / pip))    
  }
  #
  return(profit)
}
#
###
#' Функция для расчёта стоимости тиков внутри основного листа данных 
#' 
#' @param data XTS данные котировок (основной лист данных)
#' @param names Список тикеров для конвертирования 
#' @param norm.data Нормировочные данные
#' @param outnames Название столбца для результатов
#' @param convert.to
#' @param tick.val Тиков в шаге цены 
#' @param tick.price Цена тика
#'
#' @return data Основной XTS (нужные данные конвертированы к нужной валюте)
#'
#' @export
NormData_inXTS.price <- function(data, names, norm.data, outnames, convert.to, tick.val, tick.price) {
  x <- norm.data
  for (i in 1:length(names)) {
    temp.text <- paste("data$",outnames[i]," <- ",
                       "NormData.price(data = data$",names[i],",",
                                                   "norm.data = x, convert.to = \"",convert.to,"\",",
                                                   "tick.val = ",tick.val[i],",",
                                                   "tick.price = ", tick.price[i],")",
                       sep = "")
    eval(parse(text = temp.text))  
  }
  #
  return(data)  
}
#
###
#' Расчёт суммарного параметра (согласно весам инструмента в портфеле)
#' 
#' @param data xts с данными корзины
#' @param basket.weights Веса инструментов внутри корзины
#' @param target Ключ поиска нужных столбцов
#'
#' @return data Суммированные данные (столбец)
#'
#' @export
CalcSum_inXTS_byTargetCol.basket <- function(data, basket.weights, target) {
  #require()
  # 
  temp.text <- 
    names(data)[grep(target, names(data))] %>% 
    paste("data$", ., sep = "") %>%
    paste(., basket.weights, sep = " * ", collapse = " + ") %>%
    paste("data <- ", ., sep = "") 
  eval(parse(text = temp.text))
  #
  return(data)
}
#
###
#' Функция добавляет параметры инструментов (для фьючерсов: размеры ГО и курс USDRUB для пересчёта к RUB)
#' 
#' @param data XTS, содержащий нужные данные 
#' @param from.date 
#' @param to.date
#'
#' @return data XTS ряд, с добавленными параметрами
#'
#' @export
AddData_inXTS.futuresSpecs <- function(data, from.date, to.date, dir) {
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
  remove(temp.text)
  remove(data.names)
  # загрузка котировок USDRUB_TOM
  data.USDRUB <- GetData_Ticker_One(ticker = "USD000UTSTOM", from.date, to.date, period = "day", rename = TRUE)
  data$USDRUB <- data.USDRUB$Close
  remove(data.USDRUB)
  data$USDRUB <- na.locf(data$USDRUB)
  # очистка от NA (на данном этапе na.omit полезным данным не навредит)
  data <- na.omit(data)
  setwd(old.dir)
  #
  return(data)
}
#
###
#' Функция для расчёта стоимости тиков
#' 
#' @param data XTS, содержащий нужные данные 
#' @param norm.data Нормировочные данные
#' @param convert.to Валюта конвертирования (USD/RUB)
#' @param tick.val Тиков в шаге цены 
#' @param tick.price Цена тика
#'
#' @return data XTS ряд
#'
#' @export
NormData.price <- function(data, norm.data, convert.to, tick.val, tick.price) {
  if (convert.to == "RUB") {
    data <- (data * tick.price / tick.val) * norm.data
  }
  if (convert.to == "USD") {
    data <- (data * tick.price / tick.val) / norm.data  
  }
  #
  return(data)
}
#
SplitSwitchPosition <- function(data) {    
  ## Точки смены позиций
  data$action <- diff(data$pos)
  data$action[1] <- data$pos[1]
  # индекс строки-переворота
  temp.ind <- index(data[data$action == 2 | data$action == -2])
  if (length(temp.ind) == 0) {
    cat("TestStrategy INFO: No Switch Position there", "\n")
    rm(temp.ind)
  } else {
    cat("TestStrategy INFO:  Split SwitchPosition...", "\n") 
    # temp копия нужных строк (строки начала новой сделки)
    temp <- 
      data[temp.ind] %>% 
      { 
        x <- .
        x$pos <- sign(x$action)  
        # x$state <- sign(x$action)  
        x$action <- abs(sign(x$action))  
        return(x)
      }
    # cтроки предыдущей сделки
    data$pos[temp.ind] <- 0
    # data$state[temp.ind] <- sign(data$action[temp.ind])
    data$action[temp.ind] <- abs(sign(data$action[temp.ind]))
    data$pos.num[temp.ind] <- data$pos.num[temp.ind] - 1
    # правильное заполнение поля $pos.bars
    temp.ind.num <- data[temp.ind, which.i = TRUE]
    data$pos.bars[temp.ind] <- data$pos.bars[temp.ind.num - 1] 
    data <- rbind(data, temp)   
    rm(temp.ind)
  }
  #
  return(data)
}
#
CalcBarsNum <- function(x) {
  result <-   
    # вектор, содержащий номера позиций
    unique(x) %>%
    # нумерация тиков внутри состояний сигналов
    {
      if (length(.) == 1) {
        .
      } else {
        sapply(
          ., 
          function(var) {
            temp <- abs(sign(which(x == var)))
            temp[1] <- 0
            xts(x = cumsum(temp), order.by = index(x[x == var]))
          }
        ) %>% 
        MergeData_inList_byRow(.)
      }
    }
  #
  return(result)
}
#
CalcPosition.num <- function(x) {
  action <- diff(x)
  action[1] <- x[1]
  result <- 
    abs(sign(action)) %>%
    # защита от нумерации позиций "вне рынка"
    {
      abs(sign(x)) * . 
    } %>%
    cumsum(.) %>%
    # защита от нумераций пачек нулевых позиций
    {
      temp <- action
      temp[1] <- 0
      temp <- abs(sign(temp))
      x <- . * sign(temp + abs(x))
      return(x)
    }
  #
  return(result)
}
#
CleanSignal.duplicate <- function(x) {
  result <- 
    diff(x) %>%
    {
      .[1] <- x[1]
      return(.)
    } %>%
    sign(.) %>%
    abs(.) %>%
    {
      x * .
    }
  #
  return(result)
}
#
CalcPosition.byOrders <- function(open, close, FUN) {
  FUN <- match.fun(FUN)
  temp.env <- new.env()
  rows <- length(open)
  ind <- 1:rows
  result <- data.frame(open = integer(rows), close = integer(rows))
  time.ind <- index(open)
  open <- coredata(open)
  close <- coredata(close)
  assign('cache.state', 0, envir = temp.env)
  sapply(ind,
         function(x) {
           cache.state <- get('cache.state', envir = temp.env)
           result <- get('result', envir = temp.env)
           #data[x, ] <- FUN(data, x, ...) 
           temp.open <- ifelse((cache.state == 0) | is.na(cache.state),
                               open[x], 
                               ifelse(close[x] == cache.state,
                                      0,
                                      cache.state))
           temp.close <- ifelse((cache.state != 0) | !is.na(cache.state),
                                ifelse(close[x] == cache.state,
                                       close[x],
                                       0),
                                0)
           cache.state <- temp.open
           result$open[x] <- temp.open
           result$close[x] <- temp.close
           assign('cache.state', temp.open, envir = temp.env)
           assign('result', result, envir = temp.env) 
         })
  result <- get('result', envir = temp.env)
  rm(temp.env)
  result <- xts(result, order.by = time.ind)
  #
  return(result)
}
#