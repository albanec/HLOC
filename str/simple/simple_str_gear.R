STR_TestStrategy <- function(data.source, tickers = c("SPFB.SI", "SPFB.RTS", "SPFB.BR"),
                             sma.per, add.per,
                             k.mm, initial.balance, basket.weights = c()) {
  require(quantmod)
  #
  # 1 Расчёт и добавление индикаторов, сигналов и позиций (+ прочие хар-ки)
  # (открытие позиции "ОткрПозиПоРынку" ($sig | $pos = 1/-1: long/short); 
  # закрытие позиций "ЗакрПозиПоРынку" рассчитывается с тблице сделок (пункт ;;) )
  #
  data %<>%  
    # 1.1 добавляем индикаторы  (SMA)
    {
      data <- xts()
      cat("STR_TestStrategy INFO:  Calculate SMA with period:  ", sma.per, "\n")
          # тикер-индикатор: SI        
      data$sma <- SMA(data.source$SPFB.SI.Close, sma.per)
      cat("STR_TestStrategy INFO:  Calculate $sig and $pos...", "\n")
      data$sig <- ifelse((data$sma < data.source$SPFB.SI.Close), 1, 
                          ifelse(data$sma > data.source$SPFB.SI.Close, -1, 0))
      return(data)
    } %>%   
    na.omit(.) %>%
    #1.2 т.к. позиции зависят только от SMA, то добавляем их 
    {
      data <- .
      data$pos <- lag(data$sig)
      data$pos[1] <- 0
      return(data)
    } %>%
    # 1.3 расчёт сигналов на изменения внутри позиции "ИзменПоРынку"
    { 
      # 1.3.1 расчет сигналов на сброс лотов ($sig.drop - продажа по рынку)
      data <- .
      cat("STR_TestStrategy INFO:  Calculate $sig.drop...", "\n")
      data$sig.drop <- ifelse((((data$sma > data.source$SPFB.SI.Low) & (data$sig == 1)) | 
                               ((data$sma < data.source$SPFB.SI.High) & (data$sig == -1))) & 
                              (data$sig == data$pos), 
                              1, 0)
      return(data)
    } %>%
    {  
      # 1.3.2 расчет сигналов на добор лотов ($sig.add - докупка по рынку)
      data <- .
      cat("STR_TestStrategy INFO:  Calculate $sig.add...", "\n")
      # точки смены сигнала
      data$diff.sig <- diff(data$sig)
      data$diff.sig[1] <- data$sig[1]
      # нумерация состояний внутри сигналов
      data$sig.num <- 
        abs(sign(data$diff.sig)) %>%
        cumsum(.)
      # ряд позиций и число тиков внутри позиции 
      data$pos.num <- lag(data$sig.num)
      data$pos.num[1] <- 0 
      # выделение сигналов "$sig.add"
      data$sig.add <- 
        # вектор, содержащий номера состояний сигналов
        seq(1:max(data$sig.num)) %>%
        # нумерация тиков внутри состояний сигналов
        sapply(., 
               function(x) {
               xts(x = cumsum(abs(sign(which(data$sig.num == x)))) , 
                   order.by = index(data$sig.num[data$sig.num == x]))
               }) %>% 
        MergeData_inList_byRow(.) %T>%
        {
          # ветвим и проставляем тики позиций (добаляем напрямую в data)
          data$pos.ticks <<- lag(.)
          data$pos.ticks[1] <<- 0
        } %>%
        {
          . %/% add.per
        } %>%        
        {
          sign(.) * abs(sign(diff(.)))
        }
      data$sig.add[1] <- 0  
      return(data)
    } %>%
    # 1.3.3 расчёт позиций drop/add
    {
      data <- .
      cat("STR_TestStrategy INFO:  Calculate $pos.add and $pos.drop...", "\n")
      data$pos.add <- ifelse(lag(data$sig.add) == lag(data$sig.drop), 0, lag(data$sig.add))
      data$pos.add[1] <- 0
      data$pos.drop <- ifelse(lag(data$sig.drop) == lag(data$sig.add), 0, lag(data$sig.drop))
      data$pos.drop[1] <- 0
      return(data)      
    } %>%
    # 1.3.4 нумерация drop/add действий
    {
      data <- . 
      data$pos.add.num <- NA
      data$pos.drop.num <- NA
      data.temp <- 
        seq(1:max(data$sig.num)) %>%
        c(0, .) %>%
        sapply(., 
               function(x) {
                 merge(xts(cumsum(data$pos.add[data$pos.num == x]), 
                           order.by = data$pos.num[data$pos.num == x] %>% index(.)),
                       xts(cumsum(data$pos.drop[data$pos.num == x]), 
                           order.by = data$pos.num[data$pos.num == x] %>% index(.)))
               }) %>%
        MergeData_inList_byRow(.)
      data$pos.add.num <- data.temp$pos.add
      data$pos.drop.num <- data.temp$pos.drop
      # удаляем мусор
      remove(data.temp); #remove(num.vector)
      data$diff.sig <- NULL; data$sig.num <- NULL
      return(data)
    } %>%
    # 1.3.5 ряд  учёта транзакций 
    {
      data <- .
      data$action <- data$pos - lag(data$pos)
      data$action[1] <- 0 
      return(data)
    } %>%
    # 1.4 ряд состояний 
    {
      data <- .
      cat("STR_TestStrategy INFO:  Calculate state column...", "\n")
      data$state <- 
        (data$pos.add != 0 | data$pos.drop != 0) %>%
        {
          data$state <- STR_CalcState_Data(data)
          data$state[.] <- data$pos[.]
          return(data$state)
        }
      return(data)  
    } %>% 
    # 1.5 расщепление переворотов в позициях (расщепление строк с $action = +/-2)
    {
      data <- .
      cat("STR_TestStrategy INFO:  Split SwitchPosition...", "\n")
      temp.ind <- index(data[data$action == 2 | data$action == -2])
      if (length(temp.ind) == 0) {
        cat("No Switch Position there", "\n")
      } else {
        # temp копия нужных строк (строки начала новой сделки)
        temp <- 
          data[temp.ind] %>% 
          { 
            x <- .
            x$pos <- sign(x$action)  
            x$state <- sign(x$action)  
            x$action <- abs(sign(x$action))  
            return(x)
          }
        # cтроки предыдущей сделки
        data %<>% 
          {
            x <- .
            x$pos[temp.ind] <- 0
            x$state[temp.ind] <- -1 * sign(x$action[temp.ind])
            x$action[temp.ind] <- abs(sign(x$action[temp.ind]))
            x$pos.num[temp.ind] <- x$pos.num[temp.ind] - 1
            return(x)
          }
        data <- rbind(data, temp)   
      }
      return(data)
    } 
  #
  ## 2.расчет экономических параметров
  #
  ## 2.1 выгрузка данных
  # вектор инсрументов внутри портфеля
  data.names <- 
    grep(".Close", names(data.source)) %>%
    names(data.source)[.] %>%
    sub(".Close", "", .)
  data.ind <- index(data)
  #
  ## 2.2 Работа с таблицей сделок
  #
  # 2.2.1 скелет таблицы сделок
  cat("STR_TestStrategy INFO:  Build state.table...", "\n")
  data.state <- 
    {
      data.state <- xts() 
      data.state <- data[!is.na(data$state)]
    } %>% 
    {
      .$pos[nrow(.$pos)] <- 0
      return(.)
    }
  # 
  # 2.2.1.2 добавление нужных столбцов
  #
  # выгрузка Open'ов и расчёт return'ов (здесь переходим к return'ам стратегии)  
  # котировки берём из data.source
  cat("STR_TestStrategy INFO:  Loading Returns from source to StartegyData...", "\n")
  # индексы строк data.state
  data.state.ind <- index(data.state)
  # расчёт return'ов позиций и состояний
  for (i in 1:length(data.names)) {  
    temp.text <- 
      data.names[i] %>%
      {
        t <- paste(             
          # перенос Open'ов в data.state (в пунктах) с учётом проскальзываний
          "data.state$",.,".Open <- ", 
            "merge(data.state, data.source$",.,".Open[data.state.ind]) %$% ",
            "na.locf(",.,".Open) %>% 
            { . - sleeps[i] * data.state$state } ; ",
          # перенос Open'ов в data 
          "data$",.,".Open <- ", 
            "merge(data, data.source$",.,".Open[data.ind]) %$% ",
            "na.locf(",.,".Open) ; ",  
          # перенос данных по Open'ам на свечах изменения позиций (в пунктах) в data
          "temp <- merge(data$",.,".Open, data.state$",.,".Open[data.state.ind]) ; ",
          "temp[, 1][which(!is.na(temp[, 2]))] <- temp[, 2][which(!is.na(temp[, 2]))] ; ",
          "data$",.,".Open <- temp[, 1] ;",  
          # расчёт return'ов по сделкам (в пунктах) в data.state 
          "data.state$",.,".ret <- ",
            "(data.state$",.,".Open - lag(data.state$",.,".Open)) * lag(data.state$pos) ; ",  
          "data.state$",.,".ret[1] <- 0 ;",
          # расчёт return'ов по позициям (в пунктах) в data 
          "data$",.,".ret <- ",
            "(data$",.,".Open - lag(data$",.,".Open)) * lag(data$pos) ; ",  
          "data$",.,".ret[1] <- 0 ;",
          sep = "")
        return(t)
      } 
    eval(parse(text = temp.text))  
    remove(temp.text)
    remove(temp)
    cat("STR_TestStrategy INFO:  Loading Returns from source to StartegyData:  ",data.names[i],  "OK", "\n")       
  }
  # расчёт cret по инструментам в data и data.state
  cat("STR_TestStrategy INFO:  CalcCRet for Data", "\n")
  # для Si всё просто
  data$SPFB.SI.cret <- data$SPFB.SI.ret
  data.state$SPFB.SI.cret <- data.state$SPFB.SI.ret 
  # расчёт для data
  # добавление курса
  data <- 
    merge(data, data.source$USDRUB[data.ind]) %$%
    na.locf(USDRUB) %>%
    # расчёт cret по инструментам
    STR_NormData_Price_inXTS(data = data, 
                             norm.data = ., 
                             names = c("SPFB.RTS.ret", "SPFB.BR.ret"), 
                             outnames = c("SPFB.RTS.cret", "SPFB.BR.cret"), 
                             tick.val = c(10, 0.01), tick.price = c(0.02, 0.01), 
                             convert.to = "RUB")
  # суммарный cret в data
  data$cret <- STR_CalcSum_Basket_TargetPar_inXTS(data = data, target = "cret", basket.weights)
  # расчёт суммарного cret для data.state
  cat("STR_TestStrategy INFO:  CalcCRet for Data.State", "\n")
  data.state <- 
    merge(data.state, data.source$USDRUB[data.state.ind]) %$%
    na.locf(USDRUB) %>%
    # расчёт cret по инструментам
    STR_NormData_Price_inXTS(data = data.state, 
                             norm.data = ., 
                             names = c("SPFB.RTS.ret", "SPFB.BR.ret"), 
                             outnames = c("SPFB.RTS.cret", "SPFB.BR.cret"), 
                             tick.val = c(10, 0.01), tick.price = c(0.02, 0.01), 
                             convert.to = "RUB")
  # суммарный cret в data.state
  data.state$cret <- STR_CalcSum_Basket_TargetPar_inXTS(data = data.state, target = "cret", basket.weights)
  #
  #####################################################
  #####################################################
  # начальный баланс
  data.state$balance <- NA
  data.state$im.balance <- NA
  # начальное число синтетических портфельных контрактов
  data.state$n <- NA
  data.state$diff.n <- NA
  #
  data.state$margin <- NA
  data.state$commiss <- NA
  data.state$equity <- NA
  #
  ## 2.2.2 расчёт самих сделок
  for (n in 1:nrow(data.state)) {
    if (n == 1) {
      data.state$balance[1] <- balance.initial
      data.state$im.balance[1] <- 0
      data.state$commiss[1] <- 0
      data.state$margin[1] <- 0
      data.state$diff.n[1] <- 0
      data.state$n[1] <- 0
      data.state$equity[1] <- 0
    } else {
      temp.index <- index(data.state$state[n])
      data.state$margin[n] <- 
        data.state$cret[[n]] * data.state$n[[n - 1]]
      if (data.state$pos[n] == 0) {
        data.state$n[n] <- 0
      } else {
        if ((data.state$pos.add[n] + data.state$pos.drop[n]) == 0) {
          data.state$n[n] <-
            {
              data.state$balance[[n - 1]] * k.mm / 
              coredata(data.source$IM[temp.index]) * 
              runif(1, 0.6, 1.4) 
            } %>%
            round(.)
        } else {
          if (data.state$pos.add[n] == 1) {
            data.state$n[n] <- 
              {
                1.5 * data.state$n[[n - 1]]
              } %>%
              round(.)
          }
          if (data.state$pos.drop[n] == 1) {
            data.state$n[n] <- 
              {
                0.5 * data.state$n[[n - 1]]
              } %>%
              round(.)
          }
        }   
      }
      data.state$diff.n[n] <- data.state$n[[n]] - data.state$n[[n - 1]]
      data.state$im.balance[n] <- data.state$n[[n]] * coredata(data.source$IM[temp.index])
      data.state$commiss[n] <- basket.commiss * abs(data.state$diff.n[[n]])
      data.state$balance[n] <- 
        data.state$balance[[n - 1]] + data.state$margin[[n]] + 
        data.state$im.balance[[n - 1]] - data.state$im.balance[[n]] - 
        data.state$commiss[[n]]
    }
  }
  data.state$equity <- cumsum(data.state$margin)
  #
  data$n <- 
    merge(data, data.state$n) %$%
    na.locf(n)
  #
  data$margin <- 
    merge(data, data.state$commiss) %>%
    {
      lag(data$n) * data$cret
    } %>%
    {
      .[1, ] <- 0
      return(.)
    }
  #
  data$equity <- cumsum(data$margin)
  #
  for (i in 1:length(data.names)) {
    temp.text <- 
      data.names[i] %>%
      {
        t <- paste(
          "data.state$",.,".n <- data.state$n * ",basket.weights[i],"; ",
          "data$",.,".n <- ", 
            "merge(data, data.state$",.,".n) %$% ",
            "na.locf(",.,".n) ; ",
          "data$",.,".margin <- ",
            "data$",.,".cret * lag(data$",.,".n) ; ",
          "data$",.,".margin[1] <- 0 ; ",
          "data$",.,".equity <- cumsum(data$",.,".margin) ;",
           sep = "")
        return(t)
      }      
    eval(parse(text = temp.text))
  }    
  return(list(data, data.state))    
}   
  