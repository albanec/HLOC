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
  # начальный баланс
  data$balance <- NA
  data$balance[1] <- balance.initial 
  # начальное число синтетических портфельных контрактов
  data$n <- NA
  #
  ## 2.2 Работа с таблицей сделок
  #
  # 2.2.1 скелет таблицы сделок
  cat("STR_TestStrategy INFO:  Build state.table...", "\n")
  data.state <- 
    {
      data.state <- xts() 
      data.state <- data[!is.na(data$state)]
    }
  # 
  # 2.2.1.2 добавление нужных столбцов
  data.state$im.balance <- NA
  data.state$equity <- NA
  data.state$commis <- NA
  data.state$margin <- NA
  #
  # выгрузка return'ов (здесь переходим к return'ам стратегии) и Open'ов 
  # return'ы грузятся по пунктам
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
            # перенос return'ов позиций по инструментам (в пунктах) в data
            "data$",.,".ret <- ", 
            "merge(data, data.source$",.,".ret[data.ind]) %$% ",
            "na.locf(",.,".ret) %>% ", 
            "{",
                ". * lag(data$pos[data.ind])",
            "} ; ",
            "data$",.,".ret[1] <- 0 ; ",              
            # перенос Open'ов в data.state
            "data.state$",.,".Open <- ", 
            "merge(data.state, data.source$",.,".Open[data.state.ind]) %$% ",
            "na.locf(",.,".Open) ; ",
            # расчёт return'ов по сделкам (в пунктах) с учётом проскальзываний
              #"data.state$",.,".ret <- NA ; ",
            "data.state$",.,".ret <- ",
              "(data.state$",.,".Open - lag(data.state$",.,".Open)) * lag(data.state$pos) ; ",
            #    " * data.state$action ; ",  
            "data.state$",.,".ret[1] <- 0 ;", 
            # перенос данных по return'ам на свечах открытия/закрытия позиций в data (в пунктах)
            #"temp <- merge(data$",.,".ret, data.state$",.,".ret[data.state.ind]) ; ",
            #"temp[, 1][which(!is.na(temp[, 2]))] <- temp[, 2][which(!is.na(temp[, 2]))] ; ",
            #"data$",.,".ret <- temp[, 1] ;",
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
  # расчёт для data.state
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
  # расчёт csleep'ов по корзине 
  data.state$SPFB.SI.sleep <- sleeps[1]
  data.state$SPFB.RTS.sleep <- sleeps[2]
  data.state$SPFB.BR.sleep <- sleeps[3]
  data.state <- 
    merge(data.state, data.source$USDRUB[data.state.ind]) %$%
    na.locf(USDRUB) %>%
    STR_NormData_Price_inXTS(data = data.state, 
                             norm.data = ., 
                             names = c("SPFB.RTS.sleep", "SPFB.BR.sleep"), 
                             outnames = c("SPFB.RTS.csleep", "SPFB.BR.csleep"), 
                             tick.val = c(10, 0.01), tick.price = c(0.02, 0.01), 
                             convert.to = "RUB")
  # суммарный csleep в data.state
  data.state$csleep <- STR_CalcSum_Basket_TargetPar_inXTS(data = data.state, target = "csleep", 
                                                          basket.weights = c(1,1,1))
  data.state$SPFB.SI.sleep <- NULL
  data.state$SPFB.RTS.sleep <- NULL
  data.state$SPFB.BR.sleep <- NULL

  #####################################################
  #####################################################
  
  ## 2.2.2 расчёт самих сделок
  for (i in 1:nrow(data.state)) {
    if (i == 1) {
      data.state$balance[1] <- balance.initial
      data.state$im.balance[1] <- 0
      data.state$commis[1] <- 0
      data.state$equity[1] <- 0
      data.state$ret[1] <- 0
      data.state$margin[1] <- 0
    } else {
  

    } 
    
    if (coredata(data.state$balance[i-1]) > 0) {
      if (data.state$pos != 0 & )
      data.state$n[i] <-
        (balance[i - 1] / coredata(data.source$IM[temp.index]) -  * data.state$n[i]) * k.mm * 
        runif(1, 0.6, 1.4) %>%
        round(.)
      data.state$im.balance[i] <- coredata(data.state$n[i]) * data.source$IM[temp.index]
      data.state$balance[i] <- ifelse(data.state$balance[i - 1] - coredata(data.state$im.balance[i])
      data.state$commis[i] <- basket.commis * abs(coredata(data$action[state.index]))
      

      for (n in length(data.names)) {
        temp.text <- paste("data.state$",data.names[i],".Open[temp.index] <- ",
                   "data.source$",data.names[i],".Open[temp.index]" "; ",
      }
    } else {
      data.state$n[i] <- 0
      data.state$im.balance[i] <- 0
      data.state$commis[i] <- 0
    }
    
    
    
    
  }
  
  #data.state$ret[1] <- data.source$ret[index(data.state$ret[1])]
   
    index <- index(data.state[i, ])
    index.lag <- index(data.state[i-1, ])
    data.state$margin[index] <- data.state$pos[index] * data.state$pos[index]
     
  temp.text <- c()
  for (i in 1:length(data.names)) {
    temp.text <- paste("data.state$",data.names[i],".ret <- NA ; ",
               "data.state$",data.names[i],".im.balance <- NA ; ",
               "data.state$",data.names[i],".sleep <- NA ; ",
               "data.state$",data.names[i],".equity <- NA ; ",
               "data.state$",data.names[i],".margin <- NA ; ",
               "data.state$",data.names[i],".n <- NA ; ",
               "data.state$",data.names[i],".Open <- NA ; ",
               "data.state$",data.names[i],".Close <- NA ; ",
               "data.state$",data.names[i],".commis <- NA ; ",
               sep = "")
    eval(parse(text = temp.text))
  }
  #
  # расчёт количества контрактов на сделках
  #
  # цикл расчёта
  for (i in 1:nrow(data.state)) {
    if (i == 1) {
      
      


      for (n in 1:length(data.names)) {
      temp.text <- paste("temp.index <- index(",data.names[n],") ; ",
      data.state$",data.names[1],".n <- data.state$n[1] * ",basket.weights[n]," ; ",
      "data.state$",data.names[n],".commis[1] <- ",commisions[n]," * data.state$",data.names[1],".n ;",
      "data.state$",data.names[n],".sleep[1] <- ",sleeps[n]," ; ",
      "data.state$",data.names[n],".n <- data.state$n[1]*",basket.weights[n]," ; ",
      "data.state$",data.names[n],".Open <- ",
      "data.source$",data.names[n],".Open[index(data.state$",data.names[n],".margin)]"" ; ",
