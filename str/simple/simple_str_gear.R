STR_TestStrategy <- function(data.source, tickers = c("SPFB.SI", "SPFB.RTS", "SPFB.BR"),
                             sma.per, add.per,
                             k.mm, initial.balance, basket.weights = c()) {
  require(quantmod)
  #
  # 1 Расчёт и добавление индикаторов, сигналов и позиций (+ прочие хар-ки)
  # (открытие позиции "ОткрПозиПоРынку" ($sig | $pos = 1/-1: long/short); 
  # закрытие позиций "ЗакрПозиПоРынку" рассчитывается с таюлице сделок (пункт ;;) )
  data %<>%  
    # добавляем индикаторы  (SMA)
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
    # т.к. позиции зависят только от SMA, то добавляем их 
    {
      data <- .
      data$pos <- lag(data$sig)
      data$pos[1] <- 0
      return(data)
    } %>%
    ## расчёт сигналов на изменения внутри позиции "ИзменПоРынку"
    { 
      # расчет сигналов на сброс лотов ($sig.drop - продажа по рынку)
      data <- .
      cat("STR_TestStrategy INFO:  Calculate $sig.drop...", "\n")
      data$sig.drop <- ifelse((((data$sma > data.source$SPFB.SI.Low) & (data$sig == 1)) | 
                               ((data$sma < data.source$SPFB.SI.High) & (data$sig == -1))) & 
                              (data$sig == data$pos), 
                              1, 0)
      return(data)
    } %>%
    {  
      # расчет сигналов на добор лотов ($sig.add - докупка по рынку)
      cat("STR_TestStrategy INFO:  Calculate $sig.add...", "\n")
      data <- .
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
    # расчёт позиций drop/add
    {
      cat("STR_TestStrategy INFO:  Calculate $pos.add and $pos.drop...", "\n")
      data <- .
      data$pos.add <- ifelse(lag(data$sig.add) == lag(data$sig.drop), 0, lag(data$sig.add))
      data$pos.add[1] <- 0
      data$pos.drop <- ifelse(lag(data$sig.drop) == lag(data$sig.add), 0, lag(data$sig.drop))
      data$pos.drop[1] <- 0
      return(data)      
    } %>%
    # нумерация drop/add действий
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
                           order.by = index(data$pos.num[data$pos.num == x])),
                       xts(cumsum(data$pos.drop[data$pos.num == x]), 
                           order.by = index(data$pos.num[data$pos.num == x])))
               }) %>%
        MergeData_inList_byRow(.)
      data$pos.add.num <- data.temp$pos.add
      data$pos.drop.num <- data.temp$pos.drop
      # удаляем мусор
      remove(data.temp); #remove(num.vector)
      data$diff.sig <- NULL; data$sig.num <- NULL
      return(data)
    } %>%
    #ряд транзакций 
    {
      data <- .
      data$action <- data$pos - lag(data$pos)
      data$action[1] <- 0 
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
  # ряд состояний в основную таблицу
  cat("STR_TestStrategy INFO:  Calculate state column...", "\n")
  data$state <- 
    (data$pos.add != 0 | data$pos.drop != 0) %>%
    {
      data$state <- STR_CalcState_Data(data)
      data$state[.] <- data$pos[.]
      return(data$state)
    }
  # скелет таблицы сделок
  cat("STR_TestStrategy INFO:  Build state.table...", "\n")
  data.state <- data[!is.na(data$state)]
  # 
  # 2.2.1.1 расщепление переворотов в сделках
  data.state %<>%    
    {
      cat("STR_TestStrategy INFO:  Split SwitchPosition...", "\n")
      temp.ind <- index(data.state[data.state$action == 2 | data.state$action == -2])
      if (length(temp.ind) == 0) {
        cat("No Switch Position there", "\n")
      } else {
        # temp копия нужных строк (строки начала новой сделки)
        temp <- 
          data.state[temp.ind] %>% 
          { 
            x <- .
            x$pos <- sign(x$action)  
            x$state <- sign(x$action)  
            x$action <- sign(x$action)  
            return(x)
          }
        # cтроки предыдущей сделки
        data.state %<>% 
          {
            x <- .
            x$pos[temp.ind] <- 0
            x$state[temp.ind] <- -1 * sign(x$action[temp.ind])
            x$action[temp.ind] <- -1 * sign(x$action[temp.ind])
            x$pos.num[temp.ind] <- x$pos.num[temp.ind] - 1
            return(x)
          }
        data.state <- rbind(data.state, temp)   
      }
      return(data.state)
    } 
  #  
  # 2.2.1.2 добавление нужных столбцов
  data.state$im.balance <- NA
  data.state$equity <- NA
  data.state$commis <- NA
  data.state$ret <- NA
  data.state$margin <- NA
  #
  # выгрузка return'ов (здесь переходим к return'ам стратегии) и Open'ов 
  # return'ы грузятся как по пунктам, так и приведенные к деньгам
  # котировки берём из data.source
  cat("STR_TestStrategy INFO:  Loading Returns from source to StartegyData...", "\n")
  # индексы строк data.state
  data.state.ind <- index(data.state)
  temp.ind <- 
    (data.state$pos != 0) %>% 
    data.state[.] %>%
    index
  #
  for (i in 1:length(data.names)) {
    temp.text <- 
      data.names[i] %>%
      {
        paste("data$",.,".ret <- NA ; "
              "data$",.,".ret <- data.source$",.,".ret[data.ind] * data$pos ; ",
              "data$",.,".cret <- NA ; "
              "data$",.,".cret <- data.source$",.,".cret[data.ind] * data$pos ; ",
              "data.state$",.,".Open <- NA ; ",
              "data.state$",.,".Open[data.state.ind] <- ",
                "data.source$",.,".Open[data.state.ind] ; ",
              "data.state$",.,".Close <- NA ; ",
              "data.state$",.,".Close[data.state.ind] <- ",
                "data.source$",.,".Close[data.state.ind] ; ",
              "data.state$",.,".ret <- NA ; "
              "data.state$",.,".ret <- ",
                "(data.state$",.,".Open - lag(data.state$",.,".Open)) * data.state$pos - ",sleeps[i]," ; ",  
              "data.state$",.,".ret[1] <- 0 ;", 
              "data$",.,".ret[temp.ind] <- ",
                "(data.state$",.,".Close[temp.ind] - data.state$",.,".Open) * data.state$pos ; ",
              sep = "")
      }
    eval(parse(text = temp.text))  
  }
  #
  # выгрузка cret
  data$cret <- data.source$cret[temp.ind]
  data.state %<>% 
    {
      data.state <- .
      data.state$SPFB.SI.cret <- data.state$SPFB.SI.ret
      data.state <- STR_NormData_Price_inXTS(data = data.state, 
                                             names = c("SPFB.RTS.ret", "SPFB.BR.ret"),
                                             norm.data = data.source.list[[1]]$USDRUB, 
                                             outnames = c("SPFB.RTS.cret", "SPFB.BR.cret"), 
                                             tick.val = c(10, 0.01), tick.price = c(0.02, 0.01), 
                                             convert.to = "RUB")
      data.state$cret <- STR_CalcSum_Basket_TargetPar_inXTS(data = data.state, 
                                                            target = "cret", basket.weights)
      return(data.state)
    }

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
      ind <- index(data.state$n[i])    
      # заполнение параметров по инструментам

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
    }
    }
    
    if ()
  }
  # деньги, зарезервированные под ГО
  
  # расчёт проскальзываний (по каждому из инструментов)

  #data.state$sleep <- runif(1, data.source$[index(data.state)], 1.4)




   #   data$diff.pos != 0, round(abs(data$balance * k.mm / data.source$IM[1]) * runif(1, 0.6, 1.4)),
  #  data$pos != 0 & data$pos.drop == 1, 1,5)

  #data$balance <- NA
  #data$balance[1] <- initial.balance
  #n.open <- initial.balance * k.mm / 
   
  #
   
  # расчёт 
  

  
  #data.source$im <- 
}"