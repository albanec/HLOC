TestStrategy_gear <- function(data.source,
                             sma.per, add.per, 
                             k.mm, balance.start, 
                             basket.weights, sleeps, commissions) {
  # ----------
  # Общее описание:
  # тестовый робот
  # Входные данные:
  # data.souce - с котировками
  # sma.per, add.per - периоды SMA и докупок
  # k.mm, balance.start - коэффициент MM и стартовый баланс
  # basket.weights, sleeps, commissions - параметры корзины (веса, слипы и комиссии)
  # Выходные данные:
  # list(data, data.state) - лист с данными отработки и данные сделок
  # Зависимости:
  require(quantmod)
  # ----------
  # 1 Расчёт и добавление индикаторов, сигналов и позиций (+ прочие хар-ки)
  # (открытие позиции "ОткрПозиПоРынку" ($sig | $pos = 1/-1: long/short); 
  # закрытие позиций "ЗакрПозиПоРынку" рассчитывается с тблице сделок (пункт ;;) )
  #
  # вектор имён инсрументов внутри торгуемой корзины
  data.names <- 
    grep(".Close", names(data.source)) %>%
    names(data.source)[.] %>%
    sub(".Close", "", .)
  #
  # расчёт суммарной комиссии по корзине
  basket.commiss <- sum(basket.weights * commissions)
  #
  cat("STR_TestStrategy INFO:  Start TestStrategy with parameters:", "\n",
      "    TickersInBasket:     ",data.names, "\n",
      "    SMA period:          ",sma.per, "\n",
      "    PositionADD period:  ",add.per, "\n",
      "    MM kofficient:       ",k.mm, "\n",
      "    Start Balance:       ",balance.start, "\n",
      "    BasketWeights:       ",basket.weights, "\n")
  # 
  cat("STR_TestStrategy INFO:  Start StrategyData Calculation...", "\n")
  # Расчёт индикаторов и позиций
  data %<>%  
    # 1.1 Добавляем индикаторы  (SMA) и позиции по корзине
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
    # 1.2 т.к. позиции корзины зависят только от SMA, то добавляем их 
    {
      data <- .
      data$pos <- lag(data$sig)
      data$pos[1] <- 0
      return(data)
    } %>%
    # позиции по каждому из инструментов корзины описаны позднее
      # в этой стратегии позиции по BR обратны позициям по Si (т.к. инструменты обратно коррелированы)
    #
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
            # x$state <- sign(x$action)  
            x$action <- abs(sign(x$action))  
            return(x)
          }
        # cтроки предыдущей сделки
        data %<>% 
          {
            x <- .
            x$pos[temp.ind] <- 0
            # x$state[temp.ind] <- sign(x$action[temp.ind])
            x$action[temp.ind] <- abs(sign(x$action[temp.ind]))
            x$pos.num[temp.ind] <- x$pos.num[temp.ind] - 1
            return(x)
          }
        data <- rbind(data, temp)   
      }
      return(data)
    } 
  #
  ## 2.расчет результатов отработки робота
  #
  ## 2.1 выгрузка данных по инструментам
  # индексы данных (строк) data
  data.ind <- index(data)
  #
  # 2.1.2 скелет таблицы сделок
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
  # 2.1.3 добавление нужных исходных данных в data и data.state 
  #
  # Выгружаем и рассчитываем уникальные данные по инструментам (Open, ret, cret, pos)
    # все действия проходят внутри одного цикла перебера имён инструментов
  #
  # 2.1.3.1 выгрузка Open'ов и расчёт return'ов (здесь переходим к return'ам стратегии)  
    # котировки берём из data.source
  cat("STR_TestStrategy INFO:  Loading Tickers Open from data.source...", "\n")
  # индексы строк data.state
  data.state.ind <- index(data.state)
  #
  temp.vector <- c(1, 1, -1)
  #
  # расчёт return'ов позиций и состояний
  cat("STR_TestStrategy INFO:  CalcReturns for data & data.state...","\n")
  for (i in 1:length(data.names)) {  
    temp.text <- 
      data.names[i] %>%
      {
        t <- paste(             
          # перенос Open'ов в data.state (в пунктах) с учётом проскальзываний
          "data.state$",.,".Open <- ", 
            "merge(data.state, data.source$",.,".Open[data.state.ind]) %$% ",
            "na.locf(",.,".Open) %>% 
            { . + sleeps[i] * data.state$state } ; ",
          # перенос Open'ов в data 
          "data$",.,".Open <- ", 
            "merge(data, data.source$",.,".Open[data.ind]) %$% ",
            "na.locf(",.,".Open) ; ",  
          # перенос данных по Open'ам на свечах изменения позиций (в пунктах) в data
          "temp <- merge(data$",.,".Open, data.state$",.,".Open[data.state.ind]) ; ",
          "temp[, 1][which(!is.na(temp[, 2]))] <- temp[, 2][which(!is.na(temp[, 2]))] ; ",
          "data$",.,".Open <- temp[, 1] ;",  
          # расчёт позиций по инструментам корзины в data.state
          "data.state$",.,".pos <- data.state$pos * temp.vector[i] ; ",
          # расчёт return'ов по сделкам (в пунктах) в data.state 
          "data.state$",.,".ret <- ",
            "(data.state$",.,".Open - lag(data.state$",.,".Open)) * lag(data.state$",.,".pos) ; ",  
          "data.state$",.,".ret[1] <- 0 ;",
          # расчёт позиций по инструментам корзины в data
          "data$",.,".pos <- data$pos * temp.vector[i] ; ",           
          # расчёт return'ов по позициям (в пунктах) в data 
          "data$",.,".ret <- ",
            "(data$",.,".Open - lag(data$",.,".Open)) * lag(data$",.,".pos) ; ",  
          "data$",.,".ret[1] <- 0 ;",
          sep = "")
        return(t)
      } 
    eval(parse(text = temp.text))  
    remove(temp.text)
    remove(temp)
    cat("STR_TestStrategy INFO:  CalcReturns for data & data.state:  ",data.names[i],  "OK", "\n")       
  }
  #
  # 2.1.3.2 Расчёт cret
  # расчёт cret по инструментам в data и data.state
  cat("STR_TestStrategy INFO:  CalcCRet for data...", "\n")
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
  cat("STR_TestStrategy INFO:  CalcCRet for data.state", "\n")
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
  # 2.1.4 Начальные параметры для расчёта сделок
  # начальный баланс
  data.state$balance <- NA
  data.state$im.balance <- NA
  # начальное число синтетических контрактов корзины
  data.state$n <- NA
  # прочее
  data.state$diff.n <- NA
  data.state$margin <- NA
  data.state$commiss <- NA
  data.state$equity <- NA
  #
  ## 2.2 Расчёт самих сделок
  #
  cat("STR_TestStrategy INFO:  Start Calculation Deals...", "\n")
  for (n in 1:nrow(data.state)) {
    # на первой строке рассчитываются стартовые значения
    if (n == 1) {
      data.state$balance[1] <- balance.start
      data.state$im.balance[1] <- 0
      data.state$commiss[1] <- 0
      data.state$margin[1] <- 0
      data.state$diff.n[1] <- 0
      data.state$n[1] <- 0
      data.state$equity[1] <- 0
    } else {
      # основной расчёт
      # индекс строки
      temp.index <- index(data.state$state[n])
      # расчёт вариационки
      data.state$margin[n] <- 
        data.state$cret[[n]] * data.state$n[[n - 1]]
      # расчёт количества контрактов на такте
      # если закрытие позиции, то контрактов ноль
      if (data.state$pos[n] == 0) {
        data.state$n[n] <- 0
      } else {
        # если открытие позиции, то
        if ((data.state$pos.add[n] + data.state$pos.drop[n]) == 0) {
          data.state$n[n] <-
            {
              data.state$balance[[n - 1]] * k.mm / 
              coredata(data.source$IM[temp.index]) * 
              runif(1, 0.6, 1.4) 
            } %>%
            round(.)
        } else {
          # если докупка
          if (data.state$pos.add[n] == 1) {
            data.state$n[n] <- 
              {
                1.5 * data.state$n[[n - 1]]
              } %>%
              round(.)
          }
          # если сброс
          if (data.state$pos.drop[n] == 1) {
            data.state$n[n] <- 
              {
                0.5 * data.state$n[[n - 1]]
              } %>%
              round(.)
          }
        }   
      }
      # изменение контрактов на такте
      data.state$diff.n[n] <- data.state$n[[n]] - data.state$n[[n - 1]]
      # расчёт баланса, заблокированного на ГО
      data.state$im.balance[n] <- data.state$n[[n]] * coredata(data.source$IM[temp.index])
      # комиссия на такте
      data.state$commiss[n] <- basket.commiss * abs(data.state$diff.n[[n]])
      # баланс на такте
      data.state$balance[n] <- 
        data.state$balance[[n - 1]] + data.state$margin[[n]] + 
        data.state$im.balance[[n - 1]] - data.state$im.balance[[n]] - 
        data.state$commiss[[n]]
    }
  }
  cat("STR_TestStrategy INFO:  Calculation Deals    OK", "\n")
  #
  # расчёт equity по корзине в data.state
  data.state$equity <- cumsum(data.state$margin - data.state$commiss)
  #
  data %<>%  
    # перенос данных по количеству контрактов корзины в data
    {
      merge(., data.state$n) %>%
      {
        data <- .
        data$n <- na.locf(data$n)
        return(data)
      }
    } %>%
    # расчёт вариационки в data
    {
      merge(., data.state$commiss) %>%
      {
        data <- .
        data$commiss[is.na(data$commiss)] <- 0
        data$margin <- lag(data$n) * data$cret
        data$margin[1] <- 0
        return(data)
      } 
    }  
  #
  # расчёт equity по корзине в data 
  data$equity <- cumsum(data$margin - data$commiss)
  #
  # расчёт n, margin и equity по инструментам в data и data.state 
  for (i in 1:length(data.names)) {
    temp.text <- 
      data.names[i] %>%
      {
        t <- paste(
          # расчёт для data.state 
          "data.state$",.,".n <- data.state$n * ",basket.weights[i],"; ",
          "data.state$",.,".diff.n <- diff(data.state$",.,".n) ; ",
          "data.state$",.,".diff.n[1] <- 0 ; ",
          "data.state$",.,".commiss <- commissions[i] * abs(data.state$",.,".diff.n) ; ",
          "data.state$",.,".margin <- ",
            "data.state$",.,".cret * lag(data.state$",.,".n) ; ",
          "data.state$",.,".margin[1] <- 0 ; ",
          "data.state$",.,".equity <- cumsum(data.state$",.,".margin - data.state$",.,".commiss) ;",
          # расчёт для data  
          "data$",.,".n <- ", 
            "merge(data, data.state$",.,".n) %$% ",
            "na.locf(",.,".n) ; ",
          "data$",.,".margin <- ",
            "data$",.,".cret * lag(data$",.,".n) ; ",
          "data$",.,".margin[1] <- 0 ; ",
          "data <- merge(data, data.state$",.,".commiss) ; ",
          "data$",.,".commiss[is.na(data$",.,".commiss)] <- 0 ; ",
          "data$",.,".equity <- cumsum(data$",.,".margin - data$",.,".commiss) ;",
           sep = "")
        return(t)
      }      
    eval(parse(text = temp.text))
  }    
  return(list(data, data.state))    
}   
  