STR_TestStrategy <- function(data.source, tickers = c("SPFB.SI", "SPFB.RTS", "SPFB.BR"),
                             sma.per, add.per,
                             k.mm, initial.balance, basket.weights = c()) {
    require(quantmod)
    #
    ## 1 расчет сигналов и позиций
    ## 1.1 расчёт сигналов на: открытие позиции "ОткрПозиПоРынку" ($sig = 1/-1: long/short)
    #                          закрытие позиций "ЗакрПозиПоРынку" в %%%
    data <- xts()
    # добавляем индикаторы  (SMA)
    # тикер-индикатор: SI
    cat("Calculate SMA with period:  ", sma.per, "\n")
    data$sma <- SMA(data.source$SPFB.SI.Close, sma.per)
    cat("Calculate $sig and $pos...", "\n")
    data$sig <- ifelse((data$sma < data.source$SPFB.SI.Close), 1, 
                       ifelse(data$sma > data.source$SPFB.SI.Close, -1, 0))
    data <- na.omit(data)
    # т.к. открытия зависят только от SMA, то добавляем их 
    data$pos <- lag(data$sig)
    data$pos[1] <- 0
    #
    ## 1.2 расчёт сигналов на изменения внутри позиции "ИзменПоРынку"
    ## 1.2.1 сигналы на сброс лотов ($sig.drop - продажа по рынку)
    cat("Calculate $sig.drop...", "\n")
    data$sig.drop <- ifelse((((data$sma > data.source$SPFB.SI.Low) & (data$sig == 1)) | 
                            ((data$sma < data.source$SPFB.SI.High) & (data$sig == -1))) & 
                            (data$sig == data$pos), 
                            1, 0)
    #
    ## 1.2.2 расчет сигналов на добор лотов ($sig.add - докупка по рынку)
    # точки смены сигнала
    data$diff.sig <- diff(data$sig)
    data$diff.sig[1] <- data$sig[1]
    # нумерация состояний внутри сигналов
    data$sig.num <- cumsum(abs(sign(data$diff.sig)))
    # вектор, содержащий номера состояний сигналов
    num.vector <- seq(1:max(data$sig.num))
    # нумерация тиков внутри состояний сигналов
    data.temp <- list()
    data.temp <- sapply(num.vector, 
                        function(x, y) {
                            xts(cumsum(abs(sign(which(data$sig.num == x)))), 
                                order.by = index(data$sig.num[data$sig.num == x]))
                        })
    data$sig.ticks <- NA
    data$sig.ticks <- MergeData_inList_byRow(data.temp)
    # ряд позиций и число тиков внутри позиции 
    data$pos.num <- lag(data$sig.num)
    data$pos.num[1] <- 0
    data$pos.ticks <- lag(data$sig.ticks)
    data$pos.ticks[1] <- 0
    # удаляем мусор
    remove(data.temp); data$sig.num <- NULL
    # выделение сигналов "$sig.add"
    data$sig.add <- data$sig.ticks %/% add.per
    data$sig.add <- sign(data$sig.add) * abs(sign(diff(data$sig.add)))
    data$sig.add[1] <- 0
    # 
    ## 1.3 расчёт позиций drop/add
    data$pos.add <- ifelse(lag(data$sig.add) == lag(data$sig.drop), 0, lag(data$sig.add))
    data$pos.add[1] <- 0
    data$pos.drop <- ifelse(lag(data$sig.drop) == lag(data$sig.add), 0, lag(data$sig.drop))
    data$pos.drop[1] <- 0
    # нумерация drop/add действий
    data$pos.add.num <- NA
    data$pos.drop.num <- NA
    data.temp <- list()
    num.vector <- c(0, num.vector)
    data.temp <- sapply(num.vector, 
                        function(x, y) {
                            merge(xts(cumsum(data$pos.add[data$pos.num == x]), 
                                      order.by = index(data$pos.num[data$pos.num == x])),
                                  xts(cumsum(data$pos.drop[data$pos.num == x]), 
                                      order.by = index(data$pos.num[data$pos.num == x])))
                        })
    data.temp <- MergeData_inList_byRow(data.temp)
    data$pos.add.num <- data.temp$pos.add
    data$pos.drop.num <- data.temp$pos.drop
    # удаляем мусор
    remove(data.temp); #remove(num.vector)
    data$diff.sig <- NULL; data$sig.ticks <- NULL; data$sig.ticks <- NULL 
    #
    ## 1.4 ряд транзакций 
    data$action <- data$pos - lag(data$pos)
    data$action[1] <- 0 
    #
    ## 2.расчет экономических параметров
    #
    ## 2.1 расчет начальных условий
    # вектор инсрументор внутри портфеля
    data.names <- names(data.source)[grep(".Close", names(data.source))]
    data.names <- sub(".Close", "", data.names)
    # выгрузка return'ов и Open'ов 
    # начальный баланс
    data$balance <- NA
    data$balance[1] <- balance.initial 
    # начальное число синтетических портфельных контрактов
    data$n  <- NA
    #
    # скелет таблицы сделок
    data$state <- STR_CalcState_Data(data)
    #data.state <- data.state[-1, ]
    data$state[data$pos.add != 0 | data$pos.drop != 0] <- data$pos[data$pos.add != 0 | data$pos.drop != 0]
    data.state <- data[!is.na(data.state)]
    # добавление нужных столбцов
    data.state$im.balance <- NA
    data.state$equity <- NA
    data.state$comiss <- NA
    data.state$ret <- NA
    data.state$margin <- NA
    #
    for (i in 1:nrow(data.state)) {
        if (i == 1) {
            data.state$balance[1] <- balance.initial
            data.state$im.balance[1] <- 0
            data.state$comiss[1] <- 0
        } 
        temp.index <- index(data.state$n[i])    
        if (coredata(data.state$balance[i-1]) > 0) {
            data.state$n[i] <- round(abs(balance[i - 1] * k.mm / coredata(data.source$IM[temp.index])) * 
                                         runif(1, 0.6, 1.4))
            data.state$im.balance[i] <- coredata(data.state$n[i]) * data.source$IM[temp.index]
            data.state$balance[i] <- ifelse(data.state$balance[i - 1] - coredata(data.state$im.balance[i])
            data.state$comiss[i] <- basket.comiss * abs(coredata(data$action[state.index]))
            for (n in length(data.names)) {
                temp.text <- paste("data.state$",data.names[i],".Open[temp.index] <- ",
                                   "data.source$",data.names[i],".Open[temp.index]" "; ",
            }
        } else {
            data.state$n[i] <- 0
            data.state$im.balance[i] <- 0
            data.state$comiss[i] <- 0
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
                           "data.state$",data.names[i],".comiss <- NA ; ",
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
            "data.state$",data.names[n],".comiss[1] <- ",comissions[n]," * data.state$",data.names[1],".n ;",
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




   #     data$diff.pos != 0, round(abs(data$balance * k.mm / data.source$IM[1]) * runif(1, 0.6, 1.4)),
    #    data$pos != 0 & data$pos.drop == 1, 1,5)

    #data$balance <- NA
    #data$balance[1] <- initial.balance
    #n.open <- initial.balance * k.mm / 
   
    #
   
    # расчёт 
    

    
    #data.source$im <- 
}"