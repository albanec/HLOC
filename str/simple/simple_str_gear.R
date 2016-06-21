STR_TestStrategy <- function(data.source, tickers = c("SPFB.SI", "SPFB.RTS", "SPFB.BR"),
                             sma.per, add.per,
                             k.mm, initial.balance, basket.weights = c()) {
    require(quantmod)
    # тикер-индикатор: SI
    # основа для данных стратегии
    data <- xts()
    #
    # добавляем индикаторы
    cat("Calculate SMA with period:  ", sma.per, "\n")
    data$sma <- SMA(data.source$SPFB.SI.Close, sma.per)
    cat("Calculate $sig and $pos...", "\n")
    data$sig <- ifelse((data$sma < data.source$SPFB.SI.Close), 1, 
                       ifelse(data$sma > data.source$SPFB.SI.Close, -1, 0))
    data <- na.omit(data)
    #
    # позиции зависят только от SMA
    data$pos <- lag(data$sig)
    data$pos[1] <- 0
    #
    # сигналы на сброс позиций ($sig.drop)
    cat("Calculate $sig.drop...", "\n")
    data$sig.drop <- ifelse((((data$sma > data.source$SPFB.SI.Low) & (data$sig == 1)) | 
                            ((data$sma < data.source$SPFB.SI.High) & (data$sig == -1))) & 
                            (data$sig == data$pos), 
                            1, 0)
    #
    # расчет сигналов на добор позиций ($sig.add)
    data$diff.sig <- diff(data$sig)
    data$diff.sig[1] <- data$sig[1]
    data$pos.num <- cumsum(abs(sign(data$diff.sig)))
    pos.num.vector <- seq(1:max(data$pos.num))
    #
    # нумерация тиков внутри позиций/сделок
    data.temp <- list()
    data.temp <- sapply(pos.num.vector, 
                        function(x, y) {
                            xts(cumsum(abs(sign(which(data$pos.num == x)))), 
                                order.by = index(data$pos.num[data$pos.num == x]))
                        })
    data$sig.ticks <- NA
    data$sig.ticks <- MergeData_inList_byRow(data.temp)
    #
    # ряд позиций и число тиков внутри позиции 
    data$pos.num <- lag(data$pos.num)
        data$pos.num[1] <- 0
    data$pos.ticks <- lag(data$sig.ticks)
        data$pos.ticks[1] <- 0
    # удаляем мусор
    remove(data.temp)
    #
    # выделение сигналов "$sig.add"
    data$sig.add <- data$sig.ticks %/% add.per
    data$sig.add <- sign(data$sig.add) * abs(sign(diff(data$sig.add)))
        data$sig.add[1] <- 0
    # 
    # работа с позициями
    #
    # на drop/add
    data$pos.add <- ifelse(lag(data$sig.add) == lag(data$sig.drop), 0, lag(data$sig.add))
        data$pos.add[1] <- 0
    data$pos.drop <- ifelse(lag(data$sig.drop) == lag(data$sig.add), 0, lag(data$sig.drop))
        data$pos.drop[1] <- 0
    #
    # нумерация drop/add действий
    data.temp <- list()
    pos.num.vector <- c(0, pos.num.vector)
    data.temp <- sapply(pos.num.vector, 
                    function(x, y) {
                        merge(xts(cumsum(data$pos.add[data$pos.num == x]), 
                                  order.by = index(data$pos.num[data$pos.num == x])),
                              xts(cumsum(data$pos.drop[data$pos.num == x]), 
                                  order.by = index(data$pos.num[data$pos.num == x])))
                    })
    data.temp <- MergeData_inList_byRow(data.temp)
    data$pos.add.num <- NA
    data$pos.drop.num <- NA
    data$pos.add.num <- data.temp$pos.add
    data$pos.drop.num <- data.temp$pos.drop
    # удаляем мусор
    remove(data.temp); remove(pos.num.vector)
    data$diff.sig <- NULL; data$sig.ticks <- NULL; data$sig.ticks <- NULL 
    #
    # вывод транзакций 
    data$action <- data$pos - lag(data$pos)
    data$action[1] <- 0 
    ####
    # расчет экономических параметров
    ####
    # расчет начальных условий
    data.names <- names(data.source)[grep(".Close", names(data.source))]
    data.names <- sub(".Close", "", data.names)
    # начальный баланс
    data$balance <- NA
    data$balance[1] <- balance.initial 
    # начальное число синтетических портфельных контрактов
    data$n  <- NA
    data$n[index(first(data$pos != 0))] <- round(abs(balance.initial * k.mm / 
                                                 coredata(data.source$IM[index(first(data$pos != 0))])) * 
                                                 runif(1, 0.6, 1.4)) 
    data$n[index(data$n) < index(first(data$pos != 0))] <- 0
    #
    # скелет таблицы сделок
    data$state <- STR_CalcState_Data(data)
    data.state <- data.state[-1, ]
    data$state[data$pos.add != 0 | data$pos.drop != 0] <- data$pos[data$pos.add != 0 | data$pos.drop != 0]
    data.state <- na.omit(data)
    # добавление нужных столбцов
    data.state$im.balance <- NA
    data.state$equity <- NA
    data.state$comiss <- NA
    data.state$ret <- NA
    data.state$margin <- NA
    #
    data.state$n[1] <- data$n[index(data.state$n[1])]
    data.state$im.balance[1] <- coredata(data.state$n[1]) * data.source$IM[index(data.state$n[1])]
    data.state$balance[1] <- balance.initial - coredata(data.state$im.balance[1])
    data.state$comiss[1] <- basket.comiss
    #data.state$ret[1] <- data.source$ret[index(data.state$ret[1])]
    for (i in 2:nrow(data.state)) {
        index <- index(data.state[i, ])
        index.lag <- index(data.state[i-1, ])
        data.state$margin[index] <- data.state$pos[index] * data.state$pos[index]
    }   
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