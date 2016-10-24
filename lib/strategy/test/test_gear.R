# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Движок тестовой стратегии:
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
###
#' Функция расчёта числа контрактов в сделках (в state данных)
#' 
#' @param data XTS со state данными
#'
#' @return result Ряд данных с числом контрактов в сделках
#'
#' @export
CalcDeals_inStates.TestStr <- function(data) {
  #FUN <- match.fun(FUN)
  temp.env <- new.env()
  ind <- 1:nrow(data)
  n <- coredata(data$n)
  pos <- coredata(data$pos)
  pos.bars <- coredata(data$pos.bars)
  pos.add <- coredata(data$pos.add)
  assign('cache', n, envir = temp.env)
  rm(data)
  sapply(ind,
         function(x) {
           n <- get('cache', envir = temp.env)
           #data[x, ] <- FUN(data, x, ...) 
           n[x] <- ifelse(
             pos[x] == 0,
             0, 
             ifelse(
               pos.bars[x] == 0,
               n[x] <- 4,
               ifelse(
                 pos.add[x] == 1,
                 round(2 * n[x - 1]),
                 round(0.5 * n[x - 1])
               )
             )
           )
           assign('cache', n, envir = temp.env) 
         })
  result <- get('cache', envir = temp.env)
  rm(temp.env)
  #
  return(result)
}
###
#' Функция движка тестовой стратегии
#' 
#' @param data.souce XTS с котировками
#' @param sma.per Периоды SMA
#' @param add.per Период докупок
#' @param k.mm Коэффициент MM
#' @param balance.start Стартовый баланс
#' @param basket.weights Веса корзины (вектор)
#' @param slips Слипы
#' @param commissions Комиссии (вектор)
#'
#' @return list(data, data.state) Лист с данными отработки и данные сделок
#'
#' @export
TestStr_gear <- function(data.source,
                         sma.per, add.per, 
                         k.mm, balance.start, 
                         basket.weights, slips, commissions) {
  # Зависимости:
  require(quantmod)
  # 
  # >>>
  ### 1 Расчёт и добавление индикаторов, сигналов и позиций (+ прочие хар-ки)
  # (открытие позиции "ОткрПозиПоРынку" ($sig | $pos = 1/-1: long/short); 
  # закрытие позиций "ЗакрПозиПоРынку" рассчитывается с тблице сделок (пункт ;;) )
  #
  ## Вектор имён инструментов внутри торгуемой корзины
  data.names <- 
    grep(".Close", names(data.source)) %>%
    names(data.source)[.] %>%
    sub(".Close", "", .)
  #
  # расчёт суммарной комиссии по корзине
  basket.commiss <- sum(basket.weights * commissions)
  cat("TestStrategy INFO:  Start TestStrategy with parameters:", "\n",
      "  TickersInBasket:   ",data.names, "\n",
      "  SMA period:      ",sma.per, "\n",
      "  PositionADD period:  ",add.per, "\n",
      "  MM kofficient:     ",k.mm, "\n",
      "  Start Balance:     ",balance.start, "\n",
      "  BasketWeights:     ",basket.weights, "\n") 
  cat("TestStrategy INFO:  Start StrategyData Calculation...", "\n")
  # 
  # >>>
  ### Расчёт индикаторов и позиций
  ## 1.1 Добавляем индикаторы  (SMA) и позиции по корзине
  data %<>% 
    {
      data <- xts()
      cat("TestStrategy INFO:  Calculate SMA with period:  ", sma.per, "\n")
      # тикер-индикатор: SI    
      #data$sma <- 
      #  SMA(data.source$SPFB.SI.Close, sma.per) %>%
      #  round(., digits = 0)
      data$sma <- CalcIndicator_SMA(x = data.source$SPFB.SI.Close, per = sma.per)
      cat("TestStrategy INFO:  Calculate $sig and $pos...", "\n")
      data$sig <- ifelse(
        data$sma < data.source$SPFB.SI.Close, 
        1, 
        ifelse(
          data$sma > data.source$SPFB.SI.Close, -1, 0
        )
      )
      data$sig[index(last(data$sig))] <- 0
      return(data)
    } %>%   
    na.omit(.) %>%  
    ## 1.2 добавляем позиции
    {
      data <- .
      data$pos <- lag(data$sig)
      data$pos[1] <- 0
      return(data)
    } 
  #  
  # после простановки сигналов и позиций это должны быть ненулевые ряды
  temp.length <- 
    data$pos %>%
    {
      which(. != 0)
    } %>%
    length(.)
  if (temp.length == 0) {
    # если это условие не выполняется, то отработка робота д.б. завершена с нулувым триггером
    message("WARNING(TestStrategy_gear): No Deals Here!!!", "\n")
    remove(temp.length)
    return(list(NA, NA))
  }
  remove(temp.length)
  #
  # >>>
  ## 1.3 расчёт сигналов на изменения внутри позиции "ИзменПоРынку"
  # 1.3.1 расчет сигналов на сброс лотов ($sig.drop - продажа по рынку)
  data %<>%    
    { 
      data <- .
      cat("TestStrategy INFO:  Calculate $sig.drop...", "\n")
      data$sig.drop <- ifelse(
        (
          ((data$sma > data.source$SPFB.SI.Low) & (data$sig == 1)) | 
          ((data$sma < data.source$SPFB.SI.High) & (data$sig == -1))
        ) & (data$sig == data$pos), 
        1, 0
      )
      return(data)
    }
  #
  # 1.3.2 расчет сигналов на добор лотов ($sig.add - докупка по рынку)
  data %<>% 
    {  
      data <- .
      cat("TestStrategy INFO:  Calculate $sig.add...", "\n")
      # точки смены сигнала
      data$diff.sig <- diff(data$sig)
      data$diff.sig[1] <- data$sig[1]
      # нумерация состояний внутри сигналов
      data$sig.num <- 
        abs(sign(data$diff.sig)) %>%
        # защита от нумерации сигналов "вне рынка"
        {
          abs(sign(data$sig)) * . 
        } %>%
        cumsum(.)
      # ряд номеров позиций 
      data$pos.num <- 
        lag(data$sig.num) %>%
        # защита от нумераций пачек нулевых позиций
        {
          temp <- diff(data$pos)
          temp[1] <- 0
          temp <- abs(sign(temp))
          x <- . * sign(temp + abs(data$pos))
          return(x)
        }
      data$pos.num[1] <- 0 
      # выделение сигналов "$sig.add"
      data$sig.add <- 
        # вектор, содержащий номера состояний сигналов
        unique(data$sig.num) %>%
        # нумерация тиков внутри состояний сигналов
        {
          if (length(.) == 1) {
            .
          } else {
            sapply(
              ., 
              function(x) {
                temp <- abs(sign(which(data$sig.num == x)))
                temp[1] <- 0
                xts(x = cumsum(temp), order.by = index(data$sig.num[data$sig.num == x]))
              }
            ) %>% 
            MergeData_inList_byRow(.)
          }
        } %T>%
        {
          # ветвим и проставляем тики позиций (добаляем напрямую в data)
          data$pos.bars <<- lag(.)
          data$pos.bars[1] <<- 0
        } %>%
        {
          . %/% add.per
        } %>%    
        {
          temp.length <- 
            {
              which(. != 0)
            } %>%
            length(.)
          if (temp.length == 0) {
            result <- .  
          } else {
            result <- sign(.) * abs(sign(diff(.)))
          }
          return(result)          
        }
      data$sig.add[1] <- 0  
      data$sig.add <- lag(data$sig.add)
      data$sig.add[1] <- 0  
      return(data)
    } 
  #
  temp.length <- 
    {
      which(data$sig.add != 0 | data$sig.drop != 0)
    } %>%
    length(.)  
  if (temp.length != 0) {
    # 1.3.3 расчёт позиций drop/add
    data %<>% 
      {
        data <- .
        cat("TestStrategy INFO:  Calculate $pos.add and $pos.drop...", "\n")
        data$pos.add <- ifelse(lag(data$sig.add) == lag(data$sig.drop), 0, lag(data$sig.add))
        #data$pos.add <- lag(data$pos.add)
        data$pos.add[is.na(data$pos.add)] <- 0
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
          unique(data$pos.num) %>%
          {
            if (length(.) == 1) {
              0
            } else {
              sapply(
                .,
                function(x) {
                  merge(
                    xts(
                      cumsum(data$pos.add[data$pos.num == x]), 
                      order.by = 
                        data$pos.num[data$pos.num == x] %>% 
                        index(.)
                    ),
                    xts(
                      cumsum(data$pos.drop[data$pos.num == x]), 
                      order.by = 
                        data$pos.num[data$pos.num == x] %>% 
                        index(.)
                    )
                  )
                }
              ) %>%
              MergeData_inList_byRow(.)
            }
          }
        if (length(data.temp) != 1) {
          data$pos.add.num <- data.temp$pos.add
          data$pos.drop.num <- data.temp$pos.drop  
        } else {
          data$pos.add.num <- 0
          data$pos.drop.num <- 0
        }
        # удаляем мусор
        remove(data.temp); #remove(num.vector)
        #data$diff.sig <- NULL
        data$sig.num <- NULL
        return(data)
      } 
    remove(temp.length)
  } else {
    data$pos.add.num <- 0
    data$pos.drop.num <- 0
    remove(temp.length)
  } 
  #
  # >>>
  # 1.3.5 ряд  учёта транзакций
  data %<>%   
    {
      data <- .
      data$action <- data$pos - lag(data$pos)
      data$action[1] <- 0 
      return(data)
    } %>%
    # 1.4 ряд состояний 
    {
      data <- .
      cat("TestStrategy INFO:  Calculate state column...", "\n")
      data$state <- 
        (data$pos.add != 0 | data$pos.drop != 0) %>%
        {
          data$state <- CalcState_Data(x = data$pos)
          data$state[.] <- data$pos[.]
          # условие закрытия сделок в конце торгового периода
          data$state[index(last(data$state))] <- 0
          return(data$state)
        }
      return(data)  
    }  
  #
  # >>>
  # 1.5 расщепление переворотов в позициях (расщепление строк с $action = +/-2)
  # индекс строки-переворота
  temp.ind <- index(data[data$action == 2 | data$action == -2])
  if (length(temp.ind) == 0) {
    cat("TestStrategy INFO: No Switch Position there", "\n")
    remove(temp.ind)
  } else {
    data %<>% 
      {
        data <- .
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
        data %<>% 
          {
            x <- .
            x$pos[temp.ind] <- 0
            # x$state[temp.ind] <- sign(x$action[temp.ind])
            x$action[temp.ind] <- abs(sign(x$action[temp.ind]))
            x$pos.num[temp.ind] <- x$pos.num[temp.ind] - 1
            # правильное заполнение поля $pos.bars
            temp.ind.num <- x[temp.ind, which.i=TRUE]
            x$pos.bars[temp.ind] <- x$pos.bars[temp.ind.num - 1] 
            return(x)
          }
        data <- rbind(data, temp)   
        return(data)
      } 
    remove(temp.ind)
  }
  #
  # >>>
  ### 2.расчет результатов отработки робота
  #
  ## 2.1 выгрузка данных по инструментам
  # индексы данных (строк) data
  data.ind <- index(data)
  #
  # 2.1.2 скелет таблицы сделок
  cat("TestStrategy INFO:  Build state.table...", "\n")
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
  cat("TestStrategy INFO:  Loading Tickers Open from data.source...", "\n")
  # индексы строк data.state
  data.state.ind <- index(data.state)
  ## соотшение позиций внутри корзины
  temp.vector <- c(1, -1, -1)
  #
  # расчёт return'ов позиций и состояний
  cat("TestStrategy INFO:  CalcReturns for data & data.state...","\n")
  for (i in 1:length(data.names)) {  
    temp.text <- 
      data.names[i] %>%
      {
        t <- paste(       
          # перенос Open'ов в data.state (в пунктах) с учётом проскальзываний
          "data.state$",.,".Open <- ", 
          "merge(data.state, data.source$",.,".Open[data.state.ind]) %$% ",
          "na.locf(",.,".Open) %>% 
          { . + slips[i] * data.state$state } ; ",
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
    cat("TestStrategy INFO:  CalcReturns for data & data.state:  ",data.names[i],  "OK", "\n")     
  }
  #
  # 2.1.3.2 Расчёт cret
  # расчёт cret по инструментам в data и data.state
  cat("TestStrategy INFO:  CalcCRet for data...", "\n")
  # для Si всё просто
  data$SPFB.SI.cret <- data$SPFB.SI.ret
  data.state$SPFB.SI.cret <- data.state$SPFB.SI.ret 
  # расчёт для data
  # добавление курса
  
  # суммарный cret в data
  data$cret <- CalcSum_Basket_TargetPar_inXTS(data = data, target = "cret", basket.weights)
  # расчёт суммарного cret для data.state
  cat("TestStrategy INFO:  CalcCRet for data.state", "\n")
  # суммарный cret в data.state
  data.state$cret <- CalcSum_Basket_TargetPar_inXTS(data = data.state, target = "cret", basket.weights)
  #
  # 2.1.4 Начальные параметры для расчёта сделок
  # начальный баланс
  data.state$balance <- NA
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
  cat("TestStrategy INFO:  Start Calculation Deals...", "\n")
  #
  data.state$n <- CalcDeals_inStates.TestStr(data = data.state)
  # Изменение контрактов на такте
  data.state$diff.n <- data.state$n - lag(data.state$n)
  data.state$diff.n[1] <- 0
  # Расчёт баланса, заблокированного на ГО
  data.state$im.balance <- 
    #индекс uniq state строк
    index(data.state$state) %>%
    unique(.) %>%
    {
      data.state$n * data.source$IM[.]
    }
  data.state$im.balance %<>% 
    is.na(.) %>%
    {
      data.state$im.balance[.] <- data.state$n[.] * data.source$IM[index(data.state$n[.])]
      return(data.state$im.balance)
    }
  data.state$im.balance[1] <- 0                   
  # Расчёт комиссии на такте
  data.state$commiss <- basket.commiss * abs(data.state$diff.n)
  data.state$commiss[1] <- 0
  # Расчёт вариационки
  data.state$margin <- data.state$cret * lag(data.state$n)
  data.state$margin[1] <- 0
  # расчёт equity по корзине в data.state
  data.state$perfReturn <- data.state$margin - data.state$commiss
  data.state$perfReturn[1] <- 0
  data.state$equity <- cumsum(data.state$perfReturn)
  data.state$equity[1] <- 0
  # Расчёт баланса 
  data.state$balance <- balance.start + data.state$equity - data.state$im.balance
  data.state$balance[1] <- balance.start 
  #
  cat("TestStrategy INFO:  Calculation Deals  OK", "\n")
  #
  ## Перенос данных из state в full таблицу
  # перенос данных по количеству контрактов корзины 
  data$n <-  
    merge(data, data.state$n) %>%
    {
      data <- .
      data$n <- na.locf(data$n)
      return(data$n)
    }
  # перенос данных по комиссии корзины
  data$commiss <-    
    merge(data, data.state$commiss) %>%
    {
      data <- .
      data$commiss[is.na(data$commiss)] <- 0
      return(data$commiss)
    }
  # перенос данных по суммарному ГО
  data$im.balance <-  
    merge(data, data.state$im.balance) %>%
    {
      data <- .
      data$im.balance <- na.locf(data$im.balance)
      return(data$im.balance)
    }
  ## Расчёт показателей в full данных
  # расчёт вариационки в data
  data$margin <- lag(data$n) * data$cret
  data$margin[1] <- 0      
  # расчёт equity по корзине в data 
  data$perfReturn <- data$margin - data$commiss
  data$equity <- cumsum(data$perfReturn)
  # расчёт баланса
  data$balance <- balance.start + data$equity - data$im.balance
  data$balance[1] <- balance.start 
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
          #"data.state$",.,".im.balance <- NA",
          "data.state$",.,".margin <- data.state$",.,".cret * lag(data.state$",.,".n) ; ",
          "data.state$",.,".margin[1] <- 0 ; ",
          "data.state$",.,".perfReturn <- data.state$",.,".margin - data.state$",.,".commiss ;",
          "data.state$",.,".equity <- cumsum(data.state$",.,".perfReturn) ;",
          #"data.state$",.,".balance <- balance.start + data.state$",.,".equity ;",
          # расчёт для data  
          "data$",.,".n <- ", 
          "merge(data, data.state$",.,".n) %$% ",
          "na.locf(",.,".n) ; ",
          "data$",.,".margin <- ",
          "data$",.,".cret * lag(data$",.,".n) ; ",
          "data$",.,".margin[1] <- 0 ; ",
          "data <- merge(data, data.state$",.,".commiss) ; ",
          "data$",.,".commiss[is.na(data$",.,".commiss)] <- 0 ; ",
          #"data$",.,".im.balance <- NA",
          "data$",.,".perfReturn <- data$",.,".margin - data$",.,".commiss ;",
          "data$",.,".equity <- cumsum(data$",.,".perfReturn) ;",
          #"data$",.,".balance <- balance.start + data$",.,".equity ;",
          sep = "")
        return(t)
      }    
    eval(parse(text = temp.text))
  }   
  # уборка
  data.state$sig <- NULL
  data.state$sig.drop <- NULL
  data.state$sig.add <- NULL
  #
  return(list(data, data.state))  
}
