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
CalcTrades_inStates.testStr <- function(data) {
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
#' @param sma_per Периоды SMA
#' @param add_per Период докупок
#' @param k_mm Коэффициент MM
#' @param balance_start Стартовый баланс
#' @param basket_weights Веса корзины (вектор)
#' @param slips Слипы
#' @param commissions Комиссии (вектор)
#'
#' @return list(data, states) Лист с данными отработки и данные сделок
#'
#' @export
TestStr.gear <- function(data_source,
                         sma_per, add_per, 
                         k_mm, balance_start, 
                         basket_weights, slips, commissions) {
  # Зависимости:
  require(quantmod)
  # 
  # >>>
  ### 1 Расчёт и добавление индикаторов, сигналов и позиций (+ прочие хар-ки)
  # (открытие позиции 'ОткрПозиПоРынку' ($sig | $pos = 1/-1: long/short); 
  # закрытие позиций 'ЗакрПозиПоРынку' рассчитывается с тблице сделок (пункт ;;) )
  #
  ## Вектор имён инструментов внутри торгуемой корзины
  data.names <- 
    grep('.Close', names(data_source)) %>%
    names(data_source)[.] %>%
    sub('.Close', '', .)
  #
  # расчёт суммарной комиссии по корзине
  basket.commiss <- sum(basket_weights * commissions)
  cat('TestStrategy INFO:  Start TestStrategy with parameters:', '\n',
      '  TickersInBasket:   ',data.names, '\n',
      '  SMA period:      ',sma_per, '\n',
      '  PositionADD period:  ',add_per, '\n',
      '  MM kofficient:     ',k_mm, '\n',
      '  Start Balance:     ',balance_start, '\n',
      '  BasketWeights:     ',basket_weights, '\n') 
  cat('TestStrategy INFO:  Start StrategyData Calculation...', '\n')
  # 
  # >>>
  ### Расчёт индикаторов и позиций
  ## 1.1 Добавляем индикаторы  (SMA) и позиции по корзине
  data %<>% 
    {
      data <- xts()
      cat('TestStrategy INFO:  Calculate SMA with period:  ', sma_per, '\n')
      # тикер-индикатор: SI    
      #data$sma <- 
      #  SMA(data_source$SPFB.SI.Close, sma_per) %>%
      #  round(., digits = 0)
      data$sma <- CalcIndicator.SMA(x = data_source$SPFB.SI.Close, per = sma_per)
      cat('TestStrategy INFO:  Calculate $sig and $pos...', '\n')
      data$sig <- ifelse(
        data$sma < data_source$SPFB.SI.Close, 
        1, 
        ifelse(
          data$sma > data_source$SPFB.SI.Close, -1, 0
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
    message('WARNING(TestStrategy_gear): No Trades Here!!!', '\n')
    remove(temp.length)
    return(list(NA, NA))
  }
  remove(temp.length)
  #
  # >>>
  ## 1.3 расчёт сигналов на изменения внутри позиции 'ИзменПоРынку'
  # 1.3.1 расчет сигналов на сброс лотов ($sig.drop - продажа по рынку)
  data %<>%    
    { 
      data <- .
      cat('TestStrategy INFO:  Calculate $sig.drop...', '\n')
      data$sig.drop <- ifelse(
        (
          ((data$sma > data_source$SPFB.SI.Low) & (data$sig == 1)) | 
          ((data$sma < data_source$SPFB.SI.High) & (data$sig == -1))
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
      cat('TestStrategy INFO:  Calculate $sig.add...', '\n')
      # точки смены сигнала
      data$diff.sig <- diff(data$sig)
      data$diff.sig[1] <- data$sig[1]
      # нумерация состояний внутри сигналов
      data$sig.num <- 
        abs(sign(data$diff.sig)) %>%
        # защита от нумерации сигналов 'вне рынка'
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
      # выделение сигналов '$sig.add'
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
            MergeData_inList.byRow(.)
          }
        } %T>%
        {
          # ветвим и проставляем тики позиций (добаляем напрямую в data)
          data$pos.bars <<- lag(.)
          data$pos.bars[1] <<- 0
        } %>%
        {
          . %/% add_per
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
        cat('TestStrategy INFO:  Calculate $pos.add and $pos.drop...', '\n')
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
              MergeData_inList.byRow(.)
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
      cat('TestStrategy INFO:  Calculate state column...', '\n')
      data$state <- 
        (data$pos.add != 0 | data$pos.drop != 0) %>%
        {
          data$state <- CalcStates.inData(x = data$pos)
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
    cat('TestStrategy INFO: No Switch Position there', '\n')
    remove(temp.ind)
  } else {
    data %<>% 
      {
        data <- .
        cat('TestStrategy INFO:  Split SwitchPosition...', '\n') 
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
  cat('TestStrategy INFO:  Build state.table...', '\n')
  states <- 
    {
      states <- xts() 
      states <- data[!is.na(data$state)]
    } %>% 
    {
      .$pos[nrow(.$pos)] <- 0
      return(.)
    }
  # 
  # 2.1.3 добавление нужных исходных данных в data и states 
  #
  # Выгружаем и рассчитываем уникальные данные по инструментам (Price, ret, cret, pos)
  # все действия проходят внутри одного цикла перебера имён инструментов
  #
  # 2.1.3.1 выгрузка Open'ов и расчёт return'ов (здесь переходим к return'ам стратегии)  
  # котировки берём из data_source
  cat('TestStrategy INFO:  Loading Tickers Price from data_source...', '\n')
  # индексы строк states
  states.ind <- index(states)
  ## соотшение позиций внутри корзины
  temp.vector <- c(1, -1, -1)
  #
  # расчёт return'ов позиций и состояний
  cat('TestStrategy INFO:  CalcReturns for data & states...','\n')
  for (i in 1:length(data.names)) {  
    temp.text <- 
      data.names[i] %>%
      {
        t <- paste(       
          # перенос Open'ов в states (в пунктах) с учётом проскальзываний
          'states$',.,'.Price <- ', 
          'merge(states, data_source$',.,'.Open[states.ind]) %$% ',
          'na.locf(',.,'.Open) %>% 
          { . + slips[i] * states$state } ; ',
          # перенос Open'ов в data 
          'data$',.,'.Price <- ', 
          'merge(data, data_source$',.,'.Open[data.ind]) %$% ',
          'na.locf(',.,'.Open) ; ',  
          # перенос данных по Open'ам на свечах изменения позиций (в пунктах) в data
          'temp <- merge(data$',.,'.Price, states$',.,'.Price[states.ind]) ; ',
          'temp[, 1][which(!is.na(temp[, 2]))] <- temp[, 2][which(!is.na(temp[, 2]))] ; ',
          'data$',.,'.Price <- temp[, 1] ;',  
          # расчёт позиций по инструментам корзины в states
          'states$',.,'.pos <- states$pos * temp.vector[i] ; ',
          # расчёт return'ов по сделкам (в пунктах) в states 
          'states$',.,'.ret <- ',
          '(states$',.,'.Price - lag(states$',.,'.Price)) * lag(states$',.,'.pos) ; ',  
          'states$',.,'.ret[1] <- 0 ;',
          # расчёт позиций по инструментам корзины в data
          'data$',.,'.pos <- data$pos * temp.vector[i] ; ',       
          # расчёт return'ов по позициям (в пунктах) в data 
          'data$',.,'.ret <- ',
          '(data$',.,'.Price - lag(data$',.,'.Price)) * lag(data$',.,'.pos) ; ',  
          'data$',.,'.ret[1] <- 0 ;',
          sep = '')
        return(t)
      } 
    eval(parse(text = temp.text))  
    remove(temp.text)
    remove(temp)
    cat('TestStrategy INFO:  CalcReturns for data & states:  ',data.names[i],  'OK', '\n')     
  }
  #
  # 2.1.3.2 Расчёт cret
  # расчёт cret по инструментам в data и states
  cat('TestStrategy INFO:  CalcCRet for data...', '\n')
  # для Si всё просто
  data$SPFB.SI.cret <- data$SPFB.SI.ret
  states$SPFB.SI.cret <- states$SPFB.SI.ret 
  # расчёт для data
  # добавление курса
  
  # суммарный cret в data
  data$cret <- CalcSum_inXTS_byTargetCol.basket(data = data, target = 'cret', basket_weights)
  # расчёт суммарного cret для states
  cat('TestStrategy INFO:  CalcCRet for states', '\n')
  # суммарный cret в states
  states$cret <- CalcSum_inXTS_byTargetCol.basket(data = states, target = 'cret', basket_weights)
  #
  # 2.1.4 Начальные параметры для расчёта сделок
  # начальный баланс
  states$balance <- NA
  # начальное число синтетических контрактов корзины
  states$n <- NA
  # прочее
  states$diff.n <- NA
  states$margin <- NA
  states$commiss <- NA
  states$equity <- NA
  #
  ## 2.2 Расчёт самих сделок
  #
  cat('TestStrategy INFO:  Start Calculation trades...', '\n')
  #
  states$n <- CalcTrades_inStates.testStr(data = states)
  # Изменение контрактов на такте
  states$diff.n <- states$n - lag(states$n)
  states$diff.n[1] <- 0
  # Расчёт баланса, заблокированного на ГО
  states$im.balance <- 
    #индекс uniq state строк
    index(states$state) %>%
    unique(.) %>%
    {
      states$n * data_source$IM[.]
    }
  states$im.balance %<>% 
    is.na(.) %>%
    {
      states$im.balance[.] <- states$n[.] * data_source$IM[index(states$n[.])]
      return(states$im.balance)
    }
  states$im.balance[1] <- 0                   
  # Расчёт комиссии на такте
  states$commiss <- basket.commiss * abs(states$diff.n)
  states$commiss[1] <- 0
  # Расчёт вариационки
  states$margin <- states$cret * lag(states$n)
  states$margin[1] <- 0
  # расчёт equity по корзине в states
  states$perfReturn <- states$margin - states$commiss
  states$perfReturn[1] <- 0
  states$equity <- cumsum(states$perfReturn)
  states$equity[1] <- 0
  # Расчёт баланса 
  states$balance <- balance_start + states$equity - states$im.balance
  states$balance[1] <- balance_start 
  #
  cat('TestStrategy INFO:  Calculation trades  OK', '\n')
  #
  ## Перенос данных из state в full таблицу
  # перенос данных по количеству контрактов корзины 
  data$n <-  
    merge(data, states$n) %>%
    {
      data <- .
      data$n <- na.locf(data$n)
      return(data$n)
    }
  # перенос данных по комиссии корзины
  data$commiss <-    
    merge(data, states$commiss) %>%
    {
      data <- .
      data$commiss[is.na(data$commiss)] <- 0
      return(data$commiss)
    }
  # перенос данных по суммарному ГО
  data$im.balance <-  
    merge(data, states$im.balance) %>%
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
  data$balance <- balance_start + data$equity - data$im.balance
  data$balance[1] <- balance_start 
  #
  # расчёт n, margin и equity по инструментам в data и states 
  for (i in 1:length(data.names)) {
    temp.text <- 
      data.names[i] %>%
      {
        t <- paste(
          # расчёт для states 
          'states$',.,'.n <- states$n * ',basket_weights[i],'; ',
          'states$',.,'.diff.n <- diff(states$',.,'.n) ; ',
          'states$',.,'.diff.n[1] <- 0 ; ',
          'states$',.,'.commiss <- commissions[i] * abs(states$',.,'.diff.n) ; ',
          #'states$',.,'.im.balance <- NA',
          'states$',.,'.margin <- states$',.,'.cret * lag(states$',.,'.n) ; ',
          'states$',.,'.margin[1] <- 0 ; ',
          'states$',.,'.perfReturn <- states$',.,'.margin - states$',.,'.commiss ;',
          'states$',.,'.equity <- cumsum(states$',.,'.perfReturn) ;',
          #'states$',.,'.balance <- balance_start + states$',.,'.equity ;',
          # расчёт для data  
          'data$',.,'.n <- ', 
          'merge(data, states$',.,'.n) %$% ',
          'na.locf(',.,'.n) ; ',
          'data$',.,'.margin <- ',
          'data$',.,'.cret * lag(data$',.,'.n) ; ',
          'data$',.,'.margin[1] <- 0 ; ',
          'data <- merge(data, states$',.,'.commiss) ; ',
          'data$',.,'.commiss[is.na(data$',.,'.commiss)] <- 0 ; ',
          #'data$',.,'.im.balance <- NA',
          'data$',.,'.perfReturn <- data$',.,'.margin - data$',.,'.commiss ;',
          'data$',.,'.equity <- cumsum(data$',.,'.perfReturn) ;',
          #'data$',.,'.balance <- balance_start + data$',.,'.equity ;',
          sep = '')
        return(t)
      }    
    eval(parse(text = temp.text))
  }   
  # уборка
  states$sig <- NULL
  states$sig.drop <- NULL
  states$sig.add <- NULL
  #
  return(list(data, states))  
}
