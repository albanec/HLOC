# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Движок simple стратегии:
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
###
#' Функция движка simple стратегии
#' 
#' @param data.souce Лист с котировками
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
SimpleStr.gear <- function(ohlc,
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
    grep('.Close', names(ohlc)) %>%
    names(ohlc)[.] %>%
    sub('.Close', '', .)
  #
  # расчёт суммарной комиссии по корзине
  basket.commiss <- sum(basket_weights * commissions)
  cat('TestStrategy INFO:  Start TestStrategy with parameters:', '\n',
      '    TickersInBasket:     ',data.names, '\n',
      '    SMA period:          ',sma_per, '\n',
      '    PositionADD period:  ',add_per, '\n',
      '    MM kofficient:       ',k_mm, '\n',
      '    Start Balance:       ',balance_start, '\n',
      '    BasketWeights:       ',basket_weights, '\n')
  # 
  cat('TestStrategy INFO:  Start StrategyData Calculation...', '\n')
  # 
  # >>>
  ### Расчёт индикаторов и позиций
  ## 1.1 Добавляем индикаторы  (SMA) и позиции по корзине
  data %<>%  
    {
      data <- xts(NULL, order.by = ohlc)
      cat('TestStrategy INFO:  Calculate SMA with period:  ', sma_per, '\n')
      # тикер-индикатор: SI        
      #data$sma <- SMA(ohlc$SPFB.SI.Close, sma_per)
      data$sma <- CalcIndicator.SMA(x = ohlc$SPFB.SI.Close, per = sma_per)
      cat('TestStrategy INFO:  Calculate $sig and $pos...', '\n')
      data$sig <- ifelse(
        data$sma < ohlc$SPFB.SI.Close, 1, 
        ifelse(
          data$sma > ohlc$SPFB.SI.Close, -1, 0
        )
      )
      return(data)
    } %>%   
    na.omit(.) %>%
    ## 1.2 т.к. позиции корзины зависят только от SMA, то добавляем их 
    {
      data <- .
      data$pos <- stats::lag(data$sig)
      data$pos[1] <- 0
      return(data)
      # позиции по каждому из инструментов корзины описаны позднее
      # в этой стратегии позиции по BR обратны позициям по Si (т.к. инструменты обратно коррелированы)
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
          ((data$sma > ohlc$SPFB.SI.Low) & (data$sig == 1)) | 
          ((data$sma < ohlc$SPFB.SI.High) & (data$sig == -1))
        ) & (data$sig == data$pos), 
        1, 0
      )
      return(data)
    } 
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
        stats::lag(data$sig.num) %>%
        # защита от нумераций пачек нулевых позиций
        {
          temp <- diff(data$pos)
          temp[1] <- 0
          temp <- abs(sign(temp))
          x <- . * sign(temp + abs(data$pos))
          remove(temp)
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
                xts(x = cumsum(temp), order.by = index(data$sig.num[data$sig.num == x])
                )
              }
            ) %>% 
            MergeData_inList.byRow(.)
          }
        } %T>%
        {
          # ветвим и проставляем тики позиций (добаляем напрямую в data)
          data$pos.bars <<- stats::lag(.)
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
      return(data)
    } 
  # 
  temp.length <- 
    {
      which(data$sig.add != 0 | data$sig.drop != 0)
    } %>%
    length(.)      
  if (temp.length != 0) {
    data %<>%
      # 1.3.3 расчёт позиций drop/add
      {
        data <- .
        cat('TestStrategy INFO:  Calculate $pos.add and $pos.drop...', '\n')
        data$pos.add <- ifelse(stats::lag(data$sig.add) == stats::lag(data$sig.drop), 0, stats::lag(data$sig.add))
        data$pos.add <- stats::lag(data$pos.add)
        data$pos.add[is.na(data$pos.add)] <- 0
        data$pos.drop <- ifelse(stats::lag(data$sig.drop) == stats::lag(data$sig.add), 0, stats::lag(data$sig.drop))
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
        data$diff.sig <- NULL
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
      data$action <- data$pos - stats::lag(data$pos)
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
    # котировки берём из ohlc
  cat('TestStrategy INFO:  Loading Tickers Price from ohlc...', '\n')
  # индексы строк states
  states.ind <- index(states)
  ## соотшение позиций внутри корзины
  temp.vector <- c(1, 1, -1)
  #
  # расчёт return'ов позиций и состояний
  cat('TestStrategy INFO:  CalcReturns for data & states...','\n')
  for (i in 1:length(data.names)) {  
    temp.text <- 
      data.names[i] %>%
      {
        t <- paste0(             
          # перенос Open'ов в states (в пунктах) с учётом проскальзываний
          'states$',.,'.Price <- ', 
            'merge(states, ohlc$',.,'.Open[states.ind]) %$% ',
            'na.locf(',.,'.Open) %>% 
            { . + slips[i] * states$state } ; ',
          # перенос Open'ов в data 
          'data$',.,'.Price <- ', 
            'merge(data, ohlc$',.,'.Open[data.ind]) %$% ',
            'na.locf(',.,'.Open) ; ',  
          # перенос данных по Open'ам на свечах изменения позиций (в пунктах) в data
          'temp <- merge(data$',.,'.Price, states$',.,'.Price[states.ind]) ; ',
          'temp[, 1][which(!is.na(temp[, 2]))] <- temp[, 2][which(!is.na(temp[, 2]))] ; ',
          'data$',.,'.Price <- temp[, 1] ;',  
          # расчёт позиций по инструментам корзины в states
          'states$',.,'.pos <- states$pos * temp.vector[i] ; ',
          # расчёт return'ов по сделкам (в пунктах) в states 
          'states$',.,'.ret <- ',
            '(states$',.,'.Price - stats::lag(states$',.,'.Price)) * stats::lag(states$',.,'.pos) ; ',  
          'states$',.,'.ret[1] <- 0 ;',
          # расчёт позиций по инструментам корзины в data
          'data$',.,'.pos <- data$pos * temp.vector[i] ; ',           
          # расчёт return'ов по позициям (в пунктах) в data 
          'data$',.,'.ret <- ',
            '(data$',.,'.Price - stats::lag(data$',.,'.Price)) * stats::lag(data$',.,'.pos) ; ',  
          'data$',.,'.ret[1] <- 0 ;')
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
  data <- 
    merge(data, ohlc$USDRUB[data.ind]) %$%
    na.locf(USDRUB) %>%
    # расчёт cret по инструментам
    NormData_inXTS.price(data = data, 
                             norm.data = ., 
                             names = c('SPFB.RTS.ret', 'SPFB.BR.ret'), 
                             outnames = c('SPFB.RTS.cret', 'SPFB.BR.cret'), 
                             tick.val = c(10, 0.01), tick.price = c(0.02, 0.01), 
                             convert.to = 'RUB')
  # суммарный cret в data
  data$cret <- CalcSum_inXTS_byTargetCol.basket(data = data, target = 'cret', basket_weights)
  # расчёт суммарного cret для states
  cat('TestStrategy INFO:  CalcCRet for states', '\n')
  states <- 
    merge(states, ohlc$USDRUB[states.ind]) %$%
    na.locf(USDRUB) %>%
    # расчёт cret по инструментам
    NormData_inXTS.price(data = states, 
                             norm.data = ., 
                             names = c('SPFB.RTS.ret', 'SPFB.BR.ret'), 
                             outnames = c('SPFB.RTS.cret', 'SPFB.BR.cret'), 
                             tick.val = c(10, 0.01), tick.price = c(0.02, 0.01), 
                             convert.to = 'RUB')
  # суммарный cret в states
  states$cret <- CalcSum_inXTS_byTargetCol.basket(data = states, target = 'cret', basket_weights)
  #
  # 2.1.4 Начальные параметры для расчёта сделок
  # начальный баланс
  states$balance <- NA
  states$im.balance <- NA
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
  cat('TestStrategy INFO:  Start Calculation Trades...', '\n')
  for (n in 1:nrow(states)) {
    # на первой строке рассчитываются стартовые значения
    if (n == 1) {
      states$balance[1] <- balance_start
      states$im.balance[1] <- 0
      states$commiss[1] <- 0
      states$margin[1] <- 0
      states$diff.n[1] <- 0
      states$n[1] <- 0
      states$equity[1] <- 0
    } else {
      ### основной расчёт
      #индекс строки
      temp.index <- index(states$state[n])
      ## расчёт вариационки
      states$margin[n] <- 
        states$cret[[n]] * states$n[[n - 1]]
      ## расчёт количества контрактов на такте
      # если закрытие позиции, то контрактов ноль
      if (states$pos[n] == 0) {
        states$n[n] <- 0
      } else {
        # если открытие позиции, то
        #if ((states$pos.add[n] + states$pos.drop[n]) == 0) {
        if (states$pos.bars[n] == 0) {
          states$n[n] <-
            {
              states$balance[[n - 1]] * k_mm / 
              coredata(ohlc$IM[temp.index]) * 
              runif(1, 0.6, 1.4) 
            } %>%
            round(.) %>%
            {
              ifelse(. != 0, 
                     ., 
                     1)
            }
        } else {
          # если докупка
          if (states$pos.add[n] == 1) {
            states$n[n] <- 
              {
                2 * states$n[[n - 1]]
              } %>%
              round(.)
          }
          # если сброс
          if (states$pos.drop[n] == 1) {
            states$n[n] <- 
              {
                0.5 * states$n[[n - 1]]
              } %>%
              round(.)
          }
        }   
      }
      # изменение контрактов на такте
      states$diff.n[n] <- states$n[[n]] - states$n[[n - 1]]
      # расчёт баланса, заблокированного на ГО
      states$im.balance[n] <- states$n[[n]] * coredata(ohlc$IM[temp.index])
      # комиссия на такте
      states$commiss[n] <- basket.commiss * abs(states$diff.n[[n]])
      # баланс на такте
      states$balance[n] <- 
        states$balance[[n - 1]] + states$margin[[n]] + 
        states$im.balance[[n - 1]] - states$im.balance[[n]] - 
        states$commiss[[n]]
    }
  }
  cat('TestStrategy INFO:  Calculation Trades    OK', '\n')
  #
  # расчёт equity по корзине в states
  states$perfReturn <- states$margin - states$commiss
  states$equity <- cumsum(states$perfReturn)
  #
  data %<>%  
    # перенос данных по количеству контрактов корзины в data
    {
      merge(., states$n) %>%
      {
        data <- .
        data$n <- na.locf(data$n)
        return(data)
      }
    } %>%
    # расчёт вариационки в data
    {
      merge(., states$commiss) %>%
      {
        data <- .
        data$commiss[is.na(data$commiss)] <- 0
        data$margin <- stats::lag(data$n) * data$cret
        data$margin[1] <- 0
        return(data)
      } 
    }  
  #
  # расчёт equity по корзине в data 
  data$perfReturn <- data$margin - data$commiss
  data$equity <- cumsum(data$perfReturn)
  # расчёт баланса
  data$balance <- balance_start + data$equity
  #
  # расчёт n, margin и equity по инструментам в data и states 
  for (i in 1:length(data.names)) {
    temp.text <- 
      data.names[i] %>%
      {
        t <- paste0(
          # расчёт для states 
          'states$',.,'.n <- states$n * ',basket_weights[i],'; ',
          'states$',.,'.diff.n <- diff(states$',.,'.n) ; ',
          'states$',.,'.diff.n[1] <- 0 ; ',
          'states$',.,'.commiss <- commissions[i] * abs(states$',.,'.diff.n) ; ',
          'states$',.,'.margin <- ',
            'states$',.,'.cret * stats::lag(states$',.,'.n) ; ',
          'states$',.,'.margin[1] <- 0 ; ',
          'states$',.,'.perfReturn <- states$',.,'.margin - states$',.,'.commiss ;',
          'states$',.,'.equity <- cumsum(states$',.,'.perfReturn) ;',
          'states$',.,'.balance <- balance_start + states$',.,'.equity ;',
          # расчёт для data  
          'data$',.,'.n <- ', 
            'merge(data, states$',.,'.n) %$% ',
            'na.locf(',.,'.n) ; ',
          'data$',.,'.margin <- ',
            'data$',.,'.cret * stats::lag(data$',.,'.n) ; ',
          'data$',.,'.margin[1] <- 0 ; ',
          'data <- merge(data, states$',.,'.commiss) ; ',
          'data$',.,'.commiss[is.na(data$',.,'.commiss)] <- 0 ; ',
          'data$',.,'.perfReturn <- data$',.,'.margin - data$',.,'.commiss ;',
          'data$',.,'.equity <- cumsum(data$',.,'.perfReturn) ;',
          'data$',.,'.balance <- balance_start + data$',.,'.equity ;')
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
#
#
#
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Функции для оптимизации simple стратегии:
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
###
#' Тупая функция оптимизации одного параметра движка simple стратегии (multithread)
#' 
#' @param var.begin Стартовое значение оптимизации
#' @param var.end Конечное значение оптимизации
#'
#' @return result DF с perfomance'ами по всем итерациям цикла 
#'
#' @export
SimpleStr_BruteForceOpt.Parallel <- function(var.begin, var.end, ...) {
  #
  require(parallel)
  # запуск кластера
  parallel_cluster <- 
    parallel::detectCores() %>%
    parallel::makeCluster(.)
  ## Подгрузка данных в кластер
  # подгрузка библиотек
  clusterEvalQ(parallel_cluster, {
    library(quantmod)
    library(magrittr)
    library(tidyr)
    library(PerformanceAnalytics)
    source('bots/simple/linker.R')
  })
  # подгрузка переменных
  clusterExport(parallel_cluster, envir = .GlobalEnv, 
    varlist = c(
      'ohlc.list', 
      'add_per', 'k_mm', 'balance_start', 
      'basket_weights', 'slips', 'commissions', 'ret_type'
    )
  )
  #
  result <- 
    var.begin:var.end %>%
    parLapply(
      parallel_cluster,
      ., 
      function(x){
        SimpleStr_OneThreadRun(ohlc = ohlc.list[[1]],
                               sma_per = x, add_per, k_mm, balance_start, 
                               basket_weights, slips, commissions, ret_type)
      }
    ) 
  #
  parallel::stopCluster(parallel_cluster)
  parallel_cluster <- c()
  #
  result %<>% 
    {
      .[!is.na(.)]
    } %>%
    MergeData_inList.byRow(.)
  #
  if(!is.null(parallel_cluster)) {
    parallel::stopCluster(parallel_cluster)
    parallel_cluster <- c()
  }
  #  
  return(result)
}
###
#' Тупая функция оптимизации одного параметра движка simple стратегии (на одном ядре)
#' 
#' @param var.begin Стартовое значение оптимизации
#' @param var.end Конечное значение оптимизации
#' @param data.souce Лист с котировками
#' @param sma_per Периоды SMA
#' @param add_per Период докупок
#' @param k_mm Коэффициент MM
#' @param basket_weights Веса корзины (вектор)
#' @param slips Слипы (вектор)
#' @param commissions Комиссии (вектор)
#' @param balance_start Стартовый баланс
#'
#' @return result Лист с perfomance'ами по всем итерациям цикла 
#'
#' @export
SimpleStr_BruteForceOpt <- function(var.begin, var.end,
                                    ohlc, add_per, k_mm, balance_start, 
                                    basket_weights, slips, commissions, ret_type) {
  #
  result <- 
    var.begin:var.end %>%
    lapply(
      ., 
      function(x){
        SimpleStr_OneThreadRun(ohlc = ohlc.list[[1]],
                               sma_per = x, add_per, k_mm, balance_start, 
                               basket_weights, slips, commissions, ret_type)
      }
    ) %>%
    {
      .[!is.na(.)]
    } #%>%
    #MergeData_inList.byRow(.)
  #
  return(result)
}
#
###
#' Функция одного прогона вычислений движка simple стратегии
#' 
#' @param data.souce Лист с котировками
#' @param sma_per Периоды SMA
#' @param add_per Период докупок
#' @param k_mm Коэффициент MM
#' @param basket_weights Веса корзины (вектор)
#' @param slips Слипы (вектор)
#' @param commissions Комиссии (вектор)
#' @param balance_start Стартовый баланс
#'
#' @return list(data, states) Лист с данными отработки и данные сделок
#'
#' @export
SimpleStr_OneThreadRun <- function(ohlc = ohlc.list[[1]], 
                                   sma_per, add_per, k_mm, basket_weights, 
                                   slips, commissions, 
                                   balance_start, ret_type,
                                   fast = FALSE) {
  ### отработка тестового робота
  data_strategy.list <- SimpleStr.gear(ohlc,
                                       sma_per, add_per, k_mm, 
                                       basket_weights, slips, commissions,
                                       balance_start)
  ## Анализ perfomanc'ов
  # для стратегий, у которых нет сделок
  if (length(data_strategy.list[[1]]) == 1 && length(data_strategy.list[[2]]) == 1) {
    return()
  } else {
    ### Формирование таблицы сделок
    ## чистим от лишних записей
    data_strategy.list[[2]] <- StatesTable.clean(data_strategy.list[[2]])
    if (fast == TRUE) {
      ### оценка perfomance-параметров
      perfomanceTable <- PerfomanceTable(data = data_strategy.list[[1]], 
                                         states = 0,
                                         trades_table = 0,
                                         balance = balance_start, ret_type = 0, 
                                         fast = TRUE) 
    } else {
      ## лист с данными по сделкам (по тикерам и за всю корзину)
      trades_table.list <- TradesTable.calc(STATES = data_strategy.list[[2]], basket = TRUE, convert = TRUE)
      # очистка мусора по target = 'temp'
      CleanGarbage(target = 'temp', env = '.GlobalEnv')
      # 
      ### оценка perfomance-параметров
      perfomanceTable <- PerfomanceTable(DATA = data_strategy.list[[1]], 
                                         STATES = data_strategy.list[[2]],
                                         TRADES = trades_table.list,
                                         balance = balance_start, 
                                         ret_type = ret_type)  
    }
  }
  perfomanceTable %<>% 
    # добавление использованных параметров
    cbind.data.frame(., sma_per = sma_per, add_per = add_per, k_mm = k_mm)
  #
  return(perfomanceTable)
}