# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Функции для вычисления таблицы сделок
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
###
#' Создание итоговой таблицы сделок
#' 
#' @param data Входной xts ряд сделок ( == data.state)
#' @param basket Расчитывать по корзине, или нет (T/F)
#' @param ticker.names Вектор тикеров (необязательно)
#' @param convert Переносить открытия/закрытия в одну строку или нет (по умолчанию нет)
#'
#' @return list List, содержащий все сделки
#'
#' @export
DealsTables.calc <- function(data = data.strategy.list[[2]], basket = FALSE, convert = FALSE, ticker.names = NULL,
                             bto.name = 'BTO', bto_add.name = 'BTO_add',
                             sto.name = 'STO', sto_add.name = 'STO_add',
                             stc.name = 'STC', stc_drop.name = 'STC_drop',
                             btc.name = 'BTC', btc_drop.name = 'BTC_drop') {
  if (is.null(ticker.names)) {
    ticker.names <- 
      grep('.equity', names(data)) %>%
      names(data)[.] %>%
      sub('.equity', '', .)   
  }
  pos.num.list <- 
    max(data$pos.num) %>%
      1:. %>%
    {
      names(.) <- .
      return(.)
    } %>%
    as.list(.)
  #
  ### расчёт таблицы сделок (данные по каждому тикеру)
  dealsTable_byTickers <- 
    # посделочный расчёт, на выходе лист с df по каждой сделке
    lapply(pos.num.list,
           function (x) {
             DealSummary(data, type = 'byTicker', n = x, ticker.names = ticker.names, 
                         bto.name, bto_add.name,
                         sto.name, sto_add.name,
                         stc.name, stc_drop.name,
                         btc.name, btc_drop.name)
           }) %>%
    # объединение данных внутри листа в один df
    MergeData_inList.byRow(.)
  if (convert != FALSE) {
    dealsTable_byTickers %<>% DealsTable.convert(data.deals = ., type = 'byTicker')
  }
  ### расчёт таблицы сделок (данные по корзине)
  if (basket == TRUE) {
    dealsTable_byBasket <- 
      lapply(pos.num.list,
             function (x) {
               DealSummary(data, type = 'byBasket', n = x, ticker.names = ticker.names, 
                           bto.name = , bto_add.name,
                           sto.name, sto_add.name,
                           stc.name, stc_drop.name,
                           btc.name, btc_drop.name)
             }) %>%
      MergeData_inList.byRow(.)
    if (convert != FALSE) {
      dealsTable_byBasket %<>% DealsTable.convert(data.deals = ., type = 'byBasket')  
    }
    result.list <- list(dealsTable_byBasket, dealsTable_byTickers)
    names(result.list) <- c('byBasket', 'byTicker')
  } else {
    result.list <- list(dealsTable_byTickers)
    names(result.list) <- c('byTicker')
  }
  #
  return(result.list)
}
#
###
#' Конвертирование таблицы сделок в нужный вид
#' 
#' Функция производит вывод данных по открытию/закрытию позиций в одну строку
#' 
#' @param data.deal Данные таблицы сделок
#' @param type Тип данных для анализа (tickers/basket)
#'
#' @return result Изменённая таблица данных сделок
#'
#' @export
DealsTable.convert <- function(data.deals, type = 'byTicker') {
  ## Номера столбцов, касающихся закрытия 
  numDeals <- 
    # ряд номеров позиций
    last(data.deals$PositionNum) %>%
    1:. 
  # Выделение нужных столбцов (касаются закрытия)
  if (type == 'byTicker') {
    colNum <-
      c('CloseSignal', 'CloseDate', 'CloseValue', 'CloseCommiss', 'DealReturn', 'Equity', 'PositionBars') %>%
      {
        which(colnames(data.deals) %in% .) 
      }
  } else {
    colNum <-
      c('CloseSignal', 'CloseDate', 'CloseCommiss', 'DealReturn', 'DealReturnPercent', 'DealEquity', 
        'DealEquityPercent', 'Equity', 'PositionBars') %>%
      {
        which(colnames(data.deals) %in% .) 
      }
  }
  ### Перенос данных в нужные строки
  # индексы строк открытия
  openRowIndex <- which(
    (data.deals$PositionNum %in% numDeals) & !is.na(data.deals$OpenSignal)
  )
  # индексы строк закрытия
  closeRowIndex <- which(
    (data.deals$N == 0) & !is.na(data.deals$CloseSignal)
  )
  ## Вывод данных по открытию/закрытию позиций в одну строку
  data.deals <- 
    # инъекция данных
    {              
      data.deals[openRowIndex, colNum] <- data.deals[closeRowIndex, colNum]
      data.deals$DealReturn[openRowIndex] <- data.deals$DealEquity[closeRowIndex]
      data.deals$DealReturnPercent[openRowIndex] <- data.deals$DealEquityPercent[closeRowIndex] 
      return(data.deals)
    } %>%
    # очистка ненужных данных (удаление лога закрытий)
    {
      data.deals <- data.deals[-closeRowIndex, ]
      data.deals$DealEquity <- NULL
      data.deals$DealEquityPercent <- NULL
      return(data.deals)
    }
  #
  return(data.deals)
}
#
###
#' Вычисление данных по одной сделке
#' 
#' Выборка данных для конкретного немера сделки
#' 
#' @param data Входной xts ряд сделок
#' @param type ???
#' @param n Номер сделки
#' @param ticker.names Вектор тикеров
#' @type Считать по тикерам или по портфелю
#' @param bto_add.name Написание увеличения позиции long
#' @param sto.name Написание открытия short
#' @param sto_add.name Написание увеличения позиции short
#' @param stc.name Написание закрытия long
#' @param stc_drop.name Написание сброса позиции long
#' @param btc.name Написание закрытия short
#' @param btc_drop.name Написание сброса позиции short
#'
#' @return DealsTable data.frame содержащий все сделки
#'
#' @export
DealSummary <- function(data, type, n, ticker.names = NULL,
                        bto.name, bto_add.name,
                        sto.name, sto_add.name,
                        stc.name, stc_drop.name,
                        btc.name, btc_drop.name) {
  #
  if (is.null(ticker.names)) {
    ticker.names <- 
      grep('.Price', names(data)) %>%
      names(data)[.] %>%
      sub('.Price', '', .)   
  } 
  # вытаскиваем данные по сделке n
  data <- data[data$pos.num == n] 
  # лист тикеров
  ticker.names.list <- as.list(ticker.names)
  
  ## расчёт данных сделок (по тикеру или корзине)
  if (type == 'byTicker') {
    data <- 
      # данные по каждому тикеру выкидываем в отдельный подлист
      lapply(ticker.names.list, 
             function(x) {
               # правильно прописываем названия столбцов с нужными данными (в names.set)
               temp.text <- paste0('names.set <- c(\"pos\", \"pos.num\", \"pos.bars\", \"pos.add\", \"pos.drop\", ',
                                  '\"',x,'.n\", \"',x,'.diff.n\", \"',x,'.Price\", ',
                                  '\"',x,'.commiss\", \"',x,'.equity\", \"',x,'.perfReturn\") ;',
                                  sep = '')
               eval(parse(text = temp.text))
               # вытаскиваем нужные столбцы (по names.set)
               result <- data[, (which(colnames(data) %in% names.set))]
               
               # для удобства переименуем     
               names(result)[names(result) == paste0(x,'.n', sep = '')] <- 'n'
               names(result)[names(result) == paste0(x,'.diff.n', sep = '')] <- 'diff.n'
               names(result)[names(result) == paste0(x,'.Price', sep = '')] <- 'Price'
               names(result)[names(result) == paste0(x,'.commiss', sep = '')] <- 'commiss'
               names(result)[names(result) == paste0(x,'.equity', sep = '')] <- 'equity'
               names(result)[names(result) == paste0(x,'.perfReturn', sep = '')] <- 'deal.return'
            
               # нумерация субсделок (x.0 - открытия/закрытия и x.1...n - для изменений внутри)
               result$pos.num %<>% DealSummary.pos_num(x = .)
               #
               return(result)
             }) %>%
      MergeData_inList.byRow(.)
  } else {
    names.set <- c('pos', 'pos.num', 'pos.bars', 'pos.add', 'pos.drop', 'balance', 
                   'n', 'diff.n', 'commiss', 'equity', 'perfReturn') 
    data <- data[, (which(colnames(data) %in% names.set))]
    names(data)[names(data) == 'perfReturn'] <- 'deal.return' 
    data$pos.num %<>% DealSummary.pos_num(x = .)
  }
  
  # конвертируем таблицу в DF      
  data %<>% Convert.XTStoDF(.) 
  
  ### расчёт итогового DF
  deal_summary <- DealSummary.summary_df(data, type, ticker.names,
                                         bto.name, bto_add.name,
                                         sto.name, sto_add.name,
                                         stc.name, stc_drop.name,
                                         btc.name, btc_drop.name)
  
  deal_summary <- deal_summary[, -1]
  #
  return(deal_summary)
} 
#
###
#' Вычисление номеров сделок в нужном для DealsTable виде (вспомогательная функция)
#' 
#' @param x XTS с номерами позиций из data.state
#'
#' @return x Ряд сделок в нужном для DealsTable виде
#'
#' @export
DealSummary.pos_num <- function(x) {
  row.num <- nrow(x)
  x <- 
    row.num %>%
    { 
      ifelse(. < 3, 
             0,
             ifelse(. > 9,
                    0.01,
                    ifelse(. > 10,
                           0.001,
                           0.1)))
    } %>% 
    rep(., row.num) %>%
    # расчёт номеров
    {
      .[1] <- 0 
      return(.)
    } %>%
    cumsum(.) %>%
    {
      .[nrow(.)] <- 0
       x + .
       return(x)
    }
  #
  return(x)  
}
#
###
#' Вычисление итогового df по сделке (вспомогательная функция)
#' 
#' @param data Входной xts ряд одной сделки
#' @param type Тип анализа (byTicker/byBasket)
#' @param ticker.names Вектор тикеров
#' @param bto.name Написание открытия long
#' @param bto_add.name Написание увеличения позиции long
#' @param sto.name Написание открытия short
#' @param sto_add.name Написание увеличения позиции short
#' @param stc.name Написание закрытия long
#' @param stc_drop.name Написание сброса позиции long
#' @param btc.name Написание закрытия short
#' @param btc_drop.name Написание сброса позиции short
#'
#' @return summary data.frame, содержащий данные по сделке
#'
#' @export
DealSummary.summary_df <- function(x, type, ticker.names = NULL,
                                   bto.name, bto_add.name,
                                   sto.name, sto_add.name,
                                   stc.name, stc_drop.name,
                                   btc.name, btc_drop.name) {
  # bto.name = 'ОткрПозиПоРынк',
  # bto_add.name = 'ИзменПоРынку',
  # sto.name = 'ОткрПозиПоРынк1',
  # sto_add.name = 'ИзменПоРынку1',
  # stc.name = 'ЗакрПозиПоРынк',
  # stc_drop.name = 'ЗакрПозиПоРынк',
  # btc.name = 'ЗакрПозиПоРынк1',
  # btc_drop.name = 'ЗакрПозиПоРынк1'

  if (is.null(ticker.names)) {
    ticker.names <- 
      grep('.Price', names(data)) %>%
      names(data)[.] %>%
      sub('.Price', '', .)   
  } 

  summary <- 
    nrow(x) %>%
    # форомирование скелета DF
    data.frame(PositionNum = numeric(.),
               PositionType = character(.),
               Ticker = character(.),
               N = numeric(.),
               diff.N = integer(.),
               OpenSignal = character(.),
               OpenDate = character(.) %>% 
                          as.numeric() %>% 
                          as.Date(),
               OpenValue = integer(.),
               OpenCommiss = integer(.),
               CloseSignal = character(.),
               CloseDate = character(.) %>% 
                           as.numeric() %>% 
                           as.Date(),
               CloseValue = integer(.),
               CloseCommiss = integer(.),
               DealReturn = numeric(.),
               DealReturnPercent = numeric(.),
               DealEquity = numeric(.),
               DealEquityPercent = numeric(.),
               Equity = numeric(.),
               PositionBars = numeric(.),
               row.names = NULL)
  
  ## номера позиций
  summary$PositionNum <- x$pos.num
  ## тип позиции
  summary$PositionType <- ifelse(x$pos[1] == 1, 
                                 'Длинная', 
                                 'Короткая')
  ## имя тикера
  if (type == 'byTicker') {
    summary$Ticker <- ticker.names
  } else {
    summary$Ticker <- 'Basket'
  }
  ## количество контрактов тикера
  summary$N <- x$n
  ## изменения контрактов тикера на текущей сделке
  summary$diff.N <- x$diff.n
  ## сигнал открытия
  summary$OpenSignal <- ifelse(x$pos == 1, 
                               ifelse(x$pos.add == 1, 
                                      bto_add.name,
                                      ifelse(x$pos.drop == 0, 
                                             bto.name,
                                             NA)),
                               ifelse(x$pos == -1,
                                      ifelse(x$pos.add == 1, 
                                             sto_add.name,
                                             ifelse(x$pos.drop == 0, 
                                                    sto.name,
                                                    NA)),
                                      NA))
  # дата открытия позиции
  summary$OpenDate <- 
    ifelse(x$pos != 0 & x$pos.drop == 0, 
           x$date, 
           NA) %>%
    as.POSIXct(., origin = '1970-01-01') 
  ## цена тикера на открытии (не используется в "basket" режиме)
  if (type == 'byTicker') {
    summary$OpenValue <- ifelse(x$pos != 0 & x$pos.drop == 0, 
                                x$Price, 
                                NA)
  } else {
    summary$OpenValue <- NA
  }
  ## комиссия на закрытии
  summary$OpenCommiss <- ifelse(x$pos != 0 & x$pos.drop == 0, 
                                x$commiss, 
                                NA)
  ## сигнал закрытия
  summary$CloseSignal <- ifelse(x$pos == 0,
                                ifelse(x$pos[1] == 1, 
                                       stc.name,
                                       btc.name), 
                                ifelse(x$pos.drop != 0, 
                                       ifelse(x$pos[1] == 1, 
                                              stc_drop.name,
                                              btc_drop.name), 
                                       NA)
                                )
  ## дата закрытия
  summary$CloseDate <- 
    ifelse(x$pos == 0 | x$pos.drop != 0, 
           x$date, 
           NA) %>%
    as.POSIXct(., origin = '1970-01-01') 
  ## цена тикера на закрытии (не используется в 'byBasket' режиме)
  if (type == 'byTicker') {
    summary$CloseValue <- ifelse(x$pos == 0 | x$pos.drop != 0, 
                                 x$Price, 
                                 NA)
  } else {
    summary$CloseValue <- NA
  }
  ## коммиссия на закрытии
  summary$CloseCommiss <- ifelse(x$pos == 0 | x$pos.drop != 0, 
                                 x$commiss, 
                                 NA)
  ## return позиции
  summary$DealReturn <- x$deal.return
  ## return позиции в %
  if (type == 'byBasket') {
    summary$DealReturnPercent <- x$deal.return * 100 / first(x$balance)
  } else {
    summary$DealReturnPercent <- NA  
  }
  ## equity внутри сделки
  summary$DealEquity <- cumsum(x$deal.return)
  if (type == 'byBasket') {
    summary$DealEquityPercent <- summary$DealEquity * 100 / first(x$balance)
  } else {
    summary$DealEquityPercent <- NA
  }
  ## изменения equity тикера
  summary$Equity <- x$equity
  ## тики позиций
  summary$PositionBars <- x$pos.bars
  #
  return(summary)
}
#