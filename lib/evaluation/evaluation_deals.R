# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Функции для вычисления таблицы сделок
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
###
#' Создание итоговой таблицы сделок
#' 
#' @param data Входной xts ряд сделок
#' @param data.names Вектор тикеров (необязательно)
#' @param convert Переносить открытия/закрытия в одну строку или нет (по умолчанию нет)
#'
#' @return list List, содержащий все сделки
#'
#' @export
DealsTables.calc <- function(data, convert = FALSE, ...) {
  if (!exists("data.names")) {
    data.names <- 
      grep(".equity", names(data)) %>%
      names(data)[.] %>%
      sub(".equity", "", .)   
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
             CalcOneDealSummary.df(data, n = x, data.names = data.names, type = "tickers")
           }) %>%
    # объединение данных внутри листа в один df
    MergeData_inList.byRow(.)
  ### расчёт таблицы сделок (данные по корзине)
  dealsTable_byBasket <- 
    lapply(pos.num.list,
           function (x) {
             CalcOneDealSummary.df(data, n = x, data.names = data.names, type = "basket")
           }) %>%
    MergeData_inList.byRow(.)
  #
  if (convert != FALSE) {
    dealsTable_byTickers %<>%
      DealsTable.convert(data.deals = ., type = "tickers")
    dealsTable_byBasket %<>%
      DealsTable.convert(data.deals = ., type = "basket")
  }
  #
  return(list(dealsTable_byBasket, dealsTable_byTickers))
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
DealsTable.convert <- function(data.deals, type = "tickers") {
  ## Номера столбцов, касающихся закрытия 
  numDeals <- 
    # ряд номеров позиций
    last(data.deals$PositionNum) %>%
    1:. 
  # Выделение нужных столбцов (касаются закрытия)
  if (type == "tickers") {
    colNum <-
      c("CloseSignal", "CloseDate", "CloseValue", "CloseCommiss", "DealReturn", "Equity", "PositionBars") %>%
      {
        which(colnames(data.deals) %in% .) 
      }
  } else {
    colNum <-
      c("CloseSignal", "CloseDate", "CloseCommiss", 
        "DealReturn", "DealReturnPercent", 
        "DealEquity", "DealEquityPercent", "Equity", 
        "PositionBars") %>%
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
#' @param n Номер сделки
#' @param data.names Вектор тикеров
#' @type Считать по тикерам или по портфелю
#'
#' @return DealsTable data.frame содержащий все сделки
#'
#' @export
CalcOneDealSummary.df <- function(data, type, n, ...) {
  #
  if (!exists("data.names")) {
    data.names <- 
      grep(".Open", names(data)) %>%
      names(data)[.] %>%
      sub(".Open", "", .)   
  }
  deal.summary <- 
    # вытаскиваем данные по сделке n
    data[data$pos.num == n] %T>%
    {
      temp.data <<- . 
    } %>%
    {
      temp.data <- .
      # список тикеров
      data.names.list <- 
        data.names %>%
        {
          names(.) <- .
          return(.)
        } %>%
        as.list(.)
      ## расчёт данных сделок (по тикеру или корзине)
      if (type == "tickers") {
        temp.data <- 
          # данные по каждому тикеру выкидываем в отдельный подлист
          lapply(
            data.names.list, 
            function(x) {
              # правильно прописываем названия столбцов с нужными данными (в names.set)
              temp.text <- 
                paste("names.set <- c(\"pos\", \"pos.num\", \"pos.bars\", \"pos.add\", \"pos.drop\", ",
                      "\"",x,".n\", \"",x,".diff.n\", \"",x,".Open\", ",
                      "\"",x,".commiss\", \"",x,".equity\", \"",x,".perfReturn\") ;" ,
                      sep = "")                  
              eval(parse(text = temp.text))
              temp.data <- 
                # вытаскиваем нужные столбцы (по names.set)
                temp.data[, (which(colnames(temp.data) %in% names.set))] %>%
                # для удобства переименуем 
                {        
                  names(.) <- c("pos", "pos.num", "pos.bars", "pos.add", "pos.drop", "Open", "n", "diff.n",  
                                "commiss", "deal.return", "equity") 
                  return(.)
                } %>%
                # нумерация субсделок (x.0 - открытия/закрытия и x.1...n - для изменений внутри)
                {
                  df <- .
                  # субномера
                  df$subnum <- 
                    nrow(df) %>%
                    { 
                      ifelse(
                        . < 3, 
                        0,
                        ifelse(
                          . > 9,
                          0.01,
                          ifelse(
                            . > 10,
                            0.001,
                            0.1
                          )
                        )
                      )
                    } %>% 
                    as.vector(.)
                  # расчёт номеров
                  df$pos.num <- 
                    df$subnum %>%
                    {
                      .[1] <- 0 
                      return(.)
                    } %>%
                    cumsum(.) %>%
                    {
                      .[nrow(.)] <- 0
                       df$pos.num + .
                    }
                  #
                  return(df)
                }
              #
              return(temp.data)
            }
          ) %>%
          MergeData_inList.byRow(.)
      } else {
        names.set <- c("pos", "pos.num", "pos.bars", "pos.add", "pos.drop", "balance", 
                       "n", "diff.n", "commiss", "equity", "perfReturn") 
        temp.data <- 
          temp.data[, (which(colnames(temp.data) %in% names.set))] %>%
          {
            names(.) <- c("pos", "pos.num", "pos.bars", "pos.add", "pos.drop", "balance",
                          "n", "diff.n", "commiss", "equity", "deal.return") 
            return(.)
          } %>%
          {
            df <- .
            df$subnum <- 
              nrow(df) %>%
                { 
                  ifelse(
                    . < 3, 
                    0,
                    ifelse(
                      . > 9,
                      0.01,
                      ifelse(
                        . > 10,
                        0.001,
                        0.1
                      )
                    )
                  ) 
                } %>%
              as.vector(.)
            df$pos.num <- 
              df$subnum %>%
              {
                .[1] <- 0 
                return(.)
              } %>%
              cumsum(.) %>%
              {
                .[nrow(.)] <- 0
                df$pos.num + .
              } 
            #
            return(df)
          } 
      }
      #
      return(temp.data)
    } %>%
    # конвертируем таблицу в DF      
    Convert.XTStoDF(.) %>%
    ### расчёт итогового DF
    {
      df <- 
        nrow(.) %>%
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
      df$PositionNum <- .$pos.num
      ## тип позиции
      df$PositionType <- ifelse(.$pos[1] == 1, 
                                "Длинная", 
                                "Короткая")
      ## имя тикера
      if (type == "tickers") {
        df$Ticker <- data.names
      } else {
        df$Ticker <- "basket"
      }
      ## количество контрактов тикера
      df$N <- .$n
      ## изменения контрактов тикера на текущей сделке
      df$diff.N <- .$diff.n
      ## сигнал открытия
      df$OpenSignal <- ifelse(
        .$pos == 1, 
        ifelse(
          .$pos.add == 1, 
          "ИзменПоРынку", 
          ifelse(
            .$pos.drop == 0, 
            "ОткрПозиПоРынк", 
            NA
          )
        ),
        ifelse(
          .$pos == -1,
          ifelse(
            .$pos.add == 1, 
            "ИзменПоРынку1",
            ifelse(
              .$pos.drop == 0, 
              "ОткрПозиПоРынк1",
              NA
            )
          ),
          NA
        )
      )
      # дата открытия позиции
      df$OpenDate <- 
        ifelse(.$pos != 0 & .$pos.drop == 0, 
               .$date, 
               NA) %>%
        as.POSIXct(., origin = "1970-01-01") 
      ## цена тикера на открытии (не используется в "basket" режиме)
      if (type == "tickers") {
        df$OpenValue <- ifelse(.$pos != 0 & .$pos.drop == 0, 
                               .$Open, 
                               NA)
      } else {
        df$OpenValue <- NA
      }
      ## комиссия на закрытии
      df$OpenCommiss <- ifelse(.$pos != 0 & .$pos.drop == 0, 
                               .$commiss, 
                               NA)
      ## сигнал закрытия
      df$CloseSignal <- ifelse(
        .$pos == 0,
        ifelse(
          .$pos[1] == 1, 
          "ЗакрПозиПоРынк", 
          "ЗакрПозиПоРынк1"
        ), 
        ifelse(
          .$pos.drop != 0, 
          ifelse(
            .$pos[1] == 1, 
            "ЗакрПозиПоРынк", 
            "ЗакрПозиПоРынк1"
          ), 
          NA
        )
      )
      ## дата закрытия
      df$CloseDate <- 
        ifelse(.$pos == 0 | .$pos.drop != 0, 
               .$date, 
               NA) %>%
        as.POSIXct(., origin = "1970-01-01") 
      ## цена тикера на закрытии (не используется в "basket" режиме)
      if (type == "tickers") {
        df$CloseValue <- ifelse(.$pos == 0 | .$pos.drop != 0, 
                                .$Open, 
                                NA)
      } else {
        df$CloseValue <- NA
      }
      ## коммиссия на закрытии
      df$CloseCommiss <- ifelse(.$pos == 0 | .$pos.drop != 0, 
                                .$commiss, 
                                NA)
      ## return позиции
      df$DealReturn <- .$deal.return
      ## return позиции в %
      if (type == "basket") {
        df$DealReturnPercent <- .$deal.return * 100 / first(.$balance)
      } else {
        df$DealReturnPercent <- NA  
      }
      ## equity внутри сделки
      df$DealEquity <- cumsum(.$deal.return)
      if (type == "basket") {
        df$DealEquityPercent <- df$DealEquity * 100 / first(.$balance)
      } else {
        df$DealEquityPercent <- NA
      }
      ## изменения equity тикера
      df$Equity <- .$equity
      ## тики позиций
      df$PositionBars <- .$pos.bars
      #
      return(df)
    } %>%
    {
      df <- .
      df <- df[, -1]
      return(df)
    }
  #   #
  #   return(temp.data)
  # }
  # #
  return(deal.summary)
} 
#
###
#' Очистка таблицы состояний от пустых сделок
#'
#' Чистит таблицу сделок от строк, у которых df$pos != 0 & df$n == 0 & df$diff.n ==0
#' 
#' @param data Входной xts ряд состояний
#'  
#' @return data Очищенный xts ряд состояний
#'
#' @export
StatesTable.clean <- function(data) {
  data %<>%
    # условие для фильтрации "пустых сделок" (т.е. фактически ранее уже закрытых позиций)
    {
      df <- . 
      if ((df$n == 0 && df$diff.n ==0) != FALSE) {
        df <- df[-which(df$n == 0 & df$diff.n ==0)]
      }
      return(df)
    } 
  return(data)
} 
