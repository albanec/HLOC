#
###
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
CleanStatesTable <- function(data) {
  data %<>%
    # условие для фильтрации "пустых сделок" (т.е. фактически ранее уже закрытых позиций)
    {
      df <- . 
      df <-  df[-which(df$n == 0 & df$diff.n ==0)]
      return(df)
    } 
  return(data)
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
#'
#' @return DealsTable data.frame содержащий все сделки
#'
#' @export
CalcOneDealSummary_DF <- function(data, n, data.names) {
  #
  if (!exists("data.names")) {
    data.names <- 
      grep(".Open", names(data)) %>%
      names(data.source)[.] %>%
      sub(".Open", "", .)   
  }
  deal.summary <- 
    data[data$pos.num == n] %T>%
    {
      temp.data <<- . 
    } %>%
    {
      temp.data <- .
      data.names.list <- 
        data.names %>%
        {
          names(.) <- .
          return(.)
        } %>%
        as.list(.)
      temp.data <- 
        lapply(data.names.list, 
               function(x) {
                 temp.text <- 
                   paste("names.set <- c(\"pos.num\", \"pos\", \"pos.add\", \"pos.drop\", ",
                         "\"",x,".n\", \"",x,".diff.n\", \"",x,".Open\", ",
                         "\"",x,".commiss\", \"",x,".margin\", \"",x,".equity\") ;" ,
                         sep = "")
                 eval(parse(text = temp.text))
                 temp.data <- 
                   temp.data[, (which(colnames(temp.data) %in% names.set))] %>%
                   {
                     names(.) <- c("pos", "pos.num", "pos.add", "pos.drop", "Open", 
                                   "n", "diff.n", "commiss", "margin", "equity")
                     return(.)
                   } %>%
                   # нумерация субсделок (x.0 - открытия/закрытия и x.1...n - для изменений внутри)
                   {
                     df <- .
                     # субномера
                     df$subnum <- 
                       nrow(df) %>%
                       { 
                         ifelse(. < 3, 
                                0,
                                0.1)
                       } %>% 
                       as.vector
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
                     return(df)
                   }
                 return(temp.data)
               }) %>%
        MergeData_inList_byRow(.) %>%
        Convert_XTStoDF(.) %>%
        {
          df <- 
            nrow(.) %>%
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
                       Margin = numeric(.),
                       Equity = numeric(.),
                       row.names = NULL)
          df$PositionNum <- .$pos.num
          df$PositionType <- ifelse(.$pos[1] == 1, 
                                    "Длинная", 
                                    "Короткая")
          df$Ticker <- data.names
          df$N <- .$n
          df$diff.N <- .$diff.n
          df$OpenSignal <- ifelse(.$pos == 1, 
                                  ifelse(.$pos.add == 1, 
                                         "ИзменПоРынку", 
                                         ifelse(.$pos.drop == 0, 
                                                "ОткрПозиПоРынк", 
                                                NA)),
                                  ifelse(.$pos == -1,
                                         ifelse(.$pos.add == 1, 
                                                "ИзменПоРынку1",
                                                ifelse(.$pos.drop == 0, 
                                                       "ОткрПозиПоРынк1",
                                                       NA)),
                                         NA))
          df$OpenDate <- 
            ifelse(.$pos != 0 & .$pos.drop == 0, 
                   .$date, 
                   NA) %>%
            as.POSIXct(., origin = "1970-01-01") 
          df$OpenValue <- ifelse(.$pos != 0 & .$pos.drop == 0, 
                                 .$Open, 
                                 NA)
          df$OpenCommiss <- ifelse(.$pos != 0 & .$pos.drop == 0, 
                                   .$commiss, 
                                   NA)
          df$CloseSignal <- ifelse(.$pos == 0,
                                   ifelse(.$pos[1] == 1, 
                                          "ЗакрПозиПоРынк", 
                                          "ЗакрПозиПоРынк1"), 
                                   ifelse(.$pos.drop != 0, 
                                          ifelse(.$pos[1] == 1, 
                                                 "ЗакрПозиПоРынк", 
                                                 "ЗакрПозиПоРынк1"), 
                                          NA))
          df$CloseDate <- 
            ifelse(.$pos == 0 | .$pos.drop != 0, .$date, NA) %>%
            as.POSIXct(., origin = "1970-01-01") 
          df$CloseValue <- ifelse(.$pos == 0 | .$pos.drop != 0, .$Open, NA)
          df$CloseCommiss <- ifelse(.$pos == 0 | .$pos.drop != 0, .$commiss, NA)
          df$Margin <- .$margin
          df$Equity <- .$equity
          return(df)
        } %>%
        {
          df <- .
          df <- df[, -1]
          return(df)
        }
      #
      return(temp.data)
    }
  return(deal.summary)
} 
#
###
#' Создание итоговой таблицы сделок
#' 
#' @param data Входной xts ряд сделок
#' @param data.names Вектор тикеров
#'
#' @return DealsTable data.frame содержащий все сделки
#'
#' @export
CalcDealsTable_DF <- function(data, ...) {
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
  # расчёт таблицы
  DealsTable <- 
    # посделочный расчёт, на выходе лист с df по каждой сделке
    lapply(pos.num.list,
           function (x) {
             CalcOneDealSummary_DF(data, n = x, data.names)
           }) %>%
    # объединение данных внутри листа в один df
    MergeData_inList_byRow(.) %>%
    # вывод данных по открытию/закрытию позиций в одну строку
    {
      df <- . 
      df <- 
        # ряд номеров позиций
        last(df$PositionNum) %>%
        1:. %>%
        # перенос данных в нужные строки
        {
          numDeals <- .
          # нужные столбцы (касаются закрытия)
          colNum <-
            c("CloseSignal", "CloseDate", "CloseValue", "CloseCommiss", "Margin", "Equity") %>%
            {
              which(colnames(df) %in% .) 
            }
          # индексы строк открытия/закрытия
          openRowIndex <- which((df$PositionNum %in% numDeals) & !is.na(df$OpenSignal))
          closeRowIndex <- which((df$PositionNum %in% numDeals) & is.na(df$OpenSignal))
          df <- 
            # инъекция данных
            {
              df[openRowIndex, colNum] <- df[closeRowIndex, colNum]
              return(df)
            } %>%
            # очистка ненужных данных (удаление лога закрытий)
            {
              df <- df[-closeRowIndex, ]
              return(df)
            }
          #
          return(df)
        }
      return(df)
    }
  return(DealsTable)
}
#

