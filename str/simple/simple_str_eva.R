#
data.names <- 
    grep(".Open", names(data.source)) %>%
    names(data.source)[.] %>%
    sub(".Open", "", .)
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
CleanStates <- function(data) {
  data %<>%
    {
      df <- . 
      df <-  df[-which(df$pos != 0 & df$n == 0 & df$diff.n ==0)]
      return(df)
    } 
  return(data)
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
TestStrategy_DealsTable_DF <- function(data, data.names) {
  if (!exists("data.names")) {
    data.names <- 
      grep(".equity", names(data)) %>%
      names(data.source)[.] %>%
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
  DealsTable <- 
    lapply(pos.num.list,
           function (x) {
             CalcDealSummary_DF(data, n = x, data.names)
           }) %>%
    MergeData_inList_byRow(.) %>%
    {
    df <- . 
    df <- 
      last(df$PositionNum) %>%
        1:. %>%
        {
          numDeals <- .
          colNum <-
            c("CloseSignal", "CloseDate", "CloseValue", "CloseCommiss", "Margin", "Equity") %>%
            {
              which(colnames(df) %in% .) 
            }
          openRowIndex <- which((df1$PositionNum %in% numDeals) & !is.na(df1$OpenSignal))
          closeRowIndex <- which((df1$PositionNum %in% numDeals) & is.na(df1$OpenSignal))

          df <- sapply(numDeals, 
                       function(x) {
                         
                         
                         #df <- 
                           #{
                           #  df[openRowIndex, colNum] <- df[closeRowIndex, colNum]
                           #  return(df)
                           #}
                        return(list(openRowIndex, closeRowIndex))
                       })
          return(df)
        }
      return(df)
    }
  return(DealsTable)
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
CalcDealSummary_DF <- function(data, n, data.names) {
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
                   {
                     df <- .
                     df$subnum <- 
                       nrow(df) %>%
                       { 
                         ifelse(. < 3, 
                                0,
                                0.1)
                       } %>% 
                       as.vector
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
