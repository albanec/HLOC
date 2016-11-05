# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Функции расчета параметров drawdown'ов:
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
###
#' Расчёт итоговой таблицы с данными drawdown'ов
#'
#' Функция вычисляет параметры по просадкам (выводит итоговые данные)
#' 
#' @param data.balance Данные balance (после  отработки стратегии)
#'
#' @return drawdown.table DF с данными по просадкам
#'
#' @export
DrawdownsTable <- function(data.balance) {
  # ----------
  # подготовка данных
  #cat('INFO(DrawdownsTable): Calc Drawdown Data Set', '\n')
  drawdowns <- Drawdowns.dd_data(data = data.balance, fullData = TRUE)
  ### вычисление summary по data set'у
  ## max просадка
  #cat('INFO(DrawdownsTable): Calc MaxDrawdown', '\n')
  max.drawdown <- 
    min(drawdowns[[2]]$Depth) %>%
    as.numeric(.)
  max.drawdown.percent <-
    min(drawdowns[[2]]$DepthPercent) %>%
    as.numeric(.)  
  ## день max просадки
  max.dd.day <- 
    drawdowns[[1]][drawdowns[[1]]$dd == max.drawdown] %>%
    {
      index(first(.)) 
    } %>%
    as.POSIXct(., origin = '1970-01-01')
  ## средняя просадка
  #cat('Calculating Performance Metric:  MeanDrawdown', '\n')
  mean.drawdown <- 
    mean(drawdowns[[2]]$Depth) %>% 
    as.numeric(.)
  mean.drawdown.percent <-
    mean(drawdowns[[2]]$DepthPercent) %>% 
    as.numeric(.)
  ## max длина просадки в днях
  #cat('Calculating Performance Metric:  MaxDrawdownDays', '\n')
  max.drawdown.days <- 
    max(drawdowns[[2]]$Days) %>%
    as.numeric(.) 
  ## среднее число дней в просадке
  #cat('Calculating Performance Metric:  MeanDrawdownDays', '\n')
  mean.drawdown.days <- 
    drawdowns[[2]]$Days[drawdowns[[2]]$Days != 0] %>%
    mean(.) %>%
    trunc(.) %>%
    as.numeric(.)
  ## текущее число дней в просадке
  #cat('Calculating Performance Metric:  NowDrawdownDays', '\n')
  now.drawdown.days <- 
    ifelse(last(drawdowns[[1]]$dd) != 0,
           last(drawdowns[[2]]$Days),
           0) %>%
    as.numeric(.)
  ## текущее число свечей в просадке
  #cat('Calculating Performance Metric:  NowDrawdownPeriods', '\n')
  now.drawdown.periods <- 
    ifelse(last(drawdowns[[1]]$dd) != 0,
           last(drawdowns[[2]]$Length),
           0) %>%
    as.numeric(.)
  ## текущая просадка 
  #cat('Calculating Performance Metric:  NowDrawdown', '\n')
  now.drawdown <- 
    ifelse(last(drawdowns[[1]]$dd) != 0,
           last(drawdowns[[1]]$dd),
           0) %>%
    as.numeric(.)
  now.drawdown.percent <- 
    ifelse(last(drawdowns[[1]]$dd) != 0,
           last(drawdowns[[1]]$dd.percent),
           0) %>%
    as.numeric(.)
  #
  ### формирование таблицы
  drawdown.table <- 
    {
      df <-
        data.frame(MaxDrawdownDay = character(1) %>% 
                                    as.numeric(.) %>% 
                                    as.Date(.),
                   MaxDrawdown = as.numeric(1),
                   MaxDrawdownPercent = as.numeric(1),
                   MeanDrawdown = as.numeric(1),
                   MeanDrawdownPercent = as.numeric(1),
                   MaxDrawdownDays = as.numeric(1),
                   MeanDrawdownDays = as.numeric(1),
                   NowDrawdownDays = as.numeric(1),
                   NowDrawdownPeriods = as.numeric(1),
                   NowDrawdown = as.numeric(1),
                   NowDrawdownPercent = as.numeric(1)
                   )     
    } %>%
    {
      .$MaxDrawdownDay <- max.dd.day
      .$MaxDrawdown <- max.drawdown
      .$MaxDrawdownPercent <- max.drawdown.percent
      .$MeanDrawdown <- mean.drawdown
      .$MeanDrawdownPercent <- mean.drawdown.percent
      .$MaxDrawdownDays <- max.drawdown.days
      .$MeanDrawdownDays <- mean.drawdown.days
      .$NowDrawdownDays <- now.drawdown.days
      .$NowDrawdownPeriods <- now.drawdown.periods
      .$NowDrawdown <- now.drawdown
      .$NowDrawdownPercent <- now.drawdown.percent
      return(.)
    }
  #
  return(drawdown.table)
}
#
###
#' Функция расчёта таблицы с данными по всем drawdown'ам
#'
#' Функция возращает df с данными по всем просадкам
#' 
#' @param data Данные balance
#'
#' @return drawdowns Таблица просадок (или list(dd.data, drawdowns))
#'
#' @export
Drawdowns.dd_data <- function(data, fullData = FALSE) {
  # ----------
  # расчёт dd
  dd.data <- Drawdowns.calc(data = data)
  n.vec <- 1:max(dd.data$num)
  # формирование таблицы со статистикой
  drawdowns <- 
    lapply(n.vec,
           function (x) {
             Drawdowns.calcSummary(data = dd.data, n = x)
           }) %>%
    MergeData_inList.byRow(.)
  #
  if (fullData == TRUE) {
    return(list(dd.data, drawdowns))  
  } else {
    return(drawdowns)  
  }
}
#
###
#' Функция параметров одного drawdown'а
#'
#' Функция возращает таблицу данных одному dd
#' 
#' @param data Данные dd
#' @param n Номер dd
#' 
#' @return dd.summary df, содержащий данные по dd c номером n
#'
#' @export
Drawdowns.calcSummary <- function(data, n) {
  #
  dd.summary <- 
    # выгружаем данные по dd с номером n
    data[data$num == n] %>%
    Convert.XTStoDF(.) %>%
    {
      df <- 
        # создаём скелет df с нужными полями
        data.frame(From = character(1) %>% 
                          as.numeric(1) %>% 
                          as.Date(1),
                   To = character(1) %>% 
                        as.numeric(1) %>% 
                        as.Date(1),
                   Depth = numeric(1),
                   DepthPercent = numeric(1),
                   Length = numeric(1),
                   Days = numeric(1),
                   row.names = NULL)
      ## заполняем поля данными
      # начало dd
      df$From <- 
        .$date[1] %>%
        as.POSIXct(., origin = '1970-01-01') 
      # конец dd
      df$To <- 
        {
          .$date[nrow(.)]
        } %>%
        as.POSIXct(., origin = '1970-01-01') 
      # максимальная глубина
      df$Depth <- min(.$dd)
      df$DepthPercent <- min(.$dd.percent)
      # длина (количество периодов)
      df$Length <- nrow(.)
      # дни в просадке
      df$Days <-
       # as.POSIXct(.$date, origin = '1970-01-01') %>%
        CalcTradingDays(x = .$date, fullDays = TRUE) 
      return(df)
    } #%>%
    # {
    #   df <- .
    #   df <- df[, -1]
    #   return(df)
    # }
  #
  return(dd.summary)
}
#
###
#' Функция расчёта drawdown'ов по balance
#'
#' Функция возращает xts с данными по всем просадкам 
#' 
#' @param data Данные balance
#' 
#' @return data XTS, солержащий данные по dd 
#'
#' @export
Drawdowns.calc <- function(data) {
  #
  # очистка от строк c одинаковым индексом (если есть)
  data <- data[which(!duplicated(index(data)))]
  ## формируем нужные столбцы
  # пики balance
  data$peak <- cummax(data[, 1])
  # значения dd на каждой свече
  data$dd <- data[, 1] - data$peak 
  # 1 - свеча в просадке, 0 - не в просадке 
  data$temp <- abs(sign(data$dd))
  # точки перехода в/из просадки (1 - первая свеча в просадке, -1 - точка выхода из просадки, 0 - состояние не меняется)
  data$temp.diff <- diff(data$temp)
  # 1 - если свечка относится к dd (с учётом обновления пика на выходе из просадки)
  data$temp.bars <- abs(sign(data$temp + data$temp.diff))
  #
  data <- 
   {
     # индексы строк роста balance
     tempIndex <- which(data$temp.diff == 0 & data$temp.bars == 0)
     # если есть такие периоды
     if (length(tempIndex) != 0) {
       # удаляем их
       data <- data[-tempIndex]
       # точки выхода из dd метим 1 (это нужно для нумирации просадок в дальнейшем)
       data$temp.diff[which(data$temp.diff == -1)] <- 0  
     }      
     return(data)
    } %>%
    na.omit(.)
  # нумерация просадок
  data$num <- cumsum(data$temp.diff)
  # собираем мусор
  data <- 
    CleanGarbage.inCols(data) %>%
    # выкидываем balance столбец (исходные данные)
    {
      .[, -1]
    }
  # 
  data$dd.percent[data$peak == 0] <- NA
  data$dd.percent[data$peak != 0] <- data$dd[data$peak != 0] * 100 / data$peak[data$peak != 0]
  #
  return(data)
}
#