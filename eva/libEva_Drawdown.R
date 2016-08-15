# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Функции расчета параметров drawdown'ов:
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
###
#' Расчёт итоговой таблицы с данными drawdown'ов
#'
#' Функция вычисляет параметры по просадкам (выводит итоговые данные)
#' 
#' @param equity Данные equity (после  отработки стратегии)
#' @param dd.value Абсолютные ("abs"), дробные ("ratio") значения dd или и то, и другое ("both")
#'
#' @return drawdown.table DF с данными по просадкам
#'
#' @export
DrawdownTable <- function(equity, dd.value) {
  # ----------
  # подготовка данных
  #cat("INFO(DrawdownTable): Calc Drawdown Data Set", "\n")
  drawdowns <- CalcDrawdownDataSet(data = equity, dd.value = dd.value, fullData = TRUE)
  ### вычисление summary по data set'у
  # max просадка
  #cat("INFO(DrawdownTable): Calc MaxDrawdown", "\n")
  if (dd.value == "ratio") {
    max.drawdown <-
      na.omit(drawdowns[[2]]) %>%
      {
        min(.$Depth)
      } %>%
      as.numeric(.)  
  } else {
    max.drawdown <- 
      min(drawdowns[[2]]$Depth) %>%
      as.numeric(.)
    if (dd.value == "both") {
      max.drawdown.ratio <-
        na.omit(drawdowns[[1]]) %>%
        {
          min(.$dd.ratio)
        } %>%
        as.numeric(.)  
    }
  }
  # средняя просадка
  #cat("Calculating Performance Metric:  MeanDrawdown", "\n")
  if (dd.value == "ratio") {
    mean.drawdown <-
      na.omit(drawdowns[[2]]) %>% 
      {
        mean(.$Depth)
      } %>% 
      as.numeric(.)
  } else {
    mean.drawdown <- 
      mean(drawdowns[[2]]$Depth) %>% 
      as.numeric(.)
    if (dd.value == "both") {
      mean.drawdown.ratio <-
        na.omit(drawdowns[[1]]) %>% 
        {
          x <- .
          x %<>% 
            {
              cummin(.$dd.ratio)
            } %>%
            unique(.) %>%
            {
              mean(.)
            }
          return(x)
        } %>% 
        as.numeric(.)
    }
  }
  # max длина просадки в днях
  #cat("Calculating Performance Metric:  MaxDrawdownDays", "\n")
  max.drawdown.days <- 
    max(drawdowns[[2]]$Days) %>%
    as.numeric(.)
  # среднее число дней в просадке
  #cat("Calculating Performance Metric:  MeanDrawdownDays", "\n")
  mean.drawdown.days <- 
    mean(drawdowns[[2]]$Days) %>%
    trunc(.) %>%
    as.numeric(.)
  #
  # текущее число дней в просадке
  #cat("Calculating Performance Metric:  NowDrawdownDays", "\n")
  now.drawdown.days <- 
    ifelse(last(drawdowns[[1]]$dd) != 0,
           last(drawdowns[[2]]$Days),
           0) %>%
    as.numeric(.)
  # текущее число свечей в просадке
  #cat("Calculating Performance Metric:  NowDrawdownPeriods", "\n")
  now.drawdown.periods <- 
    ifelse(last(drawdowns[[1]]$dd) != 0,
           last(drawdowns[[2]]$Length),
           0) %>%
    as.numeric(.)
  # текущая просадка 
  #cat("Calculating Performance Metric:  NowDrawdown", "\n")
  if (dd.value == "both") {
    now.drawdown <- 
      ifelse(last(drawdowns[[1]]$dd) != 0,
             last(drawdowns[[1]]$dd),
             0) %>%
      as.numeric(.)
    now.drawdown.ratio <- 
      ifelse(last(drawdowns[[1]]$dd) != 0,
             last(drawdowns[[1]]$dd.ratio),
             0) %>%
      as.numeric(.)
  } else {
    now.drawdown <- 
    ifelse(last(drawdowns[[1]]$dd) != 0,
           last(drawdowns[[1]]$dd),
           0) %>%
    as.numeric(.)
  }
  # формирование таблицы
  if (dd.value == "both") {
    drawdown.table <- 
      cbind(max.drawdown, max.drawdown.ratio, mean.drawdown, mean.drawdown.ratio,
                       max.drawdown.days, mean.drawdown.days, 
                       now.drawdown.days, now.drawdown.periods, now.drawdown, now.drawdown.ratio) %>%
      data.frame(.)
    colnames(drawdown.table) <- c("MaxDrawdown", "MaxDrawdownRatio", 
                                  "MeanDrawdown", "MeanDrawdownRatio", 
                                  "MaxDrawdownDays", "MeanDrawdownDays", "NowDrawdownDays", 
                                  "NowDrawdownPeriods", "NowDrawdown", "NowDrawdownRatio")  
  } else {
    drawdown.table <- 
      cbind(max.drawdown, mean.drawdown,
                       max.drawdown.days, mean.drawdown.days, 
                       now.drawdown.days, now.drawdown.periods, now.drawdown) %>%
      data.frame(.)
    colnames(drawdown.table) <- c("MaxDrawdown", "MeanDrawdown" , 
                                  "MaxDrawdownDays", "MeanDrawdownDays", "NowDrawdownDays", 
                                  "NowDrawdownPeriods", "NowDrawdown")  
  }
  #drawdown.table %<>% 
   # Convert_XTStoDF(.)
  #
  return(drawdown.table)
}
#
###
#' Функция расчёта таблицы с данными по всем drawdown'ам
#'
#' Функция возращает df с данными по всем просадкам
#' 
#' @param data Данные equity
#' @param dd.value Абсолютные ("abs"), дробные ("ratio") значения dd или и то, и другое ("both")
#'
#' @return drawdowns Таблица просадок (или list(dd.data, drawdowns))
#'
#' @export
CalcDrawdownDataSet <- function(data, dd.value, fullData = FALSE) {
  # ----------
  # расчёт dd
  dd.data <- CalcDrawdowns(data = data, dd.value = dd.value)
  n.vec <- 1:max(dd.data$num)
  # формирование таблицы со статистикой
  drawdowns <- 
    lapply(n.vec,
           function (x) {
             CalcOneDrawdownSummary_DF(data = dd.data, n = x)
           }) %>%
    MergeData_inList_byRow(.)
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
#' @param data Данные equity
#' @param n Номер dd
#' 
#' @return dd.summary df, содержащий данные по dd c номером n
#'
#' @export
CalcOneDrawdownSummary_DF <- function(data, n) {
  #
  dd.summary <- 
    # выгружаем данные по dd с номером n
    data[data$num == n] %>%
    Convert_XTStoDF(.) %>%
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
                   Length = numeric(1),
                   row.names = NULL)
      ## заполняем поля данными
      # начало dd
      df$From <- 
        .$date[1] %>%
        as.POSIXct(., origin = "1970-01-01") 
      # конец dd
      df$To <- 
        {
          .$date[nrow(.)]
        } %>%
        as.POSIXct(., origin = "1970-01-01") 
      # максимальная глубина
      df$Depth <- min(.$dd)
      # длина (количество периодов)
      df$Length <- nrow(.)
      # дни в просадке
      df$Days <-
       # as.POSIXct(.$date, origin = "1970-01-01") %>%
        ndays(.$date) 
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
#' Функция расчёта drawdown'ов по equity
#'
#' Функция возращает xts с данными по всем просадкам 
#' 
#' @param data Данные equity
#' @param dd.value Абсолютные ("abs"), дробные ("ratio") значения dd или и то, и другое ("both")
#' 
#' @return data XTS, солержащий данные по dd 
#'
#' @export
CalcDrawdowns <- function(data, dd.value = "abs") {
  #
  # очистка от строк c одинаковым индексом (если есть)
  data <- data[-which(duplicated(index(data)))]
  ## формируем нужные столбцы
  # пики equity
  data$peak <- cummax(data[, 1])
  # значения dd на каждой свече
  data$dd <- data[, 1] - data$peak 
  # 1 - свеча в просадке, 0 - не в просадке 
  data$temp <- abs(sign(data$dd))
  # точки перехода в/из просадки (1 - первая свеча в просадке, -1 - точка выхода из просадки, 0 - состояние не меняется)
  data$temp.diff <- diff(data$temp)
  # 1 - если свечка относится к dd (с учётом обновления пика на выходе из просадки)
  data$temp.ticks <- abs(sign(data$temp + data$temp.diff))
  #
  data <- 
   {
     # индексы строк роста equity
     tempIndex <- which(data$temp.diff == 0 & data$temp.ticks == 0)
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
    CleanGarbage_inCols(data) %>%
    # выкидываем equity столбец (исходные данные)
    {
      .[, -1]
    }
  if (dd.value != "abs") {
    if (dd.value == "ratio") {
      # вычисление только дробных значений dd 
      data$dd[data$peak == 0] <- NA
      data$dd[data$peak != 0] <- data$dd[data$peak != 0] * 100 / data$peak[data$peak != 0]
    } else {
      # в слечае вычисления и дробных и абсолютных значений
      data$dd.ratio[data$peak == 0] <- NA
      data$dd.ratio[data$peak != 0] <- data$dd[data$peak != 0] * 100 / data$peak[data$peak != 0]
    }
  }
  #
  return(data)
}
#

#
