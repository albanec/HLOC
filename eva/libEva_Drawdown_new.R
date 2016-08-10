# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Функции расчета параметров drawdown'ов:
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
###
#' Расчёт итоговой таблицы drawdown'ов
#'
#' Функция вычисляет параметры по просадкам (выводит итоговые данные)
#' 
#' @param returns Данные return'ов
#' @param period Период свечей
#' @param ret.type Тип return'ов (ret/sret/lret)
#' @param TRUE/FALSE график
#'
#' @return drawdown.table Фрейм с данными по просадкам
#'
#' @export
DrawdownTable <- function(equity, dd.value, 
                          # plot = FALSE, 
                          period = "15min") {
  #
  require(PerformanceAnalytics)
  # ----------
  # определение способа суммирования
  if (ret.type == "sret") {
    TF <- TRUE
  } else {
    TF <- FALSE
  }
  # перевод периода в цифры
  period <- sub("min", "", period)
  # подготовка данных
  cat("Calculating Drawdown Metric:", "Drawdown Data Set", "\n", sep = "  ")
  drawdowns <- CalcDrawdownDataSet(returns, days = TRUE, geometric = TF)
  # max просадка
  cat("Calculating Performance Metric:", "MaxDrawdown", "\n", sep = "  ")
  max.drawdown <- as.numeric(drawdowns$Depth[1])
  # средняя просадка
  cat("Calculating Performance Metric:", "MeanDrawdown", "\n", sep = "  ")
  mean.drawdown <- as.numeric(mean(drawdowns$Depth))
  # max длина просадки в днях
  cat("Calculating Performance Metric:", "MaxDrawdownDays", "\n", sep = "  ")
  max.drawdown.days <- 
    which.max(drawdowns$Days) %>%
    drawdowns$Days[.] %>%
    as.numeric(.)
  # среднее число дней в просадке
  cat("Calculating Performance Metric:", "MeanDrawdownDays", "\n", sep = "  ")
  mean.drawdown.days <- 
    na.omit(drawdowns$Days) %>% 
    mean(.) %>%
    trunc(.) %>%
    as.numeric(.)
  # текущее число дней в просадке
  cat("Calculating Performance Metric:", "NowDrawdownDays", "\n", sep = "  ")
  now.drawdown.days <- 
    which(is.na(drawdowns$To)) %>%
    drawdowns$Length[.] %>%
    {
      . * as.numeric(period)
    } %>%
    {
      -floor(-(. / 60 / 24))
    } %>%
    as.numeric(.) %>%
    {
      x <- ifelse(is.na(.),
                  0,
                  .)
      return(x)
    }
  # текущее число свечей в просадке
  cat("Calculating Performance Metric:", "NowDrawdownPeriods", "\n", sep = "  ")
  now.drawdown.periods <- 
    which(is.na(drawdowns$To)) %>%
    drawdowns$Length[.] %>%
    {
      -floor(-(.))
    } %>%
    as.numeric(.)
  # текущая просадка 
  cat("Calculating Performance Metric:", "NowDrawdown", "\n", sep = "  ")
  now.drawdown <- 
    last(returns) %>%
    DrawdownPeak(.) %>%
    as.numeric(.)
  # формирование таблицы
  drawdown.table <- 
    cbind.data.frame(max.drawdown, 
                     mean.drawdown, max.drawdown.days, mean.drawdown.days, 
                     now.drawdown.days, now.drawdown.periods, now.drawdown) %>%
    data.frame(.)
  colnames(drawdown.table) <- c("MaxDrawdown", "MeanDrawdown" , 
                                "MaxDrawdownDays", "MeanDrawdownDays", "NowDrawdownDays", 
                                "NowDrawdownPeriods", "NowDrawdown")  
  return(drawdown.table)
}
#
###
#' Функция расчёта таблицы drawdown'ов
#'
#' Функция возращает таблицу данных по всем просадкам + кол-во дней в текущей просадке 
#' (формирует ряд для дальнейшего анализа)
#' 
#' @param data Данные equity
#' @param days Нужно ли считать текущее количество дней в просадке
#'
#' @return drawdowns Таблица просадок
#'
#' @export
CalcDrawdownDataSet <- function(data, days = TRUE, dd.value) {
  # ----------
  #
  drawdowns <- CalcDrawdowns(data, dd.value)
  n.dd <- 1:max(drawdowns$num)
  #
  drawdowns.table <- 
    lapply(n.dd,
           function (x) {
             CalcOneDrawdownSummary_DF(data, n = x)
           }) %>%
    MergeData_inList_byRow(.)
  

  if (days == TRUE) {
    for (i in seq(1:nrow(drawdowns))) {
      drawdowns$Days <- as.numeric(-floor(difftime(drawdowns$From, drawdowns$To, units = "days"))) 
    }  
  }
  return(drawdowns)
}
#
CalcOneDrawdownSummary_DF <- function(data, n) {
  dd.summary <- 
    data[data$num == n] %>%
    {
      x <- .
      from <- index(x[1])
      to <- index(last(x))
      depth <- min(x$dd)
      length <- nrow(x)

    }
}
#
###
#' Функция расчёта drawdown'ов
#'
#' Функция возращает таблицу данных по всем просадкам 
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
  data <- data[-which(duplicated(indedata(data)))]
  # формируем нужные столбцы
  data$peak <- cummadata(data[, 1])
  data$dd <- data[, 1] - data$peak 
  data$temp <- abs(sign(data$dd))
  data$temp.diff <- diff(data$temp)
  data$temp.ticks <- abs(sign(data$temp + data$temp.diff))
  #
  data <- 
   {
     data <- data[-which(data$temp.diff == 0 & data$temp.ticks == 0)]
     data$temp.diff[which(data$temp.diff == -1)] <- 0
     return(data)
    } %>%
    na.omit(data)
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
      data$dd[data$peak == 0] <- NA
      data$dd[data$peak != 0] <- data$dd[data$peak != 0] / data$peak[data$peak != 0]
    } else {
      data$dd.ratio[data$peak == 0] <- NA
      data$dd.ratio[data$peak != 0] <- data$dd[data$peak != 0] / data$peak[data$peak != 0]
    }
  }
  #
  return(data)
}
#

#
