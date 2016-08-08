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
DrawdownTable <- function(returns,
                          ret.type = "ret", 
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
#' @param returns Данные return'ов
#' @param days Нужно ли считать текущее количество дней в просадке
#' @param geometric Тип сложения  
#'
#' @return drawdowns Таблица просадок
#'
#' @export
CalcDrawdownDataSet <- function(returns, days = TRUE, geometric = TRUE) {
  require(PerformanceAnalytics)
  # ----------
  #
  drawdowns <- CalcDrawdowns(returns[, 1], top = 1000000, geometric = geometric)
  if (days == TRUE) {
    for (i in seq(1:nrow(drawdowns))) {
      drawdowns$Days <- as.numeric(-floor(difftime(drawdowns$From, drawdowns$To, units = "days"))) 
    }  
  }
  return(drawdowns)
}
#
###
#' Функция расчёта drawdown'ов
#'
#' Функция возращает таблицу данных по всем просадкам 
#' 
#' @param R Данные return'ов
#' @param top "Разрешение" итоговой таблицы
#' @param digits Округление по количеству знаков после запятой
#'
#' @return result Таблица просадок
#'
#' @export
CalcDrawdowns <- function(R, top = 5, digits = 4, geometric = TRUE) {
  R <- 
    checkData(R[, 1, drop = FALSE]) %>%
    na.omit(R)
  x <- 
    findDrawdowns(R, geometric = geometric) %>%
    sortDrawdowns(.)
  ndrawdowns <- sum(x$return < 0)
  if (ndrawdowns < top) {
    warning(paste("Only ", ndrawdowns, " available in the data.", 
                  sep = ""))
    top <- ndrawdowns
  }
  result <- data.frame(time(R)[x$from[1:top]], time(R)[x$trough[1:top]], 
                       time(R)[x$to[1:top]], base::round(x$return[1:top], digits), 
                       x$length[1:top], x$peaktotrough[1:top], ifelse(is.na(time(R)[x$to[1:top]]), 
                       NA, x$recovery[1:top]))
  colnames(result) <- c("From", "Trough", "To", "Depth", "Length", "To Trough", "Recovery")
  dummy <- TRUE
  if (!dummy) {
    Depth <- NULL
  } 
  subset(result, subset = (Depth < 0))
  return(result)
}
#

#
