# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Функции для анализа параметров бектеста
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Расчет временных метрик:
EVA_DateTable <- function(states.data) {
  # ----------
  # Общее описание:
  #   функция свода временных параметров работы стратегии
  # Входные данные:
  #   states.data: ряд сделок
  # Выходные данные:
  #   metric.table: таблица метрик
  # Зависимости:
  require(PerformanceAnalytics)
  # ----------
  #
  cat("Build Date Metrics", "\n", sep = "  ")
  date.table <- cbind(ticker, from.date, to.date, period, 
  ))
}
#
EVA_CalcTradingDays <- function(from.date, to.date, calendar = "Russia/MOEX") {
  # ----------
  # Общее описание:
  # функция возвращает количетво торговых дней за период бека по календарю биржи
  # Входные данные:
  # from.date, to.date: начало/конец торговли 
  # calendar: регион/биржа торгового календаря
  # Выходные данные:
  # tradind.days: количество торговых дней
  # Зависимости:
  require(RQuantLib)
  # ----------
  #
  from.date <- as.Date(from.date) 
  to.date <- as.Date(to.date)
  tradind.days <- businessDaysBetween(calendar, from = from.date, to = to.date, includeFirst = 1, includeLast = 1)
  return (tradind.days)
}
#
# Расчет коэффициентов продуктивности:
EVA_RatioTable <- function(returns.data, returns.type) {
  # ----------
  # Общее описание:
  #   функция вычисления стандартного наборов метрик продуктивности результатов работы стратегий
  # Входные данные:
  #   returns.data: ряд доходностей
  # Выходные данные:
  #   metric.table: таблица метрик
  # Зависимости:
  require(PerformanceAnalytics)
  # ----------
  #
  temp.logic <- ifelse(returns.type == "sret", TRUE, FALSE)    
  #
  # расчет коэффициентов:
  # SharpeRatio
  sharp.data <-  SharpeRatio.annualized(returns.data, scale = 1, geometric = temp.logic)
  # SortinoRatio
  sortino.data <- SortinoRatio(returns.data, geometric = temp.logic)
  # CalmarRatio
  calmar.data <- CalmarRatio(returns.data, scale = 1, geometric = FALSE)
  # SterlingRatio
  sterling.data <- SterlingRatio(returns.data, scale = 1, geometric = FALSE)
  # формирование таблицы
  metric.table <- cbind(EVA_TransformMetric(sharp.data, metric.name = "SharpRatio"), 
                        EVA_TransformMetric(sortino.data, metric.name = "SortinoRatio"), 
                        EVA_TransformMetric(calmar.data, metric.name = "CalmarRatio"),
                        EVA_TransformMetric(sterling.data, metric.name = "SterlingRatio"))
  return(metric.table)
}
# Вспомогательные фунции:
EVA_TransformMetric <- function(metric.data, metric.name) {
  # ----------
  # Общее описание:
  #   функция трансформации данных метрик к одному виду (нужна для RatioTable)
  # Входные данные:
  #   metric.data: данные метрики
  #   metric.name: название метрики
  # Выходные данные:
  #   metric.data: транформированная в столбец метрика
  # ----------
  #
  cat("Calculating Performance Metric:", metric.name, "\n")
  metric.data <- as.matrix(metric.data)
  # трансформация в нужный вид:
  if (nrow(metric.data) == 1) {
    metric.data <- t(metric.data)
  }
  colnames(metric.data) <- metric.name
  rownames(metric.data) <- ""
  return (metric.data)
}
# Расчет Drawdown метрик:
EVA_DrawdownDataSet <- function(returns.data, days = TRUE) {
  # ----------
  # Общее описание:
  # функция возращает таблицу просадок + кол-во дней в просадке (формирует ряд для анализа)
  # Входные данные:
  #   returns.data: ряд доходностей
  # Выходные данные:
  #   drawdowns: таблица просадок
  # Зависимости:
  require(PerformanceAnalytics)
  # ----------
  #
  drawdowns <- table.Drawdowns(returns.data[, 1], top = 1000000)
  if (days == TRUE) {
    for (i in seq(1:nrow(drawdowns))) {
      drawdowns$Days <- as.numeric(-floor(difftime(drawdowns$From, drawdowns$To, units = "days"))) 
    }  
  }
  return (drawdowns)
}
#
EVA_DrawdownTable <- function(returns.data, plot = FALSE, period = "15") {
  # ----------
  # Общее описание:
  # вычисляет параметры по просадкам (выводит итоговые данные)
  # Входные данные:
  #   returns.data: ряд доходностей
  #   plot: TRUE/FALSE график
  #   period: период свечей (в минутах)  
  # Выходные данные:
  #   drawdown.table: фрейм с данными по просадке
  # Зависимости:
  require(PerformanceAnalytics)
  # ----------
  #
  # подготовка данных
  cat("Calculating Drawdown Metric:", "Drawdown Data Set", "\n", sep = "  ")
  drawdowns <- EVA_DrawdownDataSet(returns.data, days = TRUE)
  # max просадка
  cat("Calculating Performance Metric:", "MaxDrawdown", "\n", sep = "  ")
  max.drawdown <- as.numeric(drawdowns$Depth[1])
  # средняя просадка
  cat("Calculating Performance Metric:", "MeanDrawdown", "\n", sep = "  ")
  mean.drawdown <- as.numeric(mean(drawdowns$Depth))
  # max длина просадки в днях
  cat("Calculating Performance Metric:", "MaxDrawdownDays", "\n", sep = "  ")
  max.drawdown.days <- as.numeric(drawdowns$Days[which.max(drawdowns$Days)])
  # среднее число дней в просадке
  cat("Calculating Performance Metric:", "MeanDrawdownDays", "\n", sep = "  ")
  mean.drawdown.days <- as.numeric(trunc(mean(na.omit(drawdowns$Days))))
  # текущее число дней в просадке
  cat("Calculating Performance Metric:", "NowDrawdownDays", "\n", sep = "  ")
  now.drawdown.days <- as.numeric(-floor(-(drawdowns$Length[which(is.na(drawdowns$To))] * as.numeric(period) / 60 / 24)))
  # текущее число свечей в просадке
  cat("Calculating Performance Metric:", "NowDrawdownPeriods", "\n", sep = "  ")
  now.drawdown.periods <- as.numeric(-floor(-(drawdowns$Length[which(is.na(drawdowns$To))] )))
  # текущая просадка 
  cat("Calculating Performance Metric:", "NowDrawdown", "\n", sep = "  ")
  now.drawdown <- as.numeric(DrawdownPeak(last(returns.data)))
  # формирование таблицы
  drawdown.table <- cbind(max.drawdown, mean.drawdown, max.drawdown.days, 
                          mean.drawdown.days, now.drawdown.days, now.drawdown.periods, now.drawdown)
  drawdown.table <- data.frame(drawdown.table)
  colnames(drawdown.table) <- c("MaxDrawdown", "MeanDrawdown" , "MaxDrawdownDays", 
                                "MeanDrawdownDays", "NowDrawdownDays", "NowDrawdownPeriods", "NowDrawdown")  
  return(drawdown.table)
}
#
#
# Вывод графиков:
EVA_BigPlot <- function(returns, MarginPlot = FALSE, ReturnsPlot = FALSE, DrawdownShadowPlot = FALSE, DrawdownPlot = FALSE, CandlePlot = FALSE, period = "15 min") {
  # ----------
  # Общее описание:
  # 
  # Входные данные:
  # 
  # Выходные данные:
  # 
  # Зависимости:
  require(PerformanceAnalytics)
  require(plotly)
  # ----------
  print (paste("Calculating Metric:", "Drawdown Data Set"))
  n <- 0
  mPlot <- NA
  rPlot <- NA
  dsPlot <- NA
  dPlot <- NA
  cPlot <- NA
  mPlot.par <- NA
  rPlot.par <- NA
  dsPlot.par <- NA
  dPlot.par <- NA
  cPlot.par <- NA
  if (MarginPlot == TRUE) {
    n <- n + 1
    if (drawdownShadow.plot == TRUE) {
      print(paste("Calculating Charts:", "Margin & DrawdownShadow Chart"))
      drawdowns <- EVA_DrawdownDataSet(returns, days = FALSE)
      drawdowns.dates <- cbind(format(drawdowns$From),format(drawdowns$To))
      drawdowns.dates[is.na(drawdowns.dates)] <- format(index(returns)[NROW(returns)])
      drawdowns.dates <- lapply(seq_len(nrow(drawdowns.dates)), function(i) drawdowns.dates[i,])  
      mPlot.par <- par(mar = c(1,4,4,2))
      mPlot <- chart.CumReturns(  returns, main = "PerformanceCharts", colorset = c(2,3,4), 
        period.areas = drawdowns.dates, period.color = rgb[1],
        legend.loc = "topleft", geometric = TRUE, ylab = "Cumulative Simple Returns"
        )
      if (DrawdownShadowPlot == FALSE) {
        remove(drawdowns)
        remove(drawdowns.dates)
      }
    } else {
      print(paste("Calculating Charts:", "Margin Chart"))
      mPlot.par <- par(mar = c(1,4,4,2))
      mPlot <- chart.CumReturns(returns, main = "PerformanceCharts", colorset = c(2,3,4), 
        legend.loc = "topleft", geometric = TRUE, ylab = "Cumulative Simple Returns"
        )
    }
  }
  if (DrawdownPlot == TRUE) {
    print(paste("Calculating Charts:", "Drawdown Chart"))
    n <- n+1
    dPlot.par <- par(mar = c(5,4,0,2))
    dPlot <- chart.Drawdown(returns, main = "", colorset = c(2,3,4), legend.loc = "topleft", geometric = TRUE, ylab = "Drawdowns")     
  }  
  if (ReturnsPlot == TRUE) {
    print(paste("Calculating Charts:", "Simple Returns Chart"))
    n <- n + 1
    rPlot.par <- par(mar = c(1,4,0,2))
    rPlot <- chart.BarVaR(returns, main = "", ylab = paste(date.label, "Returns"))
  }
  if (CandlePlot == TRUE) {
    print(paste("Calculating Charts:", "Drawdown Length in Candles Chart"))
    if (drawdownShadow.plot == FALSE) {
      drawdowns <- EVA_DrawdownDataSet(returns, days = FALSE)
    } 
    drawdowns <- drawdowns[order(drawdowns$From), ]
    rownames(drawdowns) <- 1:nrow(drawdowns)
    firstRun <- TRUE
    for (i in seq(1:nrow(drawdowns))) {
      time <- seq(drawdowns$From[i], drawdowns$To[i], by = period )
      candles <- seq(from = 0, to = drawdowns$Length[18], by = drawdowns$Length[18]/length(time))
      candles <- candles[-1]
      candles <- data.frame(Time = time, Length = candles2)
      cxts <- xts(candles[,-1], order.by = candles[,1])
      if(firstRun){
        firstRun <- FALSE
        final.candles <- candles 
      } else {
        final.candles <- rbind(final.candles, candles)
      }
    }
    remove(drawdowns)
    remove(drawdowns.dates)
    n <- n + 1
    cPlot.par <- par(mar = c(1,4,0,2))
    cPlot <- plot(final.candles)
  }
  print(paste("Calculating Charts:", "Build Form"))
  charts.matrix <- seq(1:n)
  layout(matrix(charts.matrix), heights = c(2, 1, 1.3), widths = 1)
  if (exists("mPlot")) {
    print(paste("Painting Chart:", "Margin Chart"))
    mPlot.par
    mPlot
  }
  if (exists("rPlot")) {
    print(paste("Painting Chart:", "Simple Returns Chart"))
    rPlot.par
    rPlot
  }
  if (exists("dsPlot")) {
    print(paste("Painting Chart:", "DrawdownShadow Chart"))
    dsPlot.par
    dsPlot
  }
  if (exists("dPlot")) {
    print(paste("Painting Chart:", "Dropdown Chart"))
    dPlot.par
    dPlot
  }
  if (exists("cPlot")) {
    print(paste("Painting Chart:", "Candles Chart"))
    cPlot.par
    cPlot
  } 
}
#

