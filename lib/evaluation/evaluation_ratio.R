# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Функции расчета коэффициентов продуктивности:
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
###
#' Итоговая функция расчёта коэф. продуктивности
#'
#' Функция вычисления стандартного наборов метрик продуктивности результатов работы стратегий.
#' Формирует таблицу с коэффициентами (Шарп, Сортино, Кальмар, Стерлинг).
#' 
#' @param returns Данные return'ов
#' @param ret.type Тип return'ов (ret/sret/lret)
#'  
#' @return ratioTable Таблица с коэффициентами
#'
#' @export
RatioTable <- function(returns, ret.type) {
  require(PerformanceAnalytics)
  #
  # от типа используемых return'ов сильно зависят дальнейшие вычисления;
  # функции пакета PerfomanceAnalythics по умолчанию настроены на расчёт SR (т.е. на геометрический рассчёт)
  if (ret.type == 'sret') {
    TF <- TRUE
  } else {
    TF <- FALSE
  }
  # расчет коэффициентов:
  # SharpeRatio
  sharp.data <-  SharpeRatio.annualized(returns, scale = 1, geometric = TF)
  # SortinoRatio
  sortino.data <- SortinoRatio(returns)
  # CalmarRatio
  calmar.data <- CalmarRatio.calc(returns, scale = 1, geometric = TF)
  # SterlingRatio
  sterling.data <- SterlingRatio.calc(returns, scale = 1, geometric = TF)
  # формирование таблицы
  ratioTable <- cbind.data.frame(Ratio.transformMetric(sharp.data, metric.name = 'SharpRatio'), 
                                  Ratio.transformMetric(sortino.data, metric.name = 'SortinoRatio'), 
                                  Ratio.transformMetric(calmar.data, metric.name = 'CalmarRatio'),
                                  Ratio.transformMetric(sterling.data, metric.name = 'SterlingRatio'))
  #pMetric[,1] <- 
  return(ratioTable)
}
#
###
#' Функция расчёта коэф. Кальмара
#'
#' Вычисляет коэф. Кальмара [Custom версия функции PerfomanceAnalytics::CalmarRatio]
#' 
#' @param R Данные return'ов
#' @param scale Коэф. масштабирования
#' @param geometric Геометрическое/арифметическое сложение
#'  
#' @return result Значение коэф. Кальмара
#'
#' @export
CalmarRatio.calc <- function(R, scale = NA, geometric = TRUE) {
  R = checkData(R)
  if (is.na(scale)) {
    freq = periodicity(R)
    switch(freq$scale, 
           minute = {
             stop('Data periodicity too high')
           }, 
           hourly = {
             stop('Data periodicity too high')
           }, 
           daily = {
             scale = 252
           }, 
           weekly = {
             scale = 52
           }, 
           monthly = {
             scale = 12
           }, 
           quarterly = {
             scale = 4
           }, 
           yearly = {
             scale = 1
           })
  }
  annualized_return = Return.annualized(R, scale = scale, geometric = geometric)
  drawdown = abs(maxDrawdown(R, geometric = geometric))
  result = annualized_return/drawdown
  rownames(result) = 'Calmar Ratio'
  return(result)
}
#
###
#' Функция расчёта коэф. Стерлинга
#'
#' Вычисляет коэф. Стерлинга. [Custom версия функции PerfomanceAnalytics::SterlingRatio]
#' 
#' @param R Данные return'ов
#' @param scale Коэф. масштабирования
#' @param geometric Геометрическое/арифметическое сложение
#' @param excess Округление
#'  
#' @return result Значение коэф. Стерлинга
#'
#' @export
SterlingRatio.calc <- function(R, scale = NA, excess = 0.1, geometric = TRUE) {
  R = checkData(R)
  if (is.na(scale)) {
    freq = periodicity(R)
    switch(freq$scale, 
           minute = {
             stop('Data periodicity too high')
           }, 
           hourly = { 
             stop('Data periodicity too high')
           }, 
           daily = {
             scale = 252
           }, 
           weekly = {
             scale = 52
           }, 
           monthly = {
             scale = 12
           }, 
           quarterly = {
             scale = 4
           }, 
           yearly = {
             scale = 1
           })
  }
  annualized_return = Return.annualized(R, scale = scale, geometric = geometric)
  drawdown = abs(maxDrawdown(R, geometric = geometric) + excess)
  result = annualized_return / drawdown
  rownames(result) = paste('Sterling Ratio (Excess = ', round(excess * 100, 0), '%)', sep = '')
  return(result)
}
#
###
#' Трансформация данных метрик
#'
#' Функция трансформации данных метрик к одному виду (нужна для RatioTable)
#' 
#' @param metric.data Данные метрики
#' @param metric.name Название метрики
#'  
#' @return metric.data Транформированная в столбец метрика
#'
#' @export
Ratio.transformMetric <- function(metric.data, metric.name) {
  #
  #cat('Calculating Performance Metric:', metric.name, '\n')
  metric.data <- as.matrix(metric.data)
  # трансформация в нужный вид:
  if (nrow(metric.data) == 1) {
    metric.data <- t(metric.data)
  }
  colnames(metric.data) <- metric.name
  rownames(metric.data) <- ''
  return (metric.data)
}
#