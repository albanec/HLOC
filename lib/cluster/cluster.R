# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Функции кластерного анализа:
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
###
#' Подготовка данных для кластерного анализа
#' 
#' @param data Perfomance данные
#' @param n.mouth Количество месяцев торговли (нужно для нормирования)
#' @param low = FALSE, 
#' @param hi = FALSE, 
#' @param q.hi = 0, 
#' @param q.low = 0,
#' @param one.scale == FALSE
#' @param plusplus: использовать простой k-mean или k-mean++
#' @param iter.max: число итераций k-mean
#'
#' @return ss.df DF суммарного отклонения по кластерам
#'
#' @export
CalcKmean.preparation <- function(data, n.mouth ,
                                  hi = TRUE, q.hi = 0.5, 
                                  low = FALSE, q.low = 0,
                                  one.scale = FALSE) {
  #
  # выделение столбцов с переменными
  df <- Subset_byTarget.col(data = data, target = '_')
  # добавление столбцов с профитом и просадкой
  df$profit <- 
    {
      which(colnames(data) %in% 'ProfitPercent')
    } %>%
    data[, .] 
  df$draw <- 
    {
      which(colnames(data) %in% 'DrawdownMaxPercent')
    } %>%
    data[, .] 
  # создание нормированной метрики
  df$profit.norm <- (df$profit * 12) / (abs(df$draw) * n.mouth)
  #
  if (hi | low != FALSE) {
    df <-
      colnames(df) %>%
      length(.) %>% 
      {
        CalcQuantile(data = df, var = ., hi = hi, q.hi = q.hi)
      }
  }
  if (one.scale == TRUE) {
    var.cols <- grep(colnames(data), '_')
    df[nrow(df)+1, ] <- c(
      rep(0, length(var.cols)+2), 
      df$profit.norm[[which.max(df$profit.norm)]]
    )
    df[nrow(df)+1, ] <- c(rep(0, length(var.cols) + 3))
  }
  #
  return(df)
}
###
#' Определение оптимального числа k-mean кластеров
#' 
#' @param data подготовленные результаты отработки бэктеста 
#' (должны содержать в себе только переменные и нужные метрики)
#' @param plusplus: использовать простой k-mean или k-mean++
#' @param iter.max: число итераций k-mean
#'
#' @return ss.df DF суммарного отклонения по кластерам
#'
#' @export
CalcKmean.parameters <- function(data, test.range = 30, iter.max = 100, plusplus = FALSE) {
  #
  n <- nrow(data)
  if (n < test.range) {
    cluster.range <- 2:(n - 1)
  } else {
    cluster.range <- 2:test.range 
  }
  #cat(cluster.range)
  #Isolate required features
  data$profit <- NULL
  data$draw <- NULL
  data <- na.omit(data)
  # расчет суммарного квадрата расстояния точек внутри тестовых кластеров
  ss <- c()
  p.exp <- c()
  for (i in cluster.range){
    #cat(i , '\n')
    if (plusplus == TRUE) {
      cluster.data <- CalcKmean.plusplus(data, n.opt = i, iter.max = iter.max)
    } else {
      cluster.data <- kmeans(data, centers = i, iter.max)  
    }
    ss <- c(ss, cluster.data$tot.withinss)
    p.exp = c(p.exp, 1 - cluster.data$tot.withinss / cluster.data$totss)
  }
  # сводим всё в df
  ss.df <- data.frame(Num.Of.Clusters = cluster.range,
                      Total.Within.SS = ss,
                      Pct.Change = c(NA, diff(ss)/ss[1:length(ss)-1]) * 100,
                      Pct.Exp = p.exp)
  # вычисление оптимального количества кластеров
  # byVar: опт. число кластеров определяется как min число, описывающее 90% пространства
  n.byVar <- 
    {
      min(which(ss.df$Pct.Exp > 0.9))
    } %>%
    ss.df$Num.Of.Clusters[.]
  # byElbow: опт. число определяется 'методом локтя'
  #n.byElbow <- FindMaxDistancePoint(ss.df$p.exp[-1]) + 1
  #n <- c(n.byVar, n.byElbow)
  #n.opt <- n[which.max(n)]
  n.opt <- n.byVar 
  #
  return(list(ss.df, n.opt))
}
#
###
#' Функция вычисления модного k-mean++ 
#' 
#' @param data Подготовленные данные
#' @param n.opt Оптимальное число кластеров для заданного набора данных
#' @param iter.max Количество итераций вычислений кластера
#'
#' @return cluster.data Лист с данными кластера
#'
#' @export
CalcKmean.plusplus <- function(data, n.opt, iter.max = 100) {
  #
  # количество точек
  n <- nrow(data)
  # число измерений 
  n.dim <- ncol(data)
  # ID центров (номера строк, содержащих точки центров)
  centers <- c()
  # цикл подбора центров
  for (i in 1:n.opt) {
    if (i == 1 ) {
      # ID первого центра
      center.id <- round(runif(1, 1, n))
      centers  <- c(centers, center.id)
      # рассчёт квадратов расстояний от точек до центра
      if (n.dim == 1) {
        data$s <- apply(
          cbind(data[center.id, ]), 
          1, 
          function(center) {
            rowSums((data - center)^2) 
          }
        )
      } else {
        data$s <- apply(
          data[center.id, ], 
          1, 
          function(center) {
            rowSums((data - center)^2) 
          }
        )
      }
      # рассчёт кум. суммы квадратов расстояний
      data$ss <- cumsum(data$s)
    } else {
      # цикл расчёта остальных центров (с проверкой на совпадения)
      repeat {
        # вероятность выбора нового центра 
        pr <- runif(1, 0, 1)
        ss.step <- pr * data$ss[[n]]
        center.id <- min(which(data$ss > ss.step))  
        if (center.id %in% centers == FALSE) break
      }
      # запись найденного центра
      centers  <- c(centers, center.id)
      # расчет дальностей для найденного центра 
      data$s <- NULL
      data$ss <- NULL
      if (n.dim == 1) {
        data$s <- apply(
          cbind(data[center.id, ]),
          1, 
          function(center) {
            rowSums((data - center)^2)
          }
        )
      } else {
        data$s <- apply(
          data[center.id, ], 
          1, 
          function(center) { 
            rowSums((data - center)^2)
          }
        )
      }
      data$ss <- cumsum(data$s)  
    }
  }
  data$s <- NULL
  data$ss <- NULL
  # рассчёт кластеров, исходя из найденных центров
  cluster.data <- kmeans(data, data[centers, ], iter.max)
  #
  return(cluster.data)
}
#
###
#' Функция вычисления k-mean кластеров 
#' 
#' @param data Подготовленные данные
#' @param n.opt Оптимальное число кластеров для заданного набора данных
#' @param iter.max Количество итераций вычислений кластера
#' @param plusplus Использовать простой k-mean или k-mean++
#' @param var.digits Количество занаков после точки в значениях центров кластеров
#'
#' @return list(data, cluster.centers) Лист с данными (сод. номера кластеров) + df с центрами кластеров
#'
#' @export
CalcKmean <- function(data, n.opt, iter.max = 100, plusplus = FALSE, var.digits = 0) {
  #
  # вычисление кластера
  if (plusplus == TRUE) {
    cluster.data <- CalcKmean.plusplus(data, n.opt, iter.max)
  } else {
    cluster.data <- kmeans(data, centers = n.opt, iter.max)
  }
  # соотнесение данных по кластерам
  data$cluster <- as.factor(cluster.data$cluster)
  # вычисление центров кластеров 
  cluster.centers <- round(cluster.data$centers[, -ncol(cluster.data$centers)], digits = var.digits)
  cluster.centers <- cbind(
    cluster.centers, 
    round(cluster.data$centers[, ncol(cluster.data$centers)], digits = 3)
  )
  colnames(cluster.centers)[ncol(cluster.centers)] <- 'profit.norm'
  #
  return(list(data, cluster.centers))
  # return(data)
}
#
###
#' Функция визуализации вычисления оптимального количества кластеров
#' 
#' @param data (=ss.df) DF суммарной дисперсии по кластерам
#' @param n.opt Оптимальное число кластеров для заданного набора данных
#'
#' @return 
#'
#' @export
PlotKmean.ss <- function(ss.df, n.opt) {
  # Зависимости:
  require(plotly)
  # ----------
  ss.df <- as.data.frame(ss.df)  
  p <- 
    plot_ly(
      ss.df, x = Num.Of.Clusters, y = Total.Within.SS, mode = 'lines+markers', color = Pct.Change, 
      marker = list(symbol = 'circle-dot', size = 10),
      line = list(dash = '2px')
    ) %>% 
    layout(
      title = 'Суммарная ошибка по кластерам', 
      annotations = list(list(x = n.opt, y = Total.Within.SS[(n.opt - 1)], 
                              text = 'nOptimal', ax = 30, ay = -40))
    )
    #
    return(p)
}  
#
###
#' Функция визуализации найденных кластеров
#' 
#' @param data.list Лист, содержащий в себе данные и центры кластеров
#' @param dimension 3D/2D
#' @param plot.title Название графика
#' @param xaxis.name Название оси X
#' @param yaxis.name Название оси Y
#' @param zaxis.name Название оси Z
#' @param point.size Размер точек
#' @param point.opacity Прозрачность точек
#' @param point.line.width Толщина линии-'подводки точек'
#' @param point.opacity Прозрачность линии-'подводки точек'
#' @param center.size Размер точек-центров кластеров
#' @param center.color Цвет точек-центров кластеров
#'
#' @return 
#'
#' @export
PlotKmean.clusters <- function(data.list, cluster.color = FALSE, dimension = '3d', 
                               plot.title = 'ClustersPlot', xaxis.name = 'FastMA', yaxis.name = 'SlowMA', 
                               zaxis.name = 'PER', 
                               point.size = 4, point.opacity = 0.8, 
                               point.line.width = 2, point.line.opacity = 0.5,
                               center.size = 10, center.color = 'black') {
  # Зависимости:
  require(plotly)
  # ----------
  # 
  # подготовка данных
  data <- as.data.frame(data.list[1])
  centers <- as.data.frame(data.list[2])
  mycolors <-  rainbow(30, start=0.3, end=0.95)
  # подсветка точек (по кластерам или стандартная по доходности)
  if (cluster.color == TRUE) {
    point.color <- data$cluster
  } else {
    point.color <- data$profit.norm  
  }
  # стиль шрифта надписей
    font.style <- list(family = 'Courier New, monospace', size = 18, color = '#6699ff')
  # выбор 3D / 2D  
  if (dimension == '3d') {
    # базовый график
    p <- 
      plot_ly(
        data, x = var1, y = var2, z = var3, type = 'scatter3d', mode = 'markers', name = 'Clusters',
        colors = mycolors, opacity = point.opacity, color = point.color,
        hoverinfo = 'text', 
        text = paste0(xaxis.name, data$var1, '<br>',
                      yaxis.name, data$var2, '<br>',
                      zaxis.name, data$var3, '<br>',
                      'ProfitNorm:', round(data$profit.norm, 3), '<br>',
                      'Cluster:', data$cluster), 
        marker = list(
          symbol = 'circle',  size = point.size, 
          line = list(color = '#262626', width = point.line.width, opacity = 0.5)
        ),
        showlegend = FALSE
      )
    # добавляем центроиды кластеров
    p <- add_trace(
      centers, x = var1, y = var2, z = var3, 
      type = 'scatter3d', mode = 'markers', name = 'Cluster Centers',
      hoverinfo = 'text', 
      text = paste0(xaxis.name, centers$var1, '<br>',
                    yaxis.name, centers$var2, '<br>',
                    zaxis.name, centers$var3, '<br>',
                    'CenterID:', centers$cluster),
      marker = list(color = center.color, symbol = 'cross', size = center.size)
    )
    # надписи на графике
    p <- layout(
      title = plot.title, 
      scene = list(xaxis = list(title = xaxis.name, titlefont = font.style), 
      yaxis = list(title = yaxis.name, titlefont = font.style), 
      zaxis = list(title = zaxis.name, titlefont = font.style))
    )
  } else {
    # базовый график
    p <- plot_ly(data, x = var1, y = var2, mode = 'markers', name = 'Clusters',
                 colors = mycolors, opacity = point.opacity, color = point.color,
                 hoverinfo = 'text', 
                 text = paste0(xaxis.name , data$var1, '<br>', 
                               yaxis.name, data$var2, '<br>',
                               'ProfitNorm:', round(data$profit.norm, 3), '<br>', 
                               'Cluster:', data$cluster), 
                 marker = list(symbol = 'circle', size = point.size, 
                               line = list(color = '#262626', width = point.line.width, 
                                           opacity = point.line.opacity)),
                 showlegend = FALSE)
    # добавляем центроиды кластеров
    p <- add_trace(centers, x = var1, y = var2, mode = 'markers', name = 'Cluster Centers',
                   hoverinfo = 'text', 
                   text = paste0(xaxis.name, centers$var1, '<br>',
                                 yaxis.name, centers$var1, '<br>',
                                 'CenterID:', centers$cluster),
                   marker = list(color = center.color, symbol = 'cross', size = center.size))
    # надписи на графике
    p <- layout(title = plot.title, 
                xaxis = list(title = xaxis.name, titlefont = font.style), 
                yaxis = list(title = yaxis.name, titlefont = font.style))
  }
  return(p)
}
