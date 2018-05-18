# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Функции кластерного анализа:
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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
# CalcKmeans.preparation
ClusterAnalysis.preparation <- function(data, n.mouth ,
                                        hi = TRUE, q.hi = 0.5, 
                                        low = FALSE, q.low = 0,
                                        one.scale = FALSE,
                                        only_profitable = FALSE,
                                        profit_colname = 'ProfitPercent',
                                        dd_colname = 'DrawdownMaxPercent') {
    #
    # выделение столбцов с переменными
    df <- Subset_byTarget.col(data = data, target = '_')
    # добавление столбцов с профитом и просадкой
    df$profit <- 
        {
            which(colnames(data) %in% profit_colname)
        } %>%
        data[, .] 
    df$draw <- 
        {
            which(colnames(data) %in% dd_colname)
        } %>%
        data[, .]
    # если нужны только профитные (profit > 0) результаты, то
    if (only_profitable == TRUE) {
        df <- df[which(df$profit > 0), ]
    }
    # создание нормированной метрики
    df$profit.norm <- (df$profit * 12) / (abs(df$draw) * n.mouth)
    #
    if (hi | low != FALSE) {
        df <- CalcQuantile(data = df, 
            var = which(colnames(df) %in% 'profit.norm'), 
            hi = hi, 
            q.hi = q.hi)
    }
    if (one.scale == TRUE) {
        var.cols <- grep(colnames(data), '_')
        df[nrow(df)+1, ] <- c(
            rep(0, length(var.cols)+2), 
            df$profit.norm[[which.max(df$profit.norm)]])
        df[nrow(df)+1, ] <- c(rep(0, length(var.cols) + 3))
    }
    #
    df$profit <- NULL
    df$draw <- NULL
    df <- na.omit(df)
    #
    return(df)
}
###
#' Определение оптимального числа k-mean кластеров
#' 
#' @param data Подготовленные результаты отработки бэктеста 
#' (должны содержать в себе только переменные и нужные метрики)
#' @param plusplus Использовать простой kmeans или kmeans++
#' @param iter.max Число итераций kmeans
#' @param nstart Число итераций kmeans
#'
#' @return ss.df DF суммарного отклонения по кластерам
#'
#' @export
#CalcKmeans.parameters
ClusterAnalysis.parameters <- function(data, 
                                       method = c('kmeans', 'plusplus', 'pam', 'clara'),
                                       k.max = 30, 
                                       ...) {
    FUN.cluster <- switch(method,
        kmeans = kmeans,
        plusplus = CalcKmeans.plusplus,
        pam = cluster::pam,
        clara = cluster::clara)

    n <- nrow(data)
    if (n < k.max) {
        k.max <- 2:(n - 1)
    } else {
        k.max <- 2:k.max 
    }

    if (method %in% c('kmeans', 'plusplus')) {
        # расчет суммарного квадрата расстояния точек внутри тестовых кластеров
        ss <- c()
        p.exp <- c()
        for (i in k.max) {
            cluster.data <- FUN.cluster(data, i, ...)
            # cluster.data <- kmeans(data, i, iter.max, nstart)    
            ss[i] <- cluster.data$tot.withinss
            p.exp[i] = 1 - cluster.data$tot.withinss / cluster.data$totss
        }
        ss <- na.omit(ss)
        p.exp <- na.omit(p.exp)
        # сводим всё в df
        ss.df <- data.frame(Num.Of.Clusters = k.max,
            Total.Within.SS = ss,
            Pct.Change = c(NA, diff(ss) / ss[1:length(ss) - 1]) * 100,
            Pct.Exp = p.exp)
        # вычисление оптимального количества кластеров
        # byVar: опт. число кластеров определяется как min число, описывающее 90% пространства
        n.byVar <- 
            {
                min(which(ss.df$Pct.Exp > 0.9))
            } %>%
            ss.df$Num.Of.Clusters[.]
        # byElbow: опт. число определяется 'методом локтя'
        # n.byElbow <- FindMaxDistancePoint(ss.df$p.exp[-1]) + 1
        # n <- c(n.byVar, n.byElbow)
        # n.opt <- n[which.max(n)]
        n.opt <- n.byVar 
        #
        return(list(ss.df = ss.df, n.opt = n.opt))    
    }
    
    if (method %in% c('pam', 'clara')) {
        best_pam <- FUN.cluster(data, k = 2, ...)
        for (i in k.max[-1]) {
            temp_pam <- FUN.cluster(data, k = i, ...)
            if (temp_pam$silinfo$avg.width < best_pam$silinfo$avg.width) {
                best_pam <- temp_pam
            }
        }
        return(list(pam = best_pam, n.opt = nrow(best_pam$medoids)))
    }
}
#
###
#' Функция вычисления k-mean кластеров 
#' 
#' @param data Подготовленные данные
#' @param method Алгоритм кластеризации
#' @param n.opt Оптимальное число кластеров для заданного набора данных
#' @param var.digits Количество занаков после точки в значениях центров кластеров
#'
#' @return list(data, cluster.centers) Лист с данными (сод. номера кластеров) + df с центрами кластеров
#'
#' @export
ClusterAnalysis <- function(data, method = c('kmeans', 'plusplus', 'pam', 'clara'),
                            n.opt, var.digits = 0, 
                            ...) {
    #
    #set.seed(1234)
    FUN.cluster <- switch(method,
        kmeans = kmeans,
        plusplus = CalcKmeanss.plusplus,
        pam = cluster::pam,
        clara = cluster::clara)
    # вычисление кластера
    cluster.data <- FUN.cluster(data, n.opt, ...)
    if (method %in% c('kmeans', 'plusplus')) {
        # соотнесение данных по кластерам
        data$cluster <- as.factor(cluster.data$cluster)
        # вычисление центров кластеров 
        cluster.centers <- round(cluster.data$centers[, -ncol(cluster.data$centers)], digits = var.digits)
        cluster.centers <- cbind(cluster.centers, 
            round(cluster.data$centers[, ncol(cluster.data$centers)], digits = 3))
        colnames(cluster.centers)[ncol(cluster.centers)] <- 'profit.norm'
        #
        return(list(data = as.data.frame(data), cluster.centers = as.data.frame(cluster.centers)))    
    }
    #
    return(
        list(data = cluster.data, 
            cluster.centers = 
                cluster.data$medoids[, -ncol(cluster.data$medoids)] %>% 
                as.data.frame(., row.names = FALSE))
    )
}
#
###
#' Функция вычисления модного k-mean++ 
#' 
#' @param data Подготовленные данные
#' @param n.opt Оптимальное число кластеров для заданного набора данных
#' @param iter.max Количество итераций вычислений кластера
#' @param nstart Количество итераций алгоритма
#'
#' @return out Лист с данными кластера
#'
#' @export
CalcKmeans.plusplus <- function(data, n.opt = 2,
                                start_poit = "random",
                                iter.max = 100,
                                nstart = 10) {
    #
    if (length(dim(data)) == 0) {
        data <- matrix(data, ncol = 1)
    } else {
        data <- cbind(data)
    }
    # количество точек
    n <- nrow(data)
    # число измерений пространства
    ndim <- ncol(data)
    
    data.avg <- colMeans(data)
    data.cov <- cov(data)
    
    out <- list()
    out$tot.withinss <- Inf
    
    # цикл вычислений
    for (i in seq_len(nstart)) {
        # вектор для ID центров (номеров строк, содержащих точки центров)
        centers <- rep(0, length = n.opt)

        # выбор стартовой точки
        if (!is.character(start_poit)) {
            centers[1:2] <- start_poit
        } else {
            if (start_poit == "random") {
                centers[1:2] <- sample.int(n, 1)
            }
            if (start_poit == "normal") {
                centers[1:2] <- which.min(dmvnorm(data, mean = data.avg, sigma = data.cov))
            }    
        }

        # цикл подбора центров
        for (ii in 2:n.opt) {
            # расчёт квадратов расстояний от точек до центра
            if (ndim == 1) {
                dists <- apply(cbind(data[centers, ]), 
                    1, 
                    function(x) {
                        rowSums((data - x)^2)
                    })
            } else {
                dists <- apply(data[centers, ], 
                    1, 
                    function(x) {
                        rowSums((data - x)^2)
                    })
            }
            # расчет дальностей для найденных центров 
            probs <- apply(dists, 1, min)
            probs[centers] <- 0
            # расчет дальностей для найденного центра 
            centers[ii] <- sample.int(n, 1, prob = probs)
        }

        # расчёт кластеров, исходя из найденных центров
        cluster_data <- kmeans(data, centers = data[centers, ], iter.max = iter.max)
        # запись инициализирующих центров
        cluster_data$inicial.centers <- data[centers, ]
        # если рассчитано оптимальное пространство, то 
        if (cluster_data$tot.withinss < out$tot.withinss) {
            out <- cluster_data
        }
    }
    #
    return(out)
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
        plot_ly(ss.df, 
            x = ~Num.Of.Clusters, y = ~Total.Within.SS, 
            mode = 'lines+markers', color = ~Pct.Change, 
            marker = list(symbol = 'circle-dot', size = 10),
            line = list(dash = '2px')) %>% 
        layout(title = 'Суммарная ошибка по кластерам', 
            annotations = list(
                list(x = n.opt, 
                    y = Total.Within.SS[(n.opt - 1)], 
                    text = 'nOptimal', ax = 30, ay = -40)
            ))
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
    mycolors <- rainbow(30, start=0.3, end=0.95)
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
        p <- plot_ly(
            data, x = ~var1, y = ~var2, z = ~var3, type = 'scatter3d', mode = 'markers', name = 'Clusters',
            colors = mycolors, opacity = point.opacity, color = point.color,
            hoverinfo = 'text', 
            text = paste0(
                xaxis.name, data$var1, '<br>',
                yaxis.name, data$var2, '<br>',
                zaxis.name, data$var3, '<br>',
                'ProfitNorm:', round(data$profit.norm, 3), '<br>',
                'Cluster:', data$cluster
            ), 
            marker = list(
                symbol = 'circle',    size = point.size, 
                line = list(color = '#262626', width = point.line.width, opacity = 0.5)
            ),
            showlegend = FALSE
        )
        # добавляем центроиды кластеров
        p <- add_trace(
            centers, x = ~var1, y = ~var2, z = ~var3, 
            type = 'scatter3d', mode = 'markers', name = 'Cluster Centers',
            hoverinfo = 'text', 
            text = paste0(
                xaxis.name, centers$var1, '<br>',
                yaxis.name, centers$var2, '<br>',
                zaxis.name, centers$var3, '<br>',
                'CenterID:', centers$cluster
            ),
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
        p <- plot_ly(
            data, x = ~var1, y = ~var2, mode = 'markers', name = 'Clusters',
            colors = mycolors, opacity = point.opacity, color = point.color,
            hoverinfo = 'text', 
            text = paste0(
                xaxis.name , data$var1, '<br>', 
                yaxis.name, data$var2, '<br>',
                'ProfitNorm:', round(data$profit.norm, 3), '<br>', 
                'Cluster:', data$cluster
            ), 
            marker = list(symbol = 'circle', size = point.size, 
                line = list(color = '#262626', width = point.line.width, 
                opacity = point.line.opacity)),
            showlegend = FALSE
        )
        # добавляем центроиды кластеров
        p <- add_trace(
            centers, x = ~var1, y = ~var2, mode = 'markers', name = 'Cluster Centers',
            hoverinfo = 'text', 
            text = paste0(
                xaxis.name, centers$var1, '<br>',
                yaxis.name, centers$var1, '<br>',
                'CenterID:', centers$cluster
            ),
            marker = list(color = center.color, symbol = 'cross', size = center.size)
        )
        # надписи на графике
        p <- layout(title = plot.title, 
            xaxis = list(title = xaxis.name, titlefont = font.style), 
            yaxis = list(title = yaxis.name, titlefont = font.style))
    }
    return(p)
}

.get_withinSS <- function(d, cluster){
  d <- stats::as.dist(d)
  cn <- max(cluster)
  clusterf <- as.factor(cluster)
  clusterl <- levels(clusterf)
  cnn <- length(clusterl)
  
  if (cn != cnn) {
    warning("cluster renumbered because maximum != number of clusters")
    for (i in 1:cnn) cluster[clusterf == clusterl[i]] <- i
    cn <- cnn
  }
  cwn <- cn
  # Compute total within sum of square
  dmat <- as.matrix(d)
  within.cluster.ss <- 0
  for (i in 1:cn) {
    cluster.size <- sum(cluster == i)
    di <- as.dist(dmat[cluster == i, cluster == i])
    within.cluster.ss <- within.cluster.ss + sum(di^2)/cluster.size
  }
  within.cluster.ss
}

