# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Функции для data mining'а
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
###
#' Функция создает матрицу из n строк вектора x
#'
#' @param x Вектор, который надо повторить
#' @param n Количество нужных строк
#'
#' @return m Матрица, состоящая их n сток вектора x
#'
#' @export
Repeat_Row <- function(x,n) {
  #
  m <- matrix(rep(x,each = n),nrow = n)
  #
  return(m)
}
#
###
#' Функция создает матрицу из n столбцов вектора x
#'
#' @param x Вектор, который надо повторить
#' @param n Количество нужных столбцов
#'
#' @return m Матрица, состоящая их n столбцов вектора x
#'
#' @export
Repeat_Col <- function(x,n) {
  #
  m <- matrix(rep(x,each = n), ncol = n, byrow = TRUE)
  #
  return(m)
}
#
###
#' Функция объединения данных в один XTS 
#'
#' @param data.list Лист, сожержащий XTS нужных тикеров
#' @param col.name Если нужно объединить опред. столбцы, то присвоить название
#'
#' @return list(merged.data) Лист с xts рядом объединенных значений (по всем тикерам)
#'
#' @export
MergeData_inList_byCol <- function(data.list, col.name = FALSE) {
  # 
  n.ticker <- length(data.list) 
  FirstTime <- TRUE
  #  чтение и объединение данных
  for (i in 1:n.ticker) {
    data <- data.list[[i]]
    data.name <- 
      names(data)[grep("Close", names(data))] %>%
      sub(".Close", "", .)
    cat("INFO(MergeData_fromAll_toOne):  Processing StocksData:  ", data.name, "\n")
    if (col.name != FALSE) {
      temp.text <-
      paste(data.name, col.name, sep = ".") %>%
      paste("data <- data$", ., sep = "")
      eval(parse(text = temp.text))
    }
    if (FirstTime == TRUE) {
      FirstTime <- FALSE
      merged.data <- data
    } else {
      merged.data <- merge(merged.data, data)
    }
  }  
  merged.data <- list(merged.data)
  names(merged.data) <- c("merged.data")
  #
  return(merged.data)
}
#
###
#' Функция объединения данных внутри листа построчно
#'
#' @param data.list Лист
#'
#' @return data.list[[1]] Объединенные данные
#'
#' @export
MergeData_inList_byRow <- function(data.list) {
  #
  while (length(data.list) > 1) {
  idxdata.list <- seq(from=1, to=length(data.list), by=2)
  data.list <- lapply(idxdata.list, 
                      function(i) {
                        if(i == length(data.list)) { 
                          return(data.list[[i]]) 
                        }
                        return(rbind(data.list[[i]], data.list[[i+1]]))
                      })
  }
  #
  return(data.list[[1]])
}
#
###
#' Функция выделения столбцов с именами, содержащими target параметр
#'
#' @param data Исходный XTS
#' @param target Ключ для поиска
#' 
#' @return data XTS ряд, очищенный от NA (по всем тикерам)
#'
#' @export
SubsetCol_inXTS_byTarget <- function(data, target) {
  #
  data <-
    colnames(data) %>%
    grep(target, .) %>%
    data[, .]
  #
  return(data)
}
#
###
#' Функция очистки мусора в столбцах данных
#'
#' @param data Исходные данные
#' @param target Ключ для поиска
#' 
#' @return data Данные, очищенный от столбцов с target в названии
#'
#' @export
CleanGarbage_inCols <- function(x, target = "temp") {
  x <- 
    colnames(x) %>%
    grep(target, .) %>%
    {
      x[, -.]
    }
  return(x)
}
#
###
#' Функция вычисление лучших / худших значений (на основе квантиля) 
#' 
#' @param data Данные 
#' @param var Столбец данных с качественной характеристикой
#' @param q.hi Уровень квантиля для вычисления лучших значений (берётся всё, что выше квантиля)
#' @param q.low Уровень квантиля для вычисления худших значений (берётся всё, что ниже квантиля)
#' @param low Вычисляем худшие значения (классический квантиль) 
#' @param hi Вычисляем лучшие значения (всё, что больше квантиля) 
#' @param two Вычисляем "середину" между двумя уровнями
#' @param abs Eсли нужно, задать абсолютное значение квантиля (вычисленное ранее)
#'
#' @return data Отфильтрованные данные
#'
#' @export 
CalcQuantile <- function(data, var, q.hi = 0, q.low = 0, 
                         two = FALSE, low = FALSE, hi = FALSE, abs = FALSE) {
  #
  if (two == TRUE) {
    # подготовка данных
    data <- data[order(-data[[var]]), ]
    # вычисление квантилей
    ifelse(abs == FALSE, 
           q.hi.value <- quantile(data[[var]], q.hi),
           q.hi.value <- as.numeric(q.hi))
    n.hi <- which(data[, var] < q.hi.value)
    ifelse(abs == FALSE, 
           q.low.value <- quantile(data[[var]], q.low), 
           q.low.value <- as.numeric(q.low)) 
    data <- data[n.hi, ]
    n.low <- which(data[, var] > q.low.value )
    data <- data[n.low, ]
    } 
  if (hi == TRUE) {
    data <- data[order(-data[[var]]), ]
    ifelse(abs == FALSE, q.hi.value <- quantile(data[[var]], q.hi), q.hi.value <- as.numeric(q.hi))
    n.hi <- which( data[, var] > q.hi.value )  
    data <- data[n.hi, ]
    }
  if (low==TRUE) {
    data <- data[order(-data[[var]]), ]
    ifelse (abs == FALSE, q.low.value <- as.numeric(quantile(data[[var]], q.low)), q.low.value <- as.numeric(q.low)) 
    n.low <- which( data[, var] < q.low.value )
    data <- data[n.low, ]
    }
  #  
  return(data)
}
#
###
#' Функция скользящей нарезки периодов
#' 
#' @param data Данные XTS
#' @param start_date Стартавая дата (гг-мм-дд) анализа
#' @param end_date Конечная дата анализа
#' @param period Период окна скольжения ('seconds', 'mins', 'hours', 'days', 'weeks', 'months', 'quarters', 'years')
#' если == NULL - посвечный период
#' @param width Глубина окна скольжения  
#' @param by Шаг окна скольжения 
#' @param align "Выравнивание" индекса результата, ставить = right (на данный момент не используется)
#' @param add_bySlice Нужно ли добавить в выходные данные нарезку с by-периодами 
#' @param lookback "Заглядывание" за дату начала анализа (на данный момент не используется)
#'
#' @return result.list Лист с данными, разложенными по индексам окон
#'
#' @export 
RollingSlicer_forXTS <- function(data, start_date, end_date, period = NULL, 
                                 width, by = NULL, align = c('left', 'right'),
                                 add_bySlice = FALSE, lookback = FALSE) {
  ## подготовка
  n_rows <- nrow(data)
  n_cols <- ncol(data)
  freq <- periodicity(data) 
  # проверка на правильность условий
  stopifnot(width > 0, width <= n_rows)
  #
  #FUN <- match.fun(FUN)
  #
  # индкесы исходных данных
  data_ind <- index(data)
  # интервал анализа
  interval <- paste(start_date,'::',end_date, sep = "")
  #
  if (is.null(by)) {
    by <- width
  }
  ## определения сдвига (зависит от направления окна)
  offset <- 
    match.arg(align) %>%
    switch(.,    
           'left' = { width - 1 },
           'right' = { 0 }
          )
  # 
  ## если период == NULL, то окна считаются по периодам свечей 
  if (is.null(period) == TRUE || (period == freq$units)  == TRUE) {
    # выделение старт/стоп номеров строк
    row_nums <- 
      {
        if (add_bySlice == FALSE) {
          data[interval]
        } else {
          data
        }
      } %>%
      index(.) %>%
      {
        which(data_ind %in% .)
      }
    remove(data_ind)
    ## подготовка данных для анализа
    # ряд расширяется, если lookback == TRUE  
    if (lookback == TRUE) {
      temp_subset <- 
        (first(row_nums) - width + 1):last(row_nums) %>%
        data[., ]   
    } else {
      temp_subset <- data[row_nums, ]
    }
    # индекс для выходных данных (пока не используется)
    result_ind <- 
      nrow(temp_subset) %>%
      {
        seq((width - offset), (. - offset), by = by) 
      } %>%
      {
        index(temp_subset)[.]
      }
    # индексы окон
    ind <- 
      nrow(temp_subset) %>%
      seq.int(width, ., by)
    #
    result.list <- lapply(ind, 
                     function(x) {
                       .subset_xts(temp_subset, (x - width + 1):x)
                     })
    #
    if (add_bySlice == TRUE) {
      temp_subset <- 
        nrow(temp_subset) %>%
        (width + 1):. %>%
        temp_subset[., ]
      bySlice.list <- RollingSlicer_forXTS(data = temp_subset, start_date, end_date, period = NULL, 
                                               width = by, by = NULL, align,
                                               add_bySlice = FALSE, lookback = FALSE)
      result.list <- list(widthSlice = result.list, bySlice = bySlice.list)
    } 
  } else {
    ## если period != NULL, то окна считаются по указанным периодам
    #
    # offset <- 
    #   match.arg(period) %>%
    #   switch(.,    
    #          'seconds' = seconds(x),
    #          'mins' = minutes(x),
    #          'hours' = hours(x),
    #          'days' = days(x),
    #          'weeks' = weeks(x),
    #          'months' = days(x),             
    #          'years' = years(x)
    #         )
    #
    # выделение старт/стоп номеров строк 
    row_nums <- 
      data[interval] %>%
      index(.) %>%
      {
        which(data_ind %in% .)
      }
    remove(data_ind)
    ## выделение нужного для анализа интервала
    #
    # if (lookback == TRUE) {
    #   temp_subset <- 
    #     first(row_nums) %>%
    #     data[., ] %>%
    #     {
    #       index(.)  - offset(x = width)
    #     } %>%
    #     # проверить!!!
    #     paste(.,'::',end_date, sep = "") %>%
    #     data[.]
    # } elxe {
    #
      temp_subset <- data[interval] 
      # простановка enpoint'ов 
      ends_ <- 
        endpoints(x = temp_subset, on = period, k = 1) %>%
        # модификация (перенос endpoint'ов на начало периода)
        {
          x <- .
          x <- 
            x[-length(x)] %>%
            {
              . + 1 
            }
          return(x)
        } %>% 
        {
          temp_subset$endpoint <- NA
          temp_subset$endpoint[.] <- 1
          temp_subset$endpoint[.] <- cumsum(temp_subset$endpoint[.])
          return(temp_subset$endpoint)
        } %>%
        na.locf(.) 
      # 
      result_ind <- 
        unique(coredata(ends_)) %>%
        max(.) %>%
        {
          seq((width - offset), (. - offset), by = by) 
        } %>%
        {
          index(temp_subset)[.]
        }
      # индексы окон
      ind <- 
        unique(coredata(ends_)) %>%
        max(.) %>%
        {
          seq.int(width, ., by)
        }
      #
      result.list <- lapply(ind, 
                       function(x) {
                         win_start <- 
                           {
                             x - width + 1
                           } %>%
                           {
                             first(which(ends_ == .))
                           } 
                         win_end <- last(which(ends_ == x))
                         #
                         result <- .subset_xts(temp_subset, win_start:win_end)
                         return(result)
                       })
    #}  
    if (add_bySlice == TRUE) {
      temp_subset <- 
        {
          last(which(ends_ == width))
        } %>%
        {
          text <- index(ends_[.])
          paste(text,"::", sep = "")
        } %>%
        temp_subset[.] %>%
        .[-1, ]
      bySlice.list <- RollingSlicer_forXTS(data = temp_subset, start_date, end_date, period, 
                                               width = by, by = NULL, align,
                                               add_bySlice = FALSE, lookback = FALSE)
      result.list <- list(widthSlice = result.list, bySlice = bySlice.list)
    }
  } 
   
  #
  return(result.list)
}
#
CalcEndpoints <- function(x, on, k, findFirst = FALSE) {
   ends <- endpoints(x, on, k) 
   if (findFirst == TRUE) {
    # модификация (перенос endpoint'ов на начало периода)
    ends <- 
      ends[-length(ends)] %>%
      {
        . + 1 
      }
  }
  #
  return(ends)
}
#
