# data XTS 
# period Период окна скольжения 
# 'microseconds', 'milliseconds', 'seconds', 'mins', 'hours', 'days', 'weeks', 'months', 'quarters', and 'years'
#
RollingApply_forXTS <- function(data, start_date, end_date, period = NULL, 
                                width, by = NULL, FUN = NULL, align = c('left', 'right'),
                                lookback = TRUE) {
  ## подготовка
  n_rows <- nrow(data)
  n_cols <- ncol(data)
  # проверка на правильность условий
  stopifnot(width > 0, width <= n_rows)
  #
  FUN <- match.fun(FUN)
  # индкесы исходных данных
  data_ind <- index(data)
  # интервал анализа
  startstop_interval <- paste(start_date,'::',end_date, sep = "")
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
  if (is.null(period) | (period == freq$units)) {
    # выделение старт/стоп номеров строк
    startstop_row_nums <- 
      data[startstop_interval] %>%
      index(.) %>%
      {
        which(data_ind %in% .)
      }
    ## подготовка данных для анализа
    # ряд расширяется, если lookback == TRUE  
    if (lookback == TRUE) {
      temp_subset <- 
        (first(startstop_row_nums) - width + 1):last(startstop_row_nums) %>%
        data[., ]   
    } else {
      temp_subset <- data[startstop_row_nums, ]
    }
    # точки "обозрения"
    points_row <- 
      nrow(temp_subset) %>%
      {
        seq((width - offset), (. - offset), by = by) 
      }
    #
    result <- 
      lapply(points_row, 
             function(x) {
             FUN(.subset_xts(temp_subset, (x - width + 1):x), ...)
             }) #%>%
      #MergeData_inList_byRow(.)
    #  
  } else {
    ## если period != NULL, то окна считаются по указанным периодам
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
    # выделение старт/стоп номеров строк 
    startstop_row_nums <- 
      data[startstop_interval] %>%
      index(.) %>%
      {
        which(data_ind %in% .)
      }
    ## выделение нужного для анализа интервала
    # if (lookback == TRUE) {
    #   temp_subset <- 
    #     first(startstop_row_nums) %>%
    #     data[., ] %>%
    #     {
    #       index(.)  - offset(x = width)
    #     } %>%
    #     # проверить!!!
    #     paste(.,'::',end_date, sep = "") %>%
    #     data[.]
    # } elxe {
      temp_subset <- data[startstop_interval] 
      temp_subset$endpoint <- 
        temp_subset %>%
        # простановка enpoint'ов 
        {
          .$endpoint <- NA
          end <- endpoints(x = temp_subset, on = period, k = 1)
          .$endpoint[end] <- 1
          return(.$endpoint)
        } %>%
        #endpoints(x = temp_subset, on = period, k = 1) %>%
        # модификация
        # {
        #   x <- .
        #   x <- 
        #     x[-length(x)] %>%
        #     {
        #       . + 1 
        #     }
        #   return(x)
        # } %>%
        cumsum(.)  
    #}
    
    

   
    
  } 

  # функция
  
  OUT <- lapply(SEQ2, function(a) FUN(x[a], ...))
  #
  return
}
