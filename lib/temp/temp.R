# data XTS 
# period Период окна скольжения 
# “microseconds”, “milliseconds”, “seconds”, “mins”, “hours”, “days”, “weeks”, “months”, “quarters”, and “years”
#
RollingApply_forXTS <- function(data, start_date, end_date, period = NULL, 
                                width, by = NULL, FUN = NULL, lookback = TRUE) {
  #
  nr <- nrow(data)
  stopifnot(width > 0, width <= nr)
  # выделение старт/стоп индексов
  startstop_ind <- paste(start_date,"::",end_date)
  if (is.null(by)) {
    by <- width  
  }
  # если период == NULL, то окна считаются по периодам свечей 
  if (is.null(period)) {
     <- 
    if (lookback == TRUE) {
      # вычисление нужного временного ряда
      temp_subset <- 
        data[startstop_ind] %>%
        index(.) %>%
        {
          which(. %in% index(data))
        } %>%
        {
          (first(.) - width + 1):last(.)
        } %>%
        data[., ]
    } else {
      temp_subset <- data[startstop_ind]
    }
    # добавление нужного интервала окна 
    result <- 
      length(temp_subset) %>%
      seq(1, . - width + 1, by = by) %>% 
      lapply(., 
             function(x) {
               x:(x + width - 1)
             })    
  }

  # функция
  FUN <- match.fun(FUN)
  OUT <- lapply(SEQ2, function(a) FUN(x[a], ...))
  #
  return
}
