library(data.table)
#
data.names <- 
    grep(".equity", names(data.source)) %>%
    names(data.source)[.] %>%
    sub(".equity", "", .)
#
TestStrategy_DealsTable <- function(data, ...) {
  if (!exists("data.names")) {
    data.names <- 
      grep(".equity", names(data)) %>%
      names(data.source)[.] %>%
      sub(".equity", "", .)   
  }
}
#
DealSummary <- function(data, n, data.names) {
  #
  deal.summary <- 
    data[data$pos.num == 4] %T>%
    {
      temp.data <<- . 
    } %>%
    {
      temp.data <- .
      data.names.list <- 
        data.names %>%
        {
          names(.) <- .
          return(.)
        } %>%
        as.list(.)
      temp.data <- lapply(data.names.list, 
                          function(x) {
                            temp.text <- 
                              paste("names.set <- c(\"pos.num\", \"pos\", \"pos.add\", \"pos.drop\", ",
                                      "\"",x,".n\", \"",x,".diff.n\", \"",x,".Open\", ",
                                      "\"",x,".commiss\", \"",x,".margin\") ;" ,
                                    sep = "")
                            eval(parse(text = temp.text))
                            temp.data <- temp.data[, (which(colnames(temp.data) %in% names.set))]
                            #temp.text <- 
                            #  paste("temp.data$ticker <- as.character(\"",x,"\") ;", sep = "")
                            #eval(parse(text = temp.text))
                            return(temp.data)
                          })
        return(temp.data)
    }
  


    { 
      nrow(.) * length(data.names) 
    } %>%
    {
      data.frame(PositionNum = rep(n, .),
                 Position = character(.),
                 Ticker = character(.),
                 N = integer(.),
                 diff.N = integer(.),
                 OpenSignal = character(.),
                 OpenDate = character(.) %>% 
                            as.numeric() %>% 
                            as.Date(),
                 OpenValue = integer(.),
                 OpenCommiss = integer(.),
                 CloseSignal = character(.),
                 CloseDate = character(.) %>% 
                             as.numeric() %>% 
                             as.Date(),
                 CloseValue = integer(.),
                 CloseCommiss = integer(.),
                 Margin = numeric(.))
    } %>%
    {
      df <- .
      temp.list <- sapply()


        for (i in 1:length(data.names)) {  
          temp.text <- 
            data.names[i] %>%
            {
              t <- paste(
                   ""
                   )
            } 
        }
      

          .[1]

      temp.data$pos != 0 & (temp.data$pos.drop + temp.data$pos.add) == 0, 
      
    
    }

}

