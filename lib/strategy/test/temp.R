data.state$equity[1] <- 0
#
TestStr_CalcN <- function(data) {
  #FUN <- match.fun(FUN)
  temp.env <- new.env()
  ind <- 1:nrow(data)
  n <- coredata(data$n)
  pos <- coredata(data$pos)
  pos.bars <- coredata(data$pos.bars)
  pos.add <- coredata(data$pos.add)
  assign('cache', n, envir = temp.env)
  rm(data)
  sapply(ind,
         function(x) {
           n <- get('cache', envir = temp.env)
           #data[x, ] <- FUN(data, x, ...) 
           n[x] <- ifelse(
             pos[x] == 0,
             0, 
             ifelse(
               pos.bars[x] == 0,
               n[x] <- 4,
               ifelse(
                 pos.add[x] == 1,
                 round(2 * n[x - 1]),
                 round(0.5 * n[x - 1])
               )
             )
           )
           assign('cache', n, envir = temp.env) 
         })
  result <- get('cache', envir = temp.env)
  rm(temp.env)
  return(result)
}
# Изменение контрактов на такте
data.state$diff.n <- data.state$n - lag(data.state$n)
data.state$diff.n[1] <- 0
# Расчёт баланса, заблокированного на ГО
data.state$im.balance <- 
  #индекс uniq state строк
  index(data.state$state) %>%
  unique(.) %>%
  {
    data.state$n * data.source$IM[.]
  }
data.state$im.balance %<>% 
  is.na(.) %>%
  {
    data.state$im.balance[.] <- data.state$n[.] * data.source$IM[index(data.state$n[.])]
    return(data.state$im.balance)
  }
data.state$im.balance[1] <- 0                   
# Расчёт комиссии на такте
data.state$commiss <- basket.commiss * abs(data.state$diff.n)
data.state$commiss[1] <- 0
# Расчёт вариационки
data.state$margin <- 
  data.state$cret * lag(data.state$n)
data.state$margin[1] <- 0
# Расчёт баланса 
data.state$balance <- 
  balance.start + data.state$margin + 
  lag(data.state$im.balance) - data.state$im.balance - 
  data.state$commiss
data.state$balance[1] <- balance.start 

      
      
    
  