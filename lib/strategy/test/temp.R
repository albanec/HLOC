data.state$balance[1] <- balance.start
data.state$im.balance[1] <- 0
data.state$commiss[1] <- 0
data.state$margin[1] <- 0
data.state$diff.n[1] <- 0
data.state$n[1] <- 0
data.state$equity[1] <- 0
#
data.state$n <- ifelse(
  data.state$pos == 0,
  0, 
  ifelse(
    data.state$pos.bars == 0,
    data.state$n <- 4,
    NA
  )
)
temp.ind <- which(is.na(data.state$n))
data.state$n[temp.ind] <- sapply(
  1:nrow(data.state),
  function(x) {
    ifelse(
      !is.na(data.state$n[x]),
      data.state$n[x],
      ifelse(
        coredata(data.state$pos.add[x]) == 1,
        round(2 * coredata(data.state$n[x - 1])),
        round(0.5 * coredata(data.state$n[x - 1]))
      )
    ) 
  }
)
vars <- 2:nrow(data.state) 
#
    # изменение контрактов на такте
    data.state$diff.n[n] <- data.state$n[[x]] - data.state$n[[x - 1]]
    # расчёт баланса, заблокированного на ГО
    data.state$im.balance[x] <- data.state$n[[x]] * coredata(data.source$IM[temp.index])
    # комиссия на такте
    data.state$commiss[x] <- basket.commiss * abs(data.state$diff.n[[x]])
    # баланс на такте
    data.state$balance[x] <- 
    data.state$balance[[x - 1]] + data.state$margin[[x]] + 
    data.state$im.balance[[x - 1]] - data.state$im.balance[[x]] - 
    data.state$commiss[[x]]
  }
)


      
      
    
  