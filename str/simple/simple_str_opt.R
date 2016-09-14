#source("str/libStrategy.R")
## простой перебор с sma.per = 1:100
rep <- 1:100
system.time({
  x <- lapply(rep, OneTradeProbe(smaPer = rep))
})
#
SimpleStr_OneTradeProbe <- function(dataSource = data.source.list[[1]], 
                          smaPer = sma.per, addPer = add.per, kMM = k.mm, 
                          basketWeights = basket.weights, sleeps. = sleeps, commissions. = commissions,
                          balance. = balance.start) {
  ### один прогон вычислений 
  ### отработка тестового робота
  data.strategy.list <- TestStrategy_gear(data.source = dataSource,
                                          sma.per = smaPer, add.per = addPer, k.mm = kMM, 
                                          basket.weights = basketWeights, sleeps = sleeps., commissions = commissions.,
                                          balance.start = balance.)
  
  ### формирование таблицы сделок
  ## чистим от лишних записей
  data.strategy.list[[2]] <- CleanStatesTable(data = data.strategy.list[[2]])
  ## лист с данными по сделкам (по тикерам и за всю корзину)
  dealsTable.list <- CalcDealsTables(data = data.strategy.list[[2]], convert = TRUE)
  # очистка мусора по target = "temp"
  CleanGarbage(target = "temp", env = ".GlobalEnv")
  #
  ### оценка perfomance-параметров
  perfomanceTable <- 
   CalcPerfomanceTable(data = data.strategy.list[[1]], 
                       data.state = data.strategy.list[[2]],
                       dealsTable = dealsTable.list,
                        balance = balance.start, 
                        ret.type = ret.type) %>%
    # добавление использованных параметров
    cbind.data.frame(., sma.per_ = sma.per, add.per_ = add.per, k.mm_ = k.mm)
  return(perfomanceTable)
}
