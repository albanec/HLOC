#
STR_Convert_SigToState <- function(x) {
    # ----------
    # Общее описание:
    #  функция для перехода к состояниям (фильтрация сигналов)
    # Входные данные:
    #  x - ряд сигналов стратегии
    # Выходные данные:
    #  ряд сделок (отфильтрованный ряд сигналов)
    # ----------
    x$a <- na.locf(x)
    x$a <- ifelse(is.na(x$a) | is.nan(x$a) | is.infinite(x$a), 0, x$a)
    ind <- which(x$a != lag(x$a))
    x$y <- rep(NA, length(x$a))
    x$y[ind] = x$a[ind]
    x$y[1] <- x$a[1]
    return (x$y)
}
#
STR_CalcState_Data <- function(data) {
    # ----------
    # Общее описание:
    #  функция для перехода к состояниям (фильтрация сигналов)
    # Входные данные:
    #  data: ряд позиций (data$pos)
    # Выходные данные:
    #  data$state: ряд состояний  
    # Зависимости:
    require(quantmod) 
    # ----------
    data$pos <- na.locf(data$pos)
    data$pos <- ifelse(is.na(data$pos) | is.nan(data$pos) | is.infinite(data$pos), 0, data$pos)
    ind <- which(data$pos != lag(data$pos))
    data$state <- rep(NA, length(data$pos))
    data$state[ind] <- data$pos[ind]
    data$state[1] <- data$pos[1]
    return(data$state)
}
#
STR_CalcState_Table <- function(data) {
    # ----------
    # Общее описание:
    # генерирует таблицу сделок
    # Входные данные:
    # data: данные
    # Выходные данные:
    # state.data: данные с рядом состояний  
    # Зависимости:
    require(quantmod) 
    # ----------
    data$state <- STR_CalcState_Data(data)
    state.data <- na.omit(data)
    return(state.data)
}
#
STR_CalcReturn_inXTS <- function(data, type = "sret") {
    # ----------
    # Общее описание:
    #  функция вычисляет return'ы по всему портфелю внутри XTS
    # Входные данные:
    #  data: XTS 
    #  type: тип return'a (ret/sret/lret)
    # Выходные данные:
    #  data: XTS + return'ы по каждому инструменту 
    # Зависимости:
    require(quantmod)
    # ----------
    data.names <- names(data)[grep("Close", names(data))]
    data.names <- sub(".Close", "", data.names)
    for (i in 1:length(data.names)) {
        temp.text <- paste("data$",data.names[i],".",type," <- ",
                           "CalcReturn(data$",data.names[i],".Close, type = \"",type,"\")", 
                           sep="")
        eval(parse(text = temp.text))
    }
    return(data)
}
#
STR_NormData_Price_inXTS <- function(data, names, outnames, convert.to, tick.val, tick.price) {
    # ----------
    # Общее описание:
    # Функция для расчёта стоимости тиков внутри основного листа данных
    # Входные данные:
    # data: XTS данные котировок (основной лист данных)
    # names: список тикеров для конвертирования 
    # Выходные данные:
    # data: основной XTS (нужные данные конвертированы к нужной валюте)
    # ----------
    for (i in 1:length(names)) {
        temp.text <- paste("data$",outnames[i]," <- ",
                           "NormData_Price_byCol(data = data$",names[i],",",
                                                 "norm.data = data$USDRUB, convert.to = \"",convert.to,"\",",
                                                 "tick.val = ",tick.val[i],",",
                                                 "tick.price = ", tick.price[i],")",
                           sep = "")
        eval(parse(text = temp.text))    
    }
    return(data)    
}
#
STR_CalcSum_Basket_TargetPar_inXTS <- function(data, basket.weights, target) {
    # расчёт суммарного параметра (согласно весам инструмента в портфеле)
    data.names <- names(data)[grep(".Close", names(data))]
    data.names <- sub(".Close", "", data.names)
    temp.text <- paste("data$",data.names,".",target, sep = "")
    temp.text <- paste(temp.text, basket.weights, sep = " * ", collapse = " + ")
    temp.text <- paste("data$",target," <- ", temp.text, sep = "") 
    eval(parse(text = temp.text))
    return(data)
}
#