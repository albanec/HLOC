# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Функции для parallel вычислений:
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
#' Copy arguments into env and re-bind any function's lexical scope to bindTargetEnv .
#' 
#' @param bindTargetEnv environment to bind to
#' @param objNames additional names to lookup in parent environment and bind
#' @param names of functions to NOT rebind the lexical environments of
#' @export
BindToEnv <- function (bindTargetEnv = parent.frame(), objNames, doNotRebind = c()) {
    # Bind the values into environment
    # and switch any functions to this environment!
    for (var in objNames) {
        val <- get(var, envir = parent.frame())
        if (is.function(val) && (!(var %in% doNotRebind))) {
            # replace function's lexical environment with our target (DANGEROUS)
            environment(val) <- bindTargetEnv
        }
        # assign object to target environment, only after any possible alteration
        assign(var, val, envir = bindTargetEnv)
    }
}
# Integer Mapping для multicore вычислений
#'
#' Выдаёт индексы строк входных данных, предназначенных для обработки конкретному worker'у
#' Если для worker'а нет задачи - выдаёт NULL
#' 
#' @param i ID процесса
#' @param n Число строк в анализируемых данных
#' @param k Окно (по умалчанию == 1)
#' @param p Число worker-процессов
#' @export
Delegate <- function(i, n, k = 1, p) {
    nOut <- n - k + 1
    nProc <- ceiling(nOut / p)
    result <- ((i - 1) * nProc + 1):min(i * nProc + k - 1, n) 
    if (any(result > n)) {
        return(NULL)    
    }
    #
    return(result)
}
#
mcTimeSeries <- function(data, tsfunc, byColumn, windowSize, workers) {
    # foreach вычисления
    SERIES <- foreach(i = 1:workers, .combine = rbind) %dopar% {
        jRange <- Delegate(i = i, n = nrow (data), k = windowSize, p = workers)
        rollapply(data[jRange, ],
            width = windowSize,
            FUN = tsfunc,
            align = "right",
            by.column = byColumn)
    }
    # переформатирование имен столбцов
    names(SERIES) <- gsub("\\..+", "", names(SERIES))
    if (windowSize > 1) {
        PAD <- zoo(
            matrix(nrow = windowSize - 1, ncol = ncol(SERIES), NA),
            order.by = index(data)[1:(windowSize - 1)]
        )
        names(PAD) <- names(SERIES)
        SERIES <- rbind(PAD, SERIES)
    }
    if (is.null(names(SERIES))) {
        names(SERIES) <- gsub("\\..+", "", names(data)[1:ncol(SERIES)])
    }
    #
    return(SERIES)
}
#