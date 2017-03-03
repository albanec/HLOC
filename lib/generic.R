# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Общеупотребительные функции
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
###
#' Функция загружает, устанавливает и подключает необходимые пакеты 
#' 
#' @param package.list.csv Путь к .csv с пакетами
#' @param package.list Вектор с пакетами
#' @param download Загрузить пакеты
#' @param update Обновить пакеты
#'
#' @return 
#'
#' @export
PrepareForWork_Packages <- function(package.list.csv, 
                                    package.list = FALSE, download = FALSE, 
                                    update = FALSE) {
    # download, update или load: загрузить, обновить или подколючить пакеты
    # ----------
    #
    if (package.list == FALSE) {
        cat('Package List Path:', package.list.csv, '\n')    
        cat('Reading Package Lists...', '\n')
        package.list <- read.csv(package.list.csv, header = F, stringsAsFactors = F)    
        package.list <- unlist(package.list, use.names = FALSE)    
    } 
    cat('Loading Installed Package List...', '\n')
    if (update == TRUE) {
        cat ('Start Upgrading packages', '\n')
        update.packages(ask = FALSE)
        cat ('Upgrade Packages: OK', '\n')
        download <- FALSE
    } 
    if (download == TRUE) {
        package.list.temp1 <- setdiff(package.list, rownames(installed.packages()))
        if (length(package.list.temp1) > 0) {
            install.packages(package.list.temp1, dependencies = TRUE)        
            package.list.temp2 <- setdiff(package.list.temp1, rownames(installed.packages()))
            if (length(package.list.temp2) > 0) {
                warning('Installation Error!!!')
            } else {
                cat (length(diff(package.list, package.list.temp1)), ' packages newly installed!', '\n')
                cat('Installation Complete', '\n')
            }
        } else {
            cat ('All Packages Already Installed!', '\n')
        }    
    }
    if (load == TRUE) {
        cat('Load Libraries to Workspace')
        lapply(package.list, library, character.only = TRUE)
        cat('Libraries Prepare to Work')
    }
}
#
###
#' Функция вычисляет max расстояние между точкой и плоскостью
#'
#' @param у
#' @param x
#'
#' @return dist Max расстояние
#'
#' @export
FindMaxDistancePoint <- function(y, x=1:len(y)) {
    #
    all.coord <- rbind(vec(y), vec(x))
    first.point <- all.coord[, 1]
    line.vec <- all.coord[, len(y)] - first.point
    line.vec.n <- line.vec / sqrt(sum(line.vec^2))
    #    
    vec.from.first <- all.coord - first.point
    scalar.product <- line.vec.n %*% vec.from.first
    #    
    vec.from.first.parallel <- t(scalar.product) %*% line.vec.n
    vec.to.line <- t(vec.from.first) - vec.from.first.parallel
    dist.to.line <- sqrt(rowSums(vec.to.line^2, 2))
    dist <- which.max(dist.to.line)
    #
    return(dist)
}    
#
###
#' Функция очистки мусора в enviroment 
#' 
#' @param target Ключ для поиска
#' @param env Нужный enviroment 
#'
#' @return
#'
#' @export
CleanGarbage <- function(target = 'temp', env = '.GlobalEnv') {
    cat('INFO(CleanTempData): Removing TempData..    Start', '\n')
    removeVector <- 
        ls(env) %>%
        {
            grep(target, ., value = TRUE) 
        } 
    rm(list = removeVector, envir = as.environment(env))
    cat('INFO(CleanTempData): Removing TempData..    OK', '\n')
    cat('INFO(CleanTempData): Garbage Collect..    Start', '\n')
    gc()
    cat('INFO(CleanTempData): Garbage Collect..    OK', '\n')
}
#
#' Copy arguments into env and re-bind any function's lexical scope to bindTargetEnv .
#' 
#' @param obj_names Вектор имен переиенных, которые надо привязать к окружению
#' @param obj_pattern Таргет для поиска имен переменных
#' @param targetEnv Окружение, к которому надо привязать данные
#' @param parentEnv Окружение-источник данных
#' @export
BindToEnv <- function(obj_names = NULL, obj_pattern = NULL, .TargetEnv, .ParentEnv = parent.frame(),
                      doNotRebind = c()) {
    # содержимое материнского окружения
    env_content <- ls(name = .ParentEnv)
    if (!is.null(obj_pattern)) {
        obj_names <- 
            grep(pattern = obj_pattern, env_content) %>%
            env_content[.]      
    }
    for (i in 1:length(obj_names)) {
        temp.obj <- get(as.character(obj_names[i]), envir = .ParentEnv)
        # if(is.function(temp.obj) && (!(temp.obj %in% doNotRebind))) {
        #     environment(temp.obj) <- .TargetEnv
        # }
        assign(paste0(obj_names[i]), temp.obj, envir = .TargetEnv)
    } 
}
#
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