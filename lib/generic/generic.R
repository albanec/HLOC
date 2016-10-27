# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Общеупотребительные функции
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
### Загрузка дочерних библиотек
source("lib/generic/generic_mining.R")
source("lib/generic/generic_getdata.R")
source("lib/generic/generic_data.R")
source("lib/generic/generic_quotes.R")
source("lib/generic/generic_perfomance.R")
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
PrepareForWork_Packages <- function(package.list.csv, package.list = FALSE, download = FALSE, 
                                    update = FALSE) {
  # download, update или load: загрузить, обновить или подколючить пакеты
  # ----------
  #
  if (package.list == FALSE) {
    cat("Package List Path:", package.list.csv, "\n")  
    cat("Reading Package Lists...", "\n")
    package.list <- read.csv(package.list.csv, header = F, stringsAsFactors = F)  
    package.list <- unlist(package.list, use.names = FALSE)  
  } 
  cat("Loading Installed Package List...", "\n")
  if (update == TRUE) {
    cat ("Start Upgrading packages", "\n")
    update.packages(ask = FALSE)
    cat ("Upgrade Packages: OK", "\n")
    download <- FALSE
  } 
  if (download == TRUE) {
    package.list.temp1 <- setdiff(package.list, rownames(installed.packages()))
    if (length(package.list.temp1) > 0) {
      install.packages(package.list.temp1, dependencies = TRUE)    
      package.list.temp2 <- setdiff(package.list.temp1, rownames(installed.packages()))
      if (length(package.list.temp2) > 0) {
        warning("Installation Error!!!")
      } else {
        cat (length(diff(package.list, package.list.temp1)), " packages newly installed!", "\n")
        cat("Installation Complete", "\n")
      }
    } else {
      cat ("All Packages Already Installed!", "\n")
    }  
  }
  if (load == TRUE) {
    cat("Load Libraries to Workspace")
    lapply(package.list, library, character.only = TRUE)
    cat("Libraries Prepare to Work")
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
CleanGarbage <- function(target = "temp", env = ".GlobalEnv") {
  cat("INFO(CleanTempData): Removing TempData..  Start", "\n")
  removeVector <- 
    ls(env) %>%
    {
      grep(target, ., value = TRUE) 
    } 
  rm(list = removeVector, envir = as.environment(env))
  cat("INFO(CleanTempData): Removing TempData..  OK", "\n")
  cat("INFO(CleanTempData): Garbage Collect..  Start", "\n")
  gc()
  cat("INFO(CleanTempData): Garbage Collect..  OK", "\n")
}