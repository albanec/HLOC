# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Функции расчета параметров drawdown'ов:
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
###
#' Расчёт итоговой таблицы с данными drawdown'ов
#'
#' Функция вычисляет параметры по просадкам (выводит итоговые данные)
#' 
#' @param data_balance Данные balance (после    отработки стратегии)
#'
#' @return drawdown_table DF с данными по просадкам
#'
#' @export
DrawdownsTable <- function(data_balance, dd_data_output = FALSE) {
    # ----------
    # подготовка данных
    #cat('INFO(DrawdownsTable): Calc Drawdown Data Set', '\n')
    drawdowns <- Drawdowns(data = data_balance, fullData = TRUE)
    ### вычисление summary по data set'у
    ## max просадка
    #cat('INFO(DrawdownsTable): Calc MaxDrawdown', '\n')
    max_drawdown <- 
        min(drawdowns[[2]]$Depth) %>%
        as.numeric(.)
    max_drawdown_percent <-
        min(drawdowns[[2]]$DepthPercent) %>%
        as.numeric(.)    
    ## день max просадки
    max_dd_day <- 
        drawdowns[[1]][drawdowns[[1]]$dd == max_drawdown] %>%
        {
            index.xts(xts::first(.)) 
        } %>%
        as.POSIXct(., origin = '1970-01-01')
    ## средняя просадка
    #cat('Calculating Performance Metric:    MeanDrawdown', '\n')
    mean_drawdown <- 
        mean(drawdowns[[2]]$Depth) %>% 
        as.numeric(.)
    mean_drawdown_percent <-
        mean(drawdowns[[2]]$DepthPercent) %>% 
        as.numeric(.)
    ## max длина просадки в днях
    #cat('Calculating Performance Metric:    MaxDrawdownDays', '\n')
    max_drawdown_days <- 
        max(drawdowns[[2]]$Days) %>%
        as.numeric(.) 
    ## среднее число дней в просадке
    #cat('Calculating Performance Metric:    MeanDrawdownDays', '\n')
    mean_drawdown_days <- 
        drawdowns[[2]]$Days[drawdowns[[2]]$Days != 0] %>%
        mean(.) %>%
        trunc(.) %>%
        as.numeric(.)
    ## текущее число дней в просадке
    #cat('Calculating Performance Metric:    NowDrawdownDays', '\n')
    now_drawdown_days <- 
        ifelse(xts::last(drawdowns[[1]]$dd) != 0,
            xts::last(drawdowns[[2]]$Days),
            0) %>%
        as.numeric(.)
    ## текущее число свечей в просадке
    #cat('Calculating Performance Metric:    NowDrawdownBars', '\n')
    now_drawdown_periods <- 
        ifelse(xts::last(drawdowns[[1]]$dd) != 0,
            xts::last(drawdowns[[2]]$Length),
            0) %>%
        as.numeric(.)
    ## текущая просадка 
    #cat('Calculating Performance Metric:    NowDrawdown', '\n')
    now_drawdown <- 
        ifelse(xts::last(drawdowns[[1]]$dd) != 0,
            xts::last(drawdowns[[1]]$dd),
            0) %>%
        as.numeric(.)
    now_drawdown_percent <- 
        ifelse(xts::last(drawdowns[[1]]$dd) != 0,
            xts::last(drawdowns[[1]]$dd.percent),
            0) %>%
        as.numeric(.)
    #
    ### формирование таблицы
    drawdown_table <- 
        {
            df <- data.frame(
                DrawdownMaxDate = character(1) %>% 
                    as.numeric(.) %>% 
                    as.Date(.),
                DrawdownMax = as.numeric(1),
                DrawdownMaxPercent = as.numeric(1),
                DrawdownAverage = as.numeric(1),
                DrawdownAveragePercent = as.numeric(1),
                DrawdownDaysMax = as.numeric(1),
                DrawdownDaysAverage = as.numeric(1),
                DrawdownNowDays = as.numeric(1),
                DrawdownNowBars = as.numeric(1),
                DrawdownNow = as.numeric(1),
                DrawdownNowPercent = as.numeric(1)
            )         
        } %>%
        {
            .$DrawdownMaxDate <- max_dd_day
            .$DrawdownMax <- max_drawdown
            .$DrawdownMaxPercent <- max_drawdown_percent
            .$DrawdownAverage <- mean_drawdown
            .$DrawdownAveragePercent <- mean_drawdown_percent
            .$DrawdownDaysMax <- max_drawdown_days
            .$DrawdownDaysAverage <- mean_drawdown_days
            .$DrawdownNowDays <- now_drawdown_days
            .$DrawdownNowBars <- now_drawdown_periods
            .$DrawdownNow <- now_drawdown
            .$DrawdownNowPercent <- now_drawdown_percent
            return(.)
        }
    #
    if (dd_data_output == TRUE) {
        return(list(drawdown_table = drawdown_table, dd.list = drawdowns))    
    } 
    return(drawdown_table)    
}
#
###
#' Функция расчёта таблицы с данными по всем drawdown'ам
#'
#' Функция возращает df с данными по всем просадкам
#' 
#' @param data Данные balance
#'
#' @return drawdowns Таблица просадок (или list(dd_data, drawdowns))
#'
#' @export
Drawdowns <- function(data, fullData = FALSE) {
    # ----------
    # расчёт dd
    dd_data <- Drawdowns.calc(data = data)
    n.vec <- 1:max(dd_data$num)
    # формирование таблицы со статистикой
    drawdowns <- 
        lapply(n.vec,
            function(x) {
                Drawdowns.summary(data = dd_data[dd_data$num == x])
            }) %>%
        MergeData_inList.byRow(.)
    #
    if (fullData == TRUE) {
        return(list(dd_data = dd_data, drawdowns = drawdowns))    
    }
    #
    return(drawdowns)    
}
#
###
#' Функция параметров одного drawdown'а
#'
#' Функция возращает таблицу данных одному dd
#' 
#' @param data Данные по нужному dd
#' 
#' @return dd_summary df, содержащий данные по dd c номером n
#'
#' @export
Drawdowns.summary <- function(data) {
    #
    # конвертация  данных в df
    data <- Convert.XTStoDF(data)
    # создаём скелет df с нужными полями
    df <- data.frame(
        From = character(1) %>% 
            as.numeric(1) %>% 
            as.Date(1),
        To = character(1) %>% 
            as.numeric(1) %>% 
            as.Date(1),
        Depth = numeric(1),
        DepthPercent = numeric(1),
        Length = numeric(1),
        Days = numeric(1),
        row.names = NULL
    )
    ## заполняем поля данными
    # начало dd
    df$From <- 
        data$INDEX[1] %>%
        as.POSIXct(., origin = '1970-01-01') 
    # конец dd
    df$To <- 
        data$INDEX[nrow(data)] %>%
        as.POSIXct(., origin = '1970-01-01') 
    # максимальная глубина
    df$Depth <- min(data$dd)
    df$DepthPercent <- min(data$dd.percent)
    # длина (количество периодов)
    df$Length <- nrow(data)
    # дни в просадке
    df$Days <-
        # as.POSIXct(data$INDEX, origin = '1970-01-01') %>%
        CalcTradingDays(x = data$INDEX, fullDays = TRUE) 
    
    # df <- df[, -1]
    #
    return(df)
}
#
###
#' Функция расчёта drawdown'ов по balance
#'
#' Функция возращает xts с данными по всем просадкам 
#' 
#' @param data Данные balance
#' 
#' @return data XTS, солержащий данные по dd 
#'
#' @export
Drawdowns.calc <- function(data) {
    #
    # очистка от строк c одинаковым индексом (если есть)
    data <- data[which(!duplicated(index.xts(data)))]
    ## формируем нужные столбцы
    # пики balance
    data$peak <- cummax(data[, 1])
    # значения dd на каждой свече
    data$dd <- data[, 1] - data$peak 
    # 1 - свеча в просадке, 0 - не в просадке 
    data$temp <- abs(sign(data$dd))
    # точки перехода в/из просадки (1 - первая свеча в просадке, -1 - точка выхода из просадки, 0 - состояние не меняется)
    data$temp.diff <- diff(data$temp)
    # 1 - если свечка относится к dd (с учётом обновления пика на выходе из просадки)
    data$temp.bars <- abs(sign(data$temp + data$temp.diff))
    #
    data <- 
        {
            # индексы строк роста balance
            tempIndex <- which(data$temp.diff == 0 & data$temp.bars == 0)
            # если есть такие периоды
            if (length(tempIndex) != 0) {
                # удаляем их
                data <- data[-tempIndex]
                # точки выхода из dd метим 1 (это нужно для нумирации просадок в дальнейшем)
                data$temp.diff[which(data$temp.diff == -1)] <- 0    
            }            
            return(data)
        } %>%
        na.omit(.)
    # нумерация просадок
    data$num <- cumsum(data$temp.diff)
    # собираем мусор
    data <- 
        CleanGarbage.inCols(data) %>%
        # выкидываем balance столбец (исходные данные)
        {
            .[, -1]
        }
    # 
    data$dd.percent[data$peak == 0] <- NA
    data$dd.percent[data$peak != 0] <- data$dd[data$peak != 0] * 100 / data$peak[data$peak != 0]
    #
    return(data)
}
#