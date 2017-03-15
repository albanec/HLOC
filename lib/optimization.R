# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Функции для оптимизации стратегий:
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
###
#' Функция BF оптимизации параметров стратегии 
#' 
#' @param var_df Оптимизационная матрица параметров стратегии
#' @param FUN.StrategyGear Функция стратегии
#' @param fast Упрощенный/полный perfomance анализ
#' @param ohlc_args Лист с параметрами котировок
#' @param str_args Лист с параметрами стратегии
#'
#' @return df data.frame с perfomance'ами по всем итерациям цикла 
#'
#' @export
BruteForceOptimizer <- function(var_df, 
                                FUN.StrategyGear,
                                fast = FALSE,
                                ohlc_args, trade_args) { 
    #
    #FUN.StrategyGear <- match.fun(FUN.StrategyGear)
    df <- 
        lapply(1:nrow(var_df),
            function(x) {
                OneThreadRun(match.fun(FUN.StrategyGear),
                    fast = fast, 
                    dd_data_output = FALSE,
                    ohlc_args, 
                    trade_args, 
                    do.call(list, var_df[x, ]))
            }) %>%
        {
            .[!is.null(.)]
        } %>%
        MergeData_inList.byRow(.)    
    #
    return(df)
}
#
###
#' Функция одного прогона вычислений движка стратегии (на готовых данных)
#' 
#' @param FUN.StrategyGear Функция движка стратегии
#' @param fast Упрощенный/полный perfomance анализ
#' @param dd_data_output Вывод данных по dd (T/F)
#' @param ohlc_args
#' @param trade_args
#' @param str_args
#'
#' @return perfomanceTable Строка с perfomance-данными прогона робота
#'
#' @export
OneThreadRun <- function(FUN.StrategyGear, 
                         fast = FALSE, 
                         dd_data_output = FALSE,
                         ohlc_args, trade_args, str_args) {
    #
    FUN.StrategyGear <- match.fun(FUN.StrategyGear) 
    ### отработка робота
    DATA <- FUN.StrategyGear(ohlc_args, trade_args, str_args)
    # для стратегий, у которых нет сделок
    if (is.null(DATA[[2]])) {
        return(NULL)
    } 
    # чистим от лишних записей
    DATA[[2]] <- StateTable.clean(DATA[[2]])
    
    ### Анализ perfomanc'ов
    # формирование таблицы сделок
    if (fast == TRUE) {
        # оценка perfomance-параметров
        perfomanceTable <- PerfomanceTable(DATA, 
            trade_table = 0,
            balance = trade_args$balance_start, 
            ret_type = 0, 
            fast = TRUE)    
    } else {
        # лист с данными по сделкам (по тикерам и за всю корзину)
        tradeTable <- TradeTable.calc(DATA[[2]], basket = FALSE, convert = TRUE)
        if (length(ohlc_args$ticker) == 1) {
            tradeTable[[1]]$TradeReturnPercent <- tradeTable[[1]]$TradeReturn * 100 / trade_args$balance_start
        }
        # оценка perfomance-параметров
        perfomanceTable <- PerfomanceTable(DATA, 
            trade_table = tradeTable,
            balance_start = trade_args$balance_start, 
            ret_type = trade_args$return_type,
            dd_data_output = dd_data_output)
        if (dd_data_output == TRUE) {
            dd_data_output.list <- perfomanceTable$dd.list
            perfomanceTable <- perfomanceTable$perfomance_table
        }
    }
    # добавление использованных параметров
    for (i in 1:length(str_args)) {
        temp_text <- paste0(
            'perfomanceTable %<>% cbind.data.frame(.,',names(str_args)[i],' = ',str_args[[names(str_args)[i]]],')'
        )
        eval(parse(text = temp_text))
    }
    #
    if (dd_data_output == TRUE) {
        return(list(perfomanceTable = perfomance_table, dd.list = dd_data_output.list))
    }
    #
    return(perfomanceTable)
}
#
###
#' Roller функция обучающей оптимизации движка стратегии (multicore)
#' 
#' @param slice_index Временные интервалы оптимизационных окон (индексы start/end)
#' @param var_df Оптимизационная матрица параметров стратегии
#' @param FUN.StrategyGear Функция стратегии
#' @param win_size Период обучения (нужно для более точной кластеризации)
#' @param ohlc_args Лист с параметрами котировок
#' @param trade_args Лист с торговыми параметрами
#' @param 
#'
#' @return result DF с perfomance'ами по всем итерациям цикла 
#'
#' @export
RollerOptimizer.learning <- function(slice_index, 
                                     var_df,
                                     FUN.StrategyGear,
                                     win_size,
                                     ohlc_args, trade_args) {
    require(doParallel)
    #.CurrentEnv <- environment()                                                    
    if (getDoParWorkers() == 1) {
        workers <- detectCores() - 1    
        registerDoParallel(cores = workers)
    } else {
        workers <- getDoParWorkers()
    }
    
    ## Вычисление оптимизаций на обучающих периодах
    # выбор оптимальной стратегии распараллеливания
    # if (length(slice_index) >= workers) {
    #     bf_data <- foreach(i = 1:workers) %dopar% {
    #         map_data <- Delegate(i, length(slice_index), p = workers)
    #         foreach(x = 1:length(map_data)) %do% {
    #             ohlc_args %>%
    #             {
    #                 .$from_date <- slice_index[[map_data[x]]][1, ]
    #                 .$to_date <- slice_index[[map_data[x]]][2, ]
    #                 return(.)
    #             } %>%    
    #             BruteForceOptimizer(var_df = var_df,
    #                 FUN.StrategyGear = match.fun(FUN.StrategyGear), 
    #                 fast = TRUE,
    #                 ohlc_args = ., trade_args)    
    #         }
    #     }
    # } else {
        bf_data <- foreach(i = 1:length(slice_index)) %do% {
            ohlc_args %>%
            {
                .$from_date <- slice_index[[i]][1, ]
                .$to_date <- slice_index[[i]][2, ]
                return(.)
            } %>%
            BruteForceOptimizer.mc(var_df = var_df,
                FUN.StrategyGear = match.fun(FUN.StrategyGear), 
                fast = TRUE,
                ohlc_args = ., 
                trade_args)
        }
    # }

    ## КА
    cluster_data <- lapply(1:length(bf_data),
        function(x) {
            ## Подготовка к КА
            data_for_cluster <- 
                bf_data[[x]] %>%
                {
                    CalcKmean.preparation(data = ., 
                        n.mouth = win_size, 
                        hi = TRUE, q.hi = 0.5, 
                        only_profitable = TRUE)
                }
            data_for_cluster$profit <- NULL
            data_for_cluster$draw <- NULL
            ## Вычисление кластеров
            clustFull.data <- 
                CalcKmean.parameters(data = data_for_cluster, 
                    iter.max = 100, 
                    plusplus = FALSE, 
                    test.range = 30) %>%
                .[[2]] %>%
                CalcKmean(data = data_for_cluster, 
                    n.opt = ., 
                    plusplus = FALSE, 
                    var.digits = 3)
            ## Округление центров до значений точек пространства    
            clustFull.data[[2]] %<>%
                {
                    for (x in 1:ncol(.[, !(colnames(.) %in% c('k_mm', 'profit.norm'))])) {
                        .[, x] <- .[, x] - .[, x] %% 5
                    }
                    return(.)        
                }    
            return(clustFull.data)
        }) 
    #
    return(cluster_data)
}
#
###
#' Roller функция торговли кластерными ботами
#' 
#' @param slice_index Временные интервалы торговых окон (индексы start/end)
#' @param bot.list Лист с параметрами ботов
#' @param FUN.CalcOneTrade Функция расчёта трейда
#' @param FUN.MM Функция MM
#' @param ohlc_args Лист с параметрами котировок
#' @param trade_args Лист с торговыми параметрами
#' @param FUN.StrategyGear_pattern Паттерн поиска StrategyGear-функций (по умолчанию 'StrategyGear.')
#' @param FUN.TradeHandler_pattern Паттерн поиска TradeHandler-функций (по умолчанию 'TradeHandler.')
#' @param var_pattern Паттерн поиска переменных стратегии внутри листа с данными по боту
#'
#' @return Лист с листами по каждому слайсу (return(list(bot = DATA, portfolio = portfolio_DATA, benchmark = benchmark_DATA)))
#'
#' @export                             
RollerOptimizer.trade <- function(slice_index, 
                                  bot.list,
                                  FUN.CalcOneTrade = CalcOneTrade,
                                  FUN.MM = CalcMM.byDCIwidth,
                                  ohlc_args, 
                                  trade_args,
                                  FUN.StrategyGear_pattern = 'StrategyGear.',
                                  FUN.TradeHandler_pattern = 'TradeHandler.',
                                  var_pattern = '_') {
    require('future')
    ## подготовка данных 
    FUN.CalcOneTrade <- match.fun(FUN.CalcOneTrade)
    FUN.MM <- match.fun(FUN.MM)
    # стартовый баланс
    available_balance <- trade_args$balance_start

    # цикл расчёта по временным слайсам $bySlices
    test <- foreach(i = 1:length(slice_index)) %do% {
        n_bots <- length(bot.list[[i]])
        ## расчёт сырых данных для портфеля ботов
        DATA <- 
            # формирование листов аргументов
            list(ohlc_args, trade_args) %>%
            {
                .[[1]]$from_date <- slice_index[[i]][1, ]
                .[[1]]$to_date <- slice_index[[i]][2, ]
                .[[2]]$balance_start <- 0
                .[[2]]$trade_handler <- 'basket'
                return(.)
            } %>%
            {
                BotPortfolio.mcapply(bot.list[[i]], 
                    FUN_pattern = FUN.StrategyGear_pattern, 
                    var_pattern = var_pattern,
                    ohlc_args = .[[1]],
                    trade_args = .[[2]])
            }
        # расчёты через future треды
        # plan(multiprocess)     
        ## расчёт бэнчмарка
        benchmark_DATA.future_thread <- future({
            # формирование листов аргументов
            list(ohlc_args, trade_args) %>%
            {
                .[[1]]$from_date <- slice_index[[i]][1, ]
                .[[1]]$to_date <- slice_index[[i]][2, ]
                .[[2]]$balance_start <- available_balance %/% n_bots
                .[[2]]$trade_handler <- 'standalone'
                return(.)
            } %>%
            {
                BotPortfolio.mcapply(bot.list[[i]], 
                    FUN_pattern = FUN.TradeHandler_pattern, 
                    var_pattern = var_pattern,
                    #append(list(dots$data[map_range[x]]), var_args[map_range[x]])
                    eval_str = paste0('var_args <- append(dots$data[map_range[x]], var_args);
                        names(var_args) <- c("data", "ohlc_args", "trade_args", "str_args")
                        do.call(FUN_names[map_range[x]], var_args, envir = .CurrentEnv)'),
                    ohlc_args = .[[1]],
                    trade_args = .[[2]],  
                    data = DATA)
            }
        }) %plan% multiprocess
            
        ## расчёт сделок портфеля
        DATA.future_thread <- future({
            list(ohlc_args, trade_args) %>%
            {
                .[[1]]$from_date <- slice_index[[i]][1, ]
                .[[1]]$to_date <- slice_index[[i]][2, ]
                .[[2]]$balance_start <- available_balance
                .[[2]]$trade_handler <- 'basket'
                return(.)
            } %>%
            {
                BotPortfolio.trade_handler(bot.list = bot.list[[i]],
                    data = DATA,
                    FUN.CalcOneTrade = CalcOneTrade,
                    FUN.MM = CalcMM.byDCIwidth,
                    var_pattern = var_pattern,
                    ohlc_args = .[[1]],
                    trade_args = .[[2]])
            }
        }) %plan% multiprocess
        #
        benchmark_DATA <- value(benchmark_DATA.future_thread)
        DATA <- value(DATA.future_thread)
        # разделение данных по роботам и портфелю
        portfolio_DATA <- DATA[[2]]
        DATA <- DATA[[1]]

        ## вычисление perfomance-метрик по портфелю
        portfolio_DATA.future_thread <- future({
            PerfomanceTable(portfolio_DATA,
                trade_table = NULL,
                asset_cols = c('balance', 'im.balance'),
                balance_start = available_balance, 
                ret_type = trade_args$return_type,
                fast = FALSE,
                dd_data_output = FALSE,
                trade_stats = FALSE) %>%
            {
                list(data = portfolio_DATA[[1]], state = portfolio_DATA[[2]], trade = NA, perf = .)
            }
        }) %plan% multiprocess
            
        # names(PORTFOLIO) <- c('data', 'state', 'trade', 'perf')
        
        ## вычисление perfomance-метрик по ботам
        DATA.future_thread <- future({
            lapply(1:n_bots,
                function(x) {
                    ## формирование таблиц сделок по каждому боту
                    # чистим от лишних записей
                    DATA[[x]][[2]] <- StateTable.clean(DATA[[x]][[2]])
                    # лист с данными по сделкам (по тикерам и за всю корзину)
                    tradeTable <- TradeTable.calc(DATA[[x]][[2]][!is.na(DATA[[x]][[2]]$state)], 
                        basket = TRUE, convert = TRUE)#TRUE     
                    # добавление perf-данных
                    DATA[[x]] <- 
                        PerfomanceTable(DATA[[x]],
                            trade_table = tradeTable,
                            asset_cols = c('balance', 'im.balance'),
                            balance_start = available_balance %/% n_bots, 
                            ret_type = trade_args$return_type,
                            fast = FALSE,
                            dd_data_output = FALSE,
                            trade_stats = TRUE) %>%
                        # добавление использованных параметров
                        #cbind.data.frame(., 
                        #    per_DCI = per_DCI, per_slowSMA = per_slowSMA, per_fastSMA = per_fastSMA, k_mm = k_mm)    
                        {
                            list(data = DATA[[x]][[1]], state = DATA[[x]][[2]], trade = tradeTable, perf = .)
                        }
                    # names(DATA[x]) <- c('data', 'state', 'trade', 'perf')
                })
        }) %plan% multiprocess
        ## вычисление perfomance-метрик по бенчмарку
        benchmark_DATA.future_thread <- future({
            lapply(1:n_bots,
                function(x) {
                    ## формирование таблиц сделок оп каждому боту
                    # чистим от лишних записей
                    benchmark_DATA[[x]][[2]] <- StateTable.clean(benchmark_DATA[[x]][[2]])
                    # лист с данными по сделкам (по тикерам и за всю корзину)
                    tradeTable <- TradeTable.calc(benchmark_DATA[[x]][[2]], basket = FALSE, convert = TRUE)#TRUE     
                    # метрики
                    benchmark_DATA[[x]] <- 
                        PerfomanceTable(benchmark_DATA[[x]], 
                            trade_table = tradeTable,
                            asset_cols = c('balance', 'im.balance'),
                            balance_start = available_balance %/% n_bots, 
                            ret_type = trade_args$return_type,
                            fast = FALSE,
                            dd_data_output = FALSE,
                            trade_stats = TRUE) %>%
                        # добавление использованных параметров
                        #cbind.data.frame(., 
                        #    per_DCI = per_DCI, per_slowSMA = per_slowSMA, per_fastSMA = per_fastSMA, k_mm = k_mm)    
                        {
                            list(data = benchmark_DATA[[x]][[1]], 
                                state = benchmark_DATA[[x]][[2]], 
                                trade = tradeTable, 
                                perf = .)
                        }
                    # names(BENCHMARK[x]) <- c('data', 'state', 'trade', 'perf')
                })
        }) %plan% multiprocess
        
        result <- list(bot = value(DATA.future_thread), 
            portfolio = value(portfolio_DATA.future_thread), 
            benchmark = value(benchmark_DATA.future_thread))
        # баланс для следующих периодов
        available_balance <- available_balance + coredata(result$portfolio$perf$Profit)
        
        # очистка мусора по target = 'temp'
        CleanGarbage(target = 'temp', env = '.GlobalEnv')
        #
        return(result)
    }
}
