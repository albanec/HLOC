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
    FUN.StrategyGear <- match.fun(FUN.StrategyGear)
    df <- 
        lapply(1:nrow(var_df),
            function(x) {
                OneThreadRun(FUN.StrategyGear,
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
    var_names <- names(str_args)[grep('_', names(str_args))]
    for (i in 1:length(var_names)) {
        temp_text <- paste0(
            'perfomanceTable %<>% cbind.data.frame(.,',var_names[i],' = ',str_args[[var_names[i]]],')'
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
#' @param cluster_args Лист с параметрами кластеризации (accuracy, 
#'  iteration_max, max, plusplus, round_type)
#' @param 
#'
#' @return result DF с perfomance'ами по всем итерациям цикла 
#'
#' @export
RollerOptimizer.learning <- function(slice_index, 
                                     var_df,
                                     FUN.StrategyGear,
                                     win_size,
                                     ohlc_args, trade_args,
                                     cluster_args) {
    require(doParallel)
    #.CurrentEnv <- environment()                                                    
    if (getDoParWorkers() == 1) {
        workers <- detectCores() - 1    
        registerDoParallel(cores = workers)
    } else {
        workers <- getDoParWorkers()
    }
    FUN.StrategyGear <- match.fun(FUN.StrategyGear)
    
    #df_data <- integer(length(slice_index)) %>% as.list(.)

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
    #                 FUN.StrategyGear = FUN.StrategyGear, 
    #                 fast = TRUE,
    #                 ohlc_args = ., trade_args)    
    #         }
    #     }
    # } else {
        bf_data <- lapply(1:length(slice_index), 
            function(x) {
                ohlc_args %>%
                {
                    .$from_date <- slice_index[[x]][1, ]
                    .$to_date <- slice_index[[x]][2, ]
                    return(.)
                } %>%
                BruteForceOptimizer.mc(var_df = var_df,
                    FUN.StrategyGear = FUN.StrategyGear, 
                    fast = TRUE,
                    ohlc_args = ., 
                    trade_args)
            })
    # }

    ## КА
    if (workers > length(bf_data)) {
        workers <- length(bf_data)
    } 
    cluster_data <- foreach(i = 1:workers, .inorder = TRUE) %dopar% {
        map_range <- Delegate(i, length(bf_data), p = workers)
        if (is.null(map_range)) {
            return(NA)
        }
        result <- lapply(1:length(map_range),
            function(x) {
                ## Подготовка к КА
                data_for_cluster <- ClusterAnalysis.preparation(data = bf_data[[map_range[x]]], 
                    n.mouth = win_size, 
                    hi = TRUE, q.hi = 0.5, 
                    only_profitable = TRUE)
                data_for_cluster$profit <- NULL
                data_for_cluster$draw <- NULL
                ## Вычисление кластеров
                if (cluster_args$method %in% c('kmeans','plusplus')) {
                    clustFull.data <- 
                        ClusterAnalysis.parameters(data = data_for_cluster, 
                            method = cluster_args$method,
                            k.max = cluster_args$k.max,
                            iter.max = cluster_args$iter.max,
                            nstart =  cluster_args$nstart) %>%
                        .[[2]] %>%
                        ClusterAnalysis(data = data_for_cluster, 
                            method = cluster_args$method,
                            n.opt = ., 
                            var.digits = 3,
                            iter.max = cluster_args$iter.max,
                            nstart =  cluster_args$nstart)
                    ## Округление центров до значений точек пространства    
                    if (!is.null(cluster_args$round_type)) {
                        clustFull.data[[2]] %<>% {
                            for (x in 1:ncol(.[, !(colnames(.) %in% c('k_mm', 'profit.norm'))])) {
                                if (cluster_args$round_type == 'space') {
                                    .[, x] <- .[, x] - .[, x] %% 5    
                                }
                                if (cluster_args$round_type == 'integer') {
                                    .[, x] <- as.integer(.[, x])    
                                }                
                                if (cluster_args$round_type == 'round') {
                                    .[, x] <- round(.[, x])    
                                }
                            }
                            return(.)        
                        }    
                    }
                }
                if (cluster_args$method == 'pam') {
                    clustFull.data <- 
                        ClusterAnalysis.parameters(data = data_for_cluster, 
                            method = cluster_args$method,
                            k.max = cluster_args$k.max) %>%
                        .[[2]] %>%
                        ClusterAnalysis(data = data_for_cluster, 
                            method = cluster_args$method,
                            n.opt = ., 
                            var.digits = 3)
                }
                if (cluster_args$method == 'clara') {
                    clustFull.data <- 
                        ClusterAnalysis.parameters(data = data_for_cluster, 
                            method = cluster_args$method,
                            k.max = cluster_args$k.max,
                            samples = cluster_args$samples) %>%
                        .[[2]] %>%
                        ClusterAnalysis(data = data_for_cluster, 
                            method = cluster_args$method,
                            n.opt = ., 
                            var.digits = 3,
                            samples = cluster_args$samples)
                }
                return(clustFull.data)
            })
            return(result)
        } %>%
        unlist(., recursive = FALSE) 
    #
    if (any(is.na(cluster_data))) {
        cluster_data <- cluster_data[-which(is.na(cluster_data))]    
    }
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
    .CurrentEnv <- environment()

    ## подготовка данных 
    FUN.CalcOneTrade <- match.fun(FUN.CalcOneTrade)
    FUN.MM <- match.fun(FUN.MM)
    # стартовый баланс
    available_balance <- trade_args$balance_start
    # цикл расчёта по временным слайсам $bySlices
    result <- foreach(i = 1:length(slice_index), .verbose = FALSE) %do% {
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
        ## расчёты через future треды
        # plan(multiprocess)     
        # расчёт бэнчмарка
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
        # расчёт сделок портфеля
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
        
        # запуск тредов
        list(value(benchmark_DATA.future_thread), value(DATA.future_thread)) %>%
        {
            assign('benchmark_DATA', .[[1]], env = .CurrentEnv)
            assign('DATA', .[[2]][[1]], env = .CurrentEnv)
            assign('portfolio_DATA', .[[2]][[2]], env = .CurrentEnv)
        }
        
        # benchmark_DATA <- value(benchmark_DATA.future_thread)
        # DATA <- value(DATA.future_thread)
    
        # разделение данных по роботам и портфелю
        # portfolio_DATA <- DATA[[2]]
        # DATA <- DATA[[1]]

        ## вычисление perfomance-метрик 
        # future тред по портфелю
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
        # future тред по ботам
        # DATA.future_thread <- future({
        #     lapply(1:n_bots,
        #         function(x) {
        #             ## формирование таблиц сделок по каждому боту
        #             # чистим от лишних записей
        #             DATA[[x]][[2]] <- StateTable.clean(DATA[[x]][[2]])
        #             # лист с данными по сделкам (по тикерам и за всю корзину)
        #             tradeTable <- TradeTable.calc(DATA[[x]][[2]][!is.na(DATA[[x]][[2]]$state)], 
        #                 basket = TRUE, convert = TRUE)#TRUE     
        #             # добавление perf-данных
        #             DATA[[x]] <- 
        #                 PerfomanceTable(DATA[[x]],
        #                     trade_table = tradeTable,
        #                     asset_cols = c('balance', 'im.balance'),
        #                     balance_start = available_balance %/% n_bots, 
        #                     ret_type = trade_args$return_type,
        #                     fast = FALSE,
        #                     dd_data_output = FALSE,
        #                     trade_stats = TRUE) %>%
        #                 # добавление использованных параметров
        #                 #cbind.data.frame(., 
        #                 #    per_DCI = per_DCI, per_slowSMA = per_slowSMA, per_fastSMA = per_fastSMA, k_mm = k_mm)    
        #                 {
        #                     list(data = DATA[[x]][[1]], state = DATA[[x]][[2]], trade = tradeTable, perf = .)
        #                 }
        #             # names(DATA[x]) <- c('data', 'state', 'trade', 'perf')
        #         })
        # }) %plan% multiprocess
        # future тред по бенчмарку
        # benchmark_DATA.future_thread <- future({
        #     lapply(1:n_bots,
        #         function(x) {
        #             ## формирование таблиц сделок оп каждому боту
        #             # чистим от лишних записей
        #             benchmark_DATA[[x]][[2]] <- StateTable.clean(benchmark_DATA[[x]][[2]])
        #             # лист с данными по сделкам (по тикерам и за всю корзину)
        #             tradeTable <- TradeTable.calc(benchmark_DATA[[x]][[2]], basket = FALSE, convert = TRUE)#TRUE     
        #             # метрики
        #             benchmark_DATA[[x]] <- 
        #                 PerfomanceTable(benchmark_DATA[[x]], 
        #                     trade_table = tradeTable,
        #                     asset_cols = c('balance', 'im.balance'),
        #                     balance_start = available_balance %/% n_bots, 
        #                     ret_type = trade_args$return_type,
        #                     fast = FALSE,
        #                     dd_data_output = FALSE,
        #                     trade_stats = TRUE) %>%
        #                 # добавление использованных параметров
        #                 #cbind.data.frame(., 
        #                 #    per_DCI = per_DCI, per_slowSMA = per_slowSMA, per_fastSMA = per_fastSMA, k_mm = k_mm)    
        #                 {
        #                     list(data = benchmark_DATA[[x]][[1]], 
        #                         state = benchmark_DATA[[x]][[2]], 
        #                         trade = tradeTable, 
        #                         perf = .)
        #                 }
        #             # names(BENCHMARK[x]) <- c('data', 'state', 'trade', 'perf')
        #         })
        # }) %plan% multiprocess
        
        # запуск вычислений
        result <- list(#bot = value(DATA.future_thread), 
            portfolio = value(portfolio_DATA.future_thread)#, 
            #benchmark = value(benchmark_DATA.future_thread)
            )

        # баланс для следующих периодов
        available_balance <- available_balance + coredata(result$portfolio$perf$Profit)

        # очистка мусора по target = 'temp'
        CleanGarbage(target = 'temp', env = '.GlobalEnv')
        #
        return(result)
    }
    # return()
}
