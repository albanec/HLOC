# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Linker для подключения библиотек
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
## Загрузка пакетов
library(quantmod)
library(rusquant)
library(PerformanceAnalytics)
library(magrittr)
library(tidyr)
library(parallel)
library(lubridate)
library(doParallel)
#library(doFuture)
#library(future)
library(foreach)
library(data.table)
library(dplyr)
library(e1071)
#
## Загрузка библиотек
### GEN
source('lib/generic_mining.R')
source('lib/generic_getdata.R')
source('lib/generic_data.R')
source('lib/generic_quotes.R')
source('lib/generic.R')

### STR
source('lib/strategy_indicators.R')
source('lib/strategy_mm.R')
source('lib/strategy.R')
source('lib/strategy_bot_combination.R')

### OPT
source('lib/optimization_cluster.R')
source('lib/optimization_multicore.R')
source('lib/optimization.R')

### CLU
source('lib/cluster.R')

### EVA
source('lib/evaluation_date.R')
source('lib/evaluation_trade.R')
source('lib/evaluation_drawdown.R')
source('lib/evaluation_profit_day.R')
source('lib/evaluation_profit_trade.R')
source('lib/evaluation_profit.R')
source('lib/evaluation_ratio.R')
source('lib/evaluation.R')

# GEAR
source('bots/rsi_svm/gear.R')
