# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Linker для подключения библиотек
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
## Загрузка пакетов
library(quantmod)
library(rusquant)
library(PerformanceAnalytics)
library(magrittr)
library(tidyr)
library(parallel)
library(lubridate)
# library(RQuantLib)
# library(dplyr)
# library(data.table)
#
## Загрузка библиотек
source('lib/generic/generic.R')
source('lib/strategy/strategy.R')
source('lib/evaluation/evaluation.R')
source('lib/optimization/optimization.R')
source('lib/cluster/cluster.R')
# движок стратегии
source('main/test/lib/gear/test_gear.R')
# движок оптимизации
source('main/test/lib/optimization/test_optimization.R')
# движок скользящей оптимизации
source('main/test/lib/optimization/test_roller.R')
