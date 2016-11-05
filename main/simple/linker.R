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
source('lib/strategy/simple/simple_gear.R')
# движок оптимизации
source('lib/optimization/simple/simple_optimization.R')
