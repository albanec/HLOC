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
source('main/simple_basket/lib/gear/simple_gear.R')
# движок оптимизации
source('main/simple_basket/lib/optimization/simple_optimization.R')
