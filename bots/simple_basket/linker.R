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
source('lib/generic.R')
source('lib/strategy.R')
source('lib/evaluation.R')
source('lib/optimization.R')
source('lib/cluster.R')
# GEAR
source('bots/simple_basket/gear.R')
