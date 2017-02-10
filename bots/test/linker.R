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
source('bots/test/gear.R')
# test roller
source('bots/test/test_roller.R')
