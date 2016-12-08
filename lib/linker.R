# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Linker для подключения библиотек (полной их подгрузки)
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
### GEN
source('lib/generic/generic_mining.R')
source('lib/generic/generic_getdata.R')
source('lib/generic/generic_data.R')
source('lib/generic/generic_quotes.R')
source('lib/generic/generic_parallel.R')
source('lib/generic/generic.R')

### STR
source('lib/strategy/strategy_indicators.R')
source('lib/strategy/strategy_mm.R')
source('lib/strategy/strategy.R')
source('lib/strategy/strategy_bot_combination.R')


### OPT
source('lib/optimization/optimization_cluster.R')
source('lib/optimization/optimization_multicore.R')
source('lib/optimization/optimization.R')

### CLU
source('lib/cluster/cluster.R')

### EVA
source('lib/evaluation/evaluation_dates.R')
source('lib/evaluation/evaluation_trades.R')
source('lib/evaluation/evaluation_drawdown.R')
source('lib/evaluation/evaluation_profit_days.R')
source('lib/evaluation/evaluation_profit_trades.R')
source('lib/evaluation/evaluation_profit.R')
source('lib/evaluation/evaluation_ratio.R')
source('lib/evaluation/evaluation.R')
