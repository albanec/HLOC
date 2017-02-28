# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Linker для подключения библиотек (полной их подгрузки)
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
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
source('lib/evaluation_dates.R')
source('lib/evaluation_trades.R')
source('lib/evaluation_drawdown.R')
source('lib/evaluation_profit_days.R')
source('lib/evaluation_profit_trades.R')
source('lib/evaluation_profit.R')
source('lib/evaluation_ratio.R')
source('lib/evaluation.R')
