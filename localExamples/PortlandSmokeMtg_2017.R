
library(PWFSLSmoke)

# Washington, Oregon August 22, 2015 ------------------------------------------

pnw <- airnow_load(20150731, 20150901, stateCodes=c('or','wa'))

# Timeseries overview
monitorPlot_timeseries(pnw, style='gnats')

# Oklahoma recent -------------------------------------------------------------

latest <- airnow_loadLatest()
ok <- monitor_subset(latest, stateCodes='OK')
monitorMap(ok)


