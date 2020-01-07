library(data.table)
library(plotly)

data.path <- "I:/ESS/SVA3D/PRRS/data/"

load(paste0(data.path,
            "prrs.surveillance.alarms.RData"))
all.alarms <- copy(alarms)
load(paste0(data.path,
            "prrs.vetsyn.alarms.RData"))
all.alarms <- merge(all.alarms,
                    alarms,
                    by = c("wk",
                           "start_date"))
rm(alarms)


