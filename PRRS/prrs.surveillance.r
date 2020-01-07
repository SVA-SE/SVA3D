library(data.table)
library(surveillance)
library(vetsyn)

data.path <- "I:/ESS/SVA3D/PRRS/data/"

#### IMPORT/CREATE DATA ####

load(paste0(data.path, "prrs.retro.timeseries.RData"))

retro.sjv.proportion <-
  retro.sjv.proportion[is.nan(N_prop), N_prop := 1][, N := N_prop * N_tot]

alarms <- data.table(
  wk = seq.int(retro.sjv.proportion[157]$absweek, retro.sva[262]$absweek),
  start_date = seq.Date(retro.sjv.proportion[157]$start_date, retro.sva[262]$start_date, by = "week")
)

#### CREATE STS OBJECTS & DISPROGOBJS ####

proportion.sts <-
  sts(
    observed = retro.sjv.proportion$N,
    start = c(2013, 1),
    frequency = 52,
    population = matrix(retro.sjv.proportion$N_prop)
  )

proportion.disprog <- sts2disProg(proportion.sts)

reproductive.sts <-
  sts(observed = retro.sjv.reproductive$N,
      start = c(2013, 1),
      frequency = 52)

reproductive.disprog <- sts2disProg(reproductive.sts)

respiratory.sts <-
  sts(observed = retro.sjv.respiratory$N,
      start = c(2013, 1),
      frequency = 52)

respiratory.disprog <- sts2disProg(respiratory.sts)

sva.sts <-
  sts(observed = retro.sva$N,
      start = c(2014, 1),
      frequency = 52)

sva.disprog <- sts2disProg(sva.sts)

#### PROPORTION DATA SURVEILLANCE ####

proportion.farrington <- algo.farrington(
  proportion.disprog,
  control = list(
    range = 157:296,
    b = 2,
    w = 4,
    reweight = TRUE,
    verbose = FALSE,
    plot = FALSE,
    alpha = 0.05,
    trend = TRUE,
    fitFun = "algo.farrington.fitGLM.populationOffset",
    population = retro.sjv.proportion$N_prop
  )
)

alarms <-
  alarms[, proportion.farrington := c(proportion.farrington$alarm,
                                      rep.int(0,
                                              nrow(alarms) - length(proportion.farrington$alarm)))]

proportion.improvedFarrington <- farringtonFlexible(
  proportion.sts,
  control = list(
    range = 157:296,
    b = 2,
    w = 4,
    reweight = TRUE,
    weightsThreshold = 2.58,
    verbose = FALSE,
    alpha = 0.05,
    trend = TRUE,
    pThresholdTrend = 1,
    limit54 = c(5, 4),
    fitFun = "algo.farrington.fitGLM.flexible",
    populationOffset = TRUE,
    noPeriods = 1,
    thresholdMethod = "muan"
  )
)

alarms <-
  alarms[, proportion.improvedFarrington := c(proportion.improvedFarrington@alarm,
                                              rep.int(0,
                                                      nrow(alarms) - length(proportion.improvedFarrington@alarm)))]

plot.prop.farr <- plot(
  proportion.farrington,
  startyear = 2016,
  ylab = "positive samples",
  legend.opts = list(
    x = "topleft",
    legend = c("Positive samples", "Upper Limit", "Alarm", "Outbreak")
  )
)

plot.prop.farrImp <- plot(
  proportion.improvedFarrington,
  ylab = "positive samples",
  legend.opts = list(
    x = "topleft",
    legend = c("Positive samples", "Upper Limit", "Alarm", "Outbreak")
  )
)

#### RESPIRATORY DATA SURVEILLANCE ####

respiratory.farrington <- algo.farrington(
  respiratory.disprog,
  control = list(
    range = 157:296,
    b = 2,
    w = 4,
    reweight = TRUE,
    verbose = FALSE,
    plot = FALSE,
    alpha = 0.05,
    trend = TRUE,
    fitFun = "algo.farrington.fitGLM.fast"
  )
)

alarms <-
  alarms[, respiratory.farrington := c(respiratory.farrington$alarm,
                                       rep.int(0,
                                               nrow(alarms) - length(respiratory.farrington$alarm)))]

respiratory.improvedFarrington <- farringtonFlexible(
  respiratory.sts,
  control = list(
    range = 157:296,
    b = 2,
    w = 4,
    reweight = TRUE,
    weightsThreshold = 2.58,
    verbose = FALSE,
    alpha = 0.05,
    trend = TRUE,
    pThresholdTrend = 1,
    limit54 = c(5, 4),
    fitFun = "algo.farrington.fitGLM.flexible",
    populationOffset = FALSE,
    noPeriods = 1,
    thresholdMethod = "muan"
  )
)

alarms <-
  alarms[, respiratory.improvedFarrington := c(respiratory.improvedFarrington@alarm,
                                               rep.int(
                                                 0,
                                                 nrow(alarms) - length(respiratory.improvedFarrington@alarm)
                                               ))]

plot.resp.farr <- plot(
  respiratory.farrington,
  startyear = 2016,
  ylab = "positive samples",
  legend.opts = list(
    x = "topleft",
    legend = c("Positive samples", "Upper Limit", "Alarm", "Outbreak")
  )
)

plot.resp.farrImp <- plot(
  respiratory.improvedFarrington,
  ylab = "positive samples",
  legend.opts = list(
    x = "topleft",
    legend = c("Positive samples", "Upper Limit", "Alarm", "Outbreak")
  )
)


#### REPRODUCTIVE DATA SURVEILLANCE ####

reproductive.farrington <- algo.farrington(
  reproductive.disprog,
  control = list(
    range = 157:296,
    b = 2,
    w = 4,
    reweight = TRUE,
    verbose = FALSE,
    plot = FALSE,
    alpha = 0.05,
    trend = TRUE,
    fitFun = "algo.farrington.fitGLM.fast"
  )
)

alarms <-
  alarms[, reproductive.farrington := c(reproductive.farrington$alarm,
                                        rep.int(0,
                                                nrow(alarms) - length(reproductive.farrington$alarm)))]

reproductive.improvedFarrington <- farringtonFlexible(
  reproductive.sts,
  control = list(
    range = 157:296,
    b = 2,
    w = 4,
    reweight = TRUE,
    weightsThreshold = 2.58,
    verbose = FALSE,
    alpha = 0.05,
    trend = TRUE,
    pThresholdTrend = 1,
    limit54 = c(5, 4),
    fitFun = "algo.farrington.fitGLM.flexible",
    populationOffset = FALSE,
    noPeriods = 1,
    thresholdMethod = "muan"
  )
)

alarms <-
  alarms[, reproductive.improvedFarrington := c(reproductive.improvedFarrington@alarm,
                                                rep.int(
                                                  0,
                                                  nrow(alarms) - length(reproductive.improvedFarrington@alarm)
                                                ))]

plot.repr.farr <- plot(
  reproductive.farrington,
  startyear = 2016,
  ylab = "positive samples",
  legend.opts = list(
    x = "topleft",
    legend = c("Positive samples", "Upper Limit", "Alarm", "Outbreak")
  )
)

plot.repr.farrImp <- plot(
  reproductive.improvedFarrington,
  ylab = "positive samples",
  legend.opts = list(
    x = "topleft",
    legend = c("Positive samples", "Upper Limit", "Alarm", "Outbreak")
  )
)

#### SVA DATA SURVEILLANCE ####

sva.farrington <- algo.farrington(
  sva.disprog,
  control = list(
    range = 157:262,
    b = 2,
    w = 4,
    reweight = TRUE,
    verbose = FALSE,
    plot = FALSE,
    alpha = 0.05,
    trend = TRUE,
    fitFun = "algo.farrington.fitGLM.fast"
  )
)

alarms <-
  alarms[, sva.farrington := c(rep.int(0,
                                       nrow(alarms) - length(sva.farrington$alarm)),
                               sva.farrington$alarm)]

sva.improvedFarrington <- farringtonFlexible(
  sva.sts,
  control = list(
    range = 157:262,
    b = 2,
    w = 4,
    reweight = TRUE,
    weightsThreshold = 2.58,
    verbose = FALSE,
    alpha = 0.05,
    trend = TRUE,
    pThresholdTrend = 1,
    limit54 = c(5, 4),
    fitFun = "algo.farrington.fitGLM.flexible",
    populationOffset = FALSE,
    noPeriods = 1,
    thresholdMethod = "muan"
  )
)

alarms <-
  alarms[, sva.improvedFarrington := c(rep.int(0,
                                               nrow(alarms) - length(sva.improvedFarrington@alarm)),
                                       sva.improvedFarrington@alarm)]

plot.sva.farr <- plot(
  sva.farrington,
  startyear = 2016,
  ylab = "positive samples",
  legend.opts = list(
    x = "topleft",
    legend = c("Positive samples", "Upper Limit", "Alarm", "Outbreak")
  )
)

plot.sva.farrImp <- plot(
  sva.improvedFarrington,
  ylab = "positive samples",
  legend.opts = list(
    x = "topleft",
    legend = c("Positive samples", "Upper Limit", "Alarm", "Outbreak")
  )
)

save(alarms,
     file = paste0(data.path,
                   "prrs.surveillance.alarms.RData"))
