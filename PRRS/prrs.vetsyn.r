library(data.table)
library(vetsyn)

data.path <- "I:/ESS/SVA3D/PRRS/data/"

#### IMPORT DATA ####

load(paste0(data.path, "prrs.retro.timeseries.RData"))

retro.sjv.proportion <-
  retro.sjv.proportion[is.nan(N_prop), N_prop := 1][, N := N_prop * N_tot]

alarms <- data.table(
  wk = seq.int(retro.sjv.proportion[157]$absweek, retro.sva[262]$absweek),
  start_date = seq.Date(retro.sjv.proportion[157]$start_date, retro.sva[262]$start_date, by = "week")
)

#### CREATE SYNDROMIC OBJECTS ####

proportion.counts <- retro.sjv.proportion$N
proportion.normalized <-
  proportion.counts - qcc::stats.np(proportion.counts, retro.sjv.proportion$N_tot)$center
proportion.prop <- retro.sjv.proportion$N_prop

proportion.counts <- syndromicW(
  observed = cbind(proportion.normalized,
                   proportion.prop),
  min.week = 1,
  min.year = 2013,
  max.week = 35,
  max.year = 2018
)

respiratory.counts <- retro.sjv.respiratory$N

respiratory.counts <- syndromicW(
  observed = cbind(respiratory.counts),
  min.week = 1,
  min.year = 2013,
  max.week = 35,
  max.year = 2018
)

reproductive.counts <- retro.sjv.reproductive$N

reproductive.counts <- syndromicW(
  observed = cbind(reproductive.counts),
  min.week = 1,
  min.year = 2013,
  max.week = 35,
  max.year = 2018
)

sva.counts <- retro.sva$N

sva.counts <- syndromicW(
  observed = cbind(sva.counts),
  min.week = 1,
  min.year = 2014,
  max.week = 53,
  max.year = 2018
)

#### PROPORTION DATA SURVEILLANCE ####

proportion.hw <- holt_winters_synd(
  x = proportion.counts,
  syndromes = NULL,
  evaluate.window = 140,
  frequency = 52,
  baseline.window = 104,
  limit.sd = c(2.5, 3, 3.5),
  nahead = 2,
  alpha = .4,
  beta = 0,
  gamma = 0.15,
  seasonal = "additive",
  correct.baseline = 1,
  alarm.dim = 1,
  UCL = 1
)

proportion.ewma <- ewma_synd(
  x = proportion.counts,
  syndromes = NULL,
  evaluate.window = 140,
  frequency = 52,
  baseline.window = 104,
  lambda = 0.2,
  limit.sd = c(2.5, 3, 3.5),
  guard.band = 2,
  correct.baseline = 1,
  alarm.dim = 1,
  UCL = 1,
  LCL = FALSE,
  pre.process = "glm",
  family = "gaussian",
  formula = list(y ~ sin + cos),
)

plot.prop.hw <- plot_syndromic(
  proportion.hw,
  syndromes = 1,
  window = 500,
  algorithms = 1
)

plot.prop.ewma <- plot_syndromic(
  proportion.ewma,
  syndromes = 1,
  window = 500,
  algorithms = 1
)

proportion.hw.normalized <- na.omit(proportion.hw@alarms[, 1, 1]) / 3
proportion.hw.prop <- na.omit(proportion.hw@alarms[, 2, 1]) / 3
proportion.ewma.normalized <- na.omit(proportion.ewma@alarms[, 1, 1]) / 3
proportion.ewma.prop <- na.omit(proportion.ewma@alarms[, 2, 1]) / 3

alarms <-
  alarms[, c(
    "proportion.hw.normalized",
    "proportion.hw.prop",
    "proportion.ewma.normalized",
    "proportion.ewma.prop"
  ) := list(
    c(proportion.hw.normalized,
      rep(
        0,
        nrow(alarms) - length(proportion.hw.normalized)
      )),
    c(proportion.hw.prop,
      rep(
        0,
        nrow(alarms) - length(proportion.hw.prop)
      )),
    c(proportion.ewma.normalized,
      rep(
        0,
        nrow(alarms) - length(proportion.ewma.normalized)
      )),
    c(proportion.ewma.prop,
      rep(
        0,
        nrow(alarms) - length(proportion.ewma.prop)
      ))
  )]

#### RESPIRATORY DATA SURVEILLANCE ####

respiratory.hw <- holt_winters_synd(
  x = respiratory.counts,
  syndromes = NULL,
  evaluate.window = 140,
  frequency = 52,
  baseline.window = 104,
  limit.sd = c(2.5, 3, 3.5),
  nahead = 2,
  alpha = .4,
  beta = 0,
  gamma = 0.15,
  seasonal = "additive",
  correct.baseline = 1,
  alarm.dim = 1,
  UCL = 1
)

respiratory.ewma <- ewma_synd(
  x = respiratory.counts,
  syndromes = NULL,
  evaluate.window = 140,
  frequency = 52,
  baseline.window = 104,
  lambda = 0.2,
  limit.sd = c(2.5, 3, 3.5),
  guard.band = 2,
  correct.baseline = 1,
  alarm.dim = 1,
  UCL = 1,
  LCL = FALSE,
  pre.process = "glm",
  family = "gaussian",
  formula = list(y ~ sin + cos),
)

plot.resp.hw <- plot_syndromic(
  respiratory.hw,
  syndromes = 1,
  window = 500,
  algorithms = 1
)

plot.resp.ewma <- plot_syndromic(
  respiratory.ewma,
  syndromes = 1,
  window = 500,
  algorithms = 1
)

respiratory.hw.alarms <- na.omit(respiratory.hw@alarms[, 1, 1]) / 3
respiratory.ewma.alarms <- na.omit(respiratory.ewma@alarms[, 1, 1]) / 3

alarms[, c("respiratory.hw",
           "respiratory.ewma") := list(c(respiratory.hw.alarms,
                                         rep(
                                           0,
                                           nrow(alarms) - length(respiratory.hw.alarms)
                                         )),
                                       c(respiratory.ewma.alarms,
                                         rep(
                                           0,
                                           nrow(alarms) - length(respiratory.ewma.alarms)
                                         )))]

#### REPRODUCTIVE DATA SURVEILLANCE ####

reproductive.hw <- holt_winters_synd(
  x = reproductive.counts,
  syndromes = NULL,
  evaluate.window = 140,
  frequency = 52,
  baseline.window = 104,
  limit.sd = c(2.5, 3, 3.5),
  nahead = 2,
  alpha = .4,
  beta = 0,
  gamma = 0.15,
  seasonal = "additive",
  correct.baseline = 1,
  alarm.dim = 1,
  UCL = 1
)

reproductive.ewma <- ewma_synd(
  x = reproductive.counts,
  syndromes = NULL,
  evaluate.window = 140,
  frequency = 52,
  baseline.window = 104,
  lambda = 0.2,
  limit.sd = c(2.5, 3, 3.5),
  guard.band = 2,
  correct.baseline = 1,
  alarm.dim = 1,
  UCL = 1,
  LCL = FALSE,
  pre.process = "glm",
  family = "gaussian",
  formula = list(y ~ sin + cos),
)

plot.repr.hw <- plot_syndromic(
  reproductive.hw,
  syndromes = 1,
  window = 500,
  algorithms = 1
)

plot.repr.ewma <- plot_syndromic(
  reproductive.ewma,
  syndromes = 1,
  window = 500,
  algorithms = 1
)

reproductive.hw.alarms <- na.omit(reproductive.hw@alarms[, 1, 1]) / 3
reproductive.ewma.alarms <- na.omit(reproductive.ewma@alarms[, 1, 1]) / 3

alarms[, c("reproductive.hw",
           "reproductive.ewma") := list(c(reproductive.hw.alarms,
                                         rep(
                                           0,
                                           nrow(alarms) - length(reproductive.hw.alarms)
                                         )),
                                       c(reproductive.ewma.alarms,
                                         rep(
                                           0,
                                           nrow(alarms) - length(reproductive.ewma.alarms)
                                         )))]

#### SVA DATA SURVEILLANCE ####

sva.hw <- holt_winters_synd(
  x = sva.counts,
  syndromes = NULL,
  evaluate.window = 140,
  frequency = 52,
  baseline.window = 104,
  limit.sd = c(2.5, 3, 3.5),
  nahead = 2,
  alpha = .4,
  beta = 0,
  gamma = 0.15,
  seasonal = "additive",
  correct.baseline = 1,
  alarm.dim = 1,
  UCL = 1
)

sva.ewma <- ewma_synd(
  x = sva.counts,
  syndromes = NULL,
  evaluate.window = 140,
  frequency = 52,
  baseline.window = 104,
  lambda = 0.2,
  limit.sd = c(2.5, 3, 3.5),
  guard.band = 2,
  correct.baseline = 1,
  alarm.dim = 1,
  UCL = 1,
  LCL = FALSE,
  pre.process = "glm",
  family = "gaussian",
  formula = list(y ~ sin + cos),
)

plot.sva.hw <- plot_syndromic(
  sva.hw,
  syndromes = 1,
  window = 500,
  algorithms = 1
)

plot.sva.ewma <- plot_syndromic(
  sva.ewma,
  syndromes = 1,
  window = 500,
  algorithms = 1
)

sva.hw.alarms <- na.omit(sva.hw@alarms[, 1, 1]) / 3
sva.ewma.alarms <- na.omit(sva.ewma@alarms[, 1, 1]) / 3

alarms[, c("sva.hw",
           "sva.ewma") := list(c(rep(0,
                                     nrow(alarms) - length(sva.hw.alarms)),
                                 sva.hw.alarms),
                               c(rep(0,
                                     nrow(alarms) - length(sva.ewma.alarms)),
                                 sva.ewma.alarms))]





save(alarms,
     file = paste0(data.path, "prrs.vetsyn.alarms.RData"))
