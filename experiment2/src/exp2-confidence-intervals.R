source("./experiment2/src/exp2-analysis.R")

# >>>>>>>>>>>>>>>>>>
# CONFIDENCE INTERVALS
# >>>>>>>>>>>>>>>>>>

# // Resumption Time
exp2.wsci.restime.atc <- 1.96*(describe(
  exp2.resumptionTime[,2] -
    rowMeans(exp2.resumptionTime[,2:4]) +
    (mean(c(exp2.resumptionTime$ATC,exp2.resumptionTime$Blank,exp2.resumptionTime$None)))
)$se
)

exp2.wsci.restime.blank <- 1.96*(describe(
  exp2.resumptionTime[,3] -
    rowMeans(exp2.resumptionTime[,2:4]) +
    (mean(c(exp2.resumptionTime$Blank,exp2.resumptionTime$Blank,exp2.resumptionTime$None)))
)$se
)

exp2.wsci.restime.none <- 1.96*(describe(
  exp2.resumptionTime[,4] -
    rowMeans(exp2.resumptionTime[,2:4]) +
    (mean(c(exp2.resumptionTime$None,exp2.resumptionTime$Blank,exp2.resumptionTime$None)))
)$se
)

# // Resumption Failures
exp2.wsci.resfail.atc <- 1.96*(describe(
  exp2.resumptionMisses[,2] -
    rowMeans(exp2.resumptionMisses[,2:4]) +
    (mean(c(exp2.resumptionMisses$ATC,exp2.resumptionMisses$Blank,exp2.resumptionMisses$None)))
)$se
)

exp2.wsci.resfail.blank <- 1.96*(describe(
  exp2.resumptionMisses[,3] -
    rowMeans(exp2.resumptionMisses[,2:4]) +
    (mean(c(exp2.resumptionMisses$Blank,exp2.resumptionMisses$Blank,exp2.resumptionMisses$None)))
)$se
)

exp2.wsci.resfail.none <- 1.96*(describe(
  exp2.resumptionMisses[,4] -
    rowMeans(exp2.resumptionMisses[,2:4]) +
    (mean(c(exp2.resumptionMisses$None,exp2.resumptionMisses$Blank,exp2.resumptionMisses$None)))
)$se
)

# // PM Handoff Misses
exp2.wsci.defhomiss.atc <- 1.96*(describe(
  exp2.deferredHandoffMiss[,2] -
    rowMeans(exp2.deferredHandoffMiss[,2:4]) +
    (mean(c(exp2.deferredHandoffMiss$ATC,exp2.deferredHandoffMiss$Blank,exp2.deferredHandoffMiss$None)))
)$se
)

exp2.wsci.defhomiss.blank <- 1.96*(describe(
  exp2.deferredHandoffMiss[,3] -
    rowMeans(exp2.deferredHandoffMiss[,2:4]) +
    (mean(c(exp2.deferredHandoffMiss$Blank,exp2.deferredHandoffMiss$Blank,exp2.deferredHandoffMiss$None)))
)$se
)

exp2.wsci.defhomiss.none <- 1.96*(describe(
  exp2.deferredHandoffMiss[,4] -
    rowMeans(exp2.deferredHandoffMiss[,2:4]) +
    (mean(c(exp2.deferredHandoffMiss$None,exp2.deferredHandoffMiss$Blank,exp2.deferredHandoffMiss$None)))
)$se
)

# // PM Handoff Time
exp2.wsci.defhotime.atc <- 1.96*(describe(
  exp2.deferredHandoffTime[,2] -
    rowMeans(exp2.deferredHandoffTime[,2:4]) +
    (mean(c(exp2.deferredHandoffTime$ATC,exp2.deferredHandoffTime$Blank,exp2.deferredHandoffTime$None)))
)$se
)

exp2.wsci.defhotime.blank <- 1.96*(describe(
  exp2.deferredHandoffTime[,3] -
    rowMeans(exp2.deferredHandoffTime[,2:4]) +
    (mean(c(exp2.deferredHandoffTime$Blank,exp2.deferredHandoffTime$Blank,exp2.deferredHandoffTime$None)))
)$se
)

exp2.wsci.defhotime.none <- 1.96*(describe(
  exp2.deferredHandoffTime[,4] -
    rowMeans(exp2.deferredHandoffTime[,2:4]) +
    (mean(c(exp2.deferredHandoffTime$None,exp2.deferredHandoffTime$Blank,exp2.deferredHandoffTime$None)))
)$se
)
