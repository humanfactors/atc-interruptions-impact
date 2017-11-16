source("./experiment1/src/exp1-analysis.R")

# >>>>>>>>>>>>>>>>>>
# CONFIDENCE INTERVALS
# >>>>>>>>>>>>>>>>>>

# // Resumption Time
exp1.wsci.restime.Nback <- 1.96*(describe(
  exp1.resumptionTime[,2] -
    rowMeans(exp1.resumptionTime[,2:4]) +
    (mean(c(exp1.resumptionTime$Nback,exp1.resumptionTime$Blank,exp1.resumptionTime$None)))
)$se
)

exp1.wsci.restime.blank <- 1.96*(describe(
  exp1.resumptionTime[,3] -
    rowMeans(exp1.resumptionTime[,2:4]) +
    (mean(c(exp1.resumptionTime$Blank,exp1.resumptionTime$Blank,exp1.resumptionTime$None)))
)$se
)

exp1.wsci.restime.none <- 1.96*(describe(
  exp1.resumptionTime[,4] -
    rowMeans(exp1.resumptionTime[,2:4]) +
    (mean(c(exp1.resumptionTime$None,exp1.resumptionTime$Blank,exp1.resumptionTime$None)))
)$se
)

# // Resumption Failures
exp1.wsci.resfail.Nback <- 1.96*(describe(
  exp1.resumptionMisses[,2] -
    rowMeans(exp1.resumptionMisses[,2:4]) +
    (mean(c(exp1.resumptionMisses$Nback,exp1.resumptionMisses$Blank,exp1.resumptionMisses$None)))
)$se
)

exp1.wsci.resfail.blank <- 1.96*(describe(
  exp1.resumptionMisses[,3] -
    rowMeans(exp1.resumptionMisses[,2:4]) +
    (mean(c(exp1.resumptionMisses$Blank,exp1.resumptionMisses$Blank,exp1.resumptionMisses$None)))
)$se
)

exp1.wsci.resfail.none <- 1.96*(describe(
  exp1.resumptionMisses[,4] -
    rowMeans(exp1.resumptionMisses[,2:4]) +
    (mean(c(exp1.resumptionMisses$None,exp1.resumptionMisses$Blank,exp1.resumptionMisses$None)))
)$se
)

# // PM Handoff Misses
exp1.wsci.defhomiss.Nback <- 1.96*(describe(
  exp1.deferredHandoffMiss[,2] -
    rowMeans(exp1.deferredHandoffMiss[,2:4]) +
    (mean(c(exp1.deferredHandoffMiss$Nback,exp1.deferredHandoffMiss$Blank,exp1.deferredHandoffMiss$None)))
)$se
)

exp1.wsci.defhomiss.blank <- 1.96*(describe(
  exp1.deferredHandoffMiss[,3] -
    rowMeans(exp1.deferredHandoffMiss[,2:4]) +
    (mean(c(exp1.deferredHandoffMiss$Blank,exp1.deferredHandoffMiss$Blank,exp1.deferredHandoffMiss$None)))
)$se
)

exp1.wsci.defhomiss.none <- 1.96*(describe(
  exp1.deferredHandoffMiss[,4] -
    rowMeans(exp1.deferredHandoffMiss[,2:4]) +
    (mean(c(exp1.deferredHandoffMiss$None,exp1.deferredHandoffMiss$Blank,exp1.deferredHandoffMiss$None)))
)$se
)

# // PM Handoff Time
exp1.wsci.defhotime.Nback <- 1.96*(describe(
  exp1.deferredHandoffTime[,2] -
    rowMeans(exp1.deferredHandoffTime[,2:4]) +
    (mean(c(exp1.deferredHandoffTime$Nback,exp1.deferredHandoffTime$Blank,exp1.deferredHandoffTime$None)))
)$se
)

exp1.wsci.defhotime.blank <- 1.96*(describe(
  exp1.deferredHandoffTime[,3] -
    rowMeans(exp1.deferredHandoffTime[,2:4]) +
    (mean(c(exp1.deferredHandoffTime$Blank,exp1.deferredHandoffTime$Blank,exp1.deferredHandoffTime$None)))
)$se
)

exp1.wsci.defhotime.none <- 1.96*(describe(
  exp1.deferredHandoffTime[,4] -
    rowMeans(exp1.deferredHandoffTime[,2:4]) +
    (mean(c(exp1.deferredHandoffTime$None,exp1.deferredHandoffTime$Blank,exp1.deferredHandoffTime$None)))
)$se
)
