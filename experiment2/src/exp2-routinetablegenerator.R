source("./experiment2/src/exp2-analysis.R")
library(xtable)
#-------------------------------------------------#
# ----------------- T A B L E  ------------------ #
#-------------------------------------------------#

listMaker <- function(inputdata.t, contrast, mean, sd, inputdata.bf,outputlistname, varnamer) {
  outputlistname <- c("varname" = varnamer,
                      "contrast" = contrast,
                      "mean" = mean,
                      "sd" = sd,
                      "t" = round(inputdata.t$statistic[[1]], 2),
                      "p" = sub("0.", ".",format.pval(pv = inputdata.t$p.value, digits = 2, eps = 0.001, nsmall = 2)),
                      "BF" = round(1/exp(inputdata.bf@bayesFactor$bf),2))
}

exp2.routinelist.routine.acceptRT.int <- listMaker(exp2.routine.acceptRT.int.t, 1, round(mean(as.matrix(exp2.routine.acceptRT[,2:4])),2), round(sd(as.matrix(exp2.routine.acceptRT[,2:4])),2), exp2.routine.acceptRT.int.bf, varnamer = "Acceptance RT")
exp2.routinelist.routine.acceptRT.none <- listMaker(exp2.routine.acceptRT.none.t, 2, "-", "-", exp2.routine.acceptRT.none.bf, varnamer = "Acceptance RT")

exp2.routinelist.routine.acceptMiss.int <- listMaker(exp2.routine.acceptMiss.int.t, 1, round(mean(as.matrix(exp2.routine.acceptMiss[,2:4])),2), round(sd(as.matrix(exp2.routine.acceptMiss[,2:4])),2), exp2.routine.acceptMiss.int.bf, varnamer = "Acceptance Misses")
exp2.routinelist.routine.acceptMiss.none <- listMaker(exp2.routine.acceptMiss.none.t, 2, "-", "-", exp2.routine.acceptMiss.none.bf, varnamer = "Acceptance Misses")

exp2.routinelist.routine.handoffRT.int <- listMaker(exp2.routine.handoffRT.int.t, 1, round(mean(as.matrix(exp2.routine.handoffRT[,2:4])),2), round(sd(as.matrix(exp2.routine.handoffRT[,2:4])),2), exp2.routine.handoffRT.int.bf, varnamer = "Handoff RT")
exp2.routinelist.routine.handoffRT.none <- listMaker(exp2.routine.handoffRT.none.t, 2, "-", "-", exp2.routine.handoffRT.none.bf, varnamer = "Handoff RT")

exp2.routinelist.routine.handoffMiss.int <- listMaker(exp2.routine.handoffMiss.int.t, 1, round(mean(as.matrix(exp2.routine.handoffMiss[,2:4])),2), round(sd(as.matrix(exp2.routine.handoffMiss[,2:4])),2), exp2.routine.handoffMiss.int.bf, varnamer = "Handoff Misses")
exp2.routinelist.routine.handoffMiss.none <- listMaker(exp2.routine.handoffMiss.none.t, 2, "-", "-", exp2.routine.handoffMiss.none.bf, varnamer = "Handoff Misses")

# exp2.routinelist.routine.falseAlarms.int <- listMaker(exp2.routine.falseAlarms.int.t, exp2.routine.falseAlarms.int.bf, varnamer = "Conflict FA Int")
# exp2.routinelist.routine.falseAlarms.none <- listMaker(exp2.routine.falseAlarms.none.t, exp2.routine.falseAlarms.none.bf, varnamer = "Conflict FA None")

exp2.routinelist.routine.cdt.int <- listMaker(exp2.routine.cdt.int.t, 1, round(mean(as.matrix(exp2.routine.cdt[,2:4])),2), round(sd(as.matrix(exp2.routine.cdt[,2:4])),2), exp2.routine.cdt.int.bf, varnamer = "CDT")
exp2.routinelist.routine.cdt.none <- listMaker(exp2.routine.cdt.none.t, 2, "-", "-", exp2.routine.cdt.none.bf, varnamer = "CDT")

exp2.routinelist.routine.conflictMisses.int <- listMaker(exp2.routine.conflictMisses.int.t, 1, round(mean(as.matrix(exp2.routine.conflictMisses[,2:4])),2), round(sd(as.matrix(exp2.routine.conflictMisses[,2:4])),2), exp2.routine.conflictMisses.int.bf, varnamer = "Conflict Misses")
exp2.routinelist.routine.conflictMisses.none <- listMaker(exp2.routine.conflictMisses.none.t, 2, "-", "-", exp2.routine.conflictMisses.none.bf, varnamer = "Conflict Misses")

# exp2.routinelist.workload.int <- listMaker(exp2.workload.int.t, exp2.workload.int.bf, varnamer = "Workload Int")
# exp2.routinelist.workload.none <- listMaker(exp2.workload.none.t, exp2.workload.none.bf, varnamer = "Workload None")



exp2.routinetable <- rbind.data.frame(exp2.routinelist.routine.acceptRT.int,
                                      exp2.routinelist.routine.acceptRT.none,
                                      exp2.routinelist.routine.acceptMiss.int,
                                      exp2.routinelist.routine.acceptMiss.none,
                                      exp2.routinelist.routine.handoffRT.int,
                                      exp2.routinelist.routine.handoffRT.none,
                                      exp2.routinelist.routine.handoffMiss.int,
                                      exp2.routinelist.routine.handoffMiss.none,
                                      exp2.routinelist.routine.cdt.int,
                                      exp2.routinelist.routine.cdt.none,
                                      exp2.routinelist.routine.conflictMisses.int,
                                      exp2.routinelist.routine.conflictMisses.none,
                                      make.row.names = FALSE)

colnames(exp2.routinetable) <- c("var","contrast","mean","sd","t","p",'BF')
print(xtable(exp2.routinetable))
