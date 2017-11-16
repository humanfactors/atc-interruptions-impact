source("./experiment1/src/exp1-analysis.R")
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

exp1.routinelist.routine.acceptRT.int <- listMaker(exp1.routine.acceptRT.int.t, 1, round(mean(as.matrix(exp1.routine.acceptRT[,2:4])),2), round(sd(as.matrix(exp1.routine.acceptRT[,2:4])),2), exp1.routine.acceptRT.int.bf, varnamer = "Acceptance RT")
exp1.routinelist.routine.acceptRT.none <- listMaker(exp1.routine.acceptRT.none.t, 2, "-", "-", exp1.routine.acceptRT.none.bf, varnamer = "Acceptance RT")

exp1.routinelist.routine.acceptMiss.int <- listMaker(exp1.routine.acceptMiss.int.t, 1, round(mean(as.matrix(exp1.routine.acceptMiss[,2:4])),2), round(sd(as.matrix(exp1.routine.acceptMiss[,2:4])),2), exp1.routine.acceptMiss.int.bf, varnamer = "Acceptance Misses")
exp1.routinelist.routine.acceptMiss.none <- listMaker(exp1.routine.acceptMiss.none.t, 2, "-", "-", exp1.routine.acceptMiss.none.bf, varnamer = "Acceptance Misses")

exp1.routinelist.routine.handoffRT.int <- listMaker(exp1.routine.handoffRT.int.t, 1, round(mean(as.matrix(exp1.routine.handoffRT[,2:4])),2), round(sd(as.matrix(exp1.routine.handoffRT[,2:4])),2), exp1.routine.handoffRT.int.bf, varnamer = "Handoff RT")
exp1.routinelist.routine.handoffRT.none <- listMaker(exp1.routine.handoffRT.none.t, 2, "-", "-", exp1.routine.handoffRT.none.bf, varnamer = "Handoff RT")

exp1.routinelist.routine.handoffMiss.int <- listMaker(exp1.routine.handoffMiss.int.t, 1, round(mean(as.matrix(exp1.routine.handoffMiss[,2:4])),2), round(sd(as.matrix(exp1.routine.handoffMiss[,2:4])),2), exp1.routine.handoffMiss.int.bf, varnamer = "Handoff Misses")
exp1.routinelist.routine.handoffMiss.none <- listMaker(exp1.routine.handoffMiss.none.t, 2, "-", "-", exp1.routine.handoffMiss.none.bf, varnamer = "Handoff Misses")

# exp1.routinelist.routine.falseAlarms.int <- listMaker(exp1.routine.falseAlarms.int.t, exp1.routine.falseAlarms.int.bf, varnamer = "Conflict FA Int")
# exp1.routinelist.routine.falseAlarms.none <- listMaker(exp1.routine.falseAlarms.none.t, exp1.routine.falseAlarms.none.bf, varnamer = "Conflict FA None")

exp1.routinelist.routine.cdt.int <- listMaker(exp1.routine.cdt.int.t, 1, round(mean(as.matrix(exp1.routine.cdt[,2:4])),2), round(sd(as.matrix(exp1.routine.cdt[,2:4])),2), exp1.routine.cdt.int.bf, varnamer = "CDT")
exp1.routinelist.routine.cdt.none <- listMaker(exp1.routine.cdt.none.t, 2, "-", "-", exp1.routine.cdt.none.bf, varnamer = "CDT")

exp1.routinelist.routine.conflictMisses.int <- listMaker(exp1.routine.conflictMisses.int.t, 1, round(mean(as.matrix(exp1.routine.conflictMisses[,2:4])),2), round(sd(as.matrix(exp1.routine.conflictMisses[,2:4])),2), exp1.routine.conflictMisses.int.bf, varnamer = "Conflict Misses")
exp1.routinelist.routine.conflictMisses.none <- listMaker(exp1.routine.conflictMisses.none.t, 2, "-", "-", exp1.routine.conflictMisses.none.bf, varnamer = "Conflict Misses")

# exp1.routinelist.workload.int <- listMaker(exp1.workload.int.t, exp1.workload.int.bf, varnamer = "Workload Int")
# exp1.routinelist.workload.none <- listMaker(exp1.workload.none.t, exp1.workload.none.bf, varnamer = "Workload None")



exp1.routinetable <- rbind.data.frame(exp1.routinelist.routine.acceptRT.int,
                                      exp1.routinelist.routine.acceptRT.none,
                                      exp1.routinelist.routine.acceptMiss.int,
                                      exp1.routinelist.routine.acceptMiss.none,
                                      exp1.routinelist.routine.handoffRT.int,
                                      exp1.routinelist.routine.handoffRT.none,
                                      exp1.routinelist.routine.handoffMiss.int,
                                      exp1.routinelist.routine.handoffMiss.none,
                                      exp1.routinelist.routine.cdt.int,
                                      exp1.routinelist.routine.cdt.none,
                                      exp1.routinelist.routine.conflictMisses.int,
                                      exp1.routinelist.routine.conflictMisses.none, make.row.names = TRUE)

colnames(exp1.routinetable) <- c("var","contrast","mean","sd","t","p",'BF')
# pandertable <- pander(exp1.routinetable)
