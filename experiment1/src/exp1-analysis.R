# ---
# title: "Experiment 1 Hypothesis Testing"
# author: "Michael Wilson"
# date: "2016"
# description: "Conduct hypothesis testing for R."
# key vars: variable.title.t, variable.title.bf, variable.title.d
# ---
library(reshape2)
library(plyr)
library(psych)
library(BayesFactor)
library(lsr)

# Contrast test functions
source("./experiment1/src/contrast-functions.R")

#-------------------------------------------------#
# -------------- D A T A  P R E P --------------- #
#-------------------------------------------------#

exp1.testing.df <- read.csv("./experiment1/data/exp1-main-hypothesis-testing.csv",header = TRUE)
exp1.testing.df.colindex <- as.data.frame(colnames(exp1.testing.df))

exp1.pm.testing.df <- read.csv("./experiment1/data/exp1-main-pm.csv",header = TRUE)
exp1.pm.testing.df.colindex <- as.data.frame(colnames(exp1.pm.testing.df))

# Exclude participant from PM sheet which did not perform any deferred handoff correctly
# This participant is already removed from exp1.testing.df
exp1.pm.testing.df <- exp1.pm.testing.df[ which(rowSums(exp1.pm.testing.df[,32:34]) > 1),]

# Arranging data to correspond with hypothesis tests
exp1.resumptionTime <- exp1.testing.df[, c("Participant", "Nback_MCorrect_ResT", "Blank_MCorrect_ResT", "None_MCorrect_ResT")]
exp1.resumptionTime <- rename(exp1.resumptionTime, c("None_MCorrect_ResT"="None", "Blank_MCorrect_ResT"="Blank","Nback_MCorrect_ResT"="Nback"))
exp1.resumptionTime$ints <- (exp1.resumptionTime$Blank + exp1.resumptionTime$Nback)/2

exp1.resumptionMisses <- exp1.testing.df[, c("Participant", "NbackM_ResTimeMissProp", "BlankM_ResTimeMissProp", "NoneM_ResTimeMissProp")]
exp1.resumptionMisses <- rename(exp1.resumptionMisses,c("NoneM_ResTimeMissProp"="None", "BlankM_ResTimeMissProp"="Blank","NbackM_ResTimeMissProp"="Nback"))
exp1.resumptionMisses$ints <- (exp1.resumptionMisses$Blank + exp1.resumptionMisses$Nback)/2

# Deferred handoff is seperate to exclude procedural response errors
exp1.pmMissNames <- c("NbackM_PMMissProp", "BlankM_PMMissProp", "NoneM_PMMissProp")
exp1.pm.testing.df[,exp1.pmMissNames[1]] <- 1-(rowSums(exp1.pm.testing.df[,17:21])/(5-exp1.pm.testing.df[,68]))
exp1.pm.testing.df[,exp1.pmMissNames[2]] <- 1-(rowSums(exp1.pm.testing.df[,22:26])/(5-exp1.pm.testing.df[,69]))
exp1.pm.testing.df[,exp1.pmMissNames[3]] <- 1-(rowSums(exp1.pm.testing.df[,27:31])/(5-exp1.pm.testing.df[,70]))

exp1.deferredHandoffMiss <- exp1.pm.testing.df[, c("Participant",exp1.pmMissNames[1],exp1.pmMissNames[2],exp1.pmMissNames[3])]
exp1.deferredHandoffMiss <- rename(exp1.deferredHandoffMiss,c("NoneM_PMMissProp"="None", "BlankM_PMMissProp"="Blank","NbackM_PMMissProp"="Nback"))
exp1.deferredHandoffMiss$ints <- (exp1.deferredHandoffMiss$Blank + exp1.deferredHandoffMiss$Nback)/2

exp1.deferredHandoffTime <- exp1.pm.testing.df[, c("Participant", "NbackM_PM_reaction", "BlankM_PM_reaction", "NoneM_PM_reaction")]
exp1.deferredHandoffTime <- rename(exp1.deferredHandoffTime,c("NoneM_PM_reaction"="None", "BlankM_PM_reaction"="Blank","NbackM_PM_reaction"="Nback"))
exp1.deferredHandoffTime <- na.omit(exp1.deferredHandoffTime)
exp1.deferredHandoffTime$ints <- (exp1.deferredHandoffTime$Blank + exp1.deferredHandoffTime$Nback)/2


# Routine analyses

exp1.routine.acceptRT <- exp1.testing.df[, c("Participant", "NbackM_PostInt_Accept_ReactionTime", "BlankM_PostInt_Accept_ReactionTime", "NoneM_PostInt_Accept_ReactionTime")]
exp1.routine.acceptRT <- rename(exp1.routine.acceptRT,c("NoneM_PostInt_Accept_ReactionTime"="None", "BlankM_PostInt_Accept_ReactionTime"="Blank","NbackM_PostInt_Accept_ReactionTime"="Nback"))
exp1.routine.acceptRT$ints <- (exp1.routine.acceptRT$Blank + exp1.routine.acceptRT$Nback)/2


exp1.routine.acceptMiss <- exp1.testing.df[, c("Participant", "NbackM_PostInt_Accept_Misses", "BlankM_PostInt_Accept_Misses", "NoneM_PostInt_Accept_Misses")]
exp1.routine.acceptMiss <- rename(exp1.routine.acceptMiss,c("NoneM_PostInt_Accept_Misses"="None", "BlankM_PostInt_Accept_Misses"="Blank","NbackM_PostInt_Accept_Misses"="Nback"))
exp1.routine.acceptMiss$ints <- (exp1.routine.acceptMiss$Blank + exp1.routine.acceptMiss$Nback)/2

exp1.routine.handoffRT <- exp1.testing.df[, c("Participant", "NbackM_PostInt_Handoff_ReactionTime", "BlankM_PostInt_Handoff_ReactionTime", "NoneM_PostInt_Handoff_ReactionTime")]
exp1.routine.handoffRT <- rename(exp1.routine.handoffRT,c("NoneM_PostInt_Handoff_ReactionTime"="None", "BlankM_PostInt_Handoff_ReactionTime"="Blank","NbackM_PostInt_Handoff_ReactionTime"="Nback"))
exp1.routine.handoffRT$ints <- (exp1.routine.handoffRT$Blank + exp1.routine.handoffRT$Nback)/2


exp1.routine.handoffMiss <- exp1.testing.df[, c("Participant", "NbackM_PostInt_Handoff_Misses", "BlankM_PostInt_Handoff_Misses", "NoneM_PostInt_Handoff_Misses")]
exp1.routine.handoffMiss <- rename(exp1.routine.handoffMiss,c("NoneM_PostInt_Handoff_Misses"="None", "BlankM_PostInt_Handoff_Misses"="Blank","NbackM_PostInt_Handoff_Misses"="Nback"))
exp1.routine.handoffMiss$ints <- (exp1.routine.handoffMiss$Blank + exp1.routine.handoffMiss$Nback)/2

exp1.routine.falseAlarms <- exp1.testing.df[, c("Participant", "Nback_ConfFA", "Blank_ConfFA", "None_ConfFA")]
exp1.routine.falseAlarms <- rename(exp1.testing.df,c("None_ConfFA"="None", "Blank_ConfFA"="Blank","Nback_ConfFA"="Nback"))
exp1.routine.falseAlarms$ints <- (exp1.routine.falseAlarms$Blank + exp1.routine.falseAlarms$Nback)/2


exp1.routine.cdt <- exp1.testing.df[, c("Participant", "NbackM_PostInt_CDT", "BlankM_PostInt_CDT", "NoneM_PostInt_CDT")]
exp1.routine.cdt <- rename(exp1.routine.cdt,c("NoneM_PostInt_CDT"="None", "BlankM_PostInt_CDT"="Blank","NbackM_PostInt_CDT"="Nback"))
exp1.routine.cdt$ints <- (exp1.routine.cdt$Blank + exp1.routine.cdt$Nback)/2


exp1.routine.conflictMisses <- exp1.testing.df[, c("Participant", "NbackM_PostInt_ConfMisses", "BlankM_PostInt_ConfMisses", "NoneM_PostInt_ConfMisses")]
exp1.routine.conflictMisses <- rename(exp1.routine.conflictMisses,c("NoneM_PostInt_ConfMisses"="None", "BlankM_PostInt_ConfMisses"="Blank","NbackM_PostInt_ConfMisses"="Nback"))
exp1.routine.conflictMisses$ints <- (exp1.routine.conflictMisses$Blank + exp1.routine.conflictMisses$Nback)/2

# Workload analyses

exp1.workload <- exp1.testing.df[, c("Participant", "NoneM_Workload1", "BlankM_Workload1", "NbackM_Workload1","NoneM_Workload2", "BlankM_Workload2", "NbackM_Workload2")]
exp1.workload <- rename(exp1.workload,c("NoneM_Workload1"="None1", "BlankM_Workload1"="Blank1","NbackM_Workload1"="Nback1", "NoneM_Workload2"="None2", "BlankM_Workload2"="Blank2","NbackM_Workload2"="Nback2"))
exp1.workload$Nback <- (exp1.workload$Nback1 + exp1.workload$Nback2)/2
exp1.workload$Blank <- (exp1.workload$Blank1 + exp1.workload$Blank2)/2
exp1.workload$None <- (exp1.workload$None1 + exp1.workload$None2)/2
exp1.workload$ints <- (exp1.workload$Nback + exp1.workload$Blank)/2


#-------------------------------------------------#
# -------------- A N A L Y S I S  --------------- #
#-------------------------------------------------#

hypothesis.test.interruption(exp1.resumptionTime, "exp1.resumptionTime.int")
hypothesis.test.interruption(exp1.resumptionMisses, "exp1.resumptionMisses.int")
hypothesis.test.interruption(exp1.deferredHandoffTime, "exp1.deferredHandoffTime.int")
hypothesis.test.interruption(exp1.deferredHandoffMiss, "exp1.deferredHandoffMiss.int")
hypothesis.test.interruption(exp1.routine.acceptRT, "exp1.routine.acceptRT.int")
hypothesis.test.interruption(exp1.routine.acceptMiss, "exp1.routine.acceptMiss.int")
hypothesis.test.interruption(exp1.routine.handoffRT, "exp1.routine.handoffRT.int")
hypothesis.test.interruption(exp1.routine.handoffMiss, "exp1.routine.handoffMiss.int")
hypothesis.test.interruption(exp1.routine.falseAlarms, "exp1.routine.falseAlarms.int")
hypothesis.test.interruption(exp1.routine.cdt, "exp1.routine.cdt.int")
hypothesis.test.interruption(exp1.routine.conflictMisses, "exp1.routine.conflictMisses.int")
hypothesis.test.interruption(exp1.workload, "exp1.workload.int")

hypothesis.test.none(exp1.resumptionTime, "exp1.resumptionTime.none")
hypothesis.test.none(exp1.resumptionMisses, "exp1.resumptionMisses.none")
hypothesis.test.none(exp1.deferredHandoffTime, "exp1.deferredHandoffTime.none")
hypothesis.test.none(exp1.deferredHandoffMiss, "exp1.deferredHandoffMiss.none")
hypothesis.test.none(exp1.routine.acceptRT, "exp1.routine.acceptRT.none")
hypothesis.test.none(exp1.routine.acceptMiss, "exp1.routine.acceptMiss.none")
hypothesis.test.none(exp1.routine.handoffRT, "exp1.routine.handoffRT.none")
hypothesis.test.none(exp1.routine.handoffMiss, "exp1.routine.handoffMiss.none")
hypothesis.test.none(exp1.routine.falseAlarms, "exp1.routine.falseAlarms.none")
hypothesis.test.none(exp1.routine.cdt, "exp1.routine.cdt.none")
hypothesis.test.none(exp1.routine.conflictMisses, "exp1.routine.conflictMisses.none")
hypothesis.test.none(exp1.workload, "exp1.workload.none")












