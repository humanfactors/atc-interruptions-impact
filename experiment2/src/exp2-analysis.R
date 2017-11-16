# ---
# title: "Experiment 2 exp2 Testing"
# author: "Michael Wilson"
# date: "2016"
# description: "Conduct exp2 testing for R."
# key vars: variable.title.t, variable.title.bf, variable.title.d
# ---

library(reshape2)
library(plyr)
library(psych)
library(BayesFactor)
library(lsr)

source("./experiment2/src/contrast-functions.R")

#-------------------------------------------------#
# -------------- D A T A  P R E P --------------- #
#-------------------------------------------------#

exp2.testing.df <- read.csv("./experiment2/data/exp2-main.csv",header = TRUE)
exp2.testing.df.colindex <- as.data.frame(colnames(exp2.testing.df))

exp2.pm.testing.df <- read.csv("./experiment2/data/exp2-main-pm.csv",header = TRUE, na.strings = "NA")
exp2.pm.testing.df.colindex <- as.data.frame(colnames(exp2.pm.testing.df))
exp2.pm.testing.df <- exp2.pm.testing.df[ which(rowSums(exp2.pm.testing.df[,32:34]) > 0),]

exp2.routine.testing.df <- read.csv("./experiment2/data/exp2-routine.csv",header = TRUE)
exp2.routine.testing.df.colindex <- as.data.frame(colnames(exp2.routine.testing.df))

# PM analyse

exp2.resumptionTime <- exp2.testing.df[, c("Participant", "ATCintM_ResTime", "BlankM_ResTime", "NoneM_ResTime")]
exp2.resumptionTime <- rename(exp2.resumptionTime, c("NoneM_ResTime"="None", "BlankM_ResTime"="Blank","ATCintM_ResTime"="ATC"))
exp2.resumptionTime$ints <- (exp2.resumptionTime$Blank + exp2.resumptionTime$ATC)/2

exp2.resumptionMisses <- exp2.testing.df[, c("Participant", "ATCintM_ResTimeMIssProp", "BlankM_ResTimeMIssProp", "NoneM_ResTimeMIssProp")]
exp2.resumptionMisses <- rename(exp2.resumptionMisses,c("NoneM_ResTimeMIssProp"="None", "BlankM_ResTimeMIssProp"="Blank","ATCintM_ResTimeMIssProp"="ATC"))
exp2.resumptionMisses$ints <- (exp2.resumptionMisses$Blank + exp2.resumptionMisses$ATC)/2

# Deferred handoff is seperate to exclude procedural response errors
exp2.pmMissNames <- c("ATCintM_PMMissProp", "BlankM_PMMissProp", "NoneM_PMMissProp")
exp2.pm.testing.df[,exp2.pmMissNames[1]] <- 1-(rowSums(exp2.pm.testing.df[,17:21], na.rm = TRUE)/(5-exp2.pm.testing.df[,68]))
exp2.pm.testing.df[,exp2.pmMissNames[2]] <- 1-(rowSums(exp2.pm.testing.df[,22:26], na.rm = TRUE)/(5-exp2.pm.testing.df[,69]))
exp2.pm.testing.df[,exp2.pmMissNames[3]] <- 1-(rowSums(exp2.pm.testing.df[,27:31], na.rm = TRUE)/(5-exp2.pm.testing.df[,70]))

exp2.deferredHandoffMiss <- exp2.pm.testing.df[, c("Participant",exp2.pmMissNames[1],exp2.pmMissNames[2],exp2.pmMissNames[3])]
exp2.deferredHandoffMiss <- rename(exp2.deferredHandoffMiss,c("NoneM_PMMissProp"="None", "BlankM_PMMissProp"="Blank","ATCintM_PMMissProp"="ATC"))
exp2.deferredHandoffMiss$ints <- (exp2.deferredHandoffMiss$Blank + exp2.deferredHandoffMiss$ATC)/2

exp2.deferredHandoffTime <- exp2.pm.testing.df[, c("Participant", "ATCintM_PM_reaction", "BlankM_PM_reaction", "NoneM_PM_reaction")]
exp2.deferredHandoffTime[exp2.deferredHandoffTime == 0] <- NA
exp2.deferredHandoffTime <-exp2.deferredHandoffTime[complete.cases(exp2.deferredHandoffTime),]
exp2.deferredHandoffTime <- rename(exp2.deferredHandoffTime,c("NoneM_PM_reaction"="None", "BlankM_PM_reaction"="Blank","ATCintM_PM_reaction"="ATC"))
exp2.deferredHandoffTime$ints <- (exp2.deferredHandoffTime$Blank + exp2.deferredHandoffTime$ATC)/2


# # Routine analyses
#
exp2.routine.acceptRT <- exp2.routine.testing.df[, c("Participant", "ATCintM_PostInt_Accept_ReactionTime", "BlankM_PostInt_Accept_ReactionTime", "NoneM_PostInt_Accept_ReactionTime")]
exp2.routine.acceptRT <- rename(exp2.routine.acceptRT,c("NoneM_PostInt_Accept_ReactionTime"="None", "BlankM_PostInt_Accept_ReactionTime"="Blank","ATCintM_PostInt_Accept_ReactionTime"="ATC"))
exp2.routine.acceptRT$ints <- (exp2.routine.acceptRT$Blank + exp2.routine.acceptRT$ATC)/2


exp2.routine.acceptMiss <- exp2.routine.testing.df[, c("Participant", "ATCintM_PostInt_Accept_Misses", "BlankM_PostInt_Accept_Misses", "NoneM_PostInt_Accept_Misses")]
exp2.routine.acceptMiss <- rename(exp2.routine.acceptMiss,c("NoneM_PostInt_Accept_Misses"="None", "BlankM_PostInt_Accept_Misses"="Blank","ATCintM_PostInt_Accept_Misses"="ATC"))
exp2.routine.acceptMiss$ints <- (exp2.routine.acceptMiss$Blank + exp2.routine.acceptMiss$ATC)/2

exp2.routine.handoffRT <- exp2.routine.testing.df[, c("Participant", "ATCintM_PostInt_Handoff_ReactionTime", "BlankM_PostInt_Handoff_ReactionTime", "NoneM_PostInt_Handoff_ReactionTime")]
exp2.routine.handoffRT <- rename(exp2.routine.handoffRT,c("NoneM_PostInt_Handoff_ReactionTime"="None", "BlankM_PostInt_Handoff_ReactionTime"="Blank","ATCintM_PostInt_Handoff_ReactionTime"="ATC"))
exp2.routine.handoffRT$ints <- (exp2.routine.handoffRT$Blank + exp2.routine.handoffRT$ATC)/2


exp2.routine.handoffMiss <- exp2.routine.testing.df[, c("Participant", "ATCintM_PostInt_Handoff_Misses", "BlankM_PostInt_Handoff_Misses", "NoneM_PostInt_Handoff_Misses")]
exp2.routine.handoffMiss <- rename(exp2.routine.handoffMiss,c("NoneM_PostInt_Handoff_Misses"="None", "BlankM_PostInt_Handoff_Misses"="Blank","ATCintM_PostInt_Handoff_Misses"="ATC"))
exp2.routine.handoffMiss$ints <- (exp2.routine.handoffMiss$Blank + exp2.routine.handoffMiss$ATC)/2

# exp2.routine.falseAlarms <- exp2.routine.testing.df[, c("Participant", "ATC_ConfFA", "Blank_ConfFA", "None_ConfFA")]
# exp2.routine.falseAlarms <- rename(exp2.routine.testing.df,c("None_ConfFA"="None", "Blank_ConfFA"="Blank","ATC_ConfFA"="ATC"))
# exp2.routine.falseAlarms$ints <- (exp2.routine.falseAlarms$Blank + exp2.routine.falseAlarms$ATC)/2


exp2.routine.cdt <- exp2.routine.testing.df[, c("Participant", "ATCintM_PostInt_CDT", "BlankM_PostInt_CDT", "NoneM_PostInt_CDT")]
exp2.routine.cdt <- rename(exp2.routine.cdt,c("NoneM_PostInt_CDT"="None", "BlankM_PostInt_CDT"="Blank","ATCintM_PostInt_CDT"="ATC"))
exp2.routine.cdt$ints <- (exp2.routine.cdt$Blank + exp2.routine.cdt$ATC)/2


exp2.routine.conflictMisses <- exp2.routine.testing.df[, c("Participant", "ATCintM_PostInt_ConfMisses", "BlankM_PostInt_ConfMisses", "NoneM_PostInt_ConfMisses")]
exp2.routine.conflictMisses <- rename(exp2.routine.conflictMisses,c("NoneM_PostInt_ConfMisses"="None", "BlankM_PostInt_ConfMisses"="Blank","ATCintM_PostInt_ConfMisses"="ATC"))
exp2.routine.conflictMisses$ints <- (exp2.routine.conflictMisses$Blank + exp2.routine.conflictMisses$ATC)/2

# Workload analyses

exp2.workload <- exp2.routine.testing.df[, c("Participant", "NoneM_Workload1", "BlankM_Workload1", "ATCintM_Workload1","NoneM_Workload2", "BlankM_Workload2", "ATCintM_Workload2")]
exp2.workload <- rename(exp2.workload,c("NoneM_Workload1"="None1", "BlankM_Workload1"="Blank1","ATCintM_Workload1"="ATC1", "NoneM_Workload2"="None2", "BlankM_Workload2"="Blank2","ATCintM_Workload2"="ATC2"))
exp2.workload$ATC <- (exp2.workload$ATC1 + exp2.workload$ATC2)/2
exp2.workload$Blank <- (exp2.workload$Blank1 + exp2.workload$Blank2)/2
exp2.workload$None <- (exp2.workload$None1 + exp2.workload$None2)/2
exp2.workload$ints <- (exp2.workload$ATC + exp2.workload$Blank)/2



#-------------------------------------------------#
# -------------- A N A L Y S I S  --------------- #
#-------------------------------------------------#

exp2.test.interruption(exp2.resumptionTime, "exp2.resumptionTime.int")
exp2.test.interruption(exp2.resumptionMisses, "exp2.resumptionMisses.int")
exp2.test.interruption(exp2.deferredHandoffMiss, "exp2.deferredHandoffMiss.int")
exp2.test.interruption(exp2.deferredHandoffTime, "exp2.deferredHandoffTime.int")
exp2.test.interruption(exp2.routine.acceptRT, "exp2.routine.acceptRT.int")
exp2.test.interruption(exp2.routine.acceptMiss, "exp2.routine.acceptMiss.int")
exp2.test.interruption(exp2.routine.handoffRT, "exp2.routine.handoffRT.int")
exp2.test.interruption(exp2.routine.handoffMiss, "exp2.routine.handoffMiss.int")
# exp2.test.interruption(exp2.routine.falseAlarms, "exp2.routine.falseAlarms.int")
exp2.test.interruption(exp2.routine.cdt, "exp2.routine.cdt.int")
exp2.test.interruption(exp2.routine.conflictMisses, "exp2.routine.conflictMisses.int")
exp2.test.interruption(exp2.workload, "exp2.workload.int")

exp2.test.none(exp2.resumptionTime, "exp2.resumptionTime.none")
exp2.test.none(exp2.resumptionMisses, "exp2.resumptionMisses.none")
exp2.test.none(exp2.deferredHandoffMiss, "exp2.deferredHandoffMiss.none")
exp2.test.none(exp2.deferredHandoffTime, "exp2.deferredHandoffTime.none")
exp2.test.none(exp2.routine.acceptRT, "exp2.routine.acceptRT.none")
exp2.test.none(exp2.routine.acceptMiss, "exp2.routine.acceptMiss.none")
exp2.test.none(exp2.routine.handoffRT, "exp2.routine.handoffRT.none")
exp2.test.none(exp2.routine.handoffMiss, "exp2.routine.handoffMiss.none")
# exp2.test.none(exp2.routine.falseAlarms, "exp2.routine.falseAlarms.none")
exp2.test.none(exp2.routine.cdt, "exp2.routine.cdt.none")
exp2.test.none(exp2.routine.conflictMisses, "exp2.routine.conflictMisses.none")
exp2.test.none(exp2.workload, "exp2.workload.none")

