library("survival")

exp2.resumptiondata <- read.csv("ex-gaussian/data/exp2_restimes_withcutoff.csv")
exp2.resumptiondata <- exp2.resumptiondata[,c(1,2,3,8,9,7)]
cols <- c("subject","trial","condition","restime","reserror", "ms_cutoff")
colnames(exp2.resumptiondata) <- cols
exp1.resumptiondata <- read.csv("ex-gaussian/data/exp1_restimes_withcutoff.csv")
full.resumptiondata <- rbind(exp2.resumptiondata, exp1.resumptiondata)
full.resumptiondata$time <- full.resumptiondata$restime/1000
full.resumptiondata$time[is.na(full.resumptiondata$restime)] <- 
  full.resumptiondata$ms_cutoff[is.na(full.resumptiondata$restime)]/1000
full.resumptiondata$rescorr <- 1-full.resumptiondata$reserror

fit <- survfit(Surv(time, rescorr) ~ condition, data = full.resumptiondata) 
pdf("cumRT.pdf", width=5, height=5)
plot(fit, col=c("firebrick2","orange", "deepskyblue", "seagreen3"), mark = 'x', mark.time=TRUE, fun="event", lty=1:4,
     xlab="Time (s)", ylab="Cumulative probability", las=1)
legend(15, 0.6, c("ATC","Blank","None","Nback"),col=c("firebrick2","orange", "deepskyblue", "seagreen3"), lty=1:4)
dev.off()

