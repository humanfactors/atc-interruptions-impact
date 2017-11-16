# Simmple script to return (M = X%) or  (M = round(float)) 
library(apa)
# source("./experiment1/src/exp1-analysis.R")



# meanpctext(exp1.resumptionMisses$ints)
# meanpctext(exp1.resumptionMisses$None)
# meanpctext(exp1.resumptionMisses$Nback)
# meanpctext(exp1.resumptionMisses$Blank)
# 
# 
# meanpctext(exp1.deferredHandoffMiss$ints)
# meanpctext(exp1.deferredHandoffMiss$None)
# meanpctext(exp1.deferredHandoffMiss$Nback)
# meanpctext(exp1.deferredHandoffMiss$Blank)
# 
# meantext(exp1.deferredHandoffTime$ints)
# meantext(exp1.deferredHandoffTime$None)
# meantext(exp1.deferredHandoffTime$Nback)
# meantext(exp1.deferredHandoffTime$Blank)

# fulltexttest(exp1.deferredHandoffMiss.int.t,exp1.deferredHandoffMiss.int.bf)  
# fulltexttest(exp1.resumptionTime.int.t,exp1.resumptionTime.int.bf)  

#-------------------------------------------------#
# --------------F U N C T I O N S --------------- #
#-------------------------------------------------#

meantext <- function(variable) {
  thismean = round(mean(variable),2)
  thissd = round(sd(variable),2)
  return(paste("(M = ",thismean,", SD = ",thissd,")",sep = ""))
}

meantextms <- function(variable) {
  thismean = round(mean(variable),0)
  thissd = round(sd(variable),0)
  return(paste("(M =",thismean,"ms,",
               "SD = ",thissd,"ms)", sep = " "))
}

meanpctext <- function(variable) {
  thismean = round(mean(variable),4)
  thissd = round(sd(variable),4)
  return(paste("(M = ",percent(thismean),", ",
               "SD = ",percent(thissd),")",sep = ""))
}

texttest <- function(ttest, bft) {
  if (bft@bayesFactor$bf < 0) {
    bft <- paste("BF01 = ",round(1/exp(bft@bayesFactor$bf),2), sep = "")
  } else if (bft@bayesFactor$bf > 0) {
    bft <- paste("BF10 = ",round(exp(bft@bayesFactor$bf),2), sep = "")
  }
  markdownt <- apa(ttest, format = 'markdown')
  paste(markdownt, bft, sep = ", ")
  }


