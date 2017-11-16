# Simmple script to return (M = X%) or  (M = round(float)) 

library(apa)
source("./experiment2/src/exp2-analysis.R")

# meantext(exp2.resumptionTime$ints)
# meantext(exp2.resumptionTime$None)
# meantext(exp2.resumptionTime$ATC)
# meantext(exp2.resumptionTime$Blank)
# 
# meanpctext(exp2.resumptionMisses$ints)
# meanpctext(exp2.resumptionMisses$None)
# meanpctext(exp2.resumptionMisses$ATC)
# meanpctext(exp2.resumptionMisses$Blank)
# 
# 
# meanpctext(exp2.deferredHandoffMiss$ints)
# meanpctext(exp2.deferredHandoffMiss$None)
# meanpctext(exp2.deferredHandoffMiss$ATC)
# meanpctext(exp2.deferredHandoffMiss$Blank)
# 
# meantext(exp2.deferredHandoffTime$ints)
# meantext(exp2.deferredHandoffTime$None)
# meantext(exp2.deferredHandoffTime$ATC)
# meantext(exp2.deferredHandoffTime$Blank)

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

