setwd("C:/Users/Michael Wilson/PhD/atc-interruptions-eerste/meta-analysis")
source("../experiment1/src/contrast-functions.R")
source("../experiment1/src/exp1-textmeans.R")

meta.conflictprop <- read.csv("data/meta_dcproportion.csv")

t.test(meta.conflictprop$Blank, meta.conflictprop$None, paired = TRUE)

meanpctext(meta.conflictprop$Blank)
meanpctext(meta.conflictprop$None)

hypothesis.test.none(meta.conflictprop, "meta.conflictprop")
1/meta.conflictprop.bf

texttest(meta.conflictprop.t, meta.conflictprop.bf)
