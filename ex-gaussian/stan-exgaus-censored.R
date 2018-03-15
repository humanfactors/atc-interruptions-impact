# Ex-gassuian model of Resumption Times/Failure Proportions code
library("tidyverse")
library("rstan")

resumptiondata <- read_csv("ex-gaussian/data/exp2_restimes_withcutoff.csv", col_name = TRUE) %>%
  select(1,2,3,8,9,7) %>%
  bind_rows(read_csv("ex-gaussian/data/exp1_restimes_withcutoff.csv", col_name = TRUE))
as.data.frame(resumptiondata)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# ATC Interruption Condition
yt <- resumptiondata$restime[resumptiondata$condition=="ATCint"]
isCensored <-  resumptiondata$reserror[resumptiondata$condition=="ATCint"]
Ncorrect <- sum(1-isCensored)
Nwrong <- sum(isCensored)
y <- yt[!isCensored]/1000
cutoff <-  resumptiondata$ms_cutoff[resumptiondata$condition=="ATCint" & resumptiondata$reserror==1]/1000
  
atc_std <- list(Ncorrect=Ncorrect, Nwrong=Nwrong, y=y, cutoff=cutoff)
atc_fit <- stan(file = "ex-gaussian/cutoffs.stan",   # Stan program
                        data = atc_std,  # named list of data
                        chains = 3,      # number of Markov chains
                        warmup = 2000,   # number of warmup iterations per chain
                        iter = 4000,     # total number of iterations per chain
                        cores = 3,       # Number of cores - can change this to auto and comment if needed       
                        refresh = 100    # show progress every 'refresh' iterations
)

# Nback Interruption Condition
yt <- resumptiondata$restime[resumptiondata$condition=="Nback"]
isCensored <-  resumptiondata$reserror[resumptiondata$condition=="Nback"]
Ncorrect <- sum(1-isCensored)
Nwrong <- sum(isCensored)
y <- yt[!isCensored]/1000
cutoff <-  resumptiondata$ms_cutoff[resumptiondata$condition=="Nback" & resumptiondata$reserror==1]/1000
  
nback_std <- list(Ncorrect=Ncorrect, Nwrong=Nwrong, y=y, cutoff=cutoff)
nback_fit <- stan(file = "ex-gaussian/cutoffs.stan",   # Stan program
                        data = nback_std,  # named list of data
                        chains = 3,      # number of Markov chains
                        warmup = 2000,   # number of warmup iterations per chain
                        iter = 4000,     # total number of iterations per chain
                        cores = 3,       # Number of cores - can change this to auto and comment if needed       
                        refresh = 100    # show progress every 'refresh' iterations
)

# No Interruption Condition
yt <- resumptiondata$restime[resumptiondata$condition=="None"]
isCensored <-  resumptiondata$reserror[resumptiondata$condition=="None"]
Ncorrect <- sum(1-isCensored)
Nwrong <- sum(isCensored)
y <- yt[!isCensored]/1000
cutoff <-  resumptiondata$ms_cutoff[resumptiondata$condition=="None" & resumptiondata$reserror==1]/1000

none_std <- list(Ncorrect=Ncorrect, Nwrong=Nwrong, y=y, cutoff=cutoff)
none_fit <- stan(file = "ex-gaussian/cutoffs.stan",   # Stan program
                  data = none_std,  # named list of data
                  chains = 3,      # number of Markov chains
                  warmup = 2000,   # number of warmup iterations per chain
                  iter = 4000,     # total number of iterations per chain
                  cores = 3,       # Number of cores - can change this to auto and comment if needed       
                  refresh = 100    # show progress every 'refresh' iterations
)

# Blank Interruption Condition
# Note that Blank fit near the edge of the parameter space - adjust reruns accordingly
yt <- resumptiondata$restime[resumptiondata$condition=="Blank"]
isCensored <-  resumptiondata$reserror[resumptiondata$condition=="Blank"]
Ncorrect <- sum(1-isCensored)
Nwrong <- sum(isCensored)
y <- yt[!isCensored]/1000
cutoff <-  resumptiondata$ms_cutoff[resumptiondata$condition=="Blank" & resumptiondata$reserror==1]/1000

blank_std <- list(Ncorrect=Ncorrect, Nwrong=Nwrong, y=y, cutoff=cutoff)
blank_fit <- stan(file = "ex-gaussian/cutoffs.stan",   # Stan program
                  data = blank_std,  # named list of data
                  chains = 3,      # number of Markov chains
                  warmup = 2000,   # number of warmup iterations per chain
                  iter = 4000,     # total number of iterations per chain
                  refresh = 100    # show progress every 'refresh' iterations
)

