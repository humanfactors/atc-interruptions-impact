# Hypothesis functions from EXP1
#-------------------------------------------------#
# --------------F U N C T I O N S --------------- #
#-------------------------------------------------#

exp2.test.interruption <- function(dataset,variable.title) {
  # generates the t,d,bf for ATC vs blank contrast
  t <- t.test(dataset$ATC,dataset$Blank, paired = T)
  assign(paste(variable.title, "t", sep="."),t,envir = .GlobalEnv)
  cat("-------------------------\n")
  cat("   Paired Samples t-test\n")
  cat("-------------------------\n")
  print(t)
  
  d <- cohensD(dataset$Blank, dataset$ATC, method = "paired")
  assign(paste(variable.title, "d", sep="."),d,envir = .GlobalEnv)
  cat("-------------------------\n")
  cat("   Cohen's D\n")
  cat("-------------------------\n")
  print(d)
  cat("\n\n")
  
  bf <- ttestBF(x = dataset$ATC, y = dataset$Blank, mu = 0,
                paired = TRUE, rscale = "medium",
                posterior = FALSE)
  assign(paste(variable.title, "bf", sep="."),bf,envir = .GlobalEnv)
  cat("-------------------------\n")
  cat("   Bayes Factor\n")
  cat("-------------------------\n")
  print(bf)
}

exp2.test.none <- function(dataset,variable.title) {
  t <- t.test(dataset$Blank,dataset$None, paired = T)
  assign(paste(variable.title, "t", sep="."),t,envir = .GlobalEnv)
  print(t)
  
  d <- cohensD(dataset$Blank, dataset$None, method = "paired")
  assign(paste(variable.title, "d", sep="."),d,envir = .GlobalEnv)
  print(d)
  
  bf <- ttestBF(x = dataset$Blank, y = dataset$None, mu = 0,
                paired = TRUE, rscale = "medium",
                posterior = FALSE)
  assign(paste(variable.title, "bf", sep="."),bf,envir = .GlobalEnv)
  print(bf)
}
