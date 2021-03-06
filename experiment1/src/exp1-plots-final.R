# ---
# title: "Experiment 1 Hypothesis Testing"
# author: "Michael Wilson"
# date: "2016"
# description: "Conduct hypothesis testing for R."
# key vars: variable.title.t, variable.title.bf, variable.title.d
# ---
library(scales)
library(ggplot2)
library(lsr)

source("./experiment1/src/exp1-analysis.R")
source("./experiment1/src/exp1-confidence-intervals.R")
#-------------------------------------------------#
# -------------- D A T A  P R E P --------------- #
#-------------------------------------------------#

exp1.resumptionTime.melt <- melt(exp1.resumptionTime[c(2:4)])
exp1.resumptionTime.melt <- dcast(exp1.resumptionTime.melt, variable ~ ., fun.aggregate = mean)
exp1.resumptionTime.melt$. <- exp1.resumptionTime.melt$.
exp1.resumptionTime.melt <- data.frame(exp1.resumptionTime.melt$variable, c(exp1.wsci.restime.Nback, exp1.wsci.restime.blank, exp1.wsci.restime.none), c(exp1.resumptionTime.melt$.))
colnames(exp1.resumptionTime.melt) <- c("Condition", "CI", "mean")
exp1.resumptionTime.melt$Condition <- factor(exp1.resumptionTime.melt$Condition, levels = c("None", "Blank", "Nback"))

exp1.resumptionMisses.melt <- melt(exp1.resumptionMisses[c(2:4)])
exp1.resumptionMisses.melt <- dcast(exp1.resumptionMisses.melt, variable ~ ., fun.aggregate = mean)
exp1.resumptionMisses.melt$. <- exp1.resumptionMisses.melt$.
exp1.resumptionMisses.melt <- data.frame(exp1.resumptionMisses.melt$variable, c(exp1.wsci.resfail.Nback, exp1.wsci.resfail.blank, exp1.wsci.resfail.none), c(exp1.resumptionMisses.melt$.))
colnames(exp1.resumptionMisses.melt) <- c("Condition", "CI", "mean")
exp1.resumptionMisses.melt$Condition <- factor(exp1.resumptionMisses.melt$Condition, levels = c("None", "Blank", "Nback"))

exp1.deferredHandoffTime.melt <- melt(exp1.deferredHandoffTime[c(2:4)])
exp1.deferredHandoffTime.melt <- dcast(exp1.deferredHandoffTime.melt, variable ~ ., fun.aggregate = mean, na.rm = TRUE)
exp1.deferredHandoffTime.melt$. <- exp1.deferredHandoffTime.melt$.
exp1.deferredHandoffTime.melt <- data.frame(exp1.deferredHandoffTime.melt$variable, c(exp1.wsci.defhotime.Nback, exp1.wsci.defhotime.blank, exp1.wsci.defhotime.none), c(exp1.deferredHandoffTime.melt$.))
colnames(exp1.deferredHandoffTime.melt) <- c("Condition", "CI", "mean")
exp1.deferredHandoffTime.melt$Condition <- factor(exp1.deferredHandoffTime.melt$Condition, levels = c("None", "Blank", "Nback"))

exp1.deferredHandoffMiss.melt <- melt(exp1.deferredHandoffMiss[c(2:4)])
exp1.deferredHandoffMiss.melt <- dcast(exp1.deferredHandoffMiss.melt, variable ~ ., fun.aggregate = mean)
exp1.deferredHandoffMiss.melt$. <- exp1.deferredHandoffMiss.melt$.
exp1.deferredHandoffMiss.melt <- data.frame(exp1.deferredHandoffMiss.melt$variable, c(exp1.wsci.defhomiss.Nback, exp1.wsci.defhomiss.blank, exp1.wsci.defhomiss.none), c(exp1.deferredHandoffMiss.melt$.))
colnames(exp1.deferredHandoffMiss.melt) <- c("Condition", "CI", "mean")
exp1.deferredHandoffMiss.melt$Condition <- factor(exp1.deferredHandoffMiss.melt$Condition, levels = c("None", "Blank", "Nback"))

#-------------------------------------------------#
# -------------- P L O T S        --------------- #
#-------------------------------------------------#
mylabels <- c("No-Interruption", "Blank", expression(paste(italic("n"), "-back")))
grays <- c("gray90", "gray75", "gray60")

theme_luke <- function(base_size = 10, base_family = "") {
  theme_gray(base_size = base_size, base_family = base_family) %+replace%
    theme(
      # panel.background = element_rect(fill = "white"),
      strip.background = element_rect(fill = "white"),
      plot.background = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      panel.background = element_blank(),
      text = element_text(size = 12, family = base_family, face = "plain", color = "black", hjust = 0.5, vjust = 0.5, angle = 0, lineheight = 0.9,margin = margin(), debug = FALSE),
      axis.line.x  = element_line(color = 'black'),
      axis.line.y  = element_line(color = 'black'),
      legend.position = "none",
      axis.ticks = element_line(colour = "black"),
      axis.title.x = element_blank(),
      axis.text.x = element_text(colour = "black", size = 12, angle = 0, hjust = .5, vjust = .5),
      axis.title.y = element_text(colour = "black", face = "bold", size = 12, angle = 90, vjust = 2, hjust = 0.5, margin = margin(0,8,0,0), debug = FALSE),
      axis.text.y = element_text(colour = "black", size = 12, angle = 0, hjust = 0, vjust = .5))
}
theme_set(theme_luke())

plot.hrtMiss <- ggplot(exp1.deferredHandoffMiss.melt, aes(factor(Condition), mean)) +
  geom_bar(size = 0.5, fill = grays, colour = "black", stat = "identity", width = .8) +
  geom_errorbar(aes(ymax = mean + CI, ymin = mean - CI, width = 0.2)) +
  scale_y_continuous(name = "Error Proportion", breaks = seq(0.0, .4, by = .05), limits = c(0.0, .4), labels = percent) +
  geom_text(aes(y = .02, x = Condition, angle = 0, label = percent(mean), size = 4.28)) +
  scale_x_discrete(labels = mylabels)
plot.hrtMiss

plot.hrt <- ggplot(exp1.deferredHandoffTime.melt, aes(factor(Condition), mean)) +
  geom_bar(size = 0.5, fill = grays, colour = "black", stat = "identity", width = .8) +
  geom_errorbar(aes(ymax = mean + CI, ymin = mean - CI, width = 0.2)) +
  ylab("Response Time (ms)") +
  scale_y_continuous(breaks = seq(0, 3000, by = 500), limits = c(0, 3200)) +
  geom_text(aes(y = 300, x = Condition, angle = 0, label = round(mean, 0), size = 4.3)) +
  scale_x_discrete(labels = mylabels)
plot.hrt

plot.reslag <- ggplot(exp1.resumptionTime.melt, aes(factor(Condition), mean)) +
  geom_bar(size = 0.5, fill = grays, colour = "black", stat = "identity", width = .8) +
  geom_errorbar(aes(ymax = mean + CI, ymin = mean - CI, width = 0.2)) +
  xlab("Condition") + ylab("Response Time") +
  scale_y_continuous(breaks = seq(0, 5300, by = 1000), limits = c(0, 5300)) +
  geom_text(aes(y = 300, x = as.numeric(Condition), angle = 0, label = round(mean, 0), size = 10)) +
  scale_x_discrete(labels = mylabels)
plot.reslag

plot.reserror <- ggplot(exp1.resumptionMisses.melt, aes(factor(Condition), mean)) +
  geom_bar(size = 0.5, fill = grays, colour = "black", stat = "identity", width = .8) +
  geom_errorbar(aes(ymax = mean + CI, ymin = mean - CI, width = 0.2)) +
  xlab("Condition") + ylab("Resumption Failure Proportion") +
  scale_y_continuous(breaks = seq(0.0, .2, by = .05), limits = c(0.0, .2), labels = percent) +
  geom_text(aes(y = .009, x = Condition, angle = 0, label = percent(mean), size = 4.3)) +
  scale_x_discrete(labels = mylabels)
plot.reserror

ggsave('figures/exp1.reslag.png', plot = plot.reslag, width=3.5, height=3.5, scale = 3, units = "cm")
ggsave('figures/exp1.reserror.png', plot = plot.reserror, width=3.5, height=3.5, scale = 3, dpi = 300, units = "cm")
ggsave('figures/exp1.hrt.png', plot = plot.hrt, width=3.5, height=3.5, scale = 3, dpi = 300, units = "cm")
ggsave('figures/exp1.deferredHandoffMiss.png', plot = plot.hrtMiss, width=3.5, height=3.5, scale = 3, dpi = 300, units = "cm")
