# Clear Memory
rm(list = ls(all.names = TRUE))
set.seed(1000)
# Import Libraries in R
library("ggplot2")
library("gridExtra")
library("ez")
library("lme4")
library("emmeans")
library("car")
library("lmTest")
library("pracma")
library("gmodels")
library("klaR")
library("C50")
library("caret")
library("latticeExtra")
library("phonR")
library("plyr") # for renaming
library("xtable")
library("ggsignif")
library("plyr")

# Printing
options(xtable.floating = FALSE)
options(xtable.timestamp = "")
options(digits=2)
options(scipen=999)
source("definitions.r")
# Definitions
.str <- function(x){
  # Modified str() to display all list elements within a level.
  # Charalambos Themistocleous 2015
  str(x, list.len=length(x))
}

# Get the trimmed mean predefined value at the 10%
mean1 <- function(x,...){
  mean(x,trim=.1)
}

haristheme <- theme(panel.background = element_rect(fill = "white", colour = "white"),
                    panel.border = element_blank(),
                    legend.title = element_blank(), 
                    panel.grid.major = element_line(colour = "grey80"),
                    axis.line.x = element_line(colour = "black", size=1),
                    legend.position = "none", text = element_text(size=18), 
                    plot.title = element_text(hjust = 0.5))
# Open Data ----------------------------------------------------
v <- read.csv("data/speechrate_data.csv")
v <- v[!v$soundname == "LABM-75GG-5KH4",] 
v <- v[!v$Materials == "new_random",]
summary(is.na(v))
v$MMSE <- as.numeric(v$MMSE)
v$npause <- as.numeric(v$npause)
v$nsyll <- as.numeric(v$nsyll)
v$phonationtime..s. <- as.numeric(v$phonationtime..s.)
v$speechrate..nsyll.dur. <- as.numeric(v$speechrate..nsyll.dur.)
v$articulation.rate..nsyll...phonationtime. <- as.numeric(v$articulation.rate..nsyll...phonationtime.)
v$ASD..speakingtime.nsyll.  <- as.numeric(v$ASD..speakingtime.nsyll.)


b = v[complete.cases(v$MMSE),]

### Differences between new and old materials
MCI_SCI <- b[!b$condition=="HC",]
MCI_SCI <- droplevels(MCI_SCI)
t.test(MCI_SCI$MMSE~MCI_SCI$Materials)
t.test(MCI_SCI$phonationtime..s.~MCI_SCI$Materials)
t.test(MCI_SCI$speechrate..nsyll.dur.~MCI_SCI$Materials) # only this is sigificant
t.test(MCI_SCI$articulation.rate..nsyll...phonationtime.~MCI_SCI$Materials)
t.test(MCI_SCI$ASD..speakingtime.nsyll.~MCI_SCI$Materials)

### MMSE
cor(b$npause,b$MMSE,  method=c("spearman"))
cor(b$nsyll,b$MMSE,  method=c("spearman"))
cor(b$phonationtime..s.,b$MMSE,  method=c("spearman")) # Phonation time seems to correlate with MMSE
cor(b$speechrate..nsyll.dur.,b$MMSE,  method=c("spearman"))
cor(b$articulation.rate..nsyll...phonationtime.,b$MMSE,  method=c("spearman"))
cor(b$ASD..speakingtime.nsyll.,b$MMSE,  method=c("spearman"))

### Differences between MCI and SCI
summary(aov(MCI_SCI$MMSE~MCI_SCI$condition))
summary(aov(MCI_SCI$phonationtime..s.~MCI_SCI$condition))
summary(aov(MCI_SCI$speechrate..nsyll.dur.~MCI_SCI$condition)) # only this is sigificant
summary(aov(MCI_SCI$articulation.rate..nsyll...phonationtime.~MCI_SCI$condition))
summary(aov(MCI_SCI$ASD..speakingtime.nsyll.~MCI_SCI$condition))

### Differences between MCI and HC
MCI_HC <- b[!b$condition=="SCI",]
MCI_HC <- droplevels(MCI_HC)
t.test(MCI_HC$phonationtime..s.~MCI_HC$condition)
t.test(MCI_HC$speechrate..nsyll.dur.~MCI_HC$condition)


# Speech Rate
speechrate..nsyll.dur.m= lm(MCI_HC$speechrate..nsyll.dur.~MCI_HC$condition)
summary(speechrate..nsyll.dur.m)

pd <- position_dodge(0.1) # move them .05 to the left and right
speechrate..nsyll.dur._ci <- summarySE(MCI_HC, measurevar="speechrate..nsyll.dur.", groupvars=c("condition"), na.rm=TRUE)
speechrate_ci_gplot = ggplot(speechrate..nsyll.dur._ci, aes(x=condition, y=speechrate..nsyll.dur., fill="condition")) + 
  geom_errorbar(aes(ymin=speechrate..nsyll.dur.-ci, ymax=speechrate..nsyll.dur.+ci), size=1.5, width=.1, position=pd) +
  geom_line(position=pd, size=1) +
  geom_point(position=pd, size=3.5) +
  xlab("Condition") + 
  ylab("Speech Rate") +
  ylim(40,75) +
  geom_signif(comparisons = list(c("HC", "MCI")), annotation=formatC(".", digits=4),  textsize=6) +
  haristheme + scale_fill_grey(start = 0.9, end = 0.6)
speechrate_ci_gplot

library(apaTables)
speechrate..nsyll.dur.m = lm(MCI_HC$speechrate..nsyll.dur.~MCI_HC$condition)
summary(speechrate..nsyll.dur.m)
apa.reg.table(speechrate..nsyll.dur.m, filename = "Averaged Speaking Time.doc", table.number = 2)
boxplot(MCI_HC$speechrate..nsyll.dur.m~MCI_HC$condition)


speaking = lm(MCI_HC$ASD..speakingtime.nsyll.~MCI_HC$condition)
summary(speaking)
plot_model(speaking, type = "pred",  terms = c("condition"))

boxplot(MCI_HC$ASD..speakingtime.nsyll.~MCI_HC$condition)
speakingtime_plot = ggplot(MCI_HC, aes(x = condition, y = ASD..speakingtime.nsyll.)) + 
  #  stat_summary(fun.y = mean, geom = "point") +
  geom_boxplot(aes(fill = condition), outlier.shape = NA) +
  ggtitle("Average Syllable Duration") +
  scale_x_discrete(name = "Condition")  + 
  scale_fill_brewer(palette = "Blues") +
  scale_y_continuous(limits = quantile(MCI_HC$ASD..speakingtime.nsyll., c(0.1, 0.9))) +
  labs(
    fill = "condition",
    x = "Condition",
    y = "Average Syllable Duration"
  ) + theme_classic()+
  geom_signif(comparisons = list(c("HC", "MCI")), annotation=formatC("**", digits=4),  textsize=6) +
  theme(legend.position="top", legend.title = element_blank(), text = element_text(size=20), plot.title = element_text(hjust = 0.5),panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 
speakingtime_plot

pd <- position_dodge(0.1) # move them .05 to the left and right
ASD..speakingtime.nsyll._ci <- summarySE(MCI_HC, measurevar="ASD..speakingtime.nsyll.", groupvars=c("condition"), na.rm=TRUE)
ASD_ci_gplot = ggplot(ASD..speakingtime.nsyll._ci, aes(x=condition, y=ASD..speakingtime.nsyll., fill="condition")) + 
  geom_errorbar(aes(ymin=ASD..speakingtime.nsyll.-ci, ymax=ASD..speakingtime.nsyll.+ci), size=1.5, width=.1, position=pd) +
  geom_line(position=pd, size=1) +
  geom_point(position=pd, size=3.5) +
  xlab("Condition") + 
  ylab("Averaged Syllable Duration") + 
  ylim(30,60) +
  geom_signif(comparisons = list(c("HC", "MCI")), annotation=formatC("**", digits=4),  textsize=6) +
  haristheme + scale_fill_grey(start = 0.9, end = 0.6)
ASD_ci_gplot

library(apaTables)
speaking_model = lm(MCI_HC$ASD..speakingtime.nsyll.~MCI_HC$condition)
summary(speaking_model)
apa.reg.table(speaking_model, filename = "Averaged_Syllable_Duration.doc", table.number = 2)


articulation_rate = lm(MCI_HC$articulation.rate..nsyll...phonationtime.~MCI_HC$condition)
summary(articulation_rate)
apa.reg.table(articulation_rate, filename = "Articulation_Rate.doc", table.number = 2)



pd <- position_dodge(0.1) # move them .05 to the left and right
articulation.rate..nsyll...phonationtime._ci <- summarySE(MCI_HC, measurevar="articulation.rate..nsyll...phonationtime.", groupvars=c("condition"), na.rm=TRUE)
articulation.rate_ci_gplot = ggplot(articulation.rate..nsyll...phonationtime._ci, aes(x=condition, y=articulation.rate..nsyll...phonationtime., fill="condition")) + 
  geom_errorbar(aes(ymin=articulation.rate..nsyll...phonationtime.-ci, ymax=articulation.rate..nsyll...phonationtime.+ci), size=1.5, width=.1, position=pd) +
  geom_line(position=pd, size=1) +
  geom_point(position=pd, size=3.5) +
  xlab("Condition") + 
  ylab("Articulation Rate") + 
  ylim(40,75) +
  geom_signif(comparisons = list(c("HC", "MCI")), annotation=formatC("**", digits=4),  textsize=6) +
  haristheme + scale_fill_grey(start = 0.9, end = 0.6)
articulation.rate_ci_gplot



library("multipanelfigure")
cols <- 2
rows <- 2
figure <- multi_panel_figure(
  width = 200,
  columns = cols,
  height = 200,
  rows = rows)

(figure %<>% fill_panel(ASD_ci_gplot))
(figure %<>% fill_panel(articulation.rate_ci_gplot))
(figure %<>% fill_panel(speechrate_ci_gplot))

