# Clear Memory  - Clean Version
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
library("ggthemes")
library("egg")
library("lmerTest")
library("ggsignif")
# Printing
library("multipanelfigure")
library("grid")
source("definitions.r")


# options defaults
options(xtable.floating = FALSE)
options(xtable.timestamp = "")
options(digits=2)
options(scipen=999)

haristheme <- theme(panel.background = element_rect(fill = "white", colour = "white"),
                    panel.border = element_blank(),
                    legend.title = element_blank(), 
                    panel.grid.major = element_line(colour = "grey80"),
                    axis.line.x = element_line(colour = "black", size=1),
                    legend.position = "none", text = element_text(size=18), 
                    plot.title = element_text(hjust = 0.5))
# Open Data ----------------------------------------------------
a <- read.csv("data/voicequality_data.csv")

d <- a[!a$condition=="SCI",]
d <- d[!d$File == "LABM-75GG-5KH4",] 
d <- d[!d$File == "79CJ-M7A3-FALX",] 
d <- d[!d$File == "TXLH-R239-CPQG",]
d <- droplevels(d)

# H1.A3  ---------------------------------------------------------------
pd <- position_dodge(0.1) # move them .05 to the left and right
H1_A3_ci <- summarySE(d, measurevar="H1.A3", groupvars=c("condition"), na.rm=TRUE)
H1_A3_ci_gplot = ggplot(H1_A3_ci, aes(x=condition, y=H1.A3, fill="condition")) + 
  geom_errorbar(aes(ymin=H1.A3-ci, ymax=H1.A3+ci), size=1.5, width=.1, position=pd) +
  geom_line(position=pd, size=1) +
  geom_point(position=pd, size=3.5) +
  ylim(25, 35) +
  xlab("Condition") + 
  ylab("H1-A3") + 
  geom_signif(comparisons = list(c("HC", "MCI")), annotation=formatC("*", digits=4),  textsize=6) +
  haristheme + scale_fill_grey(start = 0.9, end = 0.6)
H1_A3_ci_gplot

H1.A3m <- lm(H1.A3~condition*gender, data = d, na.action=na.exclude)
summary(H1.A3m)
plot_model(H1.A3m, type = "pred",  terms = c("condition", "gender"))+ggtitle("H1-A3")+  theme_minimal()
library(apaTables)
apa.reg.table(H1.A3m, filename = "Table3_APA.doc", table.number = 2)


# Cepstral Peak Prominence   ---------------------------------------------------------------
cppm <- lmer(cpp~condition*gender+(1|speaker) , data = d)
cppm1 <- lm(cpp~condition*gender, data = d)
AIC(cppm,cppm1)
summary(cppm)
summary(cppm1)
library(sjmisc)
theme_set(theme_sjplot())
cpp_plot_pred = plot_model(cppm, type = "pred",  terms = c("condition"))
grid.arrange(cpp_plot, cpp_plot_pred,nrow = 1)
apa.reg.table(cppm1, filename = "Table4_APA.doc", table.number = 4)

pd <- position_dodge(0.1) # move them .05 to the left and right
cpp_ci <- summarySE(d, measurevar="cpp", groupvars=c("condition"), na.rm=TRUE)
cpp_ci_gplot = ggplot(cpp_ci, aes(x=condition, y=cpp, fill="condition")) + 
  geom_errorbar(aes(ymin=cpp-ci, ymax=cpp+ci), size=1.5, width=.1, position=pd) +
  geom_line(position=pd, size=1) +
  geom_point(position=pd, size=3.5) +
  xlab("Condition") + 
  ylab("Cepstral Peak Prominence") + 
  ylim(69.5, 71.5) +
  geom_signif(comparisons = list(c("HC", "MCI")), annotation=formatC("**", digits=4),  textsize=6) +
  haristheme + scale_fill_grey(start = 0.9, end = 0.6)
cpp_ci_gplot

# center_of_gravity  ---------------------------------------------------------------
center_of_gravitym <- lmer( center_of_gravity~condition+(1|speaker) , data = d)
center_of_gravitym1 <- lm( center_of_gravity~condition, data = d)
summary(center_of_gravitym)
summary(center_of_gravitym1)
AIC(center_of_gravitym,center_of_gravitym1)

pd <- position_dodge(0.1) # move them .05 to the left and right
center_of_gravity_ci <- summarySE(d, measurevar="center_of_gravity", groupvars=c("condition"), na.rm=TRUE)
center_of_gravity_ci_gplot = ggplot(center_of_gravity_ci, aes(x=condition, y=center_of_gravity, fill="condition")) + 
  geom_errorbar(aes(ymin=center_of_gravity-ci, ymax=center_of_gravity+ci), size=1.5, width=.1, position=pd) +
  geom_line(position=pd, size=1) +
  geom_point(position=pd, size=3.5) +
  geom_signif(comparisons = list(c("HC", "MCI")), annotation=formatC("*", digits=4),  textsize=6) +
  ylim(380,1000) +
  xlab("Condition") + 
  ylab("Center of Gravity") + haristheme + scale_fill_grey(start = 0.9, end = 0.6)
center_of_gravity_ci_gplot

apa.reg.table(center_of_gravitym1, filename = "Table3_cog.doc", table.number = 4)


# Jitter  ---------------------------------------------------------------
jitterm <- lmer(jitter~condition+(1|speaker) , data = d) # interaction not possible 
summary(jitterm)
jitterm1 <- lm(jitter~condition, data = d)
summary(jitterm1)

# Shimmer  ---------------------------------------------------------------
pd <- position_dodge(0.1) # move them .05 to the left and right
shimmer_ci <- summarySE(d, measurevar="shimmer", groupvars=c("condition"), na.rm=TRUE)
shimmer_ci_gplot = ggplot(shimmer_ci, aes(x=condition, y=shimmer, fill="condition")) + 
  geom_errorbar(aes(ymin=shimmer-ci, ymax=shimmer+ci), size=1.5, width=.1, position=pd) +
  geom_line(position=pd, size=1) +
  geom_point(position=pd, size=3.5) +
  geom_signif(comparisons = list(c("HC", "MCI")), annotation=formatC("*", digits=4),  textsize=6) +
  ylim(0.10, 0.145) +
  xlab("Condition") + 
  ylab("Shimmer") + haristheme + scale_fill_grey(start = 0.9, end = 0.6)
shimmer_ci_gplot

shimmerm <- lmer(shimmer~condition*gender+(1|speaker) , data = d)
summary(shimmerm)

shimmerm1 <- lm(shimmer~condition, data = d)
summary(shimmerm1)
apa.reg.table(shimmerm1, filename = "Table3_shimmer.doc", table.number = 4)


# Harmonicity  ---------------------------------------------------------------
harmonicitym <- lmer(harmonicity~condition*gender+(1|speaker) , data = d)
summary(harmonicitym)
harmonicitym1 <- lmer(harmonicity~condition*gender, data = d)


################ FIGURE
library("multipanelfigure")
cols <- 2
rows <- 2
figure <- multi_panel_figure(
  width = 200,
  columns = cols,
  height = 200,
  rows = rows)

(figure %<>% fill_panel(H1_A3_ci_gplot))
(figure %<>% fill_panel(cpp_ci_gplot))
(figure %<>% fill_panel(center_of_gravity_ci_gplot))
(figure %<>% fill_panel(shimmer_ci_gplot))






