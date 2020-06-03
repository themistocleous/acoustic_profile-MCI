# Clear Memory
# Charalambos Themistocleous 2019
# We run preprocess on the output file from the python Script
# the file is titled "data2020.csv"
#
#### It is content function word ratio not the other way around

rm(list = ls(all.names = TRUE))

#sink("vowel_stats_output_11_Aug_2018.txt")
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
a <- read.csv("BID_final.csv")
str(a)

d <- a[!a$condition=="SCI",]
d <- d[!d$File == "LABM-75GG-5KH4",] 
d <- d[!d$File == "79CJ-M7A3-FALX",] 
d <- d[!d$File == "TXLH-R239-CPQG",]

d <- droplevels(d)

# H1 H2 ---------------------------------------------------------------
h1h2p1 = ggplot(d, aes(x = condition, y = h1.h2, fill = gender)) + 
  # stat_summary(fun.y = mean, geom = "point") +
  geom_boxplot( outlier.shape = NA) +
  ggtitle("H1-H2") +
  scale_x_discrete(name = "PPA conditions")  + 
  scale_fill_brewer(palette = "Blues") +
  scale_y_continuous(limits = quantile(d$h1.h2, c(0.1, 0.9))) +
  labs(
    fill = "condition",
    x = "PPA conditions",
    y = "H1-H2"
  ) +  theme_minimal() +
  theme(legend.position="top", legend.title = element_blank(), text = element_text(size=20), plot.title = element_text(hjust = 0.5),panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 
h1h2p1

hist(d$h1.h2)

h1h2m <- lm(h1.h2 ~ condition*gender, data = d)
summary(h1h2m)
h1h2p2 = plot_model(h1h2m, type = "pred",  terms = c("condition")) + 
  ggtitle("H1-H2") + 
  labs(
    fill = "condition",
    x = "PPA conditions",
    y = "Predicted H1-H2 values"
  ) +  theme_minimal() +
  theme(legend.position="top", legend.title = element_blank(), text = element_text(size=20), plot.title = element_text(hjust = 0.5),panel.grid.major = element_blank(), panel.grid.minor = element_blank())
h1h2p2 


#  Formant-Adjusted H1-H2  ---------------------------------------------------------------
formh1h2p1 = ggplot(d, aes(x = condition, y = h1.h2, fill = gender)) + 
  stat_summary(fun.y = mean, geom = "point") +
  geom_boxplot( outlier.shape = NA) +
  ggtitle("Formant-Adjusted H1-H2") +
  scale_x_discrete(name = "PPA conditions")  + 
  scale_fill_brewer(palette = "Blues") +
  scale_y_continuous(limits = quantile(d$h1h2, c(0.1, 0.9))) +
  labs(
    fill = "condition",
    x = "PPA conditions",
    y = "Formant-Adjusted H1-H2"
  ) +   theme_minimal() +
  theme(legend.position="top", legend.title = element_blank(), text = element_text(size=20), plot.title = element_text(hjust = 0.5),panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 
formh1h2p1



Bh1h2m <- lm(h1.h2~condition*gender, data = d)
summary(Bh1h2m)
plot_model(h1h2m, type = "pred",  terms = c("condition"))


# H1.A1  ---------------------------------------------------------------
H1A1plot = ggplot(d, aes(x = condition, y = H1.A1, fill = gender)) + 
  #stat_summary(fun.y = mean, geom = "point") +
  geom_boxplot( outlier.shape = NA) +
  ggtitle("H1-A1") +
  scale_x_discrete(name = "PPA conditions")  + 
  scale_fill_brewer(palette = "Blues") +
  scale_y_continuous(limits = quantile(d$h1.h2, c(0.1, 0.9))) +
  labs(
    fill = "condition",
    x = "PPA conditions",
    y = "H1-A1"
  ) +  theme_minimal() +
  theme(legend.position="top", legend.title = element_blank(), text = element_text(size=20), plot.title = element_text(hjust = 0.5),panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 
H1A1plot

H1.A1m <- lm(H1.A1~condition*gender, data = d)
summary(H1.A1m)
library(sjmisc)
theme_set(theme_sjplot())
plot_model(H1.A1m, type = "pred",  terms = c("condition","gender"))


# H1.A3  ---------------------------------------------------------------
H1A3plot = ggplot(d, aes(x = condition, y = H1.A3, fill = gender)) + 
  #  stat_summary(fun.y = mean, geom = "point") +
  geom_boxplot( outlier.shape = NA) +
  ggtitle("H1-A3") +
  scale_x_discrete(name = "PPA conditions")  + 
  scale_fill_brewer(palette = "Blues") +
  #scale_y_continuous(limits = quantile(d$h1.h2, c(0.1, 0.9))) +
  labs(
    fill = "condition",
    x = "PPA conditions",
    y = "H1-A3"
  ) +  theme_minimal() +
  theme(legend.position="top", legend.title = element_blank(), text = element_text(size=20), plot.title = element_text(hjust = 0.5),panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 

H1A3plot

boxplot(H1.A3~condition*gender, data=d)


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


grid.arrange(h1h2p1, H1A1plot,H1A3plot,nrow=1, ncol=3)




# Putting plots together -----------------------------------------------


# Cepstral Peak Prominence   ---------------------------------------------------------------
# SIGNIFICANT

cpp_plot = ggplot(d, aes(x = condition, y = cpp, fill = gender)) + 
  #stat_summary(fun.y = mean, geom = "point") +
  geom_boxplot( outlier.shape = NA) +
  ggtitle("Cepstral Peak Prominence ") +
  scale_x_discrete(name = "PPA conditions")  + 
  scale_fill_brewer(palette = "Blues") +
  scale_y_continuous(limits = quantile(d$cpp, c(0.1, 0.9))) +
  labs(
    fill = "condition",
    x = "PPA conditions",
    y = "Cepstral Peak Prominence"
  ) +  theme_minimal() +
  theme(legend.position="top", legend.title = element_blank(), text = element_text(size=20), plot.title = element_text(hjust = 0.5),panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 

cpp_plot

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
cogplot= ggplot(d, aes(x = condition, y = center_of_gravity, fill = gender)) + 
  #stat_summary(fun.y = mean, geom = "point") +
  geom_boxplot( outlier.shape = NA) +
  ggtitle("Center of Gravity") +
  scale_x_discrete(name = "PPA conditions")  + 
  scale_fill_brewer(palette = "Blues") +
  scale_y_continuous(limits = quantile(d$center_of_gravity, c(0.1, 0.9))) +
  labs(
    fill = "condition",
    x = "PPA conditions",
    y = "Center of Gravity"
  ) + 
  theme(legend.position="top", legend.title = element_blank(), text = element_text(size=20), plot.title = element_text(hjust = 0.5),panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 
cogplot

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






library(sjmisc)
theme_set(theme_sjplot())
plot_model(center_of_gravitym, type = "pred",  terms = c("condition"))


# Hammarberg_index  ---------------------------------------------------------------
hiplot = ggplot(d, aes(x = condition, y = Hammarberg_index, fill = gender)) + 
  #stat_summary(fun.y = mean, geom = "point") +
  geom_boxplot( outlier.shape = NA) +
  ggtitle("Hammarberg Index") +
  scale_x_discrete(name = "PPA conditions")  + 
  scale_fill_brewer(palette = "Blues") +
  #scale_y_continuous(limits = quantile(d$Hammarberg_index, c(0, 0.01))) +
  labs(
    fill = "condition",
    x = "PPA conditions",
    y = "Hammarberg Index"
  ) + 
  theme(legend.position="top", legend.title = element_blank(), text = element_text(size=20), plot.title = element_text(hjust = 0.5),panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 
hiplot



Hammarberg_indexm <- lmer(Hammarberg_index~condition*gender+(1|speaker) , data = d)
Hammarberg_indexm1 <- lm(Hammarberg_index~condition, data = d)
AIC(Hammarberg_indexm, Hammarberg_indexm1)
summary(Hammarberg_indexm)
summary(Hammarberg_indexm1)
library(sjmisc)
theme_set(theme_sjplot())
plot_model(Hammarberg_indexm, type = "pred",  terms = c("condition"))


# Energy_Profile__250Hz  ---------------------------------------------------------------
e250plot=ggplot(d, aes(x = condition, y = Energy_Profile__250Hz, fill = gender)) + 
  #  stat_summary(fun.y = mean, geom = "point") +
  geom_boxplot( outlier.shape = NA) +
  ggtitle("Energy below 250Hz") +
  scale_x_discrete(name = "PPA conditions")  + 
  scale_fill_brewer(palette = "Blues") +
  #scale_y_continuous(limits = (c(0, 1))) +
  labs(
    fill = "condition",
    x = "PPA conditions",
    y = "Energy below 250Hz"
  ) +  theme_minimal() +
  theme(legend.position="top", legend.title = element_blank(), text = element_text(size=20), plot.title = element_text(hjust = 0.5),panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 
e250plot

Energy_Profile__250Hzm <- lmer(Energy_Profile__250Hz~condition*gender+(1|speaker) , data = d)
summary(Energy_Profile__250Hzm)
library(sjmisc)
theme_set(theme_sjplot())
plot_model(Energy_Profile__250Hzm, type = "pred",  terms = c("condition"))

# Energy_below_500Hz  ---------------------------------------------------------------
e500plot=ggplot(d, aes(x = condition, y = energy_below_500Hz, fill = gender)) + 
  #  stat_summary(fun.y = mean, geom = "point") +
  geom_boxplot( outlier.shape = NA) +
  ggtitle("Energy below 500Hz") +
  scale_x_discrete(name = "PPA conditions")  + 
  scale_fill_brewer(palette = "Blues") +
  scale_y_continuous(limits = (c(0, 1))) +
  labs(
    fill = "condition",
    x = "PPA conditions",
    y = "Energy below 500Hz"
  ) +  theme_minimal() +
  theme(legend.position="top", legend.title = element_blank(), text = element_text(size=20), plot.title = element_text(hjust = 0.5),panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 
e500plot


energy_below_500Hzm <- lmer(energy_below_500Hz~condition*gender+(1|speaker) , data = d)
energy_below_500Hzm1 <- lm(energy_below_500Hz~condition*gender, data = d)
summary(energy_below_500Hzm)
summary(energy_below_500Hzm1)
AIC(energy_below_500Hzm,energy_below_500Hzm1)
library(sjmisc)
theme_set(theme_sjplot())
plot_model(energy_below_500Hzm, type = "pred",  terms = c("condition"))


# Energy_below_1000Hz  ---------------------------------------------------------------
e1000plot=ggplot(d, aes(x = condition, y = energy_below_1000Hz, fill = gender)) + 
  #stat_summary(fun.y = mean, geom = "point") +
  geom_boxplot( outlier.shape = NA) +
  ggtitle("Energy below 1000Hz") +
  scale_x_discrete(name = "PPA conditions")  + 
  scale_fill_brewer(palette = "Blues") +
  #scale_y_continuous(limits = quantile(d$energy_below_1000Hz, c(0, 1))) +
  labs(
    fill = "condition",
    x = "PPA conditions",
    y = "Energy below 1000Hz"
  ) + 
  theme(legend.position="top", legend.title = element_blank(), text = element_text(size=20), plot.title = element_text(hjust = 0.5),panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 
e1000plot


energy_below_1000Hzm <- lmer(energy_below_1000Hz~condition*gender+(1|speaker) , data = d)
energy_below_1000Hzm1 <- lm(energy_below_1000Hz~condition*gender, data = d)
summary(energy_below_1000Hzm)
summary(energy_below_1000Hzm1)
AIC(energy_below_1000Hzm,energy_below_1000Hzm1)
library(sjmisc)
theme_set(theme_sjplot())
plot_model(energy_below_1000Hzm, type = "pred",  terms = c("condition"))


# F_dispersion1_3  ---------------------------------------------------------------
ggplot(d, aes(x = condition, y = F_dispersion1_3, fill = gender)) + 
  stat_summary(fun.y = mean, geom = "point") +
  geom_boxplot( outlier.shape = NA) +
  ggtitle("Average Distance from F1 to F3") +
  scale_x_discrete(name = "PPA conditions")  + 
  scale_fill_brewer(palette = "Blues") +
  #  scale_y_continuous(limits = quantile(d$h1.h2, c(0, 1))) +
  labs(
    fill = "condition",
    x = "PPA conditions",
    y = "Average Distance from F1 to F3"
  ) + 
  theme(legend.position="top", legend.title = element_blank(), text = element_text(size=20), plot.title = element_text(hjust = 0.5),panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 




F_dispersion1_3m <- lmer(F_dispersion1_3~condition+(1|speaker) , data = d)
summary(F_dispersion1_3m)
library(sjmisc)
theme_set(theme_sjplot())
plot_model(F_dispersion1_3m, type = "pred",  terms = c("condition"))



# F_dispersion1_5  ---------------------------------------------------------------
ggplot(d, aes(x = condition, y = F_dispersion1_5, fill = gender)) + 
  stat_summary(fun.y = mean, geom = "point") +
  geom_boxplot( outlier.shape = NA) +
  ggtitle("Average Distance from F1 to F5") +
  scale_x_discrete(name = "PPA conditions")  + 
  scale_fill_brewer(palette = "Blues") +
  #  scale_y_continuous(limits = quantile(d$h1.h2, c(0, 1))) +
  labs(
    fill = "condition",
    x = "PPA conditions",
    y = "Average Distance from F1 to F5"
  ) + 
  theme(legend.position="top", legend.title = element_blank(), text = element_text(size=20), plot.title = element_text(hjust = 0.5),panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 

F_dispersion1_5m <- lmer(F_dispersion1_5~condition+(1|speaker) , data = d)
summary(F_dispersion1_5m)
library(sjmisc)
theme_set(theme_sjplot())
plot_model(F_dispersion1_5m, type = "pred",  terms = c("condition"))


grid.arrange(h1h2p1, H1A1plot, H1A3plot, formh1h2p1, cpp_plot, cogplot, hiplot, e500plot, e1000plot, nrow = 3, ncol=4)


grid.arrange(cpp_plot, cogplot, hiplot, e250plot, e500plot, e1000plot, nrow = 2, ncol=3)


# median_pitch  ---------------------------------------------------------------
median_pitchp1 = ggplot(d, aes(x = condition, y = median_pitch, fill = gender)) + 
  # stat_summary(fun.y = mean, geom = "point") +
  geom_boxplot( outlier.shape = NA) +
  ggtitle("Median Pitch") +
  scale_x_discrete(name = "PPA conditions")  + 
  scale_fill_brewer(palette = "Blues") +
  #scale_y_continuous(limits = c(0, 400)) +
  labs(
    fill = "condition",
    x = "PPA conditions",
    y = "Median Pitch"
  ) + 
  theme(legend.position="top", legend.title = element_blank(), text = element_text(size=20), plot.title = element_text(hjust = 0.5),panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 
median_pitchp1


median_pitchm <- lmer(median_pitch~condition*gender+(1|speaker), data = d)
summary(median_pitchm)
median_pitchm1 <- lm(median_pitch~condition*gender, data = d)
summary(median_pitchm1)

median_pitchp2 = plot_model(median_pitchm, type = "pred",  terms = c("condition")) +
  ggtitle("Predicted Median Pitch") +
  scale_x_discrete(name = "PPA conditions")  + 
  scale_y_continuous(limits = c(0, 400)) +
  scale_fill_brewer(palette = "Blues") +
  labs(
    fill = "condition",
    x = "PPA conditions",
    y = "Predicted Median Pitch"
  ) + 
  theme(legend.position="top", legend.title = element_blank(), text = element_text(size=20), plot.title = element_text(hjust = 0.5),panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 
median_pitchp2

grid.arrange(median_pitchp1, median_pitchp2, nrow = 1)

# Jitter  ---------------------------------------------------------------

jitterplot = ggplot(d, aes(x = condition, y = jitter, fill = gender)) + 
  #  stat_summary(fun.y = mean, geom = "point") +
  geom_boxplot( outlier.shape = NA) +
  ggtitle("Jitter") +
  scale_x_discrete(name = "PPA conditions")  + 
  scale_fill_brewer(palette = "Blues") +
  scale_y_continuous(limits = c(0,0.10)) +
  labs(
    fill = "condition",
    x = "PPA conditions",
    y = "Jitter"
  ) + 
  theme(legend.position="top", legend.title = element_blank(), text = element_text(size=20), plot.title = element_text(hjust = 0.5),panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 
jitterplot

jitterm <- lmer(jitter~condition+(1|speaker) , data = d) # interaction not possible 
summary(jitterm)
jitterm1 <- lm(jitter~condition, data = d)
summary(jitterm1)

library(sjmisc)
theme_set(theme_sjplot())
plot_model(jitterm, type = "pred",  terms = c("condition"))


# Shimmer  ---------------------------------------------------------------
shimmerplot = ggplot(d, aes(x = condition, y = shimmer, fill = gender)) + 
  #  stat_summary(fun.y = mean, geom = "point") +
  geom_boxplot( outlier.shape = NA) +
  ggtitle("Shimmer") +
  scale_x_discrete(name = "PPA conditions")  + 
  scale_fill_brewer(palette = "Blues") +
  scale_y_continuous(limits = c(0, 0.3)) +
  labs(
    fill = "condition",
    x = "PPA conditions",
    y = "Shimmer"
  ) + 
  theme(legend.position="top", legend.title = element_blank(), text = element_text(size=20), plot.title = element_text(hjust = 0.5),panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 
shimmerplot



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




library(sjmisc)
theme_set(theme_sjplot())
plot_model(shimmerm, type = "pred",  terms = c("condition"))



# Harmonicity  ---------------------------------------------------------------
harmonicityplot = ggplot(d, aes(x = condition, y = harmonicity, fill = gender)) + 
  #  stat_summary(fun.y = mean, geom = "point") +
  geom_boxplot( outlier.shape = NA) +
  ggtitle("Harmonicity") +
  scale_x_discrete(name = "PPA conditions")  + 
  scale_fill_brewer(palette = "Blues") +
  #  scale_y_continuous(limits = quantile(d$h1.h2, c(0, 0.3))) +
  labs(
    fill = "condition",
    x = "PPA conditions",
    y = "Harmonicity"
  ) + 
  theme(legend.position="top", legend.title = element_blank(), text = element_text(size=20), plot.title = element_text(hjust = 0.5),panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 
harmonicityplot 


harmonicitym <- lmer(harmonicity~condition*gender+(1|speaker) , data = d)
summary(harmonicitym)

harmonicitym1 <- lmer(harmonicity~condition*gender, data = d)
summary(harmonicitym1)

library(sjmisc)
theme_set(theme_sjplot())
plot_model(harmonicitym, type = "pred",  terms = c("condition"))

grid.arrange(jitterplot, shimmerplot, harmonicityplot, ncol=3, nrow=1)

# Energy_Profile__250Hz  ---------------------------------------------------------------
e250plot = ggplot(d, aes(x = condition, y = Energy_Profile__250Hz, fill = gender)) + 
  scale_fill_brewer(palette = "Blues") + 
  # stat_summary(fun.y = mean, geom = "point") +
  geom_boxplot( outlier.shape = NA)
e250plot

Energy_Profile__250Hzm <- lmer(Energy_Profile__250Hz~condition*gender+(1|speaker) , data = d)
summary(Energy_Profile__250Hzm)
library(sjmisc)
theme_set(theme_sjplot())
plot_model(Energy_Profile__250Hzm, type = "pred",  terms = c("condition"))


# Energy_below_1000Hz  ---------------------------------------------------------------
eb1000 = ggplot(d, aes(x = condition, y = energy_below_1000Hz, fill = condition)) + 
  scale_fill_brewer(palette = "Blues") + 
  stat_summary(fun.y = mean, geom = "point") +
  geom_boxplot( outlier.shape = NA)
eb1000

grid.arrange(cpp_plot, cogplot, hiplot, e250plot, e500plot, e1000plot, eb1000, nrow = 4)

energy_below_1000Hzm <- lmer(energy_below_1000Hz~condition+(1|speaker) , data = d)
summary(energy_below_1000Hzm)
library(sjmisc)
theme_set(theme_sjplot())
plot_model(energy_below_1000Hzm, type = "pred",  terms = c("condition"))




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






