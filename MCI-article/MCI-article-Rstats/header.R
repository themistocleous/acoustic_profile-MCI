# Clear Memory
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
library(plyr)
# Printing
options(xtable.floating = FALSE)
options(xtable.timestamp = "")
options(digits=2)
options(scipen=999)

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