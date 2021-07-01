### Presidents Analysis ###
###
### This compares linear models with glm non linear models to see whether
### non linear models are a better fit for the tracking of decline vs linear models

###--- SETUP ---###
# Clear Workspace
rm(list=ls())

# Options
options(scipen=999)

# Set Directory
setwd("C:/Users/Joe/Documents/Work/Presidents")

# Libraries
library(dplyr)
library(ggplot2)
library(mgcv)
library(stats)
library(neuralnet)
library(reticulate)

###--- Function to remove features ---###
# Input - Dataset
# Output - Dataset cleaned 
removeBadFeatures = function(df){
  df$JDate = NULL
  df$X = NULL
  df$index = NULL
  df$Julian = NULL
  df$Filename = NULL
  df$Date = NULL
  return(df)
}

###--- Pipeline Function to clean data ---###
# Input - Dataset
# Output - Calls a number of functions that transform and clean the data.
cleanData = function(data){
  data = removeBadFeatures(data)
  return(data)
}

###--- IMPORT AND PREPROCESS DATA ---###
# Load Data
dataBush <- read.csv("~/Work/Presidents/Data/Bush.csv")
dataReagan <- read.csv("~/Work/Presidents/Data/Reagan.csv")
dataTrump <- read.csv("~/Work/Presidents/Data/Trump.csv")

# Clean Data 
dataReagan = cleanData(dataReagan)
dataBush = cleanData(dataBush)
dataTrump = cleanData(dataTrump)

# T-tests
t.test(dataReagan$UniqueStems, dataBush$UniqueStems, alternative = "two.sided")
t.test(dataReagan$UniqueStems, dataTrump$UniqueStems, alternative = "two.sided")
t.test(dataBush$UniqueStems, dataTrump$UniqueStems, alternative = "two.sided")

t.test(dataReagan$MLU, dataBush$MLU, alternative = "two.sided")
t.test(dataReagan$MLU, dataTrump$MLU, alternative = "two.sided")
t.test(dataBush$MLU, dataTrump$MLU, alternative = "two.sided")

# Means and SD
summary(dataReagan$MLU)
sd(dataReagan$MLU)
summary(dataBush$MLU)
sd(dataBush$MLU)
summary(dataTrump$MLU)
sd(dataTrump$MLU)

loessTest = loess(ppron ~ Days, data=dataReagan)
res = loessTest$residuals
sse = sum(res^2)
plot(loessTest)

gamModel1 = gam(ppron ~ s(Days, bs="gp"), data = dataReagan, family = Gamma(link="log"), method="REML")
coef(gam_mod)
plot(gam_mod, residuals = TRUE, pch = 1)

res = gamModel1$residuals
sse = sum(res^2)
summary.gam(gamModel1)

fit = gamModel1$fitted.values
gam.check(gamModel1, pch=19, cex=.3)
plot.gam(gamModel1)
vis.gam(gamModel1)


