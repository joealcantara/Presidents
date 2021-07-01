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


df = dataReagan
df2 = df
df = filter(dataReagan, Days > 700 | Days < 1)
df2 = filter(dataReagan, Days > 700 | Days < 37 & Days > 2)
df = df %>% select(Days, ppron)
df2 = df2 %>% select(Days, ppron)
df$diff = df$ppron - 8.86
df2$diff = df2$ppron - 9.44

df$status = 'stable'
df$status[df$diff > 0.10] = 'declining' 
df$status[df$diff < -0.10] = 'improving'

df2$status = 'stable'
df2$status[df2$diff > 0.10] = 'declining' 
df2$status[df2$diff < -0.10] = 'improving'

ggplot(df, aes(x=Days, y=ppron)) + 
  geom_point() + 
  geom_segment(aes(x = 0, y = 8.83, xend = df$Days, yend = df$ppron, colour = status), data = df) +
  ggtitle("Comparison 1")
ggsave('comp1.png')

ggplot(df2, aes(x=Days, y=ppron)) + 
  geom_point() + 
  geom_segment(aes(x = 36, y = 9.44, xend = df2$Days, yend = df2$ppron, colour = status), data = df2) +
  ggtitle("Comparison 2")
ggsave('comp2.png')