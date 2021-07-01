### Presidents Analysis ###
###
### This looks at the correlations of features between the presidents dataset
### this then applies False Discovery Rate and Familywise Error Rate to correct
### for the fact we are testing multiple hypotheses at the same time.

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

###--- Function to create a dataframe ---###
# Input - Dataframe Name
# Output - Dataframe
createDataFrame = function(){
  x = data.frame(feature = character(),
                 r2      = numeric(),
                 pvalue  = numeric(),
                 stringsAsFactors = FALSE)
  return(x)
}

###--- Function to run correlations ---###
# Input - Data and Dataframe to put results in
# Output - Dataframe with results
correlations = function(data, df){
  for(i in seq_along(names(data))){
    if(is.numeric(data[, i])){
      pearson = cor.test(data$Days, data[, i], method = "pearson")
      df = rbind(df, data.frame(feature = names(data)[i],
                                r2      = unname(pearson$estimate),
                                pvalue  = pearson$p.value,
                                stringsAsFactors = FALSE))
    }
  }
  return(df)
}

###--- Function to calculate Familywise Error Rates and False Discovery rates---###
# Input - dataframe with correlations
# Output - dataframe with added results for Familywise Error Rates and False Discovery rates
fdr = function(df){
  df$Bonferroni = p.adjust(df$pvalue, method = "bonferroni")
  df$BH = p.adjust(df$pvalue, method = "BH")
  df$holm = p.adjust(df$pvalue, method = "holm")
  df$hochberg = p.adjust(df$pvalue, method = "hochberg")
  df$hommel = p.adjust(df$pvalue, method = "hommel")
  df$BY = p.adjust(df$pvalue, method = "BY")
  return(df)
}

cleanData = function(data){
  data = removeBadFeatures(data)
  return(data)
}

preprocess = function(data){
  df = createDataFrame()
  df = correlations(data, df)
  df = df[order(df$pvalue),]
  df = fdr(df)
  return(df)
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

# Preprocess Data
dfReagan = preprocess(dataReagan)
dfBush = preprocess(dataBush)
dfTrump = preprocess(dataTrump)

# Scale the Data
sdfReagan = scale(dataReagan[,2:ncol(dataReagan)])
sdfReagan = cbind(dataReagan[, 1], sdfReagan)
colnames(sdfReagan)[colnames(sdfReagan) == ''] <- 'Days'

# Generate a list of features we are interested in
# First only look at the features which are significant
subsetReagan = filter(dfReagan, hommel<0.05, BY<0.05, feature!='Days')
subsetBush = filter(dfBush, hommel<0.05, BY<0.05, feature!='Days')
subsetReagan = rbind(subsetReagan, filter(dfReagan, BY<0.05, hommel>0.05, feature!='Days'))
subsetBush = rbind(subsetBush, filter(dfBush, BY<0.05, hommel>0.05,  feature!='Days'))
# Note: Running this on Trump generates an empty set

# Then generate a vector of features 
bushFeatures = subsetBush$feature
reaganFeatures = subsetReagan$feature

###--- PLOTS ---###
# Reagan
XReagan = dfReagan$pvalue
YReagan = cbind(dfReagan$Bonferroni, dfReagan$BH, dfReagan$holm, dfReagan$hochberg,
                dfReagan$hommel, dfReagan$BY)
matplot(XReagan, YReagan, xlab = "Reagan Raw p-value", ylab = "Adjusted p-value", type = 'l', asp = 1,
        col = 1:6, lty = 1, lwd = 2)
legend('bottomright', legend = c('Bonferroni', 'BH', 'Holm', 'Hochberg', 'Hommel', 'BY'),
       col = 1:6, cex = 1, pch = 16)
abline(0, 1, col = 1, lty = 2, lwd = 1)

# Bush
XBush = dfBush$pvalue
YBush = cbind(dfBush$Bonferroni, dfBush$BH, dfBush$holm, dfBush$hochberg,
              dfBush$hommel, dfBush$BY)
matplot(XBush, YBush, xlab = "Bush Raw p-value", ylab = "Adjusted p-value", type = 'l', asp = 1,
        col = 1:6, lty = 1, lwd = 2)
legend('bottomright', legend = c('Bonferroni', 'BH', 'Holm', 'Hochberg', 'Hommel', 'BY'),
       col = 1:6, cex = 1, pch = 16)
abline(0, 1, col = 1, lty = 2, lwd = 1)

# Trump
XTrump = dfTrump$pvalue
YTrump = cbind(dfTrump$Bonferroni, dfTrump$BH, dfTrump$holm, dfTrump$hochberg,
               dfTrump$hommel, dfTrump$BY)
matplot(XTrump, YTrump, xlab = "Trump Raw p-value", ylab = "Adjusted p-value",
        type = 'l', asp = 1,
        col = 1:6, lty = 1, lwd = 2)
legend('bottomright', legend = c('Bonferroni', 'BH', 'Holm', 'Hochberg', 'Hommel', 'BY'),
       col = 1:6, cex = 1, pch = 16)
abline(0, 1, col = 1, lty = 2, lwd = 1)

