### Final Presidents Analysis ###

###--- SETUP ---###
# Clear Workspace
rm(list=ls())

# Options
options(scipen=999)

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
dataBush = read.csv("Bush.csv")
dataReagan = read.csv("Reagan.csv")
dataTrump = read.csv("Trump.csv")

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

# Load Data
results = numeric(length(dataReagan))
totalResults = data.frame(feature = character (),
                          gamPRESS = numeric(),
                          lmPRESS = numeric())

# for (var in 1:length(reaganFeatures)){
#   tmp <- as.formula(paste0(reaganFeatures[var], ' ~ s(Days, bs = "gp")'))
#   tmp2 = as.formula(paste0(reaganFeatures[var],))
#   for (i in 1:(nrow(df))){
#     gamModel = gam(tmp, 
#                    data = df[-i, ], 
#                    family = Gamma(link = "log"), 
#                    method = "REML")
#     pred = predict.gam(gamModel, df[i, ], 
#                        type = "response")
#     results[i] = (as.numeric(pred) - tmp2) ^ 2
#     store = (as.numeric(pred) - tmp2) ^ 2
#   }
#   totalResults[i] = sum(store)
# }
# PPRON Feature
for (i in 1:(nrow(dataReagan))){
  gamModel = gam(ppron ~ s(Days, bs = "gp"), 
                 data = dataReagan[-i, ], 
                 family = Gamma(link = "log"), 
                 method = "REML")
  pred = predict.gam(gamModel, dataReagan[i, ], 
                     type = "response")
  results[i] = (as.numeric(pred) - dataReagan$ppron[i]) ^ 2
}
# PRESS
gamPress = sum(results)

lmresults = numeric(nrow(dataReagan))
for (i in 1:(nrow(dataReagan))){
  linearModel = lm(ppron ~ Days, data = dataReagan[-i, ])
  pred = predict.lm(linearModel, dataReagan[i, ], type = "response")
  lmresults[i] = (as.numeric(pred) - dataReagan$ppron[i]) ^ 2
}
# PRESS
lmPress = sum(lmresults)
sdfReagan = as.data.frame(sdfReagan)

# nnresults = numeric(nrow(dataReagan))
# for (i in 1:(nrow(sdfReagan))){
#   train = sdfReagan[-i, ]
#   test = sdfReagan[i,]
#   nn = neuralnet(ppron ~ Days, data = train, hidden=c(2,2))
#   pr.nn <- compute(nn, train)
#   pr.nn_ <- pr.nn$net.result*(max(train$ppron)-min(train$ppron))+min(train$ppron)
#   test.r <- (test$ppron)*(max(test$ppron)-min(test$ppron))+min(test$ppron)
#   nnresults[i] = (test.r -pr.nn_)^2
# }
# nnPress = (sum(nnresults))

# plot(nn)
# pr.nn <- compute(nn, df[i, ])
# pr.nn_ <- pr.nn$net.result*(max(testData$ppron)-min(testData$ppron))+min(testData$ppron)
# test.r <- (predData$ppron)*(max(predData$ppron)-min(predData$ppron))+min(predData$ppron)
# MSE.nn <- sum((test.r - pr.nn_)^2)/nrow(predData)

vectortmp = data.frame("ppron",gamPress, lmPress)
names(vectortmp) = c('feature','gamPRESS', 'lmPRESS')
totalResults = rbind(totalResults, vectortmp)

df = dataReagan


### Social Feature
for (i in 1:(nrow(df))){
  gamModel = gam(social ~ s(Days, bs = "gp"), 
                 data = df[-i, ], 
                 family = Gamma(link = "log"), 
                 method = "REML")
  pred = predict.gam(gamModel, df[i, ], 
                     type = "response")
  results[i] = (as.numeric(pred) - df$social[i]) ^ 2
}
# PRESS
gamPress = sum(results)

lmresults = numeric(nrow(df))
for (i in 1:(nrow(df))){
  linearModel = lm(social ~ Days, data = df[-i, ])
  pred = predict.lm(linearModel, df[i, ], type = "response")
  lmresults[i] = (as.numeric(pred) - df$social[i]) ^ 2
}
# PRESS
lmPress = sum(lmresults)

vectortmp = data.frame("social",gamPress, lmPress)
names(vectortmp) = c('feature','gamPRESS', 'lmPRESS')
totalResults = rbind(totalResults, vectortmp)

# Nouns Normalised Feature
for (i in 1:(nrow(df))){
  gamModel = gam(NounsNormalised ~ s(Days, bs = "gp"), 
                 data = df[-i, ], 
                 family = Gamma(link = "log"), 
                 method = "REML")
  pred = predict.gam(gamModel, df[i, ], 
                     type = "response")
  results[i] = (as.numeric(pred) - df$NounsNormalised[i]) ^ 2
}
# PRESS
gamPress = sum(results)

lmresults = numeric(nrow(df))
for (i in 1:(nrow(df))){
  linearModel = lm(NounsNormalised ~ Days, data = df[-i, ])
  pred = predict.lm(linearModel, df[i, ], type = "response")
  lmresults[i] = (as.numeric(pred) - df$NounsNormalised[i]) ^ 2
}
# PRESS
lmPress = sum(lmresults)

vectortmp = data.frame("NounsNormalised",gamPress, lmPress)
names(vectortmp) = c('feature','gamPRESS', 'lmPRESS')
totalResults = rbind(totalResults, vectortmp)

# function. Feature
for (i in 1:(nrow(df))){
  gamModel = gam(function. ~ s(Days, bs = "gp"), 
                 data = df[-i, ], 
                 family = Gamma(link = "log"), 
                 method = "REML")
  pred = predict.gam(gamModel, df[i, ], 
                     type = "response")
  results[i] = (as.numeric(pred) - df$function.[i]) ^ 2
}
# PRESS
gamPress = sum(results)

lmresults = numeric(nrow(df))
for (i in 1:(nrow(df))){
  linearModel = lm(function. ~ Days, data = df[-i, ])
  pred = predict.lm(linearModel, df[i, ], type = "response")
  lmresults[i] = (as.numeric(pred) - df$function.[i]) ^ 2
}
# PRESS
lmPress = sum(lmresults)

vectortmp = data.frame("function.",gamPress, lmPress)
names(vectortmp) = c('feature','gamPRESS', 'lmPRESS')
totalResults = rbind(totalResults, vectortmp)

# conj Feature
for (i in 1:(nrow(df))){
  gamModel = gam(conj ~ s(Days, bs = "gp"), 
                 data = df[-i, ], 
                 family = Gamma(link = "log"), 
                 method = "REML")
  pred = predict.gam(gamModel, df[i, ], 
                     type = "response")
  results[i] = (as.numeric(pred) - df$conj[i]) ^ 2
}
# PRESS
gamPress = sum(results)

lmresults = numeric(nrow(df))
for (i in 1:(nrow(df))){
  linearModel = lm(conj ~ Days, data = df[-i, ])
  pred = predict.lm(linearModel, df[i, ], type = "response")
  lmresults[i] = (as.numeric(pred) - df$conj[i]) ^ 2
}
# PRESS
lmPress = sum(lmresults)

vectortmp = data.frame("conj",gamPress, lmPress)
names(vectortmp) = c('feature','gamPRESS', 'lmPRESS')
totalResults = rbind(totalResults, vectortmp)

# PronounsNormalised Feature
for (i in 1:(nrow(df))){
  gamModel = gam(PronounsNormalised ~ s(Days, bs = "gp"), 
                 data = df[-i, ], 
                 family = Gamma(link = "log"), 
                 method = "REML")
  pred = predict.gam(gamModel, df[i, ], 
                     type = "response")
  results[i] = (as.numeric(pred) - df$PronounsNormalised[i]) ^ 2
}
# PRESS
gamPress = sum(results)

lmresults = numeric(nrow(df))
for (i in 1:(nrow(df))){
  linearModel = lm(PronounsNormalised ~ Days, data = df[-i, ])
  pred = predict.lm(linearModel, df[i, ], type = "response")
  lmresults[i] = (as.numeric(pred) - df$PronounsNormalised[i]) ^ 2
}
# PRESS
lmPress = sum(lmresults)

vectortmp = data.frame("PronounsNormalised",gamPress, lmPress)
names(vectortmp) = c('feature','gamPRESS', 'lmPRESS')
totalResults = rbind(totalResults, vectortmp)


# Analytic Feature
for (i in 1:(nrow(df))){
  gamModel = gam(Analytic ~ s(Days, bs = "gp"), 
                 data = df[-i, ], 
                 family = Gamma(link = "log"), 
                 method = "REML")
  pred = predict.gam(gamModel, df[i, ], 
                     type = "response")
  results[i] = (as.numeric(pred) - df$Analytic[i]) ^ 2
}
# PRESS
gamPress = sum(results)

lmresults = numeric(nrow(df))
for (i in 1:(nrow(df))){
  linearModel = lm(Analytic ~ Days, data = df[-i, ])
  pred = predict.lm(linearModel, df[i, ], type = "response")
  lmresults[i] = (as.numeric(pred) - df$Analytic[i]) ^ 2
}
# PRESS
lmPress = sum(lmresults)

vectortmp = data.frame("Analytic",gamPress, lmPress)
names(vectortmp) = c('feature','gamPRESS', 'lmPRESS')
totalResults = rbind(totalResults, vectortmp)

# pronoun Feature
for (i in 1:(nrow(df))){
  gamModel = gam(pronoun ~ s(Days, bs = "gp"), 
                 data = df[-i, ], 
                 family = Gamma(link = "log"), 
                 method = "REML")
  pred = predict.gam(gamModel, df[i, ], 
                     type = "response")
  results[i] = (as.numeric(pred) - df$pronoun[i]) ^ 2
}
# PRESS
gamPress = sum(results)

lmresults = numeric(nrow(df))
for (i in 1:(nrow(df))){
  linearModel = lm(pronoun ~ Days, data = df[-i, ])
  pred = predict.lm(linearModel, df[i, ], type = "response")
  lmresults[i] = (as.numeric(pred) - df$pronoun[i]) ^ 2
}
# PRESS
lmPress = sum(lmresults)

vectortmp = data.frame("pronoun",gamPress, lmPress)
names(vectortmp) = c('feature','gamPRESS', 'lmPRESS')
totalResults = rbind(totalResults, vectortmp)

# NN Feature
for (i in 1:(nrow(df))){
  gamModel = gam(NN ~ s(Days, bs = "gp"), 
                 data = df[-i, ], 
                 family = Gamma(link = "log"), 
                 method = "REML")
  pred = predict.gam(gamModel, df[i, ], 
                     type = "response")
  results[i] = (as.numeric(pred) - df$NN[i]) ^ 2
}
# PRESS
gamPress = sum(results)

lmresults = numeric(nrow(df))
for (i in 1:(nrow(df))){
  linearModel = lm(NN ~ Days, data = df[-i, ])
  pred = predict.lm(linearModel, df[i, ], type = "response")
  lmresults[i] = (as.numeric(pred) - df$NN[i]) ^ 2
}
# PRESS
lmPress = sum(lmresults)

vectortmp = data.frame("NN",gamPress, lmPress)
names(vectortmp) = c('feature','gamPRESS', 'lmPRESS')
totalResults = rbind(totalResults, vectortmp)


# male Feature
for (i in 1:(nrow(df))){
  gamModel = gam(male ~ s(Days, bs = "gp"), 
                 data = df[-i, ], 
                 family = Gamma(link = "log"), 
                 method = "REML")
  pred = predict.gam(gamModel, df[i, ], 
                     type = "response")
  results[i] = (as.numeric(pred) - df$male[i]) ^ 2
}
# PRESS
gamPress = sum(results)

lmresults = numeric(nrow(df))
for (i in 1:(nrow(df))){
  linearModel = lm(male ~ Days, data = df[-i, ])
  pred = predict.lm(linearModel, df[i, ], type = "response")
  lmresults[i] = (as.numeric(pred) - df$male[i]) ^ 2
}
# PRESS
lmPress = sum(lmresults)

vectortmp = data.frame("male",gamPress, lmPress)
names(vectortmp) = c('feature','gamPRESS', 'lmPRESS')
totalResults = rbind(totalResults, vectortmp)

# UniqueWords Feature
for (i in 1:(nrow(df))){
  gamModel = gam(UniqueWords ~ s(Days, bs = "gp"), 
                 data = df[-i, ], 
                 family = Gamma(link = "log"), 
                 method = "REML")
  pred = predict.gam(gamModel, df[i, ], 
                     type = "response")
  results[i] = (as.numeric(pred) - df$UniqueWords[i]) ^ 2
}
# PRESS
gamPress = sum(results)

lmresults = numeric(nrow(df))
for (i in 1:(nrow(df))){
  linearModel = lm(UniqueWords ~ Days, data = df[-i, ])
  pred = predict.lm(linearModel, df[i, ], type = "response")
  lmresults[i] = (as.numeric(pred) - df$UniqueWords[i]) ^ 2
}
# PRESS
lmPress = sum(lmresults)

vectortmp = data.frame("UniqueWords",gamPress, lmPress)
names(vectortmp) = c('feature','gamPRESS', 'lmPRESS')
totalResults = rbind(totalResults, vectortmp)

# WDT Feature
for (i in 1:(nrow(df))){
  gamModel = gam(WDT ~ s(Days, bs = "gp", k = 40), 
                 data = df[-i, ], 
                 family = Gamma(link = "log"), 
                 method = "REML")
  pred = predict.gam(gamModel, df[i, ], 
                     type = "response")
  results[i] = (as.numeric(pred) - df$WDT[i]) ^ 2
}
# PRESS
gamPress = sum(results)

lmresults = numeric(nrow(df))
for (i in 1:(nrow(df))){
  linearModel = lm(WDT ~ Days, data = df[-i, ])
  pred = predict.lm(linearModel, df[i, ], type = "response")
  lmresults[i] = (as.numeric(pred) - df$WDT[i]) ^ 2
}
# PRESS
lmPress = sum(lmresults)

vectortmp = data.frame("WDT",gamPress, lmPress)
names(vectortmp) = c('feature','gamPRESS', 'lmPRESS')
totalResults = rbind(totalResults, vectortmp)

# Nouns Feature
for (i in 1:(nrow(df))){
  gamModel = gam(Nouns ~ s(Days, bs = "gp", k = 40), 
                 data = df[-i, ], 
                 family = Gamma(link = "log"), 
                 method = "REML")
  pred = predict.gam(gamModel, df[i, ], 
                     type = "response")
  results[i] = (as.numeric(pred) - df$Nouns[i]) ^ 2
}
# PRESS
gamPress = sum(results)

lmresults = numeric(nrow(df))
for (i in 1:(nrow(df))){
  linearModel = lm(Nouns ~ Days, data = df[-i, ])
  pred = predict.lm(linearModel, df[i, ], type = "response")
  lmresults[i] = (as.numeric(pred) - df$Nouns[i]) ^ 2
}
# PRESS
lmPress = sum(lmresults)

vectortmp = data.frame("Nouns",gamPress, lmPress)
names(vectortmp) = c('feature','gamPRESS', 'lmPRESS')
totalResults = rbind(totalResults, vectortmp)

# Nouns.100 Feature
for (i in 1:(nrow(df))){
  gamModel = gam(Nouns.100 ~ s(Days, bs = "gp", k = 40), 
                 data = df[-i, ], 
                 family = Gamma(link = "log"), 
                 method = "REML")
  pred = predict.gam(gamModel, df[i, ], 
                     type = "response")
  results[i] = (as.numeric(pred) - df$Nouns.100[i]) ^ 2
}
# PRESS
gamPress = sum(results)

lmresults = numeric(nrow(df))
for (i in 1:(nrow(df))){
  linearModel = lm(Nouns.100 ~ Days, data = df[-i, ])
  pred = predict.lm(linearModel, df[i, ], type = "response")
  lmresults[i] = (as.numeric(pred) - df$Nouns.100[i]) ^ 2
}
# PRESS
lmPress = sum(lmresults)

vectortmp = data.frame("Nouns",gamPress, lmPress)
names(vectortmp) = c('feature','gamPRESS', 'lmPRESS')
totalResults = rbind(totalResults, vectortmp)

# UniqueStems Feature
for (i in 1:(nrow(df))){
  gamModel = gam(UniqueStems ~ s(Days, bs = "gp", k = 40), 
                 data = df[-i, ], 
                 family = Gamma(link = "log"), 
                 method = "REML")
  pred = predict.gam(gamModel, df[i, ], 
                     type = "response")
  results[i] = (as.numeric(pred) - df$UniqueStems[i]) ^ 2
}
# PRESS
gamPress = sum(results)

lmresults = numeric(nrow(df))
for (i in 1:(nrow(df))){
  linearModel = lm(UniqueStems ~ Days, data = df[-i, ])
  pred = predict.lm(linearModel, df[i, ], type = "response")
  lmresults[i] = (as.numeric(pred) - df$UniqueStems[i]) ^ 2
}
# PRESS
lmPress = sum(lmresults)

vectortmp = data.frame("UniqueStems",gamPress, lmPress)
names(vectortmp) = c('feature','gamPRESS', 'lmPRESS')
totalResults = rbind(totalResults, vectortmp)

# shehe Feature
for (i in 1:(nrow(df))){
  gamModel = gam(shehe ~ s(Days, bs = "gp", k = 40), 
                 data = df[-i, ], 
                 family = Gamma(link = "log"), 
                 method = "REML")
  pred = predict.gam(gamModel, df[i, ], 
                     type = "response")
  results[i] = (as.numeric(pred) - df$shehe[i]) ^ 2
}
# PRESS
gamPress = sum(results)

lmresults = numeric(nrow(df))
for (i in 1:(nrow(df))){
  linearModel = lm(shehe ~ Days, data = df[-i, ])
  pred = predict.lm(linearModel, df[i, ], type = "response")
  lmresults[i] = (as.numeric(pred) - df$shehe[i]) ^ 2
}
# PRESS
lmPress = sum(lmresults)

vectortmp = data.frame("shehe",gamPress, lmPress)
names(vectortmp) = c('feature','gamPRESS', 'lmPRESS')
totalResults = rbind(totalResults, vectortmp)

# VBZ Feature
for (i in 1:(nrow(df))){
  gamModel = gam(VBZ ~ s(Days, bs = "gp", k = 40), 
                 data = df[-i, ], 
                 family = Gamma(link = "log"), 
                 method = "REML")
  pred = predict.gam(gamModel, df[i, ], 
                     type = "response")
  results[i] = (as.numeric(pred) - df$VBZ[i]) ^ 2
}
# PRESS
gamPress = sum(results)

lmresults = numeric(nrow(df))
for (i in 1:(nrow(df))){
  linearModel = lm(VBZ ~ Days, data = df[-i, ])
  pred = predict.lm(linearModel, df[i, ], type = "response")
  lmresults[i] = (as.numeric(pred) - df$VBZ[i]) ^ 2
}
# PRESS
lmPress = sum(lmresults)

vectortmp = data.frame("VBZ",gamPress, lmPress)
names(vectortmp) = c('feature','gamPRESS', 'lmPRESS')
totalResults = rbind(totalResults, vectortmp)

# JJ Feature
for (i in 1:(nrow(df))){
  gamModel = gam(JJ ~ s(Days, bs = "gp", k = 40), 
                 data = df[-i, ], 
                 family = Gamma(link = "log"), 
                 method = "REML")
  pred = predict.gam(gamModel, df[i, ], 
                     type = "response")
  results[i] = (as.numeric(pred) - df$JJ[i]) ^ 2
}
# PRESS
gamPress = sum(results)

lmresults = numeric(nrow(df))
for (i in 1:(nrow(df))){
  linearModel = lm(JJ ~ Days, data = df[-i, ])
  pred = predict.lm(linearModel, df[i, ], type = "response")
  lmresults[i] = (as.numeric(pred) - df$JJ[i]) ^ 2
}
# PRESS
lmPress = sum(lmresults)

vectortmp = data.frame("JJ",gamPress, lmPress)
names(vectortmp) = c('feature','gamPRESS', 'lmPRESS')
totalResults = rbind(totalResults, vectortmp)

# article Feature
for (i in 1:(nrow(df))){
  gamModel = gam(article ~ s(Days, bs = "gp", k = 40), 
                 data = df[-i, ], 
                 family = Gamma(link = "log"), 
                 method = "REML")
  pred = predict.gam(gamModel, df[i, ], 
                     type = "response")
  results[i] = (as.numeric(pred) - df$article[i]) ^ 2
}
# PRESS
gamPress = sum(results)

lmresults = numeric(nrow(df))
for (i in 1:(nrow(df))){
  linearModel = lm(article ~ Days, data = df[-i, ])
  pred = predict.lm(linearModel, df[i, ], type = "response")
  lmresults[i] = (as.numeric(pred) - df$article[i]) ^ 2
}
# PRESS
lmPress = sum(lmresults)

vectortmp = data.frame("article",gamPress, lmPress)
names(vectortmp) = c('feature','gamPRESS', 'lmPRESS')
totalResults = rbind(totalResults, vectortmp)

# Adjectives Feature
for (i in 1:(nrow(df))){
  gamModel = gam(Adjectives ~ s(Days, bs = "gp", k = 40), 
                 data = df[-i, ], 
                 family = Gamma(link = "log"), 
                 method = "REML")
  pred = predict.gam(gamModel, df[i, ], 
                     type = "response")
  results[i] = (as.numeric(pred) - df$Adjectives[i]) ^ 2
}
# PRESS
gamPress = sum(results)

lmresults = numeric(nrow(df))
for (i in 1:(nrow(df))){
  linearModel = lm(Adjectives ~ Days, data = df[-i, ])
  pred = predict.lm(linearModel, df[i, ], type = "response")
  lmresults[i] = (as.numeric(pred) - df$Adjectives[i]) ^ 2
}
# PRESS
lmPress = sum(lmresults)

vectortmp = data.frame("Adjectives",gamPress, lmPress)
names(vectortmp) = c('feature','gamPRESS', 'lmPRESS')
totalResults = rbind(totalResults, vectortmp)

# Adjectives.100 Feature
for (i in 1:(nrow(df))){
  gamModel = gam(Adjectives.100 ~ s(Days, bs = "gp", k = 40), 
                 data = df[-i, ], 
                 family = Gamma(link = "log"), 
                 method = "REML")
  pred = predict.gam(gamModel, df[i, ], 
                     type = "response")
  results[i] = (as.numeric(pred) - df$Adjectives.100[i]) ^ 2
}
# PRESS
gamPress = sum(results)

lmresults = numeric(nrow(df))
for (i in 1:(nrow(df))){
  linearModel = lm(Adjectives.100 ~ Days, data = df[-i, ])
  pred = predict.lm(linearModel, df[i, ], type = "response")
  lmresults[i] = (as.numeric(pred) - df$Adjectives.100[i]) ^ 2
}
# PRESS
lmPress = sum(lmresults)

vectortmp = data.frame("Adjectives.100",gamPress, lmPress)
names(vectortmp) = c('feature','gamPRESS', 'lmPRESS')
totalResults = rbind(totalResults, vectortmp)

# Dic Feature
for (i in 1:(nrow(df))){
  gamModel = gam(Dic ~ s(Days, bs = "gp", k = 40), 
                 data = df[-i, ], 
                 family = Gamma(link = "log"), 
                 method = "REML")
  pred = predict.gam(gamModel, df[i, ], 
                     type = "response")
  results[i] = (as.numeric(pred) - df$Dic[i]) ^ 2
}
# PRESS
gamPress = sum(results)

lmresults = numeric(nrow(df))
for (i in 1:(nrow(df))){
  linearModel = lm(Dic ~ Days, data = df[-i, ])
  pred = predict.lm(linearModel, df[i, ], type = "response")
  lmresults[i] = (as.numeric(pred) - df$Dic[i]) ^ 2
}
# PRESS
lmPress = sum(lmresults)

vectortmp = data.frame("Dic",gamPress, lmPress)
names(vectortmp) = c('feature','gamPRESS', 'lmPRESS')
totalResults = rbind(totalResults, vectortmp)

test = totalResults

test = test %>%
  mutate_at(c(2,3), funs(c(scale(.))))

ttest = t.test(totalResults[,2], totalResults[,3], paired = TRUE)
indx <- totalResults[,2] < totalResults[,3]
binom.test(sum(indx), length(indx))



scatter_plot <- ggplot(dataReagan, aes(Days, ppron))
scatter_plot + geom_point() + labs(x = "Days", y = "ppron") + geom_smooth(method="lm")
scatter_plot + geom_point() + labs(x = "Days", y = "NN") + geom_smooth(method="loess", 
                                                                       color = 'darkred')
#jpeg("~/Documents/NLP/plots/GHWBUniqueWords.jpg", width = 350, height = 350)
scatter_plot <- ggplot(dataReagan, aes(Days, ppron))
scatter_plot + geom_point() + labs(x = "Days", y = "NN") + geom_smooth(method="loess")
cor.test(dataReagan$Days, dataReagan$NN, method = "pearson", conf.level = 0.95)


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

gamModel1 = gam(ppron ~ s(Days, bs="gp"), data = dataReagan, family = Gamma(link="log"), method="REML")
coef(gamModel1)
plot(gamModel1, residuals = TRUE, pch = 1)
summary.gam(gamModel1)

fit = gamModel1$fitted.values
gam.check(gamModel1, pch=19, cex=.3)
plot.gam(gamModel1)
vis.gam(gamModel1)
preds = predict.gam(gamModel1, df, type = "response")
difference = preds - df$ppron 
sse = sum(difference^2)
linearMod1 = lm(df$ppron ~ df$Days) 
residuals = linearMod1$residuals
sseLinear = sum(residuals^2)
cat(sse, ' : SSE - GAM')
cat(sseLinear, ' : SSE Linear Model')
# press prediction error sum of squares
library(qpcR)
PRESS(gamModel1)







scatter_plot <- ggplot(dataReagan, aes(Days, ppron))
scatter_plot + geom_point() + labs(x = "Days", y = "ppron") + geom_smooth(method="lm")

scatter_plot <- ggplot(dataReagan, aes(Days, ppron))
scatter_plot + geom_point() + labs(x = "Days", y = "ppron") + geom_smooth(method="loess")
cor.test(dataReagan$Days, dataReagan$ppron, method = "pearson", conf.level = 0.95)

scatter_plot <- ggplot(dataBush, aes(Days, ppron))
scatter_plot + geom_point() + labs(x = "Days", y = "ppron") + geom_smooth(method="lm")
#jpeg("~/Documents/NLP/plots/GHWBppron.jpg", width = 350, height = 350)
scatter_plot <- ggplot(dataBush, aes(Days, ppron))
scatter_plot + geom_point() + labs(x = "Days", y = "ppron") + geom_smooth(method="loess")
cor.test(dataBush$Days, dataBush$ppron, method = "pearson", conf.level = 0.95)
#dev.off()

scatter_plot <- ggplot(dataReagan, aes(Days, UniqueWords))
scatter_plot + geom_point() + labs(x = "Days", y = "UniqueWords") + geom_smooth(method="lm")
#jpeg("~/Documents/NLP/plots/RRUniqueWords.jpg", width = 350, height = 350)
scatter_plot <- ggplot(dataReagan, aes(Days, UniqueWords))
scatter_plot + geom_point() + labs(x = "Days", y = "UniqueWords") + geom_smooth(method="loess")
cor.test(dataReagan$Days, dataReagan$UniqueWords, method = "pearson", conf.level = 0.95)
#dev.off()

scatter_plot <- ggplot(dataBush, aes(Days, UniqueWords))
scatter_plot + geom_point() + labs(x = "Days", y = "UniqueWords") + geom_smooth(method="lm")
#jpeg("~/Documents/NLP/plots/GHWBUniqueWords.jpg", width = 350, height = 350)
scatter_plot <- ggplot(dataBush, aes(Days, UniqueWords))
scatter_plot + geom_point() + labs(x = "Days", y = "UniqueWords") + geom_smooth(method="loess")
cor.test(dataBush$Days, dataBush$UniqueWords, method = "pearson", conf.level = 0.95)

scatter_plot <- ggplot(dataReagan, aes(Days, UniqueWordsNormalised))
scatter_plot + geom_point() + labs(x = "Days", y = "UniqueWordsNormalised") + geom_smooth(method="lm")

scatter_plot <- ggplot(dataReagan, aes(Days, UniqueWordsNormalised))
scatter_plot + geom_point() + labs(x = "Days", y = "UniqueWordsNormalised") + geom_smooth(method="loess")
cor.test(dataReagan$Days, dataReagan$UniqueWords, method = "pearson", conf.level = 0.95)

scatter_plot <- ggplot(dataBush, aes(Days, UniqueWordsNormalised))
scatter_plot + geom_point() + labs(x = "Days", y = "UniqueWordsNormalised") + geom_smooth(method="lm")
#jpeg("~/Documents/NLP/plots/GHWBUniqueWords.jpg", width = 350, height = 350)
scatter_plot <- ggplot(dataBush, aes(Days, UniqueWordsNormalised))
scatter_plot + geom_point() + labs(x = "Days", y = "UniqueWordsNormalised") + geom_smooth(method="loess")
cor.test(dataBush$Days, dataBush$UniqueWords, method = "pearson", conf.level = 0.95)
#dev.off()


scatter_plot <- ggplot(dataReagan, aes(Days, social))
scatter_plot + geom_point() + labs(x = "Days", y = "social") + geom_smooth(method="loess")
cor.test(dataReagan$Days, dataReagan$social, method = "pearson", conf.level = 0.95)

scatter_plot <- ggplot(dataReagan, aes(Days, NounsNormalised))
scatter_plot + geom_point() + labs(x = "Days", y = "NounsNormalised") + geom_smooth(method="loess")
cor.test(dataReagan$Days, dataReagan$social, method = "pearson", conf.level = 0.95)

scatter_plot <- ggplot(dataReagan, aes(Days, conj))
scatter_plot + geom_point() + labs(x = "Days", y = "conj") + geom_smooth(method="loess")
cor.test(dataReagan$Days, dataReagan$social, method = "pearson", conf.level = 0.95)

scatter_plot <- ggplot(dataReagan, aes(Days, PronounsNormalised))
scatter_plot + geom_point() + labs(x = "Days", y = "PronounsNormalised") + geom_smooth(method="loess")
cor.test(dataReagan$Days, dataReagan$social, method = "pearson", conf.level = 0.95)

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

gam_mod = gam(ppron ~ s(Days), data = dataReagan )
coef(gam_mod)
plot(gam_mod, residuals = TRUE, pch = 1)




r,p = pearsonr(dfGWHB['index'], (dfGWHB['TTR']))
print('R Squared', r)
print('P Value', '{0:.10f}'.format(p))



# Clear Workspace
rm(list=ls())

# Set Seed
set.seed(500)

# Load Libraries
library(MASS)

# Load Data
data <- Boston

# Check for NA's
apply(data,2,function(x) sum(is.na(x)))

index <- sample(1:nrow(data),round(0.75*nrow(data)))
train <- data[index,]
test <- data[-index,]
lm.fit <- glm(medv~., data=train)
summary(lm.fit)
pr.lm <- predict(lm.fit,test)
MSE.lm <- sum((pr.lm - test$medv)^2)/nrow(test)

maxs <- apply(data, 2, max) 
mins <- apply(data, 2, min)
scaled <- as.data.frame(scale(data, center = mins, scale = maxs - mins))
train_ <- scaled[index,]
test_ <- scaled[-index,]

library(neuralnet)
n <- names(train_)
f <- as.formula(paste("medv ~", paste(n[!n %in% "medv"], collapse = " + ")))
nn <- neuralnet(f,data=train_,hidden=c(5,3),linear.output=T)
plot(nn)

pr.nn <- compute(nn,test_[,1:13])
pr.nn_ <- pr.nn$net.result*(max(data$medv)-min(data$medv))+min(data$medv)
test.r <- (test_$medv)*(max(data$medv)-min(data$medv))+min(data$medv)
MSE.nn <- sum((test.r - pr.nn_)^2)/nrow(test_)

print(paste(MSE.lm,MSE.nn))

par(mfrow=c(1,2))
plot(test$medv,pr.nn_,col='red',main='Real vs predicted NN',pch=18,cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend='NN',pch=18,col='red', bty='n')
plot(test$medv,pr.lm,col='blue',main='Real vs predicted lm',pch=18, cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend='LM',pch=18,col='blue', bty='n', cex=.95)

plot(test$medv,pr.nn_,col='red',main='Real vs predicted NN',pch=18,cex=0.7)
points(test$medv,pr.lm,col='blue',pch=18,cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend=c('NN','LM'),pch=18,col=c('red','blue'))


library(boot)
set.seed(200)
lm.fit <- glm(medv~.,data=data)
cv.glm(data,lm.fit,K=10)$delta[1]

set.seed(450)
cv.error <- NULL
k <- 10
library(plyr) 
pbar <- create_progress_bar('text')
pbar$init(k)
for(i in 1:k){
  index <- sample(1:nrow(data),round(0.9*nrow(data)))
  train.cv <- scaled[index,]
  test.cv <- scaled[-index,]
  nn <- neuralnet(f,data=train.cv,hidden=c(5,2),linear.output=T)   
  pr.nn <- compute(nn,test.cv[,1:13])
  pr.nn <- pr.nn$net.result*(max(data$medv)-min(data$medv))+min(data$medv)   
  test.cv.r <- (test.cv$medv)*(max(data$medv)-min(data$medv))+min(data$medv)   
  cv.error[i] <- sum((test.cv.r - pr.nn)^2)/nrow(test.cv)    
  pbar$step()
}

mean(cv.error)
cv.error

boxplot(cv.error,xlab='MSE CV',col='cyan',
        border='blue',names='CV error (MSE)',
        main='CV error (MSE) for NN',horizontal=TRUE)

# To do
# FWER and FDR plots in function
# Automate plots fot significant features

###--- SETUP ---###
# Clear Workspace
rm(list=ls())

# Set Directory
setwd("~/PhD/NLP")

# Source File
source("Functions.R")

# Options
options(scipen=999)

# Libraries
library(dplyr)
library(ggplot2)
library(mgcv)
library(stats)
library(neuralnet)

###--- IMPORT AND PREPROCESS DATA ---###
# Load Data
dataChristie = read.csv("Christie.csv")
dataMurdoch = read.csv("Murdoch.csv")
dataJames = read.csv("James.csv")

# Clean Data 
dataChristie = cleanData(dataChristie)
dataMurdoch = cleanData(dataMurdoch)
dataJames = cleanData(dataJames)

scatter_plot <- ggplot(dataChristie, aes(Year.of.Publication, TTR))
scatter_plot + geom_point() + labs(x = "Year.of.Publication", y = "TTR") + geom_smooth(method="lm")
scatter_plot + geom_point() + labs(x = "Year.of.Publication", y = "TTR") + geom_smooth(method="loess", 
                                                                                       color = 'darkred')


# Preprocess Data
dfChristie = preprocess(dataChristie)
dfMurdoch = preprocess(dataMurdoch)
dfJames = preprocess(dataJames)

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
