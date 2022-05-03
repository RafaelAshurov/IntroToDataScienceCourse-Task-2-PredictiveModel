source("mechkar.R")
library(readr)
if (!require("dplyr")) {install.packages("dplyr");library("dplyr")}
if (!require("png")) {install.packages("png");library("pnf")}
if (!require("car")) {install.packages("car");library("car")}
if (!require("ggplot2")) {install.packages("ggplot2");library("ggplot2")}
if (!require("Metrics")) {install.packages("Metrics");library("Metrics")}
if (!require("sgd")) {install.packages("sgd");library("sgd")}
if (!require("glmnet")) {install.packages("glmnet");library("glmnet")}
library("pROC")
library("ROSE")
library("caret")
library(car)
path <- "C:/DevProjects/personal_projects/HIT/Data science/Corrine_task2"
setwd(path)
set.seed(255)
####################################################

data <- read_csv("Hospitalization-Final-Data-seleted.csv")
summary(data)

####################################################
####    Feature transformation
####################################################

## ONE HOT ENCODING -> categorical variables
dummy <- dummyVars(" ~ .", data = data)
transformedData <- data.frame(predict(dummy, newdata = data))
df <- data.frame(transformedData)

##############################
### Balance check
##############################

table(df$death)/length(df$death)


# The data is imbalanced. Going for all 4 of the resampling methods to find the best one. 
# I will divide the data for the purpose of finding the best method
tab1 <- train_test(data=df,train_name='train.resmp',test_name='test.resmp',prop=0.8,seed=10,tableone=TRUE)

# Methods
trainOver <- ovun.sample(death ~ .,data=train.resmp, method = "over")$data
trainUnder <- ovun.sample(death ~ .,data=train.resmp, method = "under")$data
trainBoth <- ovun.sample(death ~ .,data=train.resmp, method = "both")$data
trainRose <- ROSE(death ~ .,data=train.resmp)$data

# AUC on simple glm model
modOver <- glm(death ~., data=trainOver, family="binomial")
predOver <- predict(modOver, newdata = test.resmp,type="response")
aucOver <- Metrics::auc(test.resmp$death,predOver)
aucOver

modUnder <- glm(death ~., data=trainUnder, family="binomial")
predUnder <- predict(modUnder, newdata = test.resmp,type="response")
aucUnder <- Metrics::auc(test.resmp$death,predUnder)
aucUnder

modBoth <- glm(death ~., data=trainBoth, family="binomial")
predBoth <- predict(modBoth, newdata = test.resmp,type="response")
aucBoth <- Metrics::auc(test.resmp$death,predBoth)
aucBoth

modRose <- glm(death ~., data=trainRose, family="binomial")
predRose <- predict(modRose, newdata = test.resmp,type="response")
aucRose <- Metrics::auc(test.resmp$death,predRose)
aucRose

# `Both` method found to be the best one, apply the method to the whole data

df.both <- ovun.sample(death ~ .,data=df, method = "both")$data
table(df.both$death)

# To make the next steps less confusing, I will move the death column to be the
# last column in order
deathCol <- df.both$death
colnames(df.both)
df.both <- df.both[,-12]
df.both$death <- deathCol
colnames(df.both)

####################################################
####    SCALING
####################################################

scaling <- function(x) {(x-min(x,na.rm=TRUE))/(max(x,na.rm=TRUE)-min(x,na.rm=TRUE))}
df.both.scaled <- data.frame(apply(df.both,MARGIN = 2,FUN=scaling))

####################################################
####    TEST DATA CREATION
####################################################

# Save 20% of the dataset as test
tab1 <- train_test(data=df.both.scaled,train_name='tmp',test_name='test',prop=0.8,seed=10,tableone=TRUE)

################################################################
######  TRAIN AND DEV SPLITING
################################################################

# Divide the remaining data 70% to train, and 30% to dev
tab1 <- train_test(data=tmp,train_name='train',test_name='dev',prop=0.7,seed=10,tableone=TRUE)

# Make sure all groups are balanced
table(train$death)
table(dev$death)
table(test$death)

###############################
#### SELECTED METRIC: Accuracy
###############################

###############################
### GLM model
###############################
mod1 <- glm(death ~., data=train, family="binomial")
summary(mod1)

vif(mod1)
# Getting this error: Error in vif.default(mod1) : there are aliased coefficients in the model
# Its failing probably because there is very high correlation between some of the variables
# that are translating as alias.

# I will find the problematic variables and remove the highest correlated vars from the dataset
alias(mod1)
corolationTable <- data.frame(cor(train))
corolatedVars <- c("langUNKNOWN","INSURANCEGovernment","langSPANISH",
              "INSURANCESelf.Pay","maritalUNKNOWN","langENGLISH",
              "maritalDIVORCED","maritalSINGLE",
              "maritalWIDOWED","ethnicUNKNOWN","ethnicBLACK",
              "religionJEWISH","religionUNKNOWN", "LowHCT", "HighPotassium",
              "LowPotassium")
remainingVarNames <- setdiff(names(train),corolatedVars)
mod1.droped <- glm(death ~., data=train[,remainingVarNames], family="binomial")
summary(mod1.droped)
vif(mod1.droped)

train.drop <- findCorrelation(cor(train), cutoff=0.4, exact = TRUE)
train.drop <- sort(train.drop)

mod1.droped <- glm(death ~., data=train[,-c(train.drop)], family="binomial")
summary(mod1.droped)
vif(mod1.droped)
# Done

### Check 8 models accuracy

# Calculate the AUC 
trainAUC1 <- predict(mod1.droped,type="response")
devAUC1 <- predict(mod1.droped,newdata=dev,type="response")

# Checking Accuracy
train1Accuracy <- accuracy(train$death, ifelse(trainAUC1>0.5,1,0))
dev1Accuracy1 <- accuracy(dev$death, ifelse(devAUC1>0.5,1,0))

# Add to table
modelsAccTable <- data.frame(model="GLM",train=train1Accuracy,dev=dev1Accuracy1)

#############################################
#### GRADIENT DESCENT MODEL
#############################################
mod2 <- sgd::sgd(as.matrix(train[,-64]), train$death, model="glm")
mod2$coefficients

trainAUC2 <- predict(mod2, newdata = as.matrix(train[,-64]), type="response")
devAUC2 <- predict(mod2, newdata = as.matrix(dev[,-64]), type="response")

# Checking Accuracy
trainAccuracy2 <- accuracy(train$death,ifelse(trainAUC2>0.5,1,0))
devAccuracy2 <- accuracy(dev$death,ifelse(devAUC2>0.5,1,0))

# Add to table
modelsAccTable <- rbind(modelsAccTable, data.frame(model="SGD",train=trainAccuracy2,dev=devAccuracy2))


##########################
### RIDGE MODEL
##########################
# Finding optimal lambda for the model
cv <- cv.glmnet(as.matrix(train[,-64]), train$death, alpha=0)
cv$lambda.min

mod3 <- glmnet(as.matrix(train[,-64]), train$death, alpha=0,lambda = cv$lambda.min)


trainAUC3 <- predict(mod3, s=1,newx=as.matrix(train[,-64]),type="response")
devAUC3 <- predict(mod3,s=1,newx=as.matrix(dev[,-64]),type="response")

# Checking Accuracy
trainAccuracy3 <- accuracy(train$death, ifelse(trainAUC3>0.5,1,0))
devAccuracy3 <- accuracy(dev$death, ifelse(devAUC3>0.5,1,0))

# Add to table
modelsAccTable <- rbind(modelsAccTable, data.frame(model="GLM-Ridge",train=trainAccuracy3,dev=devAccuracy3))


##########################
### LASSO MODEL
##########################
cv.l <- cv.glmnet(as.matrix(train[,-64]), train$death, alpha=1)
cv.l$lambda.min

mod4 <- glmnet(as.matrix(train[,-64]), train$death, alpha=1,lambda = cv.l$lambda.min)

trainAUC4 <- predict(mod4, s=1,newx=as.matrix(train[,-64]),type="response")
devAUC4 <- predict(mod4,s=1,newx=as.matrix(dev[,-64]),type="response")

# Checking Accuracy
trainAccuracy4 <- accuracy(train$death, ifelse(trainAUC4>0.5,1,0))
devAccuracy4 <- accuracy(dev$death, ifelse(devAUC4>0.5,1,0))

# Add to table
modelsAccTable <- rbind(modelsAccTable, data.frame(model="GLM-Lasso",train=trainAccuracy4,dev=devAccuracy4))


##########################
### ElasticNet MODEL
##########################

cv.e <- cv.glmnet(as.matrix(train[,-64]), train$death, alpha=.5)
cv.e$lambda.min

mod5 <- glmnet(as.matrix(train[,-64]), train$death, alpha=.5,lambda = cv.e$lambda.min)

trainAUC5 <- predict(mod5, s=1,newx=as.matrix(train[,-64]),type="response")
devAUC5 <- predict(mod5,s=1,newx=as.matrix(dev[,-64]),type="response")

# Checking Accuracy
trainAccuracy5 <- accuracy(train$death, ifelse(trainAUC5>0.5,1,0))
devAccuracy5 <- accuracy(dev$death, ifelse(devAUC5>0.5,1,0))

# Add to table
modelsAccTable <- rbind(modelsAccTable, data.frame(model="GLM-ElasticNet",train=trainAccuracy5,dev=devAccuracy5))

#######################
### SGD-LASSO
#######################

mod6 <- sgd::sgd(as.matrix(train[,-64]), train$death, 
                 model="glm", model.control=list(lambda1=cv.l$lambda.min,lambda2=0))

trainAUC6 <- predict(mod6, newdata = as.matrix(train[,-64]), type="response")
devAUC6 <- predict(mod6, newdata = as.matrix(dev[,-64]), type="response")

# Checking Accuracy
trainAccuracy6 <- accuracy(train$death, ifelse(trainAUC6>0.5,1,0))
devAccuracy6 <- accuracy(dev$death, ifelse(trainAUC6>0.5,1,0))

# Add to table
modelsAccTable <- rbind(modelsAccTable, data.frame(model="SGD-LASSO",train=trainAccuracy6,dev=devAccuracy6))

#######################
### SGD-Ridge
#######################

mod7 <- sgd::sgd(as.matrix(train[,-64]), train$death, 
                 model="glm", model.control=list(lambda1=0,lambda2=cv$lambda.min))


trainAUC7 <- predict(mod7, newdata = as.matrix(train[,-64]), type="response")
devAUC7 <- predict(mod7, newdata = as.matrix(dev[,-64]), type="response")

# Checking Accuracy
trainAccuracy7 <- accuracy(train$death, ifelse(trainAUC7>0.5,1,0))
devAccuracy7 <- accuracy(dev$death, ifelse(trainAUC7>0.5,1,0))

# Add to table
modelsAccTable <- rbind(modelsAccTable, data.frame(model="SGD-Ridge",train=trainAccuracy7,dev=devAccuracy7))


#######################
### SGD-ELASTICNET
#######################

mod8 <- sgd::sgd(as.matrix(train[,-64]), train$death, 
                 model="glm", model.control=list(lambda1=cv.l$lambda.min,lambda2=cv$lambda.min))


trainAUC8 <- predict(mod8, newdata = as.matrix(train[,-64]), type="response")
devAUC8 <- predict(mod8, newdata = as.matrix(dev[,-64]), type="response")

# Checking Accuracy
trainAccuracy8 <- accuracy(train$death, ifelse(trainAUC8>0.5,1,0))
devAccuracy8 <- accuracy(dev$death, ifelse(trainAUC8>0.5,1,0))

# Add to table
modelsAccTable <- rbind(modelsAccTable, data.frame(model="SGD-ElasticNet",train=trainAccuracy8,dev=devAccuracy8))

#################################
###  BEST MODEL FOR TRAIN/DEV: Ridge 0.6991461 0.7025186
#################################

testAUC <- predict(mod3,s=1,newx=as.matrix(test[,-64]),type="response")
testAccuracy <- accuracy(test$death, ifelse(testAUC>0.5,1,0))
modelSumUp <- data.frame(model="Ridge",train=trainAccuracy3,dev=devAccuracy3,test=testAccuracy)
modelSumUp