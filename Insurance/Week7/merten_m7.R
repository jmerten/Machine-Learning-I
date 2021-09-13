# Title     : Module 7 Data Assignment
# Created by: Jason Merten
# Created on: 2/27/2021

library(caret)
library(tidyverse)
library(e1071)
library(zoo)
library(corrplot)
library(ROCR)
library(rsample)
library(tree)
library(ranger)
library(gbm)


# Data Import and Variable Type Changes
setwd("C:/Users/jmert/Documents/W&M/BUAD5122/M7")
data <- read.csv("insurance-training.csv")
test <- read.csv("insurance-test.csv")

### Need to make sure our data is understood correctly by R, since we have a mix of numerical and categorical
data$TARGET_FLAG <- as.factor(data$TARGET_FLAG)
data$SEX <- as.factor(data$SEX)
data$EDUCATION <- as.factor(data$EDUCATION)
data$PARENT1 <- as.factor(data$PARENT1)
data$INCOME <- suppressWarnings(as.numeric(gsub("[^0-9.]", "", data$INCOME)))
data$HOME_VAL <- suppressWarnings(as.numeric(gsub("[^0-9.]", "", data$HOME_VAL)))
data$MSTATUS <- as.factor(data$MSTATUS)
data$REVOKED <- as.factor(data$REVOKED)
data$RED_CAR <- as.factor(data$RED_CAR)
data$URBANICITY <- ifelse(data$URBANICITY == "Highly Urban/ Urban", "Urban", "Rural")
data$URBANICITY <- as.factor(data$URBANICITY)
data$JOB <- ifelse(data$JOB == '', 'None', data$JOB)
data$JOB <- as.factor(data$JOB)
data$CAR_USE <- as.factor(data$CAR_USE)
data$CAR_TYPE <- as.factor(data$CAR_TYPE)
data$DO_KIDS_DRIVE <- as.factor(ifelse(data$KIDSDRIV > 0, 'Yes', 'No' ))
data$OLDCLAIM <- suppressWarnings(as.numeric(gsub("[^0-9.]", "", data$HOME_VAL)))
data$BLUEBOOK <- suppressWarnings(as.numeric(gsub("[^0-9.]", "", data$BLUEBOOK)))
str(data)
# data with NAs: AGE, YOJ, INCOME, HOME_VAL, OLDCLAIM, CAR_AGE

qtest <- function(x) {
  print(table(x > quantile(x,.99,na.rm=T)))
  print(table(x < quantile(x,.01,na.rm=T)))
}

######## Same treatment on test data set ###########################
summary(test)
### Need to make sure our data is understood correctly by R, since we have a mix of numerical and categorical
test$TARGET_FLAG <- as.factor(test$TARGET_FLAG)
test$SEX <- as.factor(test$SEX)
test$EDUCATION <- as.factor(test$EDUCATION)
test$PARENT1 <- as.factor(test$PARENT1)
test$INCOME <- suppressWarnings(as.numeric(gsub("[^0-9.]", "", test$INCOME)))
test$HOME_VAL <- suppressWarnings(as.numeric(gsub("[^0-9.]", "", test$HOME_VAL)))
test$MSTATUS <- as.factor(test$MSTATUS)
test$REVOKED <- as.factor(test$REVOKED)
test$RED_CAR <- as.factor(test$RED_CAR)
test$URBANICITY <- ifelse(test$URBANICITY == "Highly Urban/ Urban", "Urban", "Rural")
test$URBANICITY <- as.factor(test$URBANICITY)
test$JOB <- ifelse(test$JOB == '', 'None', test$JOB)
test$JOB <- as.factor(test$JOB)
test$CAR_USE <- as.factor(test$CAR_USE)
test$CAR_TYPE <- as.factor(test$CAR_TYPE)
test$DO_KIDS_DRIVE <- as.factor(ifelse(test$KIDSDRIV > 0, 'Yes', 'No'))
test$OLDCLAIM <- suppressWarnings(as.numeric(gsub("[^0-9.]", "", test$HOME_VAL)))
test$BLUEBOOK <- suppressWarnings(as.numeric(gsub("[^0-9.]", "", test$BLUEBOOK)))
str(test)
# data with NAs: AGE, YOJ, INCOME, HOME_VAL, OLDCLAIM, CAR_AGE

#################### Part 1: Data Exploration ##############################################
# Histograms for all Numeric Variables
data[2:27] %>%
  keep(is.numeric) %>%
  gather() %>%
  ggplot(aes(value)) + facet_wrap(~ key, scales='free') + geom_histogram()

# AGE : has some outliers (80 > 99th quantile/ 72 < 1st quantile), seems to follow normal distribution
qtest(data$AGE)

# TRAVTIME : has 75 outliers > 99th quantile, follows a normal distribution with the exception of a large tail < 3
# YOJ : 39 outliers > 99th quantile, follows normal distribution with the exception of a large tail at 0
qtest(data$TRAVTIME)
qtest(data$YOJ)

# BLUEBOOK : 82 outliers > 99th quantile, skewed normal distribution
# TIF : 56 outliers > 99th quantile, diagonal distribution?
qtest(data$BLUEBOOK)
qtest(data$TIF)

# MVR_PTS : 71 outliers > 99th quantile, seems to follow a diagonal/chi distribution
# CAR_AGE : 65 outliers > 99th quantile, 4 outliers < 1st quantile,
# looks like a skewed distribution w/ large amount of 0 values.  contains negative values
qtest(data$MVR_PTS)
qtest(data$CAR_AGE)

########### Part 2: Data Transformation ##################
# Create flag variables
# data with NAs: AGE, YOJ, INCOME, HOME_VAL, OLDCLAIM, CAR_AGE
data$flag_AGE <- ifelse(is.na(data$AGE),1,0)
data$flag_YOJ <- ifelse(is.na(data$YOJ),1,0)
data$flag_INCOME <- ifelse(is.na(data$INCOME),1,0)
data$flag_HOME_VAL <- ifelse(is.na(data$HOME_VAL),1,0)
data$flag_OLDCLAIM <- ifelse(is.na(data$OLDCLAIM),1,0)
data$flag_CAR_AGE <- ifelse(is.na(data$CAR_AGE),1,0)

# Fix NA's, note car age
data$AGE[is.na(data$AGE)] <- rnorm(length(data$AGE[is.na(data$AGE)]),mean(data$AGE,na.rm=T),sd(data$AGE,na.rm=T))
data$YOJ <- na.aggregate(data$YOJ, data$JOB, mean, na.rm = TRUE)
data$INCOME <- na.aggregate(data$INCOME, data$JOB, mean, na.rm = TRUE)
data$HOME_VAL <- na.aggregate(data$HOME_VAL, data$JOB, mean, na.rm = TRUE )
data$CAR_AGE <- na.aggregate(data$CAR_AGE, data$CAR_TYPE, mean, na.rm = TRUE)
data$CAR_AGE[data$CAR_AGE < 0 ] <- 0 
data$OLDCLAIM <- ifelse(data$CAR_AGE < 5 & !is.na(data$CAR_AGE),0,data$OLDCLAIM)
data$OLDCLAIM <- na.aggregate(data$OLDCLAIM, data$CAR_AGE, mean, na.rm = TRUE )
data$HOME_OWNER <- as.factor(ifelse(data$HOME_VAL == 0, 'Yes', 'No'))
data$SQRT_TRAVTIME <- sqrt(data$TRAVTIME)
data$SQRT_BLUEBOOK <- sqrt(data$BLUEBOOK)

summary(data)
data[2:27] %>%
  keep(is.numeric) %>%
  gather() %>%
  ggplot(aes(value)) + facet_wrap(~ key, scales='free') + geom_histogram()

# Bin Income
data$INCOME_bin[is.na(data$INCOME)] <- "NA"
data$INCOME_bin[data$INCOME == 0] <- "Zero"
data$INCOME_bin[data$INCOME >= 1 & data$INCOME < 30000] <- "Low"
data$INCOME_bin[data$INCOME >= 30000 & data$INCOME < 80000] <- "Medium"
data$INCOME_bin[data$INCOME >= 80000] <- "High"
data$INCOME_bin <- factor(data$INCOME_bin)
data$INCOME_bin <- factor(data$INCOME_bin, levels=c("Zero","Low","Medium","High"))

# test data
test$flag_AGE <- ifelse(is.na(test$AGE),1,0)
test$flag_YOJ <- ifelse(is.na(test$YOJ),1,0)
test$flag_INCOME <- ifelse(is.na(test$INCOME),1,0)
test$flag_HOME_VAL <- ifelse(is.na(test$HOME_VAL),1,0)
test$flag_OLDCLAIM <- ifelse(is.na(test$OLDCLAIM),1,0)
test$flag_CAR_AGE <- ifelse(is.na(test$CAR_AGE),1,0)

test$AGE[is.na(test$AGE)] <- rnorm(length(test$AGE[is.na(test$AGE)]),mean(data$AGE,na.rm=T),sd(data$AGE,na.rm=T))
test$YOJ <- na.aggregate(test$YOJ, test$JOB, mean, na.rm = TRUE)
test$INCOME <- na.aggregate(test$INCOME, test$JOB, mean, na.rm = TRUE)
test$HOME_VAL <- na.aggregate(test$HOME_VAL, test$JOB, mean, na.rm = TRUE )
test$CAR_AGE <- na.aggregate(test$CAR_AGE, test$CAR_TYPE, mean, na.rm = TRUE)
test$CAR_AGE[test$CAR_AGE < 0 ] <- 0 
test$OLDCLAIM <- ifelse(test$CAR_AGE < 5 & !is.na(test$CAR_AGE),0,test$OLDCLAIM)
test$OLDCLAIM <- na.aggregate(test$OLDCLAIM, test$CAR_AGE, mean, na.rm = TRUE )
test$HOME_OWNER <- ifelse(test$HOME_VAL == 0, 'Yes', 'No')
test$SQRT_TRAVTIME <- sqrt(test$TRAVTIME)
test$SQRT_BLUEBOOK <- sqrt(test$BLUEBOOK)

# Bin Income
test$INCOME_bin[is.na(test$INCOME)] <- "NA"
test$INCOME_bin[test$INCOME == 0] <- "Zero"
test$INCOME_bin[test$INCOME >= 1 & test$INCOME < 30000] <- "Low"
test$INCOME_bin[test$INCOME >= 30000 & test$INCOME < 80000] <- "Medium"
test$INCOME_bin[test$INCOME >= 80000] <- "High"
test$INCOME_bin <- factor(test$INCOME_bin)
test$INCOME_bin <- factor(test$INCOME_bin, levels=c("Zero","Low","Medium","High"))

summary(data)
summary(test)

numeric <- subset(data, select = c(AGE, HOMEKIDS, YOJ, INCOME, HOME_VAL, TRAVTIME, BLUEBOOK, TIF,
                                   CLM_FREQ, MVR_PTS, CAR_AGE), na.rm = TRUE)
c <- cor(numeric)
corrplot(c, method = "square")


############# Part 3: Model Development ######################
###  Train/Test split (80/20)  ###
set.seed(100)
# data <- data %>% mutate_if(is.ordered,factor,ordered=FALSE)
data_split <- initial_split(data=data,prop=.8)
data_train <- training(data_split)
data_test <- testing(data_split)

# pre.proc <- data_train %>% preProcess(method = c('center','scale'))
# # transform train/test data
# train.trans <- pre.proc %>% predict(data_train)
# test.trans <- pre.proc %>% predict(data_test)

######  Model 1: Simple Tree (train/test and pruning)  ######
Model1 <- tree(TARGET_FLAG~.-INDEX - TARGET_AMT, data_train)
Model1.pred <- predict(Model1, data_test, type='class')
confusionMatrix(Model1.pred,data_test$TARGET_FLAG)  # Accuracy: .6973; Sens: .7896; Spec: .4505

Model1.reg <- tree(TARGET_AMT ~.-INDEX - TARGET_FLAG, data_train)
cv.Model1.reg <- cv.tree(Model1.reg)
cv.Model1.reg
Model1.reg <- prune.tree(Model1.reg, best=2)
Model1.reg.pred <- predict(Model1.reg, data_test)
1-(sum((Model1.reg.pred - data_test$TARGET_AMT)^2)/sum((Model1.reg$y - mean(data_test$TARGET_AMT))^2))  # .615
RMSE(Model1.reg.pred, data_test$TARGET_AMT)  # 5542.147
MAE(Model1.reg.pred, data_test$TARGET_AMT)   # 2306.847


######  Model 2: Bagging/Random Forest  ######
# ***These models take a long time to train (~20-30 seconds)
trellis.par.set(caretTheme())
# Classification Model
# min.node.size must be 1 for classification and 5 for regression
trCtl <- trainControl(method='repeatedcv',number=10,repeats=3,search='grid')
# Model2 <- train(TARGET_FLAG~., method='ranger', metric='Accuracy', data=data_train[,-c(1,3)],
#                 trControl = trCtl, tuneGrid = expand.grid(mtry=1:ncol(data_train[,-c(1,3)]),
#                                                           splitrule = 'gini', min.node.size=1))
# Model2 # Accuracy: .791
# plot(Model2)  # Best accuracy at mtry=34
Model2 <- train(TARGET_FLAG~., method='ranger', metric='Accuracy', data=data_train[,-c(1,3)],
                trControl = trCtl, tuneGrid = expand.grid(mtry=21,splitrule='gini',min.node.size=1))
Model2  # Training Accuracy: .792
Model2.pred <- predict(Model2,data_test)
confusionMatrix(Model2.pred, data_test$TARGET_FLAG)  # Acc: .7782, Sens: .9242, Spec: .3874

# Regression Model
# Model2.1 <- train(TARGET_AMT~.,method='ranger',data=data_train[,-c(1,2)],
#                   trControl=trCtl,tuneGrid = expand.grid(mtry=1:ncol(data_train[,-c(1,2)]),
#                                                          splitrule='variance',min.node.size=5))
# Model2.1
# plot(Model2.1)  # best mtry value = 4
Model2.1 <- train(TARGET_AMT~.,method='ranger', metric='Rsquared', data=data_train[,-c(1,2)],
                  trControl=trCtl, tuneGrid = expand.grid(mtry=3,splitrule='variance',min.node.size=5))
Model2.1
Model2.1.pred <- predict(Model2.1,data_test)
postResample(Model2.1.pred, data_test$TARGET_AMT) # RMSE: 5424.278; MAE: 2170.102

######  Model 3: Boosting  ######
# Each model takes approximately 30-45 seconds to train due to repeated cross-validation
# Classification
trainCtrl <- trainControl(method='repeatedcv',number=5,repeats=3,search='grid')
Model3 <- train(TARGET_FLAG~.,method='gbm',data=data_train[,-c(1,3)], metric='Accuracy', trControl=trainCtrl,
                      verbose=F, tuneGrid = expand.grid(n.trees=200, interaction.depth=2,
                                                        shrinkage=.255, n.minobsinnode=18))
Model3
# plot(Model3)
Model3.pred <- predict(Model3, data_test)
confusionMatrix(Model3.pred, data_test$TARGET_FLAG)  # Accuracy: .7776; Sensitivity: .9049; Spec: .4369

# Regression
# Model hyperparameters selected by iterating through each parameter for a range of values, best values are used
Model3.1 <- train(TARGET_AMT~.,method='gbm',data=data_train[,-c(1,2)], metric='Rsquared',trControl = trainCtrl,
                          verbose=F, tuneGrid = expand.grid(n.trees=250, interaction.depth=1,
                                                            shrinkage=.086, n.minobsinnode=25))
Model3.1
# plot(Model3.1)
Model3.1.pred <- predict(Model3.1, data_test)
postResample(Model3.1.pred, data_test$TARGET_AMT) # RMSE: 5449.978; MAE: 2138.192

########## Part 4: Model Selection
# Metrics from Module 5 to beat:
# Acc: .796
# Sens: .9300
# Spec: .4236

# Classification performance
confusionMatrix(Model1.pred,data_test$TARGET_FLAG)    # Acc: .6973, Sens: .7896, Spec: .4505  (avg: .6458)
confusionMatrix(Model2.pred,data_test$TARGET_FLAG)    # Acc: .7782, Sens: .9242, Spec: .3874  (avg: .6966)
confusionMatrix(Model3.pred,data_test$TARGET_FLAG)    # Acc: .7776 ,Sens: .9049, Spec: .4369  (avg: .7064) <<<

# Regression performance
postResample(Model1.reg.pred, data_test$TARGET_AMT)  # RMSE: 5542.147, r2: .0135, MAE: 2306.847
postResample(Model2.1.pred, data_test$TARGET_AMT)    # RMSE: 5424.279, r2: .0585, MAE: 2170.102  <<<
postResample(Model3.1.pred, data_test$TARGET_AMT)    # RMSE: 5449.978, r2: .0454, MAE: 2138.192

# We'll choose Model3 and Model3.1, here are their coefficients
Model3$coefnames
Model2.1$coefnames

#### Part 5:  Score Model on Test Data set and output csv file

# Again, double-checking to make sure we don't have any NA's in our Test Data Set
summary(test)

########### STAND ALONE SCORING PROGRAM ###############
##### Model coefficients used to create P_TARGET_FLAG
test$P_TARGET_FLAG <- predict(Model3, newdata = test)
test$P_TARGET_AMT <- as.integer(predict(Model2.1, newdata = test))
summary(test$P_TARGET_AMT)  # check for negative values
# test$P_TARGET_AMT <- ifelse(test$P_TARGET_AMT<0,0,test$P_TARGET_AMT)

#Prediction File 
prediction <- test[c("INDEX","P_TARGET_FLAG","P_TARGET_AMT")]
write.csv(prediction, file = "merten_m7_final.csv")

