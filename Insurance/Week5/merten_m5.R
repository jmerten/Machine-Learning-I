# Title     : Module 5 Data Assignment
# Created by: Jason Merten
# Created on: 2/10/2021

# install.packages(ROCR)
# install.packages(corrplot)
# install.packages(MASS)
# install.packages(tidyverse)
# install.packages(vip)
# install.packages(InformationValue)
# install.packages(caret)
# install.packages(rsample)
# install.packages(e1071)
# install.packages(zoo)
# install.packages(broom)
# install.packages(class)

library(ROCR)
library(corrplot)
library(MASS)
library(tidyverse)
library(vip)
library(InformationValue)
library(caret)
library(rsample)
library(e1071)
library(zoo)
library(broom)
library(class)


# Data Import and Variable Type Changes
setwd("C:/Users/jmert/Documents/W&M/BUAD5122/M5")
data <- read.csv("insurance-training.csv")
test <- read.csv("insurance-test.csv")

summary(data)
### Need to make sure our data is understood correctly by R, since we have a mix of numerical and categorical
data$TARGET_FLAG <- as.factor(data$TARGET_FLAG)
data$SEX <- as.factor(data$SEX)
data$EDUCATION <- as.factor(data$EDUCATION)
data$PARENT1 <- as.factor(data$PARENT1)
data$INCOME <- suppressWarnings(as.numeric(gsub("[^0-9.]", "", data$INCOME)))
data$HOME_VAL <- suppressWarnings(as.numeric(gsub("[^0-9.]", "", data$HOME_VAL)))
data$MSTATUS <- as.factor(data$MSTATUS)
data$REVOKED <- as.factor(data$REVOKED)
data$RED_CAR <- as.factor(ifelse(data$RED_CAR=="yes", 1, 0))
data$URBANICITY <- ifelse(data$URBANICITY == "Highly Urban/ Urban", "Urban", "Rural")
data$URBANICITY <- as.factor(data$URBANICITY)
data$JOB <- as.factor(data$JOB)
data$CAR_USE <- as.factor(data$CAR_USE)
data$CAR_TYPE <- as.factor(data$CAR_TYPE)
data$DO_KIDS_DRIVE <- as.factor(ifelse(data$KIDSDRIV > 0, 1, 0 ))
data$OLDCLAIM <- suppressWarnings(as.numeric(gsub("[^0-9.]", "", data$HOME_VAL)))
data$BLUEBOOK <- suppressWarnings(as.numeric(gsub("[^0-9.]", "", data$BLUEBOOK)))
summary(data)
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
test$RED_CAR <- as.factor(ifelse(test$RED_CAR=="yes", 1, 0))
test$URBANICITY <- ifelse(test$URBANICITY == "Highly Urban/ Urban", "Urban", "Rural")
test$URBANICITY <- as.factor(test$URBANICITY)
test$JOB <- as.factor(test$JOB)
test$CAR_USE <- as.factor(test$CAR_USE)
test$CAR_TYPE <- as.factor(test$CAR_TYPE)
test$DO_KIDS_DRIVE <- as.factor(ifelse(test$KIDSDRIV > 0, 1, 0 ))
test$OLDCLAIM <- suppressWarnings(as.numeric(gsub("[^0-9.]", "", test$HOME_VAL)))
test$BLUEBOOK <- suppressWarnings(as.numeric(gsub("[^0-9.]", "", test$BLUEBOOK)))
summary(test)
# data with NAs: AGE, YOJ, INCOME, HOME_VAL, OLDCLAIM, CAR_AGE

#################### Part 1: Data Exploration ##############################################
# Histograms for Numeric Variables
# AGE : has some outliers (80 > 99th quantile/ 72 < 1st quantile), seems to follow normal distribution
par(mfrow=c(1,2))
hist(data$AGE, col = "red", xlab = "Age", main = "AGE Hist")
data0<- subset(data, TARGET_FLAG == 1 )
boxplot(data$AGE, col = "red", main = "AGE BoxPlot")
qtest(data$AGE)

# TRAVTIME : has 75 outliers > 99th quantile, follows a normal distribution with the exception of a large tail < 3
# YOJ : 39 outliers > 99th quantile, follows normal distribution with the exception of a large tail at 0
par(mfrow=c(2,2))
hist(sqrt(data$TRAVTIME), col = "green", xlab = "SQRT TRAVTIME", main = "SQRT TRAVTIME Hist")
hist(data$YOJ, col = "blue", xlab = "YOJ", main = "YOJ Hist")
boxplot(sqrt(data$TRAVTIME), col = "green", main = "SQRT TRAVTIME BoxPlot")
boxplot(data$YOJ, col = "blue", main = "YOJ BoxPlot")
qtest(data$TRAVTIME)
qtest(data$YOJ)

# BLUEBOOK : 82 outliers > 99th quantile, normal distribution
# TIF : 56 outliers > 99th quantile, diagonal distribution?
hist(sqrt(data$BLUEBOOK), col = "green", xlab = "SQRT BLUEBOOK", main = "SQRT BLUEBOOK Hist")
hist((data$TIF), col = "blue", xlab = "TIF", main = "TIF Hist")
boxplot(sqrt(data$BLUEBOOK), col = "green", main = "SQRT BLUEBOOK BoxPlot")
boxplot(data$TIF, col = "blue", main = "TIF BoxPlot")
qtest(data$BLUEBOOK)
qtest(data$TIF)

# MVR_PTS : 71 outliers > 99th quantile, seems to follow a diagonal/chi distribution
# CAR_AGE : 65 outliers > 99th quantile, 4 outliers < 1st quantile,
# looks like a skewed distribution w/ large amount of 0 values.  contains negative values
hist(data$MVR_PTS, col = "red", xlab = "MVR_PTS", main = "MVR_PTS Hist")
hist(data$CAR_AGE, col = "blue", xlab = "CAR_AGE", main = "CAR_AGE Hist")
boxplot(data$MVR_PTS, col = "red", main = "MVR_PTS BoxPlot")
boxplot(data$CAR_AGE, col = "blue", xlab = "CAR_AGE", main = "CAR_AGE BoxPlot")
qtest(data$MVR_PTS)
qtest(data$CAR_AGE)
par(mfrow=c(1,1))

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
data$HOME_OWNER <- ifelse(data$HOME_VAL == 0, 0, 1)
data$SQRT_TRAVTIME <- sqrt(data$TRAVTIME)
data$SQRT_BLUEBOOK <- sqrt(data$BLUEBOOK)

# Bin Income
data$INCOME_bin[is.na(data$INCOME)] <- "NA"
data$INCOME_bin[data$INCOME == 0] <- "Zero"
data$INCOME_bin[data$INCOME >= 1 & data$INCOME < 30000] <- "Low"
data$INCOME_bin[data$INCOME >= 30000 & data$INCOME < 80000] <- "Medium"
data$INCOME_bin[data$INCOME >= 80000] <- "High"
data$INCOME_bin <- factor(data$INCOME_bin)
data$INCOME_bin <- factor(data$INCOME_bin, levels=c("Zero","Low","Medium","High"))
levels(data$JOB)[levels(data$JOB)==''] <- 'None'

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
test$HOME_OWNER <- ifelse(test$HOME_VAL == 0, 0, 1)
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
levels(test$JOB)[levels(test$JOB)==''] <- 'None'

summary(data)
summary(test)

numeric <- subset(data, select = c(AGE, HOMEKIDS, YOJ, INCOME, HOME_VAL, TRAVTIME, BLUEBOOK, TIF,
                                   CLM_FREQ, MVR_PTS, CAR_AGE), na.rm = TRUE)
c <- cor(numeric)
corrplot(c, method = "square")


############# Part 3: Model Development ######################
###  Train/Test split (80/20)  ###
set.seed(1)
data <- data %>% mutate_if(is.ordered,factor,ordered=FALSE)
data_split <- initial_split(data=data,prop=.8)
data_train <- training(data_split)
data_test <- testing(data_split)

results<-data.frame(data_test$INDEX, data_test$TARGET_FLAG)

# Metrics from Module 3 to beat:
# AIC: 7335.7
# BIC: 7595
# LogLik: 7262
# AUC: .8158
# k-stat: .4662

######  Model 1: Logistic Regression (train/test split)  ######

stepwisemodel <- stepwisemodel <- glm(TARGET_FLAG ~ . - INDEX + MVR_PTS:URBANICITY + SQRT_BLUEBOOK:CAR_TYPE + CLM_FREQ:OLDCLAIM + MSTATUS:HOMEKIDS,
                                      data = data_train, family=binomial)
Model1 <- stepAIC(stepwisemodel, direction = "both")
# summary(Model1)
glance(Model1)
results$Model1Prob <- predict(Model1, newdata=data_test, type = "response")
results$Model1Prediction <- as.factor(ifelse(results$Model1Prob > 0.5,1,0))
confusionMatrix(results$Model1Prediction,data_test$TARGET_FLAG)
# Accuracy: .8021
# AIC: 5912
# BIC: 6204

# Estimate pre-processing parameter #
pre.proc <- data_train %>% preProcess(method = c('center','scale'))
# transform train/test data
train.trans <- pre.proc %>% predict(data_train)
test.trans <- pre.proc %>% predict(data_test)

Model1.1<-train(TARGET_FLAG~.-INDEX - flag_OLDCLAIM + MVR_PTS:URBANICITY + SQRT_BLUEBOOK:CAR_TYPE + CLM_FREQ:OLDCLAIM + MSTATUS:HOMEKIDS,
                data=train.trans,
                method='glm',
                family='binomial',
                trControl=trainControl(method='cv',number=10))
results$Model1.1Predictions <- Model1.1 %>% predict(test.trans)
results$Model1.1Prob <- Model1.1 %>% predict(test.trans,type='prob')
confusionMatrix(results$Model1.1Prediction,test.trans$TARGET_FLAG)
# Accuracy: .8033


######  Model 2: Linear Discriminant Analysis  ######
Model2 <- lda(TARGET_FLAG ~ . - INDEX + MVR_PTS:URBANICITY + SQRT_BLUEBOOK:CAR_TYPE + CLM_FREQ:OLDCLAIM + MSTATUS:HOMEKIDS,
              data = data_train)
Model2
# results$Model2Prediction <-
results$Model2Prob <- predict(Model2, newdata=data_test)$posterior[,2]
results$Model2Prediction <- as.factor(ifelse(results$Model2Prob>0.5,1,0))
confusionMatrix(results$Model2Prediction,data_test$TARGET_FLAG)
# Accuracy: .807

######  Model 3: K-Nearest Neighbors  ######
###### (I did this for practice and to see if it would be a good model, 47% accuracy is slightly better than the initial training data)  ######
# Qualitative variables (exluded from KNN calculation): PARENT1(8), MSTATUS(10), SEX(11), EDUCATION(12), JOB (13),
# CAR_USE(15), CAR_TYPE(18) RED_CAR(19), REVOKED(22), URBANICITY(25)
standardized.X<-scale(data[,-c(1,2,8,10,11,12,13,15,18,19,22,25,26,36)])
train<-1:as.integer(dim(data)[1]*.8)
train.X<-standardized.X[train,]; test.X<-standardized.X[-train,]
train.Y<-data$TARGET_FLAG[train]; test.Y<-data$TARGET_FLAG[-train]

table(data$TARGET_FLAG[-train])
421/(1212+421)    #.2578
Model3<-knn(train.X,test.X,train.Y,k=1)
summary(Model3)
table(Model3,test.Y)
148/(283+148)  # 0.3433

Model3.1 <- knn(train.X,test.X,train.Y,k=3)
summary(Model3.1)
table(Model3.1,test.Y)
119/(119+184)  # 0.3927

Model3.2 <- knn(train.X,test.X,train.Y,k=7)
summary(Model3.2)
table(Model3.2,test.Y)
96/(107+96)  # 0.4729

.4729/.2578   # 183% improvement than test data

########## Part 4: Model Selection
glance(Model1)

# ROC plots with AUC
roc1.perf <- performance(prediction(results$Model1Prob,data_test$TARGET_FLAG), 'tpr','fpr')
auc1 <- performance(prediction(results$Model1Prob,data_test$TARGET_FLAG), 'auc')@y.values
roc1.1.perf <- performance(prediction(results$Model1.1Prob[,2],test.trans$TARGET_FLAG), 'tpr','fpr')
auc1.1 <- performance(prediction(results$Model1.1Prob[2],test.trans$TARGET_FLAG), 'auc')@y.values
roc2.perf <- performance(prediction(results$Model2Prob,test.trans$TARGET_FLAG), 'tpr','fpr')
auc2 <- performance(prediction(results$Model2Prob,test.trans$TARGET_FLAG), 'auc')@y.values

par(mfrow=c(1,3))
plot(roc1.perf,colorize=T,lwd=2); abline(a=0,b=1); text(x=.35,y=.65,paste0("AUC = ",round(auc1[[1]],3)))
plot(roc1.1.perf,colorize=T,lwd=2); abline(a=0,b=1); text(x=.35,y=.65,paste0("AUC = ",round(auc1.1[[1]],3)))
plot(roc2.perf,colorize=T,lwd=2); abline(a=0,b=1); text(x=.35,y=.65,paste0("AUC = ",round(auc2[[1]],3)))

confusionMatrix(results$Model1Prediction,data_test$TARGET_FLAG)    # Acc: .7947, Sens: .9283, Spec: .4236
confusionMatrix(results$Model1.1Prediction,test.trans$TARGET_FLAG) # Acc: .796, Sens: .9300, Spec: .4236
confusionMatrix(results$Model2Prediction,test.trans$TARGET_FLAG)   # Acc: .7929 , Sens: .9292, Spec: .4144

# We'll choose Model 1.2, here are its coefficients
coef(Model1.1)

#### Part 5:  Score Model on Test Data set and output csv file

# Again, double-checking to make sure we don't have any NA's in our Test Data Set
summary(test)

########### STAND ALONE SCORING PROGRAM ###############
##### Model coefficients used to create P_TARGET_FLAG
test$P_TARGET_FLAG <- predict(Model1.1, newdata = test)

#Prediction File 
prediction <- test[c("INDEX","P_TARGET_FLAG")]
write.csv(prediction, file = "merten_m5_final.csv")

