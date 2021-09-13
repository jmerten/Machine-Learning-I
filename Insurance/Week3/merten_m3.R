# Title     : Module 3 Data Assignment
# Created by: Jason Merten
# Created on: 1/29/2021

# install.packages(ROCR)
# install.packages(corrplot)
# install.packages(car)
# install.packages(MASS)
# install.packages(leaps)
# install.packages(glm2)
# install.packages(zoo)
# install.packages(class)
# install.packages(InformationValue)

library(zoo)
library(ROCR)
library(corrplot)
library(car)
library(InformationValue)
library(leaps)
library(MASS)
library(glm2)
library(class)

# Data Import and Variable Type Changes
setwd("C:/Users/jmert/Documents/W&M/BUAD5122/M3")
data <- read.csv("insurance-training.csv")
test <- read.csv("insurance-test.csv")

summary(data)
### Need to make sure our data is understood correctly by R, since we have a mix of numerical and categorical
data$INDEX <- as.factor(data$INDEX)
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
test$INDEX <- as.factor(test$INDEX)
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
par(mfrow=c(2,2))
hist(data$AGE, col = "red", xlab = "Age", main = "AGE Hist")
data0<- subset(data, TARGET_FLAG == 1 )
boxplot(data$AGE, col = "red", main = "AGE BoxPlot")
qtest(data$AGE)

# TRAVTIME : has 75 outliers > 99th quantile, follows a normal distribution with the exception of a large tail < 3
# YOJ : 39 outliers > 99th quantile, follows normal distribution with the exception of a large tail at 0
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
data$INCOME_bin <- factor(data$INCOME_bin, levels=c("NA","Zero","Low","Medium","High"))

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
test$INCOME_bin <- factor(test$INCOME_bin, levels=c("NA","Zero","Low","Medium","High"))

summary(data)
summary(test)

numeric <- subset(data, select = c(AGE, HOMEKIDS, YOJ, INCOME, HOME_VAL, TRAVTIME, BLUEBOOK, TIF,
                                   CLM_FREQ, MVR_PTS, CAR_AGE), na.rm = TRUE)
c <- cor(numeric)
corrplot(c, method = "square")

############# Part 3: Model Development ######################
results<-data.frame(data$INDEX, data$TARGET_FLAG)
# Model Development for TARGET_FLAG
######  Model 1: Logistic Regression  ######
Model1 <- glm(TARGET_FLAG ~ . - INDEX - KIDSDRIV - INCOME - JOB - SEX - HOME_VAL - HOMEKIDS - RED_CAR - CAR_AGE - TRAVTIME - OLDCLAIM - flag_AGE - flag_CAR_AGE - flag_OLDCLAIM - flag_HOME_VAL - flag_INCOME - flag_YOJ,
              data = data, family = binomial)
summary(Model1) # AIC 7336
results$Model1Prediction <- predict(Model1, type = "response")

Model1.1 <- glm(TARGET_FLAG ~ MVR_PTS*URBANICITY + SQRT_TRAVTIME + SQRT_BLUEBOOK*CAR_TYPE + CAR_TYPE + CAR_USE + CLM_FREQ*OLDCLAIM + MSTATUS*HOMEKIDS + KIDSDRIV + REVOKED + INCOME_bin + PARENT1 + TIF, data=data, family=binomial)
summary(Model1.1)  # AIC 7413
results$Model1.1Prediction <- predict(Model1.1, type = "response")

stepwisemodel <- stepwisemodel <- glm(TARGET_FLAG ~ . - INDEX, data = data, family=binomial)
Model1.2 <- stepAIC(stepwisemodel, direction = "both")
summary(Model1.2)
results$Model1.2Prediction <- predict(Model1.2, type = "response")

######  Model 2: Linear Discriminant Analysis  ######
Model2 <- lda(TARGET_FLAG ~ MVR_PTS*URBANICITY + SQRT_TRAVTIME + SQRT_BLUEBOOK*CAR_TYPE + CAR_TYPE + CAR_USE + CLM_FREQ*OLDCLAIM + MSTATUS*HOMEKIDS + KIDSDRIV + REVOKED + INCOME_bin + PARENT1 + TIF,
                data = data)
Model2
results$Model2Prediction <- predict(Model2, type='response')$posterior[,2]


######  Model 3: Quadratic Discriminant Analysis  ######
Model3 <- qda(TARGET_FLAG ~ AGE + JOB + YOJ + EDUCATION + INCOME + TRAVTIME + CLM_FREQ + RED_CAR + CAR_TYPE + SEX,
              data = data)
Model3
results$Model3Prediction <- predict(Model3, type = "response")$posterior[,2]


######  Model 4: K-Nearest Neighbors  ######
###### (I did this for practice and to see if it would be a good model, 47% accuracy is slightly better than the initial training data)  ######
# Qualitative variables (exluded from KNN calculation): PARENT1(8), MSTATUS(10), SEX(11), EDUCATION(12), JOB (13),
# CAR_USE(15), CAR_TYPE(18) RED_CAR(19), REVOKED(22), URBANICITY(25)
standardized.X<-scale(data[,-c(1,2,8,10,11,12,13,15,18,19,22,25,26,36)])
train<-1:as.integer(8161*.8)
train.X<-standardized.X[train,]; test.X<-standardized.X[-train,]
train.Y<-data$TARGET_FLAG[train]; test.Y<-data$TARGET_FLAG[-train]

table(data$TARGET_FLAG[-train])
421/1212    #.3473
Model4 <- knn(train.X,test.X,train.Y,k=1)
summary(Model4)
table(Model4,test.Y)
148/(283+148)  # 0.3433

Model4.1 <- knn(train.X,test.X,train.Y,k=3)
summary(Model4.1)
table(Model4.1,test.Y)
103/(103+132)  # 0.4382

Model4.2 <- knn(train.X,test.X,train.Y,k=7)
summary(Model4.2)
table(Model4.2,test.Y)
96/(107+96)  # 0.4729

.4729/.3473   # 36% improvement than test data

########## Part 4: Model Selection 
AIC(Model1)
AIC(Model1.1)
AIC(Model1.2)  # Lowest AIC value
BIC(Model1)    # Lowest BIC value
BIC(Model1.1)
BIC(Model1.2)

print(-2*logLik(Model1, REML = TRUE))
print(-2*logLik(Model1.1, REML = TRUE))
print(-2*logLik(Model1.2, REML = TRUE))  # Lowest logLik value

# ROC plots
roc1 <- performance(prediction(results$Model1Prediction,data$TARGET_FLAG), 'tpr','fpr')
roc1.1 <- performance(prediction(results$Model1.1Prediction,data$TARGET_FLAG), 'tpr','fpr')
roc1.2 <- performance(prediction(results$Model1.2Prediction,data$TARGET_FLAG), 'tpr','fpr')
roc2 <- performance(prediction(results$Model2Prediction,data$TARGET_FLAG), 'tpr','fpr')
roc3 <- performance(prediction(results$Model3Prediction,data$TARGET_FLAG), 'tpr','fpr')
par(mfrow=c(2,3))
plot(roc1,colorize=T,lwd=2); abline(a=0,b=1)
plot(roc1.1,colorize=T,lwd=2); abline(a=0,b=1)
plot(roc1.2,colorize=T,lwd=2); abline(a=0,b=1)
plot(roc2,colorize=T,lwd=2); abline(a=0,b=1)
plot(roc3,colorize=T,lwd=2); abline(a=0,b=1)  # worst ROC curve

# AUC values
auc1 <- performance(prediction(results$Model1Prediction,data$TARGET_FLAG), 'auc')
auc1.1 <- performance(prediction(results$Model1.1Prediction,data$TARGET_FLAG), 'auc')
auc1.2 <- performance(prediction(results$Model1.2Prediction,data$TARGET_FLAG), 'auc')
auc2 <- performance(prediction(results$Model2Prediction,data$TARGET_FLAG), 'auc')
auc3 <- performance(prediction(results$Model3Prediction,data$TARGET_FLAG), 'auc')
print(auc1@y.values)
print(auc1.1@y.values)
print(auc1.2@y.values)  # Highest AUC value
print(auc2@y.values)
print(auc3@y.values)

ks_stat(actuals=data$TARGET_FLAG, predictedScores=results$Model1Prediction)
ks_stat(actuals=data$TARGET_FLAG, predictedScores=results$Model1.1Prediction)
ks_stat(actuals=data$TARGET_FLAG, predictedScores=results$Model1.2Prediction)  # highest value
ks_stat(actuals=data$TARGET_FLAG, predictedScores=results$Model2Prediction)
ks_stat(actuals=data$TARGET_FLAG, predictedScores=results$Model3Prediction)

# We'll choose Model 3, here are its coefficients
coef(Model1.2)

#### Part 5:  Score Model on Test Data set and output csv file

# Again, double-checking to make sure we don't have any NA's in our Test Data Set
summary(test)

########### STAND ALONE SCORING PROGRAM ###############
##### Model coefficients used to create P_TARGET_FLAG
test$P_TARGET_FLAG <- predict(Model1.2, newdata = test, type = "response")

#Prediction File 
prediction <- test[c("INDEX","P_TARGET_FLAG")]
write.csv(prediction, file = "merten_m3_final.csv")

