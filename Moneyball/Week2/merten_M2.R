# Title     : Module 2: Data Assignment
# Created by: Jason Merten
# Created on: 1/21/2021

# install.packages(MASS)
# install.packages(broom)
# install.packages(tidyverse)
# install.packages(car)
# install.packages(leaps)
library(MASS)
library(broom)
library(tidyverse)
library(car)
library(leaps)

setwd('C:/Users/jmert/Documents/W&M/BUAD5122/M2')
train <- read.csv('buad5122-m2-moneyball-training.csv')
test <- read.csv('buad5122-m2-moneyball-test.csv')
summary(train)
summary(test)

# Create Single Hit Column
train$TEAM_BATTING_1B <- train$TEAM_BATTING_H - train$TEAM_BATTING_2B - train$TEAM_BATTING_3B - train$TEAM_BATTING_HR
test$TEAM_BATTING_1B <- test$TEAM_BATTING_H - test$TEAM_BATTING_2B - test$TEAM_BATTING_3B - test$TEAM_BATTING_HR

# Create Stolen Base Percentage Column
train$SB_P <- train$TEAM_BASERUN_SB/(1.0*train$TEAM_BASERUN_CS+train$TEAM_BASERUN_SB)
train$SB_P[is.na(train$SB_P)]<-mean(train$SB_P,na.rm=TRUE)
test$SB_P <- test$TEAM_BASERUN_SB/(test$TEAM_BASERUN_CS+test$TEAM_BASERUN_SB)
test$SB_P[is.na(test$SB_P)]<-mean(train$SB_P,na.rm=TRUE)

######## Fix train/test NA values and outliers ########
# Batting Strike Outs
train$flag_TEAM_BATTING_SO <- as.factor(ifelse(is.na(train$TEAM_BATTING_SO),1,0))
train$TEAM_BATTING_SO[is.na(train$TEAM_BATTING_SO)] <- as.integer(rnorm(length(train$TEAM_BATTING_SO[is.na(train$TEAM_BATTING_SO)]),mean(train$TEAM_BATTING_SO, na.rm=TRUE),sd(train$TEAM_BATTING_SO,na.rm=TRUE)))
test$flag_TEAM_BATTING_SO <- as.factor(ifelse(is.na(test$TEAM_BATTING_SO),1,0))
test$TEAM_BATTING_SO[is.na(test$TEAM_BATTING_SO)] <- as.integer(rnorm(length(test$TEAM_BATTING_SO[is.na(test$TEAM_BATTING_SO)]),mean(train$TEAM_BATTING_SO, na.rm=TRUE),sd(train$TEAM_BATTING_SO,na.rm=TRUE)))
summary(train$TEAM_BATTING_SO)
summary(test$TEAM_BATTING_SO)

# Stolen Bases
par(mfrow=c(1,3))
boxplot(train$TEAM_BASERUN_SB); hist(train$TEAM_BASERUN_SB); plot(train$TEAM_BASERUN_SB)
summary(train$TEAM_BASERUN_SB)
# determine number of outliers
table(train$TEAM_BASERUN_SB > quantile(train$TEAM_BASERUN_SB,.99,na.rm=TRUE))
outlier <- as.integer(quantile(train$TEAM_BASERUN_SB,.99,na.rm=TRUE))
# handle outliers
# train dataset
train$outlier_TEAM_BASERUN_SB <- as.factor(ifelse(train$TEAM_BASERUN_SB > outlier,1,0))
train$TEAM_BASERUN_SB[train$TEAM_BASERUN_SB > quantile(train$TEAM_BASERUN_SB,.99,na.rm=TRUE)] <- outlier
train$outlier_TEAM_BASERUN_SB[is.na(train$outlier_TEAM_BASERUN_SB)] <- 0
# test dataset
test$outlier_TEAM_BASERUN_SB <- as.factor(ifelse(test$TEAM_BASERUN_SB > outlier,1,0))
test$TEAM_BASERUN_SB[test$TEAM_BASERUN_SB > quantile(train$TEAM_BASERUN_SB,.99,na.rm=TRUE)] <- outlier
test$outlier_TEAM_BASERUN_SB[is.na(test$outlier_TEAM_BASERUN_SB)] <- 0
# handle NA's
train$flag_TEAM_BASERUN_SB <- as.factor(ifelse(is.na(train$TEAM_BASERUN_SB),1,0))
train$TEAM_BASERUN_SB[is.na(train$TEAM_BASERUN_SB)] <- as.integer(rgamma(length(train$TEAM_BASERUN_SB[is.na(train$TEAM_BASERUN_SB)]),1.25,.012))
test$flag_TEAM_BASERUN_SB <- as.factor(ifelse(is.na(test$TEAM_BASERUN_SB),1,0))
test$TEAM_BASERUN_SB[is.na(test$TEAM_BASERUN_SB)] <- as.integer(rgamma(length(test$TEAM_BASERUN_SB[is.na(test$TEAM_BASERUN_SB)]),1.25,.012))
summary(train$TEAM_BASERUN_SB)
summary(test$TEAM_BASERUN_SB)

# Caught Stealing
# Suggest removing this column, there are 772 missing values (33.9%)
par(mfrow=c(1,3))
boxplot(train$TEAM_BASERUN_CS, main='Boxplot of Caught Stealing'); hist(train$TEAM_BASERUN_CS); plot(train$TEAM_BASERUN_CS)
summary(train$TEAM_BASERUN_CS)
# handle outliers first
train$outlier_TEAM_BASERUN_CS <- as.factor(ifelse(train$TEAM_BASERUN_CS > quantile(train$TEAM_BASERUN_CS,.99,na.rm=TRUE),1,0))
train$outlier_TEAM_BASERUN_CS[is.na(train$outlier_TEAM_BASERUN_CS)==TRUE] <- 0  # replace any NA outlier variable as 0
train$TEAM_BASERUN_CS[train$TEAM_BASERUN_CS > quantile(train$TEAM_BASERUN_CS,.99,na.rm=TRUE)] <- as.integer(quantile(train$TEAM_BASERUN_CS,.99,na.rm=TRUE))
# test dataset
test$outlier_TEAM_BASERUN_CS <- as.factor(ifelse(test$TEAM_BASERUN_CS > quantile(train$TEAM_BASERUN_CS,.99,na.rm=TRUE),1,0))
test$outlier_TEAM_BASERUN_CS[is.na(test$outlier_TEAM_BASERUN_CS)==TRUE] <- 0  # replace any NA outlier variable as 0
test$TEAM_BASERUN_CS[test$TEAM_BASERUN_CS > quantile(train$TEAM_BASERUN_CS,.99,na.rm=TRUE)] <- as.integer(quantile(train$TEAM_BASERUN_CS,.99,na.rm=TRUE))
# Data looks to mostly follow a normal distribution with the exception of the transformed outliers
train$flag_TEAM_BASERUN_CS <- as.factor(ifelse(is.na(train$TEAM_BASERUN_CS),1,0))
train$TEAM_BASERUN_CS[is.na(train$TEAM_BASERUN_CS)] <- as.integer(rnorm(length(train$TEAM_BASERUN_CS[is.na(train$TEAM_BASERUN_CS)]), mean(train$TEAM_BASERUN_CS,na.rm=TRUE),sd(train$TEAM_BASERUN_CS,na.rm=TRUE)))
train$TEAM_BASERUN_CS <- ifelse(train$TEAM_BASERUN_CS < 0,0,train$TEAM_BASERUN_CS) # change any value less than 0 to 0
# test dataset
test$flag_TEAM_BASERUN_CS <- as.factor(ifelse(is.na(test$TEAM_BASERUN_CS),1,0))
test$TEAM_BASERUN_CS[is.na(test$TEAM_BASERUN_CS)] <- as.integer(rnorm(length(test$TEAM_BASERUN_CS[is.na(test$TEAM_BASERUN_CS)]), mean(train$TEAM_BASERUN_CS,na.rm=TRUE),sd(train$TEAM_BASERUN_CS,na.rm=TRUE)))
test$TEAM_BASERUN_CS <- ifelse(test$TEAM_BASERUN_CS < 0,0,test$TEAM_BASERUN_CS) # change any value less than 0 to 0

# Hit by Pitch
# Would recommend removing this column because there are 2085 missing values (91%)
boxplot(train$TEAM_BATTING_HBP, main='Boxplot of Hit by Pitch'); hist(train$TEAM_BATTING_HBP); plot(train$TEAM_BATTING_HBP)
summary(train$TEAM_BATTING_HBP)
# no major outliers, data appears to follow a normal distribution
train$flag_TEAM_BATTING_HBP <- as.factor(ifelse(is.na(train$TEAM_BATTING_HBP),1,0))
train$TEAM_BATTING_HBP[is.na(train$TEAM_BATTING_HBP)] <- as.integer(rnorm(length(train$TEAM_BATTING_HBP[is.na(train$TEAM_BATTING_HBP)]),mean(train$TEAM_BATTING_HBP,na.rm=TRUE),sd(train$TEAM_BATTING_HBP,na.rm=TRUE)))
# test dataset
test$flag_TEAM_BATTING_HBP <- as.factor(ifelse(is.na(test$TEAM_BATTING_HBP),1,0))
test$TEAM_BATTING_HBP[is.na(test$TEAM_BATTING_HBP)] <- as.integer(rnorm(length(test$TEAM_BATTING_HBP[is.na(test$TEAM_BATTING_HBP)]),mean(train$TEAM_BATTING_HBP,na.rm=TRUE),sd(train$TEAM_BATTING_HBP,na.rm=TRUE)))
boxplot(test$TEAM_BATTING_HBP, main='Boxplot of Hit by Pitch'); hist(test$TEAM_BATTING_HBP); plot(test$TEAM_BATTING_HBP)

# Pitching walks allowed
boxplot(train$TEAM_PITCHING_BB, main='Boxplot of Pitching Walks Allowed'); hist(train$TEAM_PITCHING_BB); plot(train$TEAM_PITCHING_BB)
summary(train$TEAM_PITCHING_BB)
outlier <- as.integer(quantile(train$TEAM_PITCHING_BB,.99))
train$outlier_TEAM_PITCHING_BB <- as.factor(ifelse(train$TEAM_PITCHING_BB > outlier,1,0))
train$TEAM_PITCHING_BB[train$TEAM_PITCHING_BB > outlier] <- outlier
# test dataset
test$outlier_TEAM_PITCHING_BB <- as.factor(ifelse(test$TEAM_PITCHING_BB > outlier,1,0))
test$TEAM_PITCHING_BB[test$TEAM_PITCHING_BB > outlier] <- outlier

# Pitching Strikeouts; contains 102 NA values (~5%)
boxplot(train$TEAM_PITCHING_SO, main='Boxplot of Pitching Strikeouts'); hist(train$TEAM_PITCHING_SO); plot(train$TEAM_PITCHING_SO)
summary(train$TEAM_PITCHING_SO)
# There are extreme outliers in this data set (22 greater than 99th quantile)
table(train$TEAM_PITCHING_SO > quantile(train$TEAM_PITCHING_SO,.99,na.rm=TRUE))
# Handle outliers first, then see if there's a distribution
train$outlier_TEAM_PITCHING_SO <- as.factor(ifelse(train$TEAM_PITCHING_SO > quantile(train$TEAM_PITCHING_SO,.99,na.rm=TRUE),1,0))
train$outlier_TEAM_PITCHING_SO[is.na(train$outlier_TEAM_PITCHING_SO)==TRUE] <- 0  # replace any NA outlier variable as 0
train$TEAM_PITCHING_SO[train$TEAM_PITCHING_SO > quantile(train$TEAM_PITCHING_SO,.99,na.rm=TRUE)] <- as.integer(quantile(train$TEAM_PITCHING_SO,.99,na.rm=TRUE))
# test dataset
test$outlier_TEAM_PITCHING_SO <- as.factor(ifelse(test$TEAM_PITCHING_SO > quantile(train$TEAM_PITCHING_SO,.99,na.rm=TRUE),1,0))
test$outlier_TEAM_PITCHING_SO[is.na(test$outlier_TEAM_PITCHING_SO)==TRUE] <- 0  # replace any NA outlier variable as 0
test$TEAM_PITCHING_SO[test$TEAM_PITCHING_SO > quantile(train$TEAM_PITCHING_SO,.99,na.rm=TRUE)] <- as.integer(quantile(train$TEAM_PITCHING_SO,.99,na.rm=TRUE))
# handle NA's
train$flag_TEAM_PITCHING_SO <- as.factor(ifelse(is.na(train$TEAM_PITCHING_SO),1,0))
train$TEAM_PITCHING_SO[is.na(train$TEAM_PITCHING_SO)] <- as.integer(rnorm(length(train$TEAM_PITCHING_SO[train$flag_TEAM_PITCHING_SO==1]),mean(train$TEAM_PITCHING_SO,na.rm=TRUE),sd(train$TEAM_PITCHING_SO,na.rm=TRUE)))
# test dataset
test$flag_TEAM_PITCHING_SO <- as.factor(ifelse(is.na(test$TEAM_PITCHING_SO),1,0))
test$TEAM_PITCHING_SO[is.na(test$TEAM_PITCHING_SO)] <- as.integer(rnorm(length(test$TEAM_PITCHING_SO[test$flag_TEAM_PITCHING_SO==1]),mean(train$TEAM_PITCHING_SO,na.rm=TRUE),sd(train$TEAM_PITCHING_SO,na.rm=TRUE)))

# Fielding Double Plays; contains 286 NA values (~10%)
boxplot(train$TEAM_FIELDING_DP, main='Boxplot of Double Plays'); hist(train$TEAM_FIELDING_DP); plot(train$TEAM_FIELDING_DP)
summary(train$TEAM_FIELDING_DP)
# No apparent significant outliers
# Data looks to be following a normal distribution
train$flag_TEAM_FIELDING_DP <- as.factor(ifelse(is.na(train$TEAM_FIELDING_DP),1,0))
train$TEAM_FIELDING_DP[is.na(train$TEAM_FIELDING_DP)] <- as.integer(rnorm(length(train$TEAM_FIELDING_DP[train$flag_TEAM_FIELDING_DP==1]),mean(train$TEAM_FIELDING_DP,na.rm=TRUE),sd(train$TEAM_FIELDING_DP,na.rm=TRUE)))
# test dataset
test$flag_TEAM_FIELDING_DP <- as.factor(ifelse(is.na(test$TEAM_FIELDING_DP),1,0))
test$TEAM_FIELDING_DP[is.na(test$TEAM_FIELDING_DP)] <- as.integer(rnorm(length(test$TEAM_FIELDING_DP[test$flag_TEAM_FIELDING_DP==1]),mean(train$TEAM_FIELDING_DP,na.rm=TRUE),sd(train$TEAM_FIELDING_DP,na.rm=TRUE)))

# Pitching hits allowed
boxplot(train$TEAM_PITCHING_H, main='Boxplot of Pitching Hits Allowed'); hist(train$TEAM_PITCHING_H); plot(train$TEAM_PITCHING_H)
summary(train$TEAM_PITCHING_H)
outlier <- as.integer(quantile(train$TEAM_PITCHING_H,.99))
train$outlier_TEAM_PITCHING_H <- as.factor(ifelse(train$TEAM_PITCHING_H > outlier,1,0))
train$TEAM_PITCHING_H[train$TEAM_PITCHING_H > outlier] <- outlier
# test dataset
test$outlier_TEAM_PITCHING_H <- as.factor(ifelse(test$TEAM_PITCHING_H > outlier,1,0))
test$TEAM_PITCHING_H[test$TEAM_PITCHING_H > outlier] <- outlier

# Batting Triples
boxplot(train$TEAM_BATTING_3B, main='Boxplot of Batting Triples'); hist(train$TEAM_BATTING_3B); plot(train$TEAM_BATTING_3B)
summary(train$TEAM_BATTING_3B)
outlier <- as.integer(quantile(train$TEAM_BATTING_3B,.99))
train$outlier_TEAM_BATTING_3B <- as.factor(ifelse(train$TEAM_BATTING_3B > outlier,1,0))
train$TEAM_BATTING_3B[train$TEAM_BATTING_3B > outlier] <- outlier
# test dataset
test$outlier_TEAM_BATTING_3B <- as.factor(ifelse(test$TEAM_BATTING_3B > outlier,1,0))
test$TEAM_BATTING_3B[test$TEAM_BATTING_3B > outlier] <- outlier

# Fielding Errors
boxplot(train$TEAM_FIELDING_E, main='Boxplot of Fielding Errors'); hist(train$TEAM_FIELDING_E); plot(train$TEAM_FIELDING_E)
summary(train$TEAM_FIELDING_E)
outlier <- as.integer(quantile(train$TEAM_FIELDING_E,.99))
train$outlier_TEAM_FIELDING_E <- as.factor(ifelse(train$TEAM_FIELDING_E > outlier,1,0))
train$TEAM_FIELDING_E[train$TEAM_FIELDING_E > outlier] <- outlier
# test dataset
test$outlier_TEAM_FIELDING_E <- as.factor(ifelse(test$TEAM_FIELDING_E > outlier,1,0))
test$TEAM_FIELDING_E[test$TEAM_FIELDING_E > outlier] <- outlier

# Batting Single Base
boxplot(train$TEAM_BATTING_1B, main='Boxplot of Batting Singles'); hist(train$TEAM_BATTING_1B); plot(train$TEAM_BATTING_1B)
summary(train$TEAM_BATTING_1B)
outlier <- as.integer(quantile(train$TEAM_BATTING_1B,.99))
train$outlier_TEAM_BATTING_1B <- as.factor(ifelse(train$TEAM_BATTING_1B > outlier,1,0))
train$TEAM_BATTING_1B[train$TEAM_BATTING_1B > outlier] <- outlier
# test dataset
test$outlier_TEAM_BATTING_1B <- as.factor(ifelse(test$TEAM_BATTING_1B > outlier,1,0))
test$TEAM_BATTING_1B[test$TEAM_BATTING_1B > outlier] <- outlier

par(mfrow=c(1,1))

panel.cor <- function(x, y, digits=2, prefix="", cex.cor)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}

# Batting stats and wins
# pairs(train[c(2:8,11,18,19)], lower.panel = panel.smooth, upper.panel = panel.cor)
# # Baserun stats and wins
# pairs(~ train$TARGET_WINS + train$TEAM_BASERUN_CS + train$TEAM_BASERUN_SB, lower.panel = panel.smooth)
# # Pitching stats and wins
# pairs(~ train$TARGET_WINS + train$TEAM_PITCHING_SO + train$TEAM_PITCHING_BB + train$TEAM_PITCHING_H + train$TEAM_PITCHING_HR, lower.panel = panel.smooth)
# # Wins with baseruns, hit by pitch, and pitching hits/home runs
# pairs(train[c(2,9,10,11,12,13)], lower.panel = panel.smooth)

########## Model Creation ##########

mse <- function(sm)
  mean(sm$residuals^2)

per <- function(sm)
  glance(sm)$sigma / mean(train$TARGET_WINS)

########### Model Creation ###########
# Model 1: Stepwise model
stepwisemodel <- lm(TARGET_WINS ~ ., data = train)
stepwise <- stepAIC(stepwisemodel, direction = "both")
summary(stepwise)

# Stepwise R^2: .4342, aR^2: .4289

# Model 2: Subset Regression
subsets <- regsubsets(TARGET_WINS ~ .,  data = train, nbest = 1, method = "exhaustive", nvmax=14, really.big=T)
plot(subsets, scale="adjr2")
summary(subsets)$rsq    # .4159

subset <- lm(TARGET_WINS ~
               TEAM_BATTING_H + TEAM_BATTING_2B + TEAM_BATTING_BB + TEAM_BATTING_SO + TEAM_BASERUN_SB +
               TEAM_PITCHING_H + TEAM_FIELDING_E + TEAM_FIELDING_DP + TEAM_BATTING_1B + flag_TEAM_BASERUN_SB +
               flag_TEAM_FIELDING_DP + outlier_TEAM_PITCHING_H , data = train)
summary(subset)

# Model 3: Custom Model
# Initial model
model3 <- lm(TARGET_WINS ~. - INDEX - flag_TEAM_PITCHING_SO,data=train)
# Backward selection
model3.1 <- lm(TARGET_WINS ~. - INDEX - flag_TEAM_PITCHING_SO - TEAM_BATTING_HR - TEAM_BATTING_SO - outlier_TEAM_FIELDING_E - TEAM_BATTING_HBP - SB_P - outlier_TEAM_PITCHING_BB - outlier_TEAM_PITCHING_SO - outlier_TEAM_PITCHING_H, data=train)
# Mixed selection with interactions
model3.2 <- lm(TARGET_WINS ~ TEAM_BATTING_2B + TEAM_BATTING_3B + outlier_TEAM_BATTING_3B + TEAM_BATTING_HR + TEAM_BATTING_H*TEAM_BATTING_BB + TEAM_BATTING_SO*flag_TEAM_BATTING_SO + TEAM_PITCHING_H + TEAM_PITCHING_BB + TEAM_FIELDING_DP + TEAM_FIELDING_E + TEAM_FIELDING_DP:flag_TEAM_FIELDING_DP + TEAM_BASERUN_SB*flag_TEAM_BASERUN_SB + TEAM_BASERUN_CS + outlier_TEAM_BASERUN_CS, data=train)

summary(model3)
vif(model3)
summary(model3.1)
vif(model3.1)
summary(model3.2)
vif(model3.2)

# Each custom model has a few high VIF values, but removing them drastically reduces the R^2 of the model

# Model3:   R^2 = .4354, aR^2 = .4279
# Model3.1: R^2 = .4346, aR^2 = .4291
# Model3.2: R^2 = .4484, aR^2 = .4435

# Visual diagnostic plots of each model
par(mfrow=c(2,2))
plot(stepwise)
plot(subset) # not as strong of fit in the QQ model
plot(model3.1)
plot(model3.2)

######## Performance #######
glance(stepwise) %>%
  dplyr::select(r.squared, adj.r.squared, sigma, AIC, BIC, p.value)
glance(subset) %>%
  dplyr::select(r.squared, adj.r.squared, sigma, AIC, BIC, p.value)
glance(model3.1) %>%
  dplyr::select(r.squared, adj.r.squared, sigma, AIC, BIC, p.value)
glance(model3.2) %>%
  dplyr::select(r.squared, adj.r.squared, sigma, AIC, BIC, p.value)
mse(stepwise)
mse(subset)
mse(model3.1)
mse(model3.2) # lowest MSE (136.7995)
per(stepwise)
per(subset)
per(model3.1)
per(model3.2) # lowest prediction error rate

# Stand Alone Scoring
# Predictions and corrections
test$P_TARGET_WINS <- as.integer(predict.lm(model3.2, newdata=test))
outlier_l <- as.integer(quantile(test$P_TARGET_WINS,.01))
outlier_h <- as.integer(quantile(test$P_TARGET_WINS,.99))
test$P_TARGET_WINS[test$P_TARGET_WINS > outlier_h] <- outlier_h
test$P_TARGET_WINS[test$P_TARGET_WINS < outlier_l] <- outlier_l

#subset of data set for the deliverable "Scored data file"
prediction <- test[c("INDEX","P_TARGET_WINS")]

# Write results to CSV
write.csv(prediction, 'merten_M2_predict.csv')