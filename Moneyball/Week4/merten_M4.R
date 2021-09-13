# Title     : Module 4: Data Assignment
# Created by: Jason Merten
# Created on: 2/06/2021

# install.packages(MASS)
# install.packages(broom)
# install.packages(tidyverse)
# install.packages(car)
# install.packages(leaps)
# install.packages(boot)
# install.packages(glmnet)
# install.packages(pls)
# install.packages(splines)
# install.packages(gam)
# install.packages(akima)

library(MASS)
library(broom)
library(tidyverse)
library(car)
library(leaps)
library(boot)
library(glmnet)
library(pls)
library(splines)
library(gam)
# library(akima)

setwd('C:/Users/jmert/Documents/W&M/BUAD5122/M4')
train <- read.csv('moneyball-training.csv')
test <- read.csv('moneyball-test.csv')
summary(train)
summary(test)

# (Training) Columns with NAs: BATTING_SO, BASERUN_SB, BASERUN_CS, BATTING_HBP, PITCHING_SO, FIELDING_DP
# (Test) Columns with NAs: BATTING_SO, BASERUN_SB, BASERUN_CS, BATTING_HBP, PITCHING_SO, FIELDING_DP

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

########## Model Creation ##########

# AIC to beat: 17694
# BIC to beat: 17820
# Adjusted R2 to beat: .444

# Create train/test split
train_split<-1:(nrow(train)*.8)
test_split<-(nrow(train)*.8):nrow(train)

# Model 1: Best Subset w/ Cross Validation
model1<- regsubsets(TARGET_WINS ~ . + TEAM_BATTING_H*TEAM_BATTING_BB + TEAM_BATTING_SO*flag_TEAM_BATTING_SO +
  TEAM_FIELDING_DP:flag_TEAM_FIELDING_DP + TEAM_BASERUN_SB*flag_TEAM_BASERUN_SB,
                    data = train[train_split,], nbest = 1, method = 'exhaustive', nvmax=28, really.big=T)
plot(model1,scale='adjr2')
test.mat<-model.matrix(TARGET_WINS~.+ TEAM_BATTING_H*TEAM_BATTING_BB + TEAM_BATTING_SO*flag_TEAM_BATTING_SO +
  TEAM_FIELDING_DP:flag_TEAM_FIELDING_DP + TEAM_BASERUN_SB*flag_TEAM_BASERUN_SB,train[test_split,])
val.errors<-rep(NA,29)
for(i in 1:29){
  coefi<-coef(model1,id=i)
  pred<-test.mat[,names(coefi)]%*%coefi
  val.errors[i]<-mean((train$TARGET_WINS[test_split]-pred)^2)
}
val.errors
which.min(val.errors)  # 8 has the lowest errors

# create predict function
predict.regsubsets<-function(object,newdata,id){
  form<-as.formula(object$call [[2]])
  mat<-model.matrix(form,newdata)
  coefi<-coef(object,id=id)
  xvars<-names(coefi)
  mat[,xvars]%*%coefi
}

k<-8
set.seed(1)
folds<-sample(1:k,nrow(train),rep=T)
cv.errors<-matrix(NA,k,29,dimnames = list(NULL,paste(1:29)))
# for loop for cross-validation, returns a 10x19 matrix
for(j in 1:k){
  model1<-regsubsets(TARGET_WINS~.+ TEAM_BATTING_H*TEAM_BATTING_BB + TEAM_BATTING_SO*flag_TEAM_BATTING_SO +
  TEAM_FIELDING_DP:flag_TEAM_FIELDING_DP + TEAM_BASERUN_SB*flag_TEAM_BASERUN_SB,train[folds!=j,],nvmax=28)
  for(i in 1:29){
    pred<-predict(model1,train[folds==j,],id=i)
    cv.errors[j,i]<-mean((train$TARGET_WINS[folds==j]-pred)^2)
  }
}
mean.cv.errors<-apply(cv.errors,2,mean)
mean.cv.errors
which.min(mean.cv.errors)
plot(mean.cv.errors,type='b')  # 29 variable model is best
model1<-regsubsets(TARGET_WINS~.+ TEAM_BATTING_H*TEAM_BATTING_BB + TEAM_BATTING_SO*flag_TEAM_BATTING_SO +
  TEAM_FIELDING_DP:flag_TEAM_FIELDING_DP + TEAM_BASERUN_SB*flag_TEAM_BASERUN_SB,train,nvmax=29)
model1.pred<-predict.regsubsets(model1,train[test_split,],29)


# Model 2: Lasso
x<-model.matrix(TARGET_WINS~.+ TEAM_BATTING_H*TEAM_BATTING_BB + TEAM_BATTING_SO*flag_TEAM_BATTING_SO +
  TEAM_FIELDING_DP:flag_TEAM_FIELDING_DP + TEAM_BASERUN_SB*flag_TEAM_BASERUN_SB,train)[,-1]
y<-train$TARGET_WINS
grid<-10^seq(10,-2,length=100)
y.test<-y[test_split]
model2<-glmnet(x[train_split,],y[train_split],alpha=1,lambda=grid)
plot(model2)
set.seed(1)
cv.out<-cv.glmnet(x[train_split,],y[train_split],alpha=1)
plot(cv.out)
bestlam<-cv.out$lambda.min
model2.pred<-predict(model2,s=bestlam,newx=x[test_split,])
mean((model2.pred-y.test)^2)
# benefit of lasso: lasso selects predictors to use
out<-glmnet(x,y,alpha=1,lambda=grid)
model2.coef<-predict(out,type='coefficients',s=bestlam)[1:20,]
model2.coef

# Model 3: Partial Least Squares
set.seed(1)
model3<-plsr(TARGET_WINS~.+ TEAM_BATTING_H*TEAM_BATTING_BB + TEAM_BATTING_SO*flag_TEAM_BATTING_SO +
  TEAM_FIELDING_DP:flag_TEAM_FIELDING_DP + TEAM_BASERUN_SB*flag_TEAM_BASERUN_SB,data=train,scale=T,validation='CV')
summary(model3)
validationplot(model3,val.type = 'MSEP')  # lowest cv error is M=10
# evaluate test set using ncomp=2
model3.pred<-predict(model3,x[test_split,],ncomp=10)
mean((model3.pred-y.test)^2)
model3<-plsr(TARGET_WINS~.+ TEAM_BATTING_H*TEAM_BATTING_BB + TEAM_BATTING_SO*flag_TEAM_BATTING_SO +
  TEAM_FIELDING_DP:flag_TEAM_FIELDING_DP + TEAM_BASERUN_SB*flag_TEAM_BASERUN_SB,data=train,scale=T,ncomp=10)
summary(model3)


###  Legacy Model  ###
legacy <- lm(TARGET_WINS ~ TEAM_BATTING_2B + TEAM_BATTING_3B + outlier_TEAM_BATTING_3B + TEAM_BATTING_HR +
  TEAM_BATTING_H*TEAM_BATTING_BB + TEAM_BATTING_SO*flag_TEAM_BATTING_SO + TEAM_PITCHING_H + TEAM_PITCHING_BB +
  TEAM_FIELDING_DP + TEAM_FIELDING_E + TEAM_FIELDING_DP:flag_TEAM_FIELDING_DP + TEAM_BASERUN_SB*flag_TEAM_BASERUN_SB +
  TEAM_BASERUN_CS + outlier_TEAM_BASERUN_CS, data=train[train_split,])
legacy.pred<-predict(legacy,train[test_split,])


######## Performance #######
mse<-function(model){
  mean((as.integer(model)-y.test)^2)
}
r2<-function(x) cor(x,y.test)^2

mse(legacy.pred)  # 165.2697
mse(model1.pred)  # 158.0724
mse(model2.pred)  # 163.4868
mse(model3.pred)  # 154.5548  <***
r2(legacy.pred)   # .3964
r2(model1.pred)   # .4224
r2(model2.pred)   # .4020
r2(model3.pred)   # .4361  <***


# Stand Alone Scoring
# Predictions and corrections
test$P_TARGET_WINS <- as.integer(predict(model3,test,ncomp=10))
outlier_l <- as.integer(quantile(test$P_TARGET_WINS,.01))
outlier_h <- as.integer(quantile(test$P_TARGET_WINS,.99))
test$P_TARGET_WINS[test$P_TARGET_WINS > outlier_h] <- outlier_h
test$P_TARGET_WINS[test$P_TARGET_WINS < outlier_l] <- outlier_l

#subset of data set for the deliverable "Scored data file"
prediction <- test[c("INDEX","P_TARGET_WINS")]

# Write results to CSV
write.csv(prediction, 'merten_M4_predict.csv')