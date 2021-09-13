######## Download appropriate packages and install them from (https://cran.r-project.org/web/packages/available_packages_by_name.html)

# This is generic code that creates a few simple models and does a few simple things with data preparation.
# It is not intended to be a "best practices" or "good model"

# Note that some of the Zero Inflated models will take a few seconds/moments to run.  Especially if you have a larger
# number of variables involved in the model.

#Read File in from your working directory
setwd("C:/Users/jmert/Documents/W&M/BUAD5122/M6")
wine_train <- read.csv("wine-training.csv")  # read csv file

#call libraries
library(ggplot2) # For graphical tools
library(MASS) # For some advanced statistics
library(pscl) # For "counting" models (e.g., Poisson and Negative Binomial)
library(tidyverse) # For general needs and functions
library(readr)
library(corrplot)
library(broom)
library(rsample)
library(caret)

#take a look at the high level characteristics of the wine data
str(wine_train) # 12795 rows with 14 predictor variables
summary(wine_train)
# variables with missing data: ResidualSugar, Chlorides, FreeSulfurDioxide, pH, Sulphates, Alcohol, STARS

#examine the target variable
ggplot(data=wine_train, aes(wine_train$TARGET)) +
  geom_histogram(binwidth =1, 
                 col="BLUE", 
                 aes(fill=..count..))+
  scale_fill_gradient("Count", low = "blue", high = "red")

# wine_clean <- na.omit(wine_train)
# cor(wine_clean[sapply(wine_clean, is.numeric)])

# look at the distribution of all variables
wine_train[3:16] %>%
  keep(is.numeric) %>%
  gather() %>%
  ggplot(aes(value)) + facet_wrap(~ key, scales='free') + geom_histogram()


#creat IMP versions of each independent variable
wine_train$FixedAcidity_IMP <- wine_train$FixedAcidity
wine_train$VolatileAcidity_IMP <- wine_train$VolatileAcidity
wine_train$CitricAcid_IMP <- wine_train$CitricAcid
wine_train$ResidualSugar_IMP <- wine_train$ResidualSugar
wine_train$Chlorides_IMP <- wine_train$Chlorides
wine_train$FreeSulfurDioxide_IMP <- wine_train$FreeSulfurDioxide
wine_train$TotalSulfurDioxide_IMP <- wine_train$TotalSulfurDioxide
wine_train$Density_IMP <- wine_train$Density
wine_train$pH_IMP <- wine_train$pH
wine_train$Sulphates_IMP <- wine_train$Sulphates
wine_train$Alcohol_IMP <- wine_train$Alcohol
wine_train$LabelAppeal_IMP <- wine_train$LabelAppeal
wine_train$AcidIndex_IMP <- wine_train$AcidIndex
wine_train$STARS_IMP <- wine_train$STARS

#replace NA's in each column with median
wine_train$FixedAcidity_IMP[which(is.na(wine_train$FixedAcidity_IMP))] <- mean(wine_train$FixedAcidity_IMP, na.rm = TRUE)
wine_train$VolatileAcidity_IMP[which(is.na(wine_train$VolatileAcidity_IMP))] <- mean(wine_train$VolatileAcidity_IMP, na.rm = TRUE)
wine_train$CitricAcid_IMP[which(is.na(wine_train$CitricAcid_IMP))] <- mean(wine_train$CitricAcid_IMP, na.rm = TRUE)
wine_train$ResidualSugar_IMP[which(is.na(wine_train$ResidualSugar_IMP))] <- mean(wine_train$ResidualSugar_IMP, na.rm = TRUE)
wine_train$Chlorides_IMP[which(is.na(wine_train$Chlorides_IMP))] <- mean(wine_train$Chlorides_IMP, na.rm = TRUE)
wine_train$FreeSulfurDioxide_IMP[which(is.na(wine_train$FreeSulfurDioxide_IMP))] <- mean(wine_train$FreeSulfurDioxide_IMP, na.rm = TRUE)
wine_train$TotalSulfurDioxide_IMP[which(is.na(wine_train$TotalSulfurDioxide_IMP))] <- mean(wine_train$TotalSulfurDioxide_IMP, na.rm = TRUE)
wine_train$Density_IMP[which(is.na(wine_train$Density_IMP))] <- mean(wine_train$Density_IMP, na.rm = TRUE)
wine_train$pH_IMP[which(is.na(wine_train$pH_IMP))] <- mean(wine_train$pH_IMP, na.rm = TRUE)
wine_train$Sulphates_IMP[which(is.na(wine_train$Sulphates_IMP))] <- mean(wine_train$Sulphates_IMP, na.rm = TRUE)
wine_train$Alcohol_IMP[which(is.na(wine_train$Alcohol_IMP))] <- mean(wine_train$Alcohol_IMP, na.rm = TRUE)
wine_train$LabelAppeal_IMP[which(is.na(wine_train$LabelAppeal_IMP))] <- mean(wine_train$LabelAppeal_IMP, na.rm = TRUE)
wine_train$AcidIndex_IMP[which(is.na(wine_train$AcidIndex_IMP))] <- mean(wine_train$AcidIndex_IMP, na.rm = TRUE)
wine_train$STARS_IMP[which(is.na(wine_train$STARS_IMP))] <- mean(wine_train$STARS_IMP, na.rm = TRUE)

wine_train[17:30] %>%
  keep(is.numeric) %>%
  gather() %>%
  ggplot(aes(value)) + facet_wrap(~ key, scales='free') + geom_histogram()

#flag NA values with new field
#first, create new field
#second, replace NA's with 1 else 0

wine_train$ResidualSugar_IMP_Flag <- wine_train$ResidualSugar
wine_train$Chlorides_IMP_Flag <- wine_train$Chlorides
wine_train$FreeSulfurDioxide_IMP_Flag <- wine_train$FreeSulfurDioxide
wine_train$TotalSulfurDioxide_IMP_Flag <- wine_train$TotalSulfurDioxide
wine_train$pH_IMP_Flag <- wine_train$pH
wine_train$Sulphates_IMP_Flag <- wine_train$Sulphates
wine_train$Alcohol_IMP_Flag <- wine_train$Alcohol
wine_train$STARS_IMP_Flag <- wine_train$STARS


#NEW BINARY FIELDS TO SHOW na's
wine_train$ResidualSugar_IMP_Flag <- ifelse(is.na(wine_train$ResidualSugar_IMP_Flag)==TRUE, 1, 0)
wine_train$Chlorides_IMP_Flag <- ifelse(is.na(wine_train$Chlorides_IMP_Flag)==TRUE, 1, 0)
wine_train$FreeSulfurDioxide_IMP_Flag <- ifelse(is.na(wine_train$FreeSulfurDioxide_IMP_Flag)==TRUE, 1, 0)
wine_train$TotalSulfurDioxide_IMP_Flag <- ifelse(is.na(wine_train$TotalSulfurDioxide_IMP_Flag)==TRUE, 1, 0)
wine_train$pH_IMP_Flag <- ifelse(is.na(wine_train$pH_IMP_Flag)==TRUE, 1, 0)
wine_train$Sulphates_IMP_Flag <- ifelse(is.na(wine_train$Sulphates_IMP_Flag)==TRUE, 1, 0)
wine_train$Alcohol_IMP_Flag <- ifelse(is.na(wine_train$Alcohol_IMP_Flag)==TRUE, 1, 0)
wine_train$STARS_IMP_Flag <- ifelse(is.na(wine_train$STARS_IMP_Flag)==TRUE, 1, 0) #LOOK FOR MISSING STAR OBSERVATIONS


#Is it possible to distinguish red vs white wines by the chemical property makeup?
plot(wine_train$VolatileAcidity_IMP)

#A better way to visualize volatile acidity
ggplot(data=wine_train, aes(wine_train$VolatileAcidity_IMP)) +
  geom_histogram(binwidth =1, 
                 col="BLUE", 
                 aes(fill=..count..))+
  scale_fill_gradient("Count", low = "blue", high = "red")

summary(wine_train$VolatileAcidity_IMP)

#make new indicator that indicates red vs white based on volatile acidity
wine_train$VolatileAcidity_IMP_REDFLAG <- ifelse(wine_train$VolatileAcidity_IMP > mean(wine_train$VolatileAcidity_IMP), 1, 0)
wine_train$ResidualSugar_IMP_REDFLAG <- ifelse(wine_train$ResidualSugar_IMP < mean(wine_train$ResidualSugar_IMP), 1, 0)
wine_train$TotalSulfurDioxide_IMP_REDFLAG <- ifelse(wine_train$TotalSulfurDioxide_IMP < mean(wine_train$TotalSulfurDioxide_IMP), 1, 0)
wine_train$Density_IMP_REDFLAG <- ifelse(wine_train$Density_IMP > mean(wine_train$Density_IMP), 1, 0)
wine_train$TallyUp <- wine_train$VolatileAcidity_IMP_REDFLAG + wine_train$ResidualSugar_IMP_REDFLAG + wine_train$TotalSulfurDioxide_IMP_REDFLAG + wine_train$Density_IMP_REDFLAG
wine_train$Final_REDFLAG <- ifelse(wine_train$TallyUp > mean(wine_train$TallyUp), 1, 0)

pairs(wine_train[, c("Final_REDFLAG", "VolatileAcidity_IMP")])

plot(wine_train$VolatileAcidity_IMP, wine_train$TARGET)

#Add Target Flag for 0 sale scenarios
wine_train$TARGET_Flag <- ifelse(wine_train$TARGET >0, 1, 0)
wine_train$TARGET_AMT <- wine_train$TARGET - 1
wine_train$TARGET_AMT <- ifelse(wine_train$TARGET_Flag == 0, NA, wine_train$TARGET-1)

# # train/test split on training data
# set.seed(1)
# data <- wine_train %>% mutate_if(is.ordered,factor,ordered=FALSE)
# data_split <- initial_split(data=data,prop=.8)
# data_train <- training(data_split)
# data_test <- testing(data_split)

#######################################################
#######################################################
## FIRST MODEL ... REGULAR LINEAR REGRESSION MODEL#####
lm_fit <- lm(TARGET~ Chlorides_IMP + FreeSulfurDioxide_IMP + Density_IMP
             + Sulphates_IMP + Alcohol_IMP + LabelAppeal_IMP + AcidIndex_IMP
             + STARS_IMP_Flag + VolatileAcidity_IMP_REDFLAG + ResidualSugar_IMP_REDFLAG + TotalSulfurDioxide_IMP_REDFLAG
             + Density_IMP_REDFLAG + Final_REDFLAG , data = wine_train)
summary(lm_fit)
glance(lm_fit)
RMSE(lm_fit$fitted.values, wine_train$TARGET)
# R2: .457
# RMSE: 1.4183
# AIC: 45284
# BIC: 45396

wine_train$fittedLM <-lm_fit$fitted.values

##########################################################################################
##########################################################################################
## SECOND MODEL ... REGULAR LINEAR REGRESSION MODEL USING STEPWISE VARIABLE SELECTION (AIC)
##########################################################################################

train_control <- trainControl(method='cv', number=10)

lm_fit_stepwise <- train(TARGET ~ . + LabelAppeal_IMP:STARS_IMP + AcidIndex_IMP:pH_IMP + ResidualSugar_IMP:AcidIndex_IMP,
                         method='lmStepAIC', trControl = train_control, data=wine_train[c(2,17:44)])

lm_fit_stepwise
lm_fit_stepwise$finalModel
summary(lm_fit_stepwise)
# R2: .5432
# RMSE: 1.302


wine_train$fittedLMStepwise <-fitted(lm_fit_stepwise)

##########################################################################################
##########################################################################################
## THIRD MODEL ... POISSON################################################################
##########################################################################################

train_control <- trainControl(method='repeatedcv', number=10, repeats=3)

poisson_model <- train(TARGET ~ ResidualSugar_IMP + Chlorides_IMP + FreeSulfurDioxide_IMP +
    Density_IMP + Sulphates_IMP + Alcohol_IMP + LabelAppeal_IMP +
    AcidIndex_IMP + STARS_IMP + pH_IMP_Flag + STARS_IMP_Flag +
    VolatileAcidity_IMP_REDFLAG + ResidualSugar_IMP_REDFLAG +
    TotalSulfurDioxide_IMP_REDFLAG + Density_IMP_REDFLAG + Final_REDFLAG +
    LabelAppeal_IMP:STARS_IMP + pH_IMP:AcidIndex_IMP,
                       trControl = train_control, method='glm',family=poisson(link="log"), data=wine_train[c(2,17:44)])
poisson_model
summary(poisson_model)
# R2: .5363
# RMSE: 1.312
# AIC: 45626

wine_train$poisson_yhat <- predict(poisson_model, newdata = wine_train,type='raw')


##########################################################################################
##########################################################################################
## FOURTH MODEL ... NEGATIVE BINOMIAL DISTRIBUTION########################################
##########################################################################################

train_control <- trainControl(method='cv', number=10)

NBR_Model<-train(TARGET ~ ResidualSugar_IMP + Chlorides_IMP + FreeSulfurDioxide_IMP +
    Density_IMP + Sulphates_IMP + Alcohol_IMP + LabelAppeal_IMP +
    AcidIndex_IMP + STARS_IMP + pH_IMP_Flag + STARS_IMP_Flag +
    VolatileAcidity_IMP_REDFLAG + ResidualSugar_IMP_REDFLAG +
    TotalSulfurDioxide_IMP_REDFLAG + Density_IMP_REDFLAG + Final_REDFLAG +
    LabelAppeal_IMP:STARS_IMP + pH_IMP:AcidIndex_IMP, trControl = train_control, method='glm.nb', data=wine_train[c(2,17:44)])
NBR_Model
summary(NBR_Model)

# R2: .5364
# RMSE: 1.312
# AIC: 45629

wine_train$NBRphat <- predict(NBR_Model, newdata = wine_train, type = "raw")



##########################################################################################
##########################################################################################
## FIFTH MODEL ... ZERO INFLATED POISSON (ZIP)############################################
##########################################################################################

ZIP_Model<-zeroinfl(TARGET ~ ResidualSugar_IMP + FreeSulfurDioxide_IMP +
    Density_IMP + Sulphates_IMP + Alcohol_IMP + LabelAppeal_IMP +
    AcidIndex_IMP + STARS_IMP + pH_IMP_Flag + STARS_IMP_Flag +
    VolatileAcidity_IMP_REDFLAG + ResidualSugar_IMP_REDFLAG +
    TotalSulfurDioxide_IMP_REDFLAG + Density_IMP_REDFLAG + pH_IMP:AcidIndex_IMP,
                    data=wine_train)

summary(ZIP_Model)
AIC(ZIP_Model)
BIC(ZIP_Model)
1-(sum(ZIP_Model$residuals^2)/sum((ZIP_Model$y - mean(wine_train$TARGET))^2))
RMSE(ZIP_Model$fitted.values, wine_train$TARGET)
# R2: .5718
# RMSE: 1.2604
# AIC: 40708
# BIC: 40947
# plot(residuals(ZIP_Model) ~ fitted(ZIP_Model))

wine_train$ZIPphat <- predict(ZIP_Model, newdata = wine_train, type = "response")


##########################################################################################
##########################################################################################
## 6TH MODEL ... ZERO INFLATED NEGATIVE BINOMIAL REGRESSION (ZINB)########################
##########################################################################################

ZINB_Model<-zeroinfl(TARGET ~ ResidualSugar_IMP + FreeSulfurDioxide_IMP +
    Density_IMP + Sulphates_IMP + Alcohol_IMP + LabelAppeal_IMP +
    AcidIndex_IMP + STARS_IMP + pH_IMP_Flag + STARS_IMP_Flag +
    VolatileAcidity_IMP_REDFLAG + ResidualSugar_IMP_REDFLAG +
    TotalSulfurDioxide_IMP_REDFLAG + Density_IMP_REDFLAG + pH_IMP:AcidIndex_IMP,
                     data=wine_train, dist = "negbin")
summary(ZINB_Model)
AIC(ZINB_Model)
BIC(ZINB_Model)
1-(sum(ZINB_Model$residuals^2)/sum((ZINB_Model$y - mean(wine_train$TARGET))^2))
RMSE(ZINB_Model$fitted.values, wine_train$TARGET)
# R2: .5718
# RMSE: 1.2604
# AIC: 40710
# BIC: 40956

wine_train$ZINBphat <- predict(ZINB_Model, newdata = wine_train, type = "response")

#what type of dispersion does sample have?
mean(wine_train$TARGET)
var(wine_train$TARGET)



###################################################################################################################
###################################################################################################################
###################################################################################################################
###################################################################################################################
###################################################################################################################
# Everything below could be a "stand alone" scoring program, in its separate file

#Read File in from your working directory
wine_test <- read.csv("wine-test.csv")  # read csv file

wine_test$INDEX <- as.factor(wine_test$INDEX)
wine_test$TARGET <- as.factor(wine_test$TARGET)

#creat IMP versions of each independent variable (wine_test)
wine_test$FixedAcidity_IMP <- wine_test$FixedAcidity
wine_test$VolatileAcidity_IMP <- wine_test$VolatileAcidity
wine_test$CitricAcid_IMP <- wine_test$CitricAcid
wine_test$ResidualSugar_IMP <- wine_test$ResidualSugar
wine_test$Chlorides_IMP <- wine_test$Chlorides
wine_test$FreeSulfurDioxide_IMP <- wine_test$FreeSulfurDioxide
wine_test$TotalSulfurDioxide_IMP <- wine_test$TotalSulfurDioxide
wine_test$Density_IMP <- wine_test$Density
wine_test$pH_IMP <- wine_test$pH
wine_test$Sulphates_IMP <- wine_test$Sulphates
wine_test$Alcohol_IMP <- wine_test$Alcohol
wine_test$LabelAppeal_IMP <- wine_test$LabelAppeal
wine_test$AcidIndex_IMP <- wine_test$AcidIndex
wine_test$STARS_IMP <- wine_test$STARS

#replace NA's in each column with median since regression approach is not showing any value (wine_test)
wine_test$FixedAcidity_IMP[which(is.na(wine_test$FixedAcidity_IMP))] <- mean(wine_test$FixedAcidity_IMP,na.rm = TRUE)
wine_test$VolatileAcidity_IMP[which(is.na(wine_test$VolatileAcidity_IMP))] <- mean(wine_test$VolatileAcidity_IMP,na.rm = TRUE)
wine_test$CitricAcid_IMP[which(is.na(wine_test$CitricAcid_IMP))] <- mean(wine_test$CitricAcid_IMP,na.rm = TRUE)
wine_test$ResidualSugar_IMP[which(is.na(wine_test$ResidualSugar_IMP))] <- mean(wine_test$ResidualSugar_IMP,na.rm = TRUE)
wine_test$Chlorides_IMP[which(is.na(wine_test$Chlorides_IMP))] <- mean(wine_test$Chlorides_IMP,na.rm = TRUE)
wine_test$FreeSulfurDioxide_IMP[which(is.na(wine_test$FreeSulfurDioxide_IMP))] <- mean(wine_test$FreeSulfurDioxide_IMP,na.rm = TRUE)
wine_test$TotalSulfurDioxide_IMP[which(is.na(wine_test$TotalSulfurDioxide_IMP))] <- mean(wine_test$TotalSulfurDioxide_IMP,na.rm = TRUE)
wine_test$Density_IMP[which(is.na(wine_test$Density_IMP))] <- mean(wine_test$Density_IMP,na.rm = TRUE)
wine_test$pH_IMP[which(is.na(wine_test$pH_IMP))] <- mean(wine_test$pH_IMP,na.rm = TRUE)
wine_test$Sulphates_IMP[which(is.na(wine_test$Sulphates_IMP))] <- mean(wine_test$Sulphates_IMP,na.rm = TRUE)
wine_test$Alcohol_IMP[which(is.na(wine_test$Alcohol_IMP))] <- mean(wine_test$Alcohol_IMP,na.rm = TRUE)
wine_test$LabelAppeal_IMP[which(is.na(wine_test$LabelAppeal_IMP))] <- mean(wine_test$LabelAppeal_IMP,na.rm = TRUE)
wine_test$AcidIndex_IMP[which(is.na(wine_test$AcidIndex_IMP))] <- mean(wine_test$AcidIndex_IMP,na.rm = TRUE)
wine_test$STARS_IMP[which(is.na(wine_test$STARS_IMP))] <- mean(wine_test$STARS_IMP,na.rm = TRUE)

#flag NA values with new field...first, create new field...second, replace NA's with 1 else 0 (wine_test)
wine_test$ResidualSugar_IMP_Flag <- wine_test$ResidualSugar
wine_test$Chlorides_IMP_Flag <- wine_test$Chlorides
wine_test$FreeSulfurDioxide_IMP_Flag <- wine_test$FreeSulfurDioxide
wine_test$TotalSulfurDioxide_IMP_Flag <- wine_test$TotalSulfurDioxide
wine_test$pH_IMP_Flag <- wine_test$pH
wine_test$Sulphates_IMP_Flag <- wine_test$Sulphates
wine_test$Alcohol_IMP_Flag <- wine_test$Alcohol
wine_test$STARS_IMP_Flag <- wine_test$STARS

#NEW BINARY FIELDS TO SHOW na's (wine_test)
wine_test$ResidualSugar_IMP_Flag <- ifelse(is.na(wine_test$ResidualSugar_IMP_Flag)==TRUE, 1,0) 
wine_test$Chlorides_IMP_Flag <- ifelse(is.na(wine_test$Chlorides_IMP_Flag)==TRUE, 1,0)
wine_test$FreeSulfurDioxide_IMP_Flag <- ifelse(is.na(wine_test$FreeSulfurDioxide_IMP_Flag)==TRUE, 1,0)
wine_test$TotalSulfurDioxide_IMP_Flag <- ifelse(is.na(wine_test$TotalSulfurDioxide_IMP_Flag)==TRUE, 1,0)
wine_test$pH_IMP_Flag <- ifelse(is.na(wine_test$pH_IMP_Flag)==TRUE, 1,0)
wine_test$Sulphates_IMP_Flag <- ifelse(is.na(wine_test$Sulphates_IMP_Flag)==TRUE, 1,0)
wine_test$Alcohol_IMP_Flag <- ifelse(is.na(wine_test$Alcohol_IMP_Flag)==TRUE, 1,0)
wine_test$STARS_IMP_Flag <- ifelse(is.na(wine_test$STARS_IMP_Flag)==TRUE, 1,0) #LOOK FOR MISSING STAR OBSERVATIONS

#make new indicator that indicates red vs white based on volatile acidity (wine_test)
wine_test$VolatileAcidity_IMP_REDFLAG <- ifelse(wine_test$VolatileAcidity_IMP > mean(wine_test$VolatileAcidity_IMP),1,0)
wine_test$ResidualSugar_IMP_REDFLAG <- ifelse(wine_test$ResidualSugar_IMP < mean(wine_test$ResidualSugar_IMP),1,0)
wine_test$TotalSulfurDioxide_IMP_REDFLAG <- ifelse(wine_test$TotalSulfurDioxide_IMP < mean(wine_test$TotalSulfurDioxide_IMP),1,0)
wine_test$Density_IMP_REDFLAG <- ifelse(wine_test$Density_IMP > mean(wine_test$Density_IMP),1,0)
wine_test$TallyUp <- wine_test$VolatileAcidity_IMP_REDFLAG + wine_test$ResidualSugar_IMP_REDFLAG + wine_test$TotalSulfurDioxide_IMP_REDFLAG + wine_test$Density_IMP_REDFLAG
wine_test$Final_REDFLAG <- ifelse(wine_test$TallyUp > mean(wine_test$TallyUp),1,0)

#Add Target Flag for 0 sale scenarios (wine_test)
wine_test$TARGET_Flag <- ifelse(wine_test$TARGET >0,1,0)
wine_test$TARGET_AMT <- wine_test$TARGET - 1
wine_test$TARGET_AMT <- ifelse(wine_test$TARGET_Flag == 0,NA,wine_test$TARGET-1)

# Again, double-checking to make sure we don't have any NA's in our Test Data Set
summary(wine_test)

##########################################################################################
##########################################################################################
## CHAMPION MODEL ... Zero-Inflated Poisson #########################################
##########################################################################################

summary(ZIP_Model)


wine_test$TARGET <- predict(ZIP_Model, newdata = wine_test, type = "response")

summary(wine_test)


# Scored Data File
scores <- wine_test[c("INDEX","TARGET")]
write.csv(scores, file = "merten_m6_final.csv")

