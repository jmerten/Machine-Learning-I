# Title     : Module 1: Data Preparation
# Created by: Jason Merten
# Created on: 1/16/2021

setwd('C:/Users/jmert/Documents/W&M/BUAD5122/M1')
data <- read.csv('buad5122-m1-moneyball-data.csv')
data$INDEX <- as.factor(data$INDEX)
data[2:16] <- sapply(data[2:16],as.numeric)
row.names(data) <- data$INDEX
str(data)

# There are 6 columns with NA values
# BATTING_SO, BASERUN_SB, BASERUN_CS, BATTING_HBP, PITCHING_SO, and FIELDING_DP
summary(data)

# Batting Strikeouts; contains 102 NA values (4%)
par(mfrow=c(1,3))
boxplot(data$TEAM_BATTING_SO); hist(data$TEAM_BATTING_SO); plot(data$TEAM_BATTING_SO)
summary(data$TEAM_BATTING_SO)
# No extreme outliers, data looks to follow a normal distribution
data$flag_TEAM_BATTING_SO <- as.factor(ifelse(is.na(data$TEAM_BATTING_SO),1,0))
data$TEAM_BATTING_SO[is.na(data$TEAM_BATTING_SO)] <- as.integer(rnorm(length(data$TEAM_BATTING_SO[is.na(data$TEAM_BATTING_SO)]),mean(data$TEAM_BATTING_SO, na.rm=TRUE),sd(data$TEAM_BATTING_SO,na.rm=TRUE)))
par(mfrow=c(1,1))
plot(data$TEAM_BATTING_SO); points(x=data$INDEX[data$flag_TEAM_BATTING_SO == 1],y=data$TEAM_BATTING_SO[data$flag_TEAM_BATTING_SO == 1],pch=19,col='red')
summary(data$TEAM_BATTING_SO)  # no NA values

# Stolen Bases; contains 131 NA values (~6%)
par(mfrow=c(1,3))
boxplot(data$TEAM_BASERUN_SB); hist(data$TEAM_BASERUN_SB); plot(data$TEAM_BASERUN_SB)
summary(data$TEAM_BASERUN_SB)
# Large amount of outliers outside the 99th quantile (22)
table(data$TEAM_BASERUN_SB > quantile(data$TEAM_BASERUN_SB,.99,na.rm=TRUE))
outlier <- as.integer(quantile(data$TEAM_BASERUN_SB,.99,na.rm=TRUE))
# Based on the histogram, the data looks to fit a gamma distribution
data$flag_TEAM_BASERUN_SB <- as.factor(ifelse(is.na(data$TEAM_BASERUN_SB),1,0))
data$TEAM_BASERUN_SB[is.na(data$TEAM_BASERUN_SB)] <- as.integer(rgamma(length(data$TEAM_BASERUN_SB[is.na(data$TEAM_BASERUN_SB)]),1.25,.012))
# Handle outliers first and then determine the distribution
data$outlier_TEAM_BASERUN_SB <- as.factor(ifelse(data$TEAM_BASERUN_SB > outlier,1,0))
data$TEAM_BASERUN_SB[data$TEAM_BASERUN_SB > quantile(data$TEAM_BASERUN_SB,.99,na.rm=TRUE)] <- outlier
# Determine distribution
par(mfrow=c(1,1))
plot(data$TEAM_BASERUN_SB); points(x=data$INDEX[data$flag_TEAM_BASERUN_SB==1],y=data$TEAM_BASERUN_SB[data$flag_TEAM_BASERUN_SB == 1],pch=19,col='red')
summary(data$TEAM_BASERUN_SB)  # no NA values

# Caught Stealing
# Suggest removing this column, there are 772 missing values (33.9%)
par(mfrow=c(1,3))
boxplot(data$TEAM_BASERUN_CS, main='Boxplot of Caught Stealing'); hist(data$TEAM_BASERUN_CS); plot(data$TEAM_BASERUN_CS)
summary(data$TEAM_BASERUN_CS)
# Handle outliers first to understand the distribution
data$outlier_TEAM_BASERUN_CS <- as.factor(ifelse(data$TEAM_BASERUN_CS > quantile(data$TEAM_BASERUN_CS,.99,na.rm=TRUE),1,0))
data$outlier_TEAM_BASERUN_CS[is.na(data$outlier_TEAM_BASERUN_CS)==TRUE] <- 0  # replace any NA outlier variable as 0
data$TEAM_BASERUN_CS[data$TEAM_BASERUN_CS > quantile(data$TEAM_BASERUN_CS,.99,na.rm=TRUE)] <- as.integer(quantile(data$TEAM_BASERUN_CS,.99,na.rm=TRUE))
par(mfrow=c(1,1))
hist(data$TEAM_BASERUN_CS)
# Data looks to mostly follow a normal distribution with the exception of the transformed outliers
data$flag_TEAM_BASERUN_CS <- as.factor(ifelse(is.na(data$TEAM_BASERUN_CS),1,0))
data$TEAM_BASERUN_CS[is.na(data$TEAM_BASERUN_CS)] <- as.integer(rnorm(length(data$TEAM_BASERUN_CS[is.na(data$TEAM_BASERUN_CS)]), mean(data$TEAM_BASERUN_CS,na.rm=TRUE),sd(data$TEAM_BASERUN_CS,na.rm=TRUE)))
data$TEAM_BASERUN_CS <- ifelse(data$TEAM_BASERUN_CS < 0,0,data$TEAM_BASERUN_CS) # change any value less than 0 to 0
plot(data$TEAM_BASERUN_CS); points(x=data$INDEX[data$flag_TEAM_BASERUN_CS==1],y=data$TEAM_BASERUN_CS[data$flag_TEAM_BASERUN_CS==1],pch=19,col='red')
summary(data$TEAM_BASERUN_CS)  # no NA values

# Hit by Pitch
# Would recommend removing this column because there are 2085 missing values (91%)
par(mfrow=c(1,3))
boxplot(data$TEAM_BATTING_HBP, main='Boxplot of Hit by Pitch'); hist(data$TEAM_BATTING_HBP); plot(data$TEAM_BATTING_HBP)
summary(data$TEAM_BATTING_HBP)
par(mfrow=c(1,1))
data$flag_TEAM_BATTING_HBP <- as.factor(ifelse(is.na(data$TEAM_BATTING_HBP),1,0))
data$TEAM_BATTING_HBP[is.na(data$TEAM_BATTING_HBP)] <- as.integer(rnorm(length(data$TEAM_BATTING_HBP[is.na(data$TEAM_BATTING_HBP)]),mean(data$TEAM_BATTING_HBP,na.rm=TRUE),sd(data$TEAM_BATTING_HBP,na.rm=TRUE)))
plot(data$TEAM_BATTING_HBP); points(x=data$INDEX[data$flag_TEAM_BATTING_HBP==1],y=data$TEAM_BATTING_HBP[data$flag_TEAM_BATTING_HBP==1],pch=19,col='red')
summary(data$TEAM_BATTING_HBP)  # no NA values

# Pitching Strikeouts; contains 102 NA values (~5%)
par(mfrow=c(1,3))
boxplot(data$TEAM_PITCHING_SO, main='Boxplot of Pitching Strikeouts'); hist(data$TEAM_PITCHING_SO); plot(data$TEAM_PITCHING_SO)
summary(data$TEAM_PITCHING_SO)
par(mfrow=c(1,1))
# There are extreme outliers in this data set (22 greater than 99th quantile)
table(data$TEAM_PITCHING_SO > quantile(data$TEAM_PITCHING_SO,.99,na.rm=TRUE))
# Handle outliers first, then see if there's a distribution
data$outlier_TEAM_PITCHING_SO <- as.factor(ifelse(data$TEAM_PITCHING_SO > quantile(data$TEAM_PITCHING_SO,.99,na.rm=TRUE),1,0))
data$outlier_TEAM_PITCHING_SO[is.na(data$outlier_TEAM_PITCHING_SO)==TRUE] <- 0  # replace any NA outlier variable as 0
data$TEAM_PITCHING_SO[data$TEAM_PITCHING_SO > quantile(data$TEAM_PITCHING_SO,.99,na.rm=TRUE)] <- as.integer(quantile(data$TEAM_PITCHING_SO,.99,na.rm=TRUE))
hist(data$TEAM_PITCHING_SO)
summary(data$TEAM_PITCHING_SO)
# Data looks to follow a normal distribution
data$flag_TEAM_PITCHING_SO <- as.factor(ifelse(is.na(data$TEAM_PITCHING_SO),1,0))
data$TEAM_PITCHING_SO[is.na(data$TEAM_PITCHING_SO)] <- as.integer(rnorm(length(data$TEAM_PITCHING_SO[data$flag_TEAM_PITCHING_SO==1]),mean(data$TEAM_PITCHING_SO,na.rm=TRUE),sd(data$TEAM_PITCHING_SO,na.rm=TRUE)))
plot(data$TEAM_PITCHING_SO); points(x=data$INDEX[data$flag_TEAM_PITCHING_SO==1],y=data$TEAM_PITCHING_SO[data$flag_TEAM_PITCHING_SO==1],pch=19,col='red')
summary(data$TEAM_PITCHING_SO)  # no NA values

# Fielding Double Plays; contains 286 NA values (~10%)
par(mfrow=c(1,3))
boxplot(data$TEAM_FIELDING_DP, main='Boxplot of Double Plays'); hist(data$TEAM_FIELDING_DP); plot(data$TEAM_FIELDING_DP)
summary(data$TEAM_FIELDING_DP)
# No apparent significant outliers
# Data looks to be following a normal distribution
par(mfrow=c(1,1))
data$flag_TEAM_FIELDING_DP <- as.factor(ifelse(is.na(data$TEAM_FIELDING_DP),1,0))
data$TEAM_FIELDING_DP[is.na(data$TEAM_FIELDING_DP)] <- as.integer(rnorm(length(data$TEAM_FIELDING_DP[data$flag_TEAM_FIELDING_DP==1]),mean(data$TEAM_FIELDING_DP,na.rm=TRUE),sd(data$TEAM_FIELDING_DP,na.rm=TRUE)))
plot(data$TEAM_FIELDING_DP); points(x=data$INDEX[data$flag_TEAM_FIELDING_DP==1],y=data$TEAM_FIELDING_DP[data$flag_TEAM_FIELDING_DP==1],pch=19,col='red')
summary(data$TEAM_FIELDING_DP)  # no NA values

# Write data to a csv
write.csv(data,'merten_moneyball_final.csv')