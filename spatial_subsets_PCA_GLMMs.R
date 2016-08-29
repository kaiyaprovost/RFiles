## 0. load all required packages

library(lme4)  # load library
library(arm) # convenience functions for regression in R
library(car) ## for normality check
library(MASS) # for normality check
library("mlmRev") ## for gamma dist
library("AICcmodavg")

## 1. load the subsets

card100 <- 
  read.csv("C:/Users/Kaiya/Dropbox/Docs for Brian/THESIS/Field Work/Cardinal Data Analysis/Master Files/CSVs/spatial subsets/card_data_100A_PCA_2dec2015.csv")
card150 <- 
  read.csv("C:/Users/Kaiya/Dropbox/Docs for Brian/THESIS/Field Work/Cardinal Data Analysis/Master Files/CSVs/spatial subsets/card_data_150A_PCA_2dec2015.csv")
card200 <- 
  read.csv("C:/Users/Kaiya/Dropbox/Docs for Brian/THESIS/Field Work/Cardinal Data Analysis/Master Files/CSVs/spatial subsets/card_data_200A_PCA_2dec2015.csv")

PC1adj100 = card100$PC1 + 2 ## the min PC1 value is -1.30, so adding 2 will make all of the PC1 values non-zero
PC1adj150 = card150$PC1 + 2 ## the min PC1 value is -1.30, so adding 2 will make all of the PC1 values non-zero
PC1adj200 = card200$PC1 + 2 ## the min PC1 value is -1.30, so adding 2 will make all of the PC1 values non-zero

rand100 <- glmer(PC1adj100 ~ 1 + (1 | Point),data = card_data_models, family = Gamma(link = "identity")) ## Problem with Hessian check (infinite or missing values?) unable to evaluate scaled gradient
full100 <- glmer(PC1adj100 ~ Type + (1 | Point),data = card_data_models, family = Gamma(link = "identity"))
