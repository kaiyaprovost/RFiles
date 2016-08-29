,# paired csv stuff
setwd("C:/Users/Kaiya/Dropbox/Docs for Brian/Field Work/Cardinal Data Analysis")
save.image("C:/Users/Kaiya/Dropbox/Docs for Brian/Field Work/Cardinal Data Analysis/card_paired_workspace_r.RData")

## import data
card_paired <- read.csv("C:/Users/Kaiya/Dropbox/Docs for Brian/Field Work/Cardinal Data Analysis/card_paired_csv_7july2015.csv")

## see names and attach
names(card_paired)
attach(card_paired)

## univariate models
plot(card_paired[,90:93],panel=panel.smooth)
cor(card_paired[,90:93])