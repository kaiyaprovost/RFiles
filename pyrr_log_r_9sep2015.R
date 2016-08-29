## set wd to dropbox folder
setwd("C:/Users/Kaiya/Dropbox/Docs for Brian/Field Work/Pyrrhuloxia Data Analysis")

# import csv
pyrr_data <- 
  read.csv("C:/Users/Kaiya/Dropbox/Docs for Brian/Field Work/Pyrrhuloxia Data Analysis/pyrr_data_analysis_csv_9Sep2015.csv")

## refactor type and song
pyrr_data$Type = as.factor(pyrr_data$Type)
levels(pyrr_data$Type) = c("Cactus.Wren","Texas","Bill.Williams","Portal")
print(levels(pyrr_data$Type))

pyrr_data$Song = as.factor(pyrr_data$Song)
levels(pyrr_data$Song) = c("Cactus.Wren",
                           "Texas.1","Texas.2","Texas.4","Texas.5","Texas.10","Texas.11","Texas.12",
                           "Bill.Williams.1","Bill.Williams.2","Bill.Williams.4","Bill.Williams.5","Bill.Williams.6",
                           "Portal.4","Portal.14","Portal.21","Portal.23","Portal.24")
print(levels(pyrr_data$Song))

mean(pyrr_data$Close.Resp.As.Number[pyrr_data$Type=="Cactus.Wren"]) #19.16418
mean(pyrr_data$Close.Resp.As.Number[pyrr_data$Type=="Bill.Williams"]) #22.23881
mean(pyrr_data$Close.Resp.As.Number[pyrr_data$Type=="Texas"]) #22.92537
mean(pyrr_data$Close.Resp.As.Number[pyrr_data$Type=="Portal"]) #19.01493

sum(pyrr_data$Close.Resp.As.Number[pyrr_data$Type=="Cactus.Wren"]==24) #48
sum(pyrr_data$Close.Resp.As.Number[pyrr_data$Type=="Bill.Williams"]==24) #59
sum(pyrr_data$Close.Resp.As.Number[pyrr_data$Type=="Texas"]==24) #61
sum(pyrr_data$Close.Resp.As.Number[pyrr_data$Type=="Portal"]==24) #48

sd(pyrr_data$Close.Resp.As.Number[pyrr_data$Type=="Cactus.Wren"]) #8.239956
sd(pyrr_data$Close.Resp.As.Number[pyrr_data$Type=="Bill.Williams"]) #5.193497
sd(pyrr_data$Close.Resp.As.Number[pyrr_data$Type=="Texas"]) #3.65693
sd(pyrr_data$Close.Resp.As.Number[pyrr_data$Type=="Portal"]) #8.285607

## cardinal data for comparison
#mean(card_data$Close.Resp.As.Number[card_data$Type=="Cactus.Wren"]) #21.07463
#mean(card_data$Close.Resp.As.Number[card_data$Type=="Bill.Williams"]) #18.86567
#mean(card_data$Close.Resp.As.Number[card_data$Type=="Texas"]) #20.83582
#mean(card_data$Close.Resp.As.Number[card_data$Type=="Portal"]) #10.71642

#sum(card_data$Close.Resp.As.Number[card_data$Type=="Cactus.Wren"]==24) #50
#sum(card_data$Close.Resp.As.Number[card_data$Type=="Bill.Williams"]==24) #45
#sum(card_data$Close.Resp.As.Number[card_data$Type=="Texas"]==24) #53
#sum(card_data$Close.Resp.As.Number[card_data$Type=="Portal"]==24) #23

#sd(card_data$Close.Resp.As.Number[card_data$Type=="Cactus.Wren"]) #5.638916
#sd(card_data$Close.Resp.As.Number[card_data$Type=="Bill.Williams"]) #8.077082
#sd(card_data$Close.Resp.As.Number[card_data$Type=="Texas"]) #6.725341
#sd(card_data$Close.Resp.As.Number[card_data$Type=="Portal"]) #10.20666

list.name = c("Card.CW","Pyrr.CW","Card.NM","Pyrr.NM","Card.BW","Pyrr.BW","Card.PT","Pyrr.PT")
sum24.list = c(50,48,53,59,45,61,23,48)
barplot(sum24.list)
mean.list = c(21.07463,19.16418,20.83582,22.92537,18.86567,22.23881,10.71642,19.01493)
barplot(mean.list)