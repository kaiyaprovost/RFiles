## set wd to dropbox folder
setwd("C:/Users/Kaiya/Dropbox/Docs for Brian/THESIS/Field Work/Cardinal Data Analysis")

## import csv
card_data <- 
  read.csv("C:/Users/Kaiya/Dropbox/Docs for Brian/THESIS/Field Work/Cardinal Data Analysis/Master Files/CSVs/card_data_analysis_csv_7july2015.csv")

# REFACTORING MESSES STUFF UP - DO NOT DO IT

## refactor type and song
#card_data$Type = as.factor(card_data$Type)
#levels(card_data$Type) = c("Cactus.Wren","Texas","Bill.Williams","Portal")
#print(levels(card_data$Type))

#card_data$Song = as.factor(card_data$Song)
#levels(card_data$Song) = c("Cactus.Wren",
#                           "Texas.1","Texas.2","Texas.4","Texas.5","Texas.10","Texas.11","Texas.12",
#                           "Bill.Williams.1","Bill.Williams.2","Bill.Williams.4","Bill.Williams.5","Bill.Williams.6",
#                           "Portal.4","Portal.14","Portal.21","Portal.23","Portal.24")
#print(levels(card_data$Song))


## flyby data
t.test(card_data$Flyby.Resp[card_data$Type=="Bill.Williams"],
       card_data$Flyby.Resp[card_data$Type=="Cactus.Wren"],paired=TRUE) #p-value = 0.02158 ## sig

t.test(card_data$Flyby.Resp[card_data$Type=="Bill.Williams"],
       card_data$Flyby.Resp[card_data$Type=="Texas"],paired=TRUE) # p-value = 0.226

t.test(card_data$Flyby.Resp[card_data$Type=="Bill.Williams"],
       card_data$Flyby.Resp[card_data$Type=="Portal"],paired=TRUE) #p-value = 2.203e-08 ## SIG, has smallest p-val

t.test(card_data$Flyby.Resp[card_data$Type=="Cactus.Wren"],
       card_data$Flyby.Resp[card_data$Type=="Texas"],paired=TRUE) # p-value = 0.3766

t.test(card_data$Flyby.Resp[card_data$Type=="Cactus.Wren"],
       card_data$Flyby.Resp[card_data$Type=="Portal"],paired=TRUE) #  p-value = 1.093e-09 ## SIG

t.test(card_data$Flyby.Resp[card_data$Type=="Texas"],
       card_data$Flyby.Resp[card_data$Type=="Portal"],paired=TRUE) #p-value = 2.902e-09 ## SIG

flyby_means = c(0.4029850746,0.6417910448,1.0298507463,5.9402985075)
flyby_stdev = c(1.1941740076,1.7641333398,2.0741566322,6.278584566)
flyby_songs = c("Cactus.Wren","New.Mexico","Bill.Williams","Portal")
png(filename="C:/Users/Kaiya/Dropbox/Docs for Brian/Field Work/Flyby_barplot.png",width=350,height=450,units="px")
flyby_barplot = barplot(flyby_means,col=c("grey15","blue","green","purple"),ylim=c(0,7),ylab="Flybys",xlab="Song Locality")
error.bar(flyby_barplot,flyby_means,flyby_stdev/sqrt(67))
dev.off()

error.bar <- function(barplot, means, upper, lower=upper, length=0.1,...){
  if(length(barplot) != length(means) | length(means) !=length(lower) | length(lower) != length(upper))
    stop("vectors must be same length")
  arrows(barplot,means+upper, barplot, means-lower, angle=90, code=3, length=length, ...)
}



hist(card_data$Flyby.Resp[card_data$Type=="Cactus.Wren"],freq=F,breaks=seq(0,23),col=rgb(1,1,1,0.25),ylim=c(0,1))
hist(card_data$Flyby.Resp[card_data$Type=="Bill.Williams"],add=T,freq=F,breaks=seq(0,23),col=rgb(1,0,0,0.25),ylim=c(0,1))
hist(card_data$Flyby.Resp[card_data$Type=="Texas"],add=T,freq=F,breaks=seq(0,23),col=rgb(0,0,1,0.25),ylim=c(0,1))
hist(card_data$Flyby.Resp[card_data$Type=="Portal"],add=T,freq=F,breaks=seq(0,23),col=rgb(1,1,0,0.25),ylim=c(0,1))

## chips
t.test(card_data$Chips.Pres.Abs.Resp[card_data$Type=="Bill.Williams"],
       card_data$Chips.Pres.Abs.Resp[card_data$Type=="Cactus.Wren"],paired=TRUE) #p-value = 0.1817

t.test(card_data$Chips.Pres.Abs.Resp[card_data$Type=="Bill.Williams"],
       card_data$Chips.Pres.Abs.Resp[card_data$Type=="Texas"],paired=TRUE) # p-value = 0.6207

t.test(card_data$Chips.Pres.Abs.Resp[card_data$Type=="Bill.Williams"],
       card_data$Chips.Pres.Abs.Resp[card_data$Type=="Portal"],paired=TRUE) #p-value = 2.188e-07 ## SIG, has smallest p-val

t.test(card_data$Chips.Pres.Abs.Resp[card_data$Type=="Cactus.Wren"],
       card_data$Chips.Pres.Abs.Resp[card_data$Type=="Texas"],paired=TRUE) # p-value = 0.3751

t.test(card_data$Chips.Pres.Abs.Resp[card_data$Type=="Cactus.Wren"],
       card_data$Chips.Pres.Abs.Resp[card_data$Type=="Portal"],paired=TRUE) #  p-value = 1.773e-09 ## SIG

t.test(card_data$Chips.Pres.Abs.Resp[card_data$Type=="Texas"],
       card_data$Chips.Pres.Abs.Resp[card_data$Type=="Portal"],paired=TRUE) #p-value = 1.462e-07 ## SIG

Chips_means = c(mean(card_data$Chips.Pres.Abs.Resp[card_data$Type=="Cactus.Wren"]),
                mean(card_data$Chips.Pres.Abs.Resp[card_data$Type=="Texas"]),
                mean(card_data$Chips.Pres.Abs.Resp[card_data$Type=="Bill.Williams"]),
                mean(card_data$Chips.Pres.Abs.Resp[card_data$Type=="Portal"]))
Chips_stdev = c(sd(card_data$Chips.Pres.Abs.Resp[card_data$Type=="Cactus.Wren"]),
                sd(card_data$Chips.Pres.Abs.Resp[card_data$Type=="Texas"]),
                sd(card_data$Chips.Pres.Abs.Resp[card_data$Type=="Bill.Williams"]),
                sd(card_data$Chips.Pres.Abs.Resp[card_data$Type=="Portal"]))
Chips_songs = c("Cactus.Wren","New.Mexico","Bill.Williams","Portal")
png(filename="C:/Users/Kaiya/Dropbox/Docs for Brian/Field Work/Chips_barplot.png",width=350,height=450,units="px")
Chips_barplot = barplot(Chips_means,col=c("grey15","blue","green","purple"),
                        ylim=c(0,1),ylab="Chips",xlab="Song Locality")
error.bar(Chips_barplot,Chips_means,Chips_stdev/sqrt(67))
dev.off()


## <24 songs
t.test(card_data$Songs.Less.24.Resp[card_data$Type=="Bill.Williams"],
       card_data$Songs.Less.24.Resp[card_data$Type=="Cactus.Wren"],paired=TRUE) #p-value = 0.973

t.test(card_data$Songs.Less.24.Resp[card_data$Type=="Bill.Williams"],
       card_data$Songs.Less.24.Resp[card_data$Type=="Texas"],paired=TRUE) # p-value = 0.7475

t.test(card_data$Songs.Less.24.Resp[card_data$Type=="Bill.Williams"],
       card_data$Songs.Less.24.Resp[card_data$Type=="Portal"],paired=TRUE) #p-value = 6.143e-05 ## SIG, has smallest p-val

t.test(card_data$Songs.Less.24.Resp[card_data$Type=="Cactus.Wren"],
       card_data$Songs.Less.24.Resp[card_data$Type=="Texas"],paired=TRUE) # p-value = 0.7339

t.test(card_data$Songs.Less.24.Resp[card_data$Type=="Cactus.Wren"],
       card_data$Songs.Less.24.Resp[card_data$Type=="Portal"],paired=TRUE) #  p-value = 8.163e-05 ## SIG

t.test(card_data$Songs.Less.24.Resp[card_data$Type=="Texas"],
       card_data$Songs.Less.24.Resp[card_data$Type=="Portal"],paired=TRUE) #p-value = 2.535e-05 ## SIG

Less_means = c(mean(card_data$Songs.Less.24.Resp[card_data$Type=="Cactus.Wren"]),
               mean(card_data$Songs.Less.24.Resp[card_data$Type=="Texas"]),
               mean(card_data$Songs.Less.24.Resp[card_data$Type=="Bill.Williams"]),
               mean(card_data$Songs.Less.24.Resp[card_data$Type=="Portal"]))
Less_stdev = c(sd(card_data$Songs.Less.24.Resp[card_data$Type=="Cactus.Wren"]),
               sd(card_data$Songs.Less.24.Resp[card_data$Type=="Texas"]),
               sd(card_data$Songs.Less.24.Resp[card_data$Type=="Bill.Williams"]),
               sd(card_data$Songs.Less.24.Resp[card_data$Type=="Portal"]))
Less_songs = c("Cactus.Wren","New.Mexico","Bill.Williams","Portal")
png(filename="C:/Users/Kaiya/Dropbox/Docs for Brian/Field Work/Less_barplot.png",width=350,height=450,units="px")
Less_barplot = barplot(Less_means,col=c("grey15","blue","green","purple"),
                       ylim=c(0,14),ylab="<24 Songs",xlab="Song Locality")
error.bar(Less_barplot,Less_means,Less_stdev/sqrt(67))
dev.off()


## 24+
t.test(card_data$Songs.24.Plus.Resp[card_data$Type=="Bill.Williams"],
       card_data$Songs.24.Plus.Resp[card_data$Type=="Cactus.Wren"],paired=TRUE) #p-value = 0.07187

t.test(card_data$Songs.24.Plus.Resp[card_data$Type=="Bill.Williams"],
       card_data$Songs.24.Plus.Resp[card_data$Type=="Texas"],paired=TRUE) # p-value = 0.8708

t.test(card_data$Songs.24.Plus.Resp[card_data$Type=="Bill.Williams"],
       card_data$Songs.24.Plus.Resp[card_data$Type=="Portal"],paired=TRUE) #p-value = 0.1617 ## SIG, has smallest p-val

t.test(card_data$Songs.24.Plus.Resp[card_data$Type=="Cactus.Wren"],
       card_data$Songs.24.Plus.Resp[card_data$Type=="Texas"],paired=TRUE) # p-value = 0.01916

t.test(card_data$Songs.24.Plus.Resp[card_data$Type=="Cactus.Wren"],
       card_data$Songs.24.Plus.Resp[card_data$Type=="Portal"],paired=TRUE) #  p-value = 0.5735 ## SIG

t.test(card_data$Songs.24.Plus.Resp[card_data$Type=="Texas"],
       card_data$Songs.24.Plus.Resp[card_data$Type=="Portal"],paired=TRUE) #p-value = 0.09506 ## SIG

Plus_means = c(mean(card_data$Songs.24.Plus.Resp[card_data$Type=="Cactus.Wren"]),
               mean(card_data$Songs.24.Plus.Resp[card_data$Type=="Texas"]),
               mean(card_data$Songs.24.Plus.Resp[card_data$Type=="Bill.Williams"]),
               mean(card_data$Songs.24.Plus.Resp[card_data$Type=="Portal"]))
Plus_stdev = c(sd(card_data$Songs.24.Plus.Resp[card_data$Type=="Cactus.Wren"]),
               sd(card_data$Songs.24.Plus.Resp[card_data$Type=="Texas"]),
               sd(card_data$Songs.24.Plus.Resp[card_data$Type=="Bill.Williams"]),
               sd(card_data$Songs.24.Plus.Resp[card_data$Type=="Portal"]))
Plus_songs = c("Cactus.Wren","New.Mexico","Bill.Williams","Portal")
png(filename="C:/Users/Kaiya/Dropbox/Docs for Brian/Field Work/Plus_barplot.png",width=350,height=450,units="px")
Plus_barplot = barplot(Plus_means,col=c("grey15","blue","green","purple"),
                       ylim=c(0,14),ylab="<24 Songs",xlab="Song Locality")
error.bar(Plus_barplot,Plus_means,Plus_stdev/sqrt(67))
dev.off()