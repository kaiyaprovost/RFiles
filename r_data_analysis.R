## import the data
prelim_data_summary <- read.csv("C:/Users/Kaiya/Dropbox/Docs for Brian/Field Work/prelim_data_summary.csv")

## get names of the headers
names(prelim_data_summary)

## allow you to use the names of the headers
attach(prelim_data_summary)

#see correlations between summary columns
plot(prelim_data_summary[,c(11,15,19,23,27)],panel=panel.smooth)

## create matrix of means of data based on location
N = length(prelim_data_summary)

## import alternate version with just summaries
data_rework <- read.csv("C:/Users/Kaiya/Dropbox/Docs for Brian/Field Work/data_rework.csv")

## linear model
d

--
  
## boxplots
## import different data  
boxplot_data <- read.csv("C:/Users/Kaiya/Dropbox/Docs for Brian/Field Work/preliminary_data_6jun2015.csv", header=TRUE)
names(boxplot_data)

## count 24
  ## do pre/pb/post for each locality individually
  png(filename="PrePBPost_AllLocalities.png",width=800,height=800)
  boxplot(boxplot_data$Count.24.Pre[boxplot_data$Type=="Bill Williams"],
        boxplot_data$Count.24.PB[boxplot_data$Type=="Bill Williams"],
        boxplot_data$Count.24.Post[boxplot_data$Type=="Bill Williams"],
        boxplot_data$Count.24.Pre[boxplot_data$Type=="Cactus Wren"],
        boxplot_data$Count.24.PB[boxplot_data$Type=="Cactus Wren"],
        boxplot_data$Count.24.Post[boxplot_data$Type=="Cactus Wren"],
        boxplot_data$Count.24.Pre[boxplot_data$Type=="Portal"],
        boxplot_data$Count.24.PB[boxplot_data$Type=="Portal"],
        boxplot_data$Count.24.Post[boxplot_data$Type=="Portal"],
        boxplot_data$Count.24.Pre[boxplot_data$Type=="Texas"],
        boxplot_data$Count.24.PB[boxplot_data$Type=="Texas"],
        boxplot_data$Count.24.Post[boxplot_data$Type=="Texas"],
        names=c("Pre.B","PB.B","Post.B","Pre.C","PB.C",
                "Post.C","Pre.P","PB.P","Post.P","Pre.T","PB.T","Post.T"),
        main="Count of 24+",ylab="Trial",xlab="Count of 24+ Distances",
        col=c("red","red","red","white","white","white",
              "goldenrod","goldenrod","goldenrod","blue","blue","blue"),
        outcol=c("red","red","red","white","white","white",
              "goldenrod","goldenrod","goldenrod","blue","blue","blue"),
        horizontal=TRUE)
  dev.off()  
  ## create boxplots of songs vs pre/pb/post
    ## just pre
    boxplot(boxplot_data$Count.24.Pre~boxplot_data$Type,main="Pre-Playback",
            horizontal=TRUE)
    ## just playback
    boxplot(boxplot_data$Count.24.PB~boxplot_data$Type,main="Playback",
            horizontal=TRUE)
    ## just post
    boxplot(boxplot_data$Count.24.Post~boxplot_data$Type,main="Post-Playback",
            horizontal=TRUE)
  ## create boxplots of pre vs pb vs post
  ## for each song type
    ## just Bill Will
    boxplot(boxplot_data$Count.24.Pre[boxplot_data$Type=="Bill Williams"],
          boxplot_data$Count.24.PB[boxplot_data$Type=="Bill Williams"],
          boxplot_data$Count.24.Post[boxplot_data$Type=="Bill Williams"],
          names=c("Pre","PB","Post"),main="Bill Williams",horizontal=TRUE)
    ## just Cactus Wren
    boxplot(boxplot_data$Count.24.Pre[boxplot_data$Type=="Cactus Wren"],
          boxplot_data$Count.24.PB[boxplot_data$Type=="Cactus Wren"],
          boxplot_data$Count.24.Post[boxplot_data$Type=="Cactus Wren"],
          names=c("Pre","PB","Post"),main="Cactus Wren",horizontal=TRUE)
    ## Just Portal
    boxplot(boxplot_data$Count.24.Pre[boxplot_data$Type=="Portal"],
          boxplot_data$Count.24.PB[boxplot_data$Type=="Portal"],
          boxplot_data$Count.24.Post[boxplot_data$Type=="Portal"],
          names=c("Pre","PB","Post"),main="Portal",horizontal=TRUE)
    ## Just Texas
    boxplot(boxplot_data$Count.24.Pre[boxplot_data$Type=="Texas"],
          boxplot_data$Count.24.PB[boxplot_data$Type=="Texas"],
          boxplot_data$Count.24.Post[boxplot_data$Type=="Texas"],
          names=c("Pre","PB","Post"),main="Texas",horizontal=TRUE)

## chips
  ## create boxplots of songs vs sums of chips over entire period
    boxplot(boxplot_data$Chips.Sum~boxplot_data$Type,main="Pre-Playback",
        horizontal=TRUE)



## closest distance with pb/post concatenated and no pre
## import the data
closest_distance <- read.csv("C:/Users/Kaiya/Dropbox/Docs for Brian/Field Work/closest_distance.csv", header=TRUE)

## make the boxplot
png(filename="ClosestDistance_PBPostOnly.png",width=800,height=800)
boxplot(closest_distance$closest.dist.pb.post.only~closest_distance$Type,
        col=c("red","white","goldenrod","blue"),
        outcol=c("red","white","goldenrod","blue"),
        ylab="Closest Distance From Speaker (m)",
        xlab="Song Type Played",
        main="Closest Distance",
        horizontal=TRUE)
dev.off()


boxplot(boxplot_data$Count.24.Pre[boxplot_data$Type=="Bill Williams"],c(
        boxplot_data$Count.24.PB[boxplot_data$Type=="Bill Williams"],
        boxplot_data$Count.24.Post[boxplot_data$Type=="Bill Williams"]),
        boxplot_data$Count.24.Pre[boxplot_data$Type=="Cactus Wren"],c(
        boxplot_data$Count.24.PB[boxplot_data$Type=="Cactus Wren"],
        boxplot_data$Count.24.Post[boxplot_data$Type=="Cactus Wren"]),
        boxplot_data$Count.24.Pre[boxplot_data$Type=="Portal"],c(
        boxplot_data$Count.24.PB[boxplot_data$Type=="Portal"],
        boxplot_data$Count.24.Post[boxplot_data$Type=="Portal"]),
        boxplot_data$Count.24.Pre[boxplot_data$Type=="Texas"],c(
        boxplot_data$Count.24.PB[boxplot_data$Type=="Texas"],
        boxplot_data$Count.24.Post[boxplot_data$Type=="Texas"]),
        names=c("Pre.B","PB.B and Post.B","Pre.C","PB.C and Post.C",
                "Pre.P","PB.P and Post.P","Pre.T","PB.T and Post.T"),
        main="Count of 24+",ylab="Trial",xlab="Count of 24+ Distances",
        col=c("red","red","white","white",
              "goldenrod","goldenrod","blue","blue"),
        outcol=c("red","red","white","white",
                 "goldenrod","goldenrod","blue","blue"),
        horizontal=TRUE)
dev.off()  
## create boxplots of songs vs pre/pb/post
## just pre
boxplot(boxplot_data$Count.24.Pre~boxplot_data$Type,main="Pre-Playback",
        horizontal=TRUE)
## just playback
boxplot(boxplot_data$Count.24.PB~boxplot_data$Type,main="Playback",
        horizontal=TRUE)
## just post
boxplot(boxplot_data$Count.24.Post~boxplot_data$Type,main="Post-Playback",
        horizontal=TRUE)
## create boxplots of pre vs pb vs post
## for each song type
## just Bill Will
boxplot(boxplot_data$Count.24.Pre[boxplot_data$Type=="Bill Williams"],
        boxplot_data$Count.24.PB[boxplot_data$Type=="Bill Williams"],
        boxplot_data$Count.24.Post[boxplot_data$Type=="Bill Williams"],
        names=c("Pre","PB","Post"),main="Bill Williams",horizontal=TRUE)
## just Cactus Wren
boxplot(boxplot_data$Count.24.Pre[boxplot_data$Type=="Cactus Wren"],
        boxplot_data$Count.24.PB[boxplot_data$Type=="Cactus Wren"],
        boxplot_data$Count.24.Post[boxplot_data$Type=="Cactus Wren"],
        names=c("Pre","PB","Post"),main="Cactus Wren",horizontal=TRUE)
## Just Portal
boxplot(boxplot_data$Count.24.Pre[boxplot_data$Type=="Portal"],
        boxplot_data$Count.24.PB[boxplot_data$Type=="Portal"],
        boxplot_data$Count.24.Post[boxplot_data$Type=="Portal"],
        names=c("Pre","PB","Post"),main="Portal",horizontal=TRUE)
## Just Texas
boxplot(boxplot_data$Count.24.Pre[boxplot_data$Type=="Texas"],
        boxplot_data$Count.24.PB[boxplot_data$Type=="Texas"],
        boxplot_data$Count.24.Post[boxplot_data$Type=="Texas"],
        names=c("Pre","PB","Post"),main="Texas",horizontal=TRUE)

## trying the categorical thing brian wants
## import data
data_14jun2015 <- read.csv("C:/Users/Kaiya/Dropbox/Docs for Brian/Field Work/data_14jun2015.csv", header=TRUE)
names(data_14jun2015)

## get only the categorical distance stuff
smalldata_14jun = data_14jun2015[,c(1:6,53,30:32,7,54,36:38,8,55,42:44,9,56,48:50,10)]
names(smalldata_14jun)

## only the sum stuff
sumdata_14jun = smalldata_14jun[,c(1:6,22:26)]
names(sumdata_14jun)

## make the boxplot
png(filename="CategoricalBoxPlotAll.png",width=1000,height=600)
boxplot(sumdata_14jun$Sum0to4[sumdata_14jun$Type=="Portal"],sumdata_14jun$Sum4to8[sumdata_14jun$Type=="Portal"],
        sumdata_14jun$Sum8to16[sumdata_14jun$Type=="Portal"],sumdata_14jun$Sum16to24[sumdata_14jun$Type=="Portal"],
        sumdata_14jun$Count24PlusSum[sumdata_14jun$Type=="Portal"],sumdata_14jun$Sum0to4[sumdata_14jun$Type=="BillWilliams"],
        sumdata_14jun$Sum4to8[sumdata_14jun$Type=="BillWilliams"],sumdata_14jun$Sum8to16[sumdata_14jun$Type=="BillWilliams"],
        sumdata_14jun$Sum16to24[sumdata_14jun$Type=="BillWilliams"],sumdata_14jun$Count24PlusSum[sumdata_14jun$Type=="BillWilliams"],
        sumdata_14jun$Sum0to4[sumdata_14jun$Type=="Texas"],sumdata_14jun$Sum4to8[sumdata_14jun$Type=="Texas"],
        sumdata_14jun$Sum8to16[sumdata_14jun$Type=="Texas"],sumdata_14jun$Sum16to24[sumdata_14jun$Type=="Texas"],
        sumdata_14jun$Count24PlusSum[sumdata_14jun$Type=="Texas"],
        sumdata_14jun$Sum0to4[sumdata_14jun$Type=="CactusWren"],
        sumdata_14jun$Sum4to8[sumdata_14jun$Type=="CactusWren"],
        sumdata_14jun$Sum8to16[sumdata_14jun$Type=="CactusWren"],
        sumdata_14jun$Sum16to24[sumdata_14jun$Type=="CactusWren"],
        sumdata_14jun$Count24PlusSum[sumdata_14jun$Type=="CactusWren"],
        horizontal=FALSE,
        names=c("P0","P4","P8","P16","P24","B0","B4","B8","B16","B24",
                "T0","T4","T8","T16","T24","C0","C4","C8","C16","C24"),
        col=c("goldenrod","goldenrod","goldenrod","goldenrod","goldenrod",
                 "red","red","red","red","red",
                 "blue","blue","blue","blue","blue",
                 "white","white","white","white","white"),
        outcol=c("goldenrod","goldenrod","goldenrod","goldenrod","goldenrod",
                 "red","red","red","red","red",
                 "blue","blue","blue","blue","blue",
                 "black","black","black","black","black"),
        main="Distance Categories vs Song Type",xlab="Type-Distance",ylab="Freq of Category")
dev.off()
## try spineplot
data_14jun_longview <- read.csv("C:/Users/Kaiya/Dropbox/Docs for Brian/Field Work/data_14jun_longview.csv")
spinedata = table(data_14jun_longview)
spinedata = spinedata[,c(1,3,5,6,7,2,4)]

png(filename="Spineplot.png",width=800,height=800)
spineplot(spinedata,col=c("red","orange","yellow","green","blue","purple","pink"))
dev.off()


## import new data
data_14jun_responsesonly <- read.csv("C:/Users/Kaiya/Dropbox/Docs for Brian/Field Work/data_14jun_responsesonly.csv")
png(filename="CategoricalBoxPlotResp.png",width=1000,height=600)
boxplot(data_14jun_responsesonly$Resp.0.to.4[data_14jun_responsesonly$Type=="Portal"],data_14jun_responsesonly$Resp.4.to.8[data_14jun_responsesonly$Type=="Portal"],
        data_14jun_responsesonly$Resp.8.to.16[data_14jun_responsesonly$Type=="Portal"],data_14jun_responsesonly$Resp.16.to.24[data_14jun_responsesonly$Type=="Portal"],
        data_14jun_responsesonly$Resp.24.[data_14jun_responsesonly$Type=="Portal"],data_14jun_responsesonly$Resp.0.to.4[data_14jun_responsesonly$Type=="Bill Williams"],
        data_14jun_responsesonly$Resp.4.to.8[data_14jun_responsesonly$Type=="Bill Williams"],data_14jun_responsesonly$Resp.8.to.16[data_14jun_responsesonly$Type=="Bill Williams"],
        data_14jun_responsesonly$Resp.16.to.24[data_14jun_responsesonly$Type=="Bill Williams"],data_14jun_responsesonly$Resp.24.[data_14jun_responsesonly$Type=="Bill Williams"],
        data_14jun_responsesonly$Resp.0.to.4[data_14jun_responsesonly$Type=="Texas"],data_14jun_responsesonly$Resp.4.to.8[data_14jun_responsesonly$Type=="Texas"],
        data_14jun_responsesonly$Resp.8.to.16[data_14jun_responsesonly$Type=="Texas"],data_14jun_responsesonly$Resp.16.to.24[data_14jun_responsesonly$Type=="Texas"],
        data_14jun_responsesonly$Resp.24.[data_14jun_responsesonly$Type=="Texas"],
        data_14jun_responsesonly$Resp.0.to.4[data_14jun_responsesonly$Type=="Cactus Wren"],
        data_14jun_responsesonly$Resp.4.to.8[data_14jun_responsesonly$Type=="Cactus Wren"],
        data_14jun_responsesonly$Resp.8.to.16[data_14jun_responsesonly$Type=="Cactus Wren"],
        data_14jun_responsesonly$Resp.16.to.24[data_14jun_responsesonly$Type=="Cactus Wren"],
        data_14jun_responsesonly$Resp.24.[data_14jun_responsesonly$Type=="Cactus Wren"],
        horizontal=FALSE,
        names=c("P0","P4","P8","P16","P24","B0","B4","B8","B16","B24",
                "T0","T4","T8","T16","T24","C0","C4","C8","C16","C24"),
        col=c("goldenrod","goldenrod","goldenrod","goldenrod","goldenrod",
              "red","red","red","red","red",
              "blue","blue","blue","blue","blue",
              "white","white","white","white","white"),
        outcol=c("goldenrod","goldenrod","goldenrod","goldenrod","goldenrod",
                 "red","red","red","red","red",
                 "blue","blue","blue","blue","blue",
                 "black","black","black","black","black"),
        main="Distance Categories vs Song Type",xlab="Type-Distance (resp only)",ylab="Freq of Category")
dev.off()


## closest distance again
png(filename="ClosestDistanceBoxplot.png",width=800,height=800)
closest_distance_14jun <- read.csv("C:/Users/Kaiya/Dropbox/Docs for Brian/Field Work/closest_distance_14jun.csv")
boxplot(closest_distance_14jun$CloseResp~closest_distance_14jun$Type,
        col=c("red","white","goldenrod","blue"),
        outcol=c("red","black","goldenrod","blue"),
        main="Closest Distance to Speaker (Response Period)",
        xlab="Song Type", ylab="Closest Distance (m)",ylim=c(0,24),
        horizontal=TRUE,xaxt="n")
axis(1,at=c(0,1,2,4,8,16,24),labels=c(0,1,2,4,8,16,24))
dev.off()

## density plot
png(filename="DensityPlot.png",width=800,height=800)
plot(density(closest_distance_14jun$CloseResp[closest_distance_14jun$Type=="BillWilliams"]),
     xlim=c(0,24),ylim=c(0,0.14),main="Density of Distance from Speaker by song Type",xlab="Distance",ylab="Density",
     col="red",lwd=2,xaxt="n")
axis(1,at=c(0,1,2,4,8,16,24),labels=c(0,1,2,4,8,16,24))
lines(density(closest_distance_14jun$CloseResp[closest_distance_14jun$Type=="CactusWren"]),
     xlim=c(0,24),ylim=c(0,0.14),main="Density",col="black",lwd=2)
lines(density(closest_distance_14jun$CloseResp[closest_distance_14jun$Type=="Portal"]),
     xlim=c(0,24),ylim=c(0,0.14),main="Density",col="goldenrod",lwd=2)
lines(density(closest_distance_14jun$CloseResp[closest_distance_14jun$Type=="Texas"]),
     xlim=c(0,24),ylim=c(0,0.14),main="Density",col="blue",lwd=2)
text(5,0.12,labels=c("Bill Williams"),col=c("red"))
text(5,0.10,labels=c("Cactus Wren"),col=c("black"))
text(5,0.13,labels=c("Portal"),col=c("goldenrod"))
text(5,0.11,labels=c("Texas"),col=c("blue"))
dev.off()

## pie chart


pie(closest_distance_14jun$CloseResp)

## bar chart
barchart_closestdist_14jun <- (read.csv("C:/Users/Kaiya/Dropbox/Docs for Brian/Field Work/barchart_closestdist_14jun.csv"))
rownames(barchart_closestdist_14jun) = barchart_closestdist_14jun[,1]
barchart_closestdist_14jun = barchart_closestdist_14jun[,-1]
