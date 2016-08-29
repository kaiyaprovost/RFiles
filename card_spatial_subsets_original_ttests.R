## import data

card_100A <- read.csv("C:/Users/Kaiya/Dropbox/Docs for Brian/THESIS/Field Work/Cardinal Data Analysis/Master Files/CSVs/spatial subsets/card_data_100A_6nov2015.csv")
card_100B <- read.csv("C:/Users/Kaiya/Dropbox/Docs for Brian/THESIS/Field Work/Cardinal Data Analysis/Master Files/CSVs/spatial subsets/card_data_100B_6nov2015.csv")
card_100C <- read.csv("C:/Users/Kaiya/Dropbox/Docs for Brian/THESIS/Field Work/Cardinal Data Analysis/Master Files/CSVs/spatial subsets/card_data_100C_6nov2015.csv")

card_150A <- read.csv("C:/Users/Kaiya/Dropbox/Docs for Brian/THESIS/Field Work/Cardinal Data Analysis/Master Files/CSVs/spatial subsets/card_data_150A_6nov2015.csv")
card_150B <- read.csv("C:/Users/Kaiya/Dropbox/Docs for Brian/THESIS/Field Work/Cardinal Data Analysis/Master Files/CSVs/spatial subsets/card_data_150B_6nov2015.csv")

card_200A <- read.csv("C:/Users/Kaiya/Dropbox/Docs for Brian/THESIS/Field Work/Cardinal Data Analysis/Master Files/CSVs/spatial subsets/card_data_200A_6nov2015.csv")
card_200B <- read.csv("C:/Users/Kaiya/Dropbox/Docs for Brian/THESIS/Field Work/Cardinal Data Analysis/Master Files/CSVs/spatial subsets/card_data_200B_6nov2015.csv")

error.bar <- function(barplot, means, upper, lower=upper, length=0.1,...){
  if(length(barplot) != length(means) | length(means) !=length(lower) | length(lower) != length(upper))
    stop("vectors must be same length")
  arrows(barplot,means+upper, barplot, means-lower, angle=90, code=3, length=length, ...)
}

## PCAS 1 and 2
#####

## PLOTS
#####
## PC1

card_100A_PCA <- read.csv("C:/Users/Kaiya/Dropbox/Docs for Brian/THESIS/Field Work/Cardinal Data Analysis/Master Files/CSVs/spatial subsets/card_data_100A_PCA_2dec2015.csv")
card_150A_PCA <- read.csv("C:/Users/Kaiya/Dropbox/Docs for Brian/THESIS/Field Work/Cardinal Data Analysis/Master Files/CSVs/spatial subsets/card_data_150A_PCA_2dec2015.csv")
card_200A_PCA <- read.csv("C:/Users/Kaiya/Dropbox/Docs for Brian/THESIS/Field Work/Cardinal Data Analysis/Master Files/CSVs/spatial subsets/card_data_200A_PCA_2dec2015.csv")

pc1_mean_100A_PCA = c(mean(card_100A_PCA$PC1[card_100A_PCA$Type=="Cactus.Wren"]),
                      mean(card_100A_PCA$PC1[card_100A_PCA$Type=="Texas"]),
                      mean(card_100A_PCA$PC1[card_100A_PCA$Type=="Bill.Williams"]),
                      mean(card_100A_PCA$PC1[card_100A_PCA$Type=="Portal"]))
pc1_sd_100A_PCA = c(sd(card_100A_PCA$PC1[card_100A_PCA$Type=="Cactus.Wren"]),
                    sd(card_100A_PCA$PC1[card_100A_PCA$Type=="Texas"]),
                    sd(card_100A_PCA$PC1[card_100A_PCA$Type=="Bill.Williams"]),
                    sd(card_100A_PCA$PC1[card_100A_PCA$Type=="Portal"]))
songs = c("Cactus.Wren","New.Mexico","Bill.Williams","Portal")
pc1_plot_100A_PCA = barplot(pc1_mean_100A_PCA,col=c("grey15","blue","green","purple"),
                            ylim=c(-1,2),ylab="pc1",xlab="Song Locality",names=songs,
                            main="100A_PCA")
error.bar(pc1_plot_100A_PCA,pc1_mean_100A_PCA,pc1_sd_100A_PCA/sqrt(length(card_100A_PCA$Type)/4))


pc1_mean_150A_PCA = c(mean(card_150A_PCA$PC1[card_150A_PCA$Type=="Cactus.Wren"]),
                   mean(card_150A_PCA$PC1[card_150A_PCA$Type=="Texas"]),
                   mean(card_150A_PCA$PC1[card_150A_PCA$Type=="Bill.Williams"]),
                   mean(card_150A_PCA$PC1[card_150A_PCA$Type=="Portal"]))
pc1_sd_150A_PCA = c(sd(card_150A_PCA$PC1[card_150A_PCA$Type=="Cactus.Wren"]),
                 sd(card_150A_PCA$PC1[card_150A_PCA$Type=="Texas"]),
                 sd(card_150A_PCA$PC1[card_150A_PCA$Type=="Bill.Williams"]),
                 sd(card_150A_PCA$PC1[card_150A_PCA$Type=="Portal"]))
songs = c("Cactus.Wren","New.Mexico","Bill.Williams","Portal")
pc1_plot_150A_PCA = barplot(pc1_mean_150A_PCA,col=c("grey15","blue","green","purple"),
                         ylim=c(-1,2.5),ylab="pc1",xlab="Song Locality",names=songs,
                         main="150A_PCA")
error.bar(pc1_plot_150A_PCA,pc1_mean_150A_PCA,pc1_sd_150A_PCA/sqrt(length(card_150A_PCA$Type)/4))

pc1_mean_200A_PCA = c(mean(card_200A_PCA$PC1[card_200A_PCA$Type=="Cactus.Wren"]),
                      mean(card_200A_PCA$PC1[card_200A_PCA$Type=="Texas"]),
                      mean(card_200A_PCA$PC1[card_200A_PCA$Type=="Bill.Williams"]),
                      mean(card_200A_PCA$PC1[card_200A_PCA$Type=="Portal"]))
pc1_sd_200A_PCA = c(sd(card_200A_PCA$PC1[card_200A_PCA$Type=="Cactus.Wren"]),
                    sd(card_200A_PCA$PC1[card_200A_PCA$Type=="Texas"]),
                    sd(card_200A_PCA$PC1[card_200A_PCA$Type=="Bill.Williams"]),
                    sd(card_200A_PCA$PC1[card_200A_PCA$Type=="Portal"]))
songs = c("Cactus.Wren","New.Mexico","Bill.Williams","Portal")
pc1_plot_200A_PCA = barplot(pc1_mean_200A_PCA,col=c("grey15","blue","green","purple"),
                            ylim=c(-1,2),ylab="pc1",xlab="Song Locality",names=songs,
                            main="200A_PCA")
error.bar(pc1_plot_200A_PCA,pc1_mean_200A_PCA,pc1_sd_200A_PCA/sqrt(length(card_200A_PCA$Type)/4))

## PC2
PC2_mean_100A_PCA = c(mean(card_100A_PCA$PC2[card_100A_PCA$Type=="Cactus.Wren"]),
                      mean(card_100A_PCA$PC2[card_100A_PCA$Type=="Texas"]),
                      mean(card_100A_PCA$PC2[card_100A_PCA$Type=="Bill.Williams"]),
                      mean(card_100A_PCA$PC2[card_100A_PCA$Type=="Portal"]))
PC2_sd_100A_PCA = c(sd(card_100A_PCA$PC2[card_100A_PCA$Type=="Cactus.Wren"]),
                    sd(card_100A_PCA$PC2[card_100A_PCA$Type=="Texas"]),
                    sd(card_100A_PCA$PC2[card_100A_PCA$Type=="Bill.Williams"]),
                    sd(card_100A_PCA$PC2[card_100A_PCA$Type=="Portal"]))
songs = c("Cactus.Wren","New.Mexico","Bill.Williams","Portal")
PC2_plot_100A_PCA = barplot(PC2_mean_100A_PCA,col=c("grey15","blue","green","purple"),
                            ylim=c(-0.2,0.4),ylab="PC2",xlab="Song Locality",names=songs,
                            main="100A_PCA")
error.bar(PC2_plot_100A_PCA,PC2_mean_100A_PCA,PC2_sd_100A_PCA/sqrt(length(card_100A_PCA$Type)/4))


PC2_mean_150A_PCA = c(mean(card_150A_PCA$PC2[card_150A_PCA$Type=="Cactus.Wren"]),
                      mean(card_150A_PCA$PC2[card_150A_PCA$Type=="Texas"]),
                      mean(card_150A_PCA$PC2[card_150A_PCA$Type=="Bill.Williams"]),
                      mean(card_150A_PCA$PC2[card_150A_PCA$Type=="Portal"]))
PC2_sd_150A_PCA = c(sd(card_150A_PCA$PC2[card_150A_PCA$Type=="Cactus.Wren"]),
                    sd(card_150A_PCA$PC2[card_150A_PCA$Type=="Texas"]),
                    sd(card_150A_PCA$PC2[card_150A_PCA$Type=="Bill.Williams"]),
                    sd(card_150A_PCA$PC2[card_150A_PCA$Type=="Portal"]))
songs = c("Cactus.Wren","New.Mexico","Bill.Williams","Portal")
PC2_plot_150A_PCA = barplot(PC2_mean_150A_PCA,col=c("grey15","blue","green","purple"),
                            ylim=c(-0.4,0.4),ylab="PC2",xlab="Song Locality",names=songs,
                            main="150A_PCA")
error.bar(PC2_plot_150A_PCA,PC2_mean_150A_PCA,PC2_sd_150A_PCA/sqrt(length(card_150A_PCA$Type)/4))

PC2_mean_200A_PCA = c(mean(card_200A_PCA$PC2[card_200A_PCA$Type=="Cactus.Wren"]),
                      mean(card_200A_PCA$PC2[card_200A_PCA$Type=="Texas"]),
                      mean(card_200A_PCA$PC2[card_200A_PCA$Type=="Bill.Williams"]),
                      mean(card_200A_PCA$PC2[card_200A_PCA$Type=="Portal"]))
PC2_sd_200A_PCA = c(sd(card_200A_PCA$PC2[card_200A_PCA$Type=="Cactus.Wren"]),
                    sd(card_200A_PCA$PC2[card_200A_PCA$Type=="Texas"]),
                    sd(card_200A_PCA$PC2[card_200A_PCA$Type=="Bill.Williams"]),
                    sd(card_200A_PCA$PC2[card_200A_PCA$Type=="Portal"]))
songs = c("Cactus.Wren","New.Mexico","Bill.Williams","Portal")
PC2_plot_200A_PCA = barplot(PC2_mean_200A_PCA,col=c("grey15","blue","green","purple"),
                            ylim=c(-0.3,0.4),ylab="PC2",xlab="Song Locality",names=songs,
                            main="200A_PCA")
error.bar(PC2_plot_200A_PCA,PC2_mean_200A_PCA,PC2_sd_200A_PCA/sqrt(length(card_200A_PCA$Type)/4))

## T TESTS
##### 
## T TESTS

## 100
#####
## 100

t.test(card_100A_PCA$PC1[card_100A_PCA$Type=="Bill.Williams"],card_100A_PCA$PC1[card_100A_PCA$Type=="Cactus.Wren"],paired=TRUE)$p.value
#p-value 0.09337535

t.test(card_100A_PCA$PC1[card_100A_PCA$Type=="Bill.Williams"],card_100A_PCA$PC1[card_100A_PCA$Type=="Texas"],paired=TRUE)$p.value
# p-value 0.9029984

t.test(card_100A_PCA$PC1[card_100A_PCA$Type=="Bill.Williams"],card_100A_PCA$PC1[card_100A_PCA$Type=="Portal"],paired=TRUE)$p.value
#p-value 4.312597e-06

t.test(card_100A_PCA$PC1[card_100A_PCA$Type=="Cactus.Wren"],card_100A_PCA$PC1[card_100A_PCA$Type=="Texas"],paired=TRUE)$p.value
# p-value 0.07032801

t.test(card_100A_PCA$PC1[card_100A_PCA$Type=="Cactus.Wren"],card_100A_PCA$PC1[card_100A_PCA$Type=="Portal"],paired=TRUE)$p.value
#  p-value 2.715014e-08

t.test(card_100A_PCA$PC1[card_100A_PCA$Type=="Texas"],card_100A_PCA$PC1[card_100A_PCA$Type=="Portal"],paired=TRUE)$p.value
#p-value = 3.942792e-06

## 150
#####
## 150
t.test(card_150A_PCA$PC1[card_150A_PCA$Type=="Bill.Williams"],card_150A_PCA$PC1[card_150A_PCA$Type=="Cactus.Wren"],paired=TRUE)$p.value
#p-value 0.3332045

t.test(card_150A_PCA$PC1[card_150A_PCA$Type=="Bill.Williams"],card_150A_PCA$PC1[card_150A_PCA$Type=="Texas"],paired=TRUE)$p.value
# p-value 0.1484761

t.test(card_150A_PCA$PC1[card_150A_PCA$Type=="Bill.Williams"],card_150A_PCA$PC1[card_150A_PCA$Type=="Portal"],paired=TRUE)$p.value
#p-value 4.295388e-05

t.test(card_150A_PCA$PC1[card_150A_PCA$Type=="Cactus.Wren"],card_150A_PCA$PC1[card_150A_PCA$Type=="Texas"],paired=TRUE)$p.value
# p-value 0.8316162

t.test(card_150A_PCA$PC1[card_150A_PCA$Type=="Cactus.Wren"],card_150A_PCA$PC1[card_150A_PCA$Type=="Portal"],paired=TRUE)$p.value
#  p-value 1.056978e-05

t.test(card_150A_PCA$PC1[card_150A_PCA$Type=="Texas"],card_150A_PCA$PC1[card_150A_PCA$Type=="Portal"],paired=TRUE)$p.value
#p-value 2.261066e-06

## 200
#####
## 200

t.test(card_200A_PCA$PC1[card_200A_PCA$Type=="Bill.Williams"],card_200A_PCA$PC1[card_200A_PCA$Type=="Cactus.Wren"],paired=TRUE)$p.value
#p-value 0.9789093

t.test(card_200A_PCA$PC1[card_200A_PCA$Type=="Bill.Williams"],card_200A_PCA$PC1[card_200A_PCA$Type=="Texas"],paired=TRUE)$p.value
# p-value 0.4996327

t.test(card_200A_PCA$PC1[card_200A_PCA$Type=="Bill.Williams"],card_200A_PCA$PC1[card_200A_PCA$Type=="Portal"],paired=TRUE)$p.value
#p-value 5.663073e-05

t.test(card_200A_PCA$PC1[card_200A_PCA$Type=="Cactus.Wren"],card_200A_PCA$PC1[card_200A_PCA$Type=="Texas"],paired=TRUE)$p.value
# p-value 0.5724404

t.test(card_200A_PCA$PC1[card_200A_PCA$Type=="Cactus.Wren"],card_200A_PCA$PC1[card_200A_PCA$Type=="Portal"],paired=TRUE)$p.value
#  p-value 0.0001355793

t.test(card_200A_PCA$PC1[card_200A_PCA$Type=="Texas"],card_200A_PCA$PC1[card_200A_PCA$Type=="Portal"],paired=TRUE)$p.value
#p-value 0.0001288589


## closest dist
#####
## closest dist

## PLOTS
#####
## PLOTS

## 100
#####
## 100

dist_mean_100A = c(mean(card_100A$Close.Resp.As.Number[card_100A$Type=="Cactus.Wren"]),
                   mean(card_100A$Close.Resp.As.Number[card_100A$Type=="Texas"]),
                   mean(card_100A$Close.Resp.As.Number[card_100A$Type=="Bill.Williams"]),
                   mean(card_100A$Close.Resp.As.Number[card_100A$Type=="Portal"]))
dist_sd_100A = c(sd(card_100A$Close.Resp.As.Number[card_100A$Type=="Cactus.Wren"]),
                 sd(card_100A$Close.Resp.As.Number[card_100A$Type=="Texas"]),
                 sd(card_100A$Close.Resp.As.Number[card_100A$Type=="Bill.Williams"]),
                 sd(card_100A$Close.Resp.As.Number[card_100A$Type=="Portal"]))
songs = c("Cactus.Wren","New.Mexico","Bill.Williams","Portal")
dist_plot_100A = barplot(dist_mean_100A,col=c("grey15","blue","green","purple"),
                         ylim=c(0,24),ylab="dist",xlab="Song Locality",names=songs,
                         main="100A")
error.bar(dist_plot_100A,dist_mean_100A,dist_sd_100A/sqrt(length(card_100A$Type)/4))

dist_mean_100B = c(mean(card_100B$Close.Resp.As.Number[card_100B$Type=="Cactus.Wren"]),
                   mean(card_100B$Close.Resp.As.Number[card_100B$Type=="Texas"]),
                   mean(card_100B$Close.Resp.As.Number[card_100B$Type=="Bill.Williams"]),
                   mean(card_100B$Close.Resp.As.Number[card_100B$Type=="Portal"]))
dist_sd_100B = c(sd(card_100B$Close.Resp.As.Number[card_100B$Type=="Cactus.Wren"]),
                 sd(card_100B$Close.Resp.As.Number[card_100B$Type=="Texas"]),
                 sd(card_100B$Close.Resp.As.Number[card_100B$Type=="Bill.Williams"]),
                 sd(card_100B$Close.Resp.As.Number[card_100B$Type=="Portal"]))
songs = c("Cactus.Wren","New.Mexico","Bill.Williams","Portal")
dist_plot_100B = barplot(dist_mean_100B,col=c("grey15","blue","green","purple"),
                         ylim=c(0,24),ylab="dist",xlab="Song Locality",names=songs,
                         main="100B")
error.bar(dist_plot_100B,dist_mean_100B,dist_sd_100B/sqrt(length(card_100B$Type)/4))

dist_mean_100C = c(mean(card_100C$Close.Resp.As.Number[card_100C$Type=="Cactus.Wren"]),
                   mean(card_100C$Close.Resp.As.Number[card_100C$Type=="Texas"]),
                   mean(card_100C$Close.Resp.As.Number[card_100C$Type=="Bill.Williams"]),
                   mean(card_100C$Close.Resp.As.Number[card_100C$Type=="Portal"]))
dist_sd_100C = c(sd(card_100C$Close.Resp.As.Number[card_100C$Type=="Cactus.Wren"]),
                 sd(card_100C$Close.Resp.As.Number[card_100C$Type=="Texas"]),
                 sd(card_100C$Close.Resp.As.Number[card_100C$Type=="Bill.Williams"]),
                 sd(card_100C$Close.Resp.As.Number[card_100C$Type=="Portal"]))
songs = c("Cactus.Wren","New.Mexico","Bill.Williams","Portal")
dist_plot_100C = barplot(dist_mean_100C,col=c("grey15","blue","green","purple"),
                         ylim=c(0,15),ylab="dist",xlab="Song Locality",names=songs,
                         main="100C")
error.bar(dist_plot_100C,dist_mean_100C,dist_sd_100C/sqrt(length(card_100C$Type)/4))


## 150
#####
## 150

dist_mean_150A = c(mean(card_150A$Close.Resp.As.Number[card_150A$Type=="Cactus.Wren"]),
                   mean(card_150A$Close.Resp.As.Number[card_150A$Type=="Texas"]),
                   mean(card_150A$Close.Resp.As.Number[card_150A$Type=="Bill.Williams"]),
                   mean(card_150A$Close.Resp.As.Number[card_150A$Type=="Portal"]))
dist_sd_150A = c(sd(card_150A$Close.Resp.As.Number[card_150A$Type=="Cactus.Wren"]),
                 sd(card_150A$Close.Resp.As.Number[card_150A$Type=="Texas"]),
                 sd(card_150A$Close.Resp.As.Number[card_150A$Type=="Bill.Williams"]),
                 sd(card_150A$Close.Resp.As.Number[card_150A$Type=="Portal"]))
songs = c("Cactus.Wren","New.Mexico","Bill.Williams","Portal")
dist_plot_150A = barplot(dist_mean_150A,col=c("grey15","blue","green","purple"),
                         ylim=c(0,24),ylab="dist",xlab="Song Locality",names=songs,
                         main="150A")
error.bar(dist_plot_150A,dist_mean_150A,dist_sd_150A/sqrt(length(card_150A$Type)/4))

dist_mean_150B = c(mean(card_150B$Close.Resp.As.Number[card_150B$Type=="Cactus.Wren"]),
                   mean(card_150B$Close.Resp.As.Number[card_150B$Type=="Texas"]),
                   mean(card_150B$Close.Resp.As.Number[card_150B$Type=="Bill.Williams"]),
                   mean(card_150B$Close.Resp.As.Number[card_150B$Type=="Portal"]))
dist_sd_150B = c(sd(card_150B$Close.Resp.As.Number[card_150B$Type=="Cactus.Wren"]),
                 sd(card_150B$Close.Resp.As.Number[card_150B$Type=="Texas"]),
                 sd(card_150B$Close.Resp.As.Number[card_150B$Type=="Bill.Williams"]),
                 sd(card_150B$Close.Resp.As.Number[card_150B$Type=="Portal"]))
songs = c("Cactus.Wren","New.Mexico","Bill.Williams","Portal")
dist_plot_150B = barplot(dist_mean_150B,col=c("grey15","blue","green","purple"),
                         ylim=c(0,15),ylab="dist",xlab="Song Locality",names=songs,
                         main="150B")
error.bar(dist_plot_150B,dist_mean_150B,dist_sd_150B/sqrt(length(card_150B$Type)/4))

## 200
#####
## 200

dist_mean_200A = c(mean(card_200A$Close.Resp.As.Number[card_200A$Type=="Cactus.Wren"]),
                   mean(card_200A$Close.Resp.As.Number[card_200A$Type=="Texas"]),
                   mean(card_200A$Close.Resp.As.Number[card_200A$Type=="Bill.Williams"]),
                   mean(card_200A$Close.Resp.As.Number[card_200A$Type=="Portal"]))
dist_sd_200A = c(sd(card_200A$Close.Resp.As.Number[card_200A$Type=="Cactus.Wren"]),
                 sd(card_200A$Close.Resp.As.Number[card_200A$Type=="Texas"]),
                 sd(card_200A$Close.Resp.As.Number[card_200A$Type=="Bill.Williams"]),
                 sd(card_200A$Close.Resp.As.Number[card_200A$Type=="Portal"]))
songs = c("Cactus.Wren","New.Mexico","Bill.Williams","Portal")
dist_plot_200A = barplot(dist_mean_200A,col=c("grey15","blue","green","purple"),
                         ylim=c(0,24),ylab="dist",xlab="Song Locality",names=songs,
                         main="200A")
error.bar(dist_plot_200A,dist_mean_200A,dist_sd_200A/sqrt(length(card_200A$Type)/4))

dist_mean_200B = c(mean(card_200B$Close.Resp.As.Number[card_200B$Type=="Cactus.Wren"]),
                   mean(card_200B$Close.Resp.As.Number[card_200B$Type=="Texas"]),
                   mean(card_200B$Close.Resp.As.Number[card_200B$Type=="Bill.Williams"]),
                   mean(card_200B$Close.Resp.As.Number[card_200B$Type=="Portal"]))
dist_sd_200B = c(sd(card_200B$Close.Resp.As.Number[card_200B$Type=="Cactus.Wren"]),
                 sd(card_200B$Close.Resp.As.Number[card_200B$Type=="Texas"]),
                 sd(card_200B$Close.Resp.As.Number[card_200B$Type=="Bill.Williams"]),
                 sd(card_200B$Close.Resp.As.Number[card_200B$Type=="Portal"]))
songs = c("Cactus.Wren","New.Mexico","Bill.Williams","Portal")
dist_plot_200B = barplot(dist_mean_200B,col=c("grey15","blue","green","purple"),
                         ylim=c(0,24),ylab="dist",xlab="Song Locality",names=songs,
                         main="200B")
error.bar(dist_plot_200B,dist_mean_200B,dist_sd_200B/sqrt(length(card_200B$Type)/4))

## P VALUES
#####
## P VALUES

## 100
#####
## 100

t.test(card_100A$Close.Resp.As.Number[card_100A$Type=="Bill.Williams"],card_100A$Close.Resp.As.Number[card_100A$Type=="Cactus.Wren"],paired=TRUE)$p.value
#p-value = 0.02355773

t.test(card_100A$Close.Resp.As.Number[card_100A$Type=="Bill.Williams"],card_100A$Close.Resp.As.Number[card_100A$Type=="Texas"],paired=TRUE)$p.value
# p-value = 0.4176458

t.test(card_100A$Close.Resp.As.Number[card_100A$Type=="Bill.Williams"],card_100A$Close.Resp.As.Number[card_100A$Type=="Portal"],paired=TRUE)$p.value
#p-value =0.0002208759

t.test(card_100A$Close.Resp.As.Number[card_100A$Type=="Cactus.Wren"],card_100A$Close.Resp.As.Number[card_100A$Type=="Texas"],paired=TRUE)$p.value
# p-value = 0.1165813

t.test(card_100A$Close.Resp.As.Number[card_100A$Type=="Cactus.Wren"],card_100A$Close.Resp.As.Number[card_100A$Type=="Portal"],paired=TRUE)$p.value
#  p-value = 3.102505e-08

t.test(card_100A$Close.Resp.As.Number[card_100A$Type=="Texas"],card_100A$Close.Resp.As.Number[card_100A$Type=="Portal"],paired=TRUE)$p.value
#p-value = 5.579214e-05

## 150
#####
## 150
t.test(card_150A$Close.Resp.As.Number[card_150A$Type=="Bill.Williams"],card_150A$Close.Resp.As.Number[card_150A$Type=="Cactus.Wren"],paired=TRUE)$p.value
#p-value 0.1359165

t.test(card_150A$Close.Resp.As.Number[card_150A$Type=="Bill.Williams"],card_150A$Close.Resp.As.Number[card_150A$Type=="Texas"],paired=TRUE)$p.value
# p-value 0.0235971

t.test(card_150A$Close.Resp.As.Number[card_150A$Type=="Bill.Williams"],card_150A$Close.Resp.As.Number[card_150A$Type=="Portal"],paired=TRUE)$p.value
#p-value 0.005218478

t.test(card_150A$Close.Resp.As.Number[card_150A$Type=="Cactus.Wren"],card_150A$Close.Resp.As.Number[card_150A$Type=="Texas"],paired=TRUE)$p.value
# p-value 0.71076

t.test(card_150A$Close.Resp.As.Number[card_150A$Type=="Cactus.Wren"],card_150A$Close.Resp.As.Number[card_150A$Type=="Portal"],paired=TRUE)$p.value
#  p-value 1.756241e-05

t.test(card_150A$Close.Resp.As.Number[card_150A$Type=="Texas"],card_150A$Close.Resp.As.Number[card_150A$Type=="Portal"],paired=TRUE)$p.value
#p-value 8.157857e-06

## 200
#####
## 200

t.test(card_200A$Close.Resp.As.Number[card_200A$Type=="Bill.Williams"],card_200A$Close.Resp.As.Number[card_200A$Type=="Cactus.Wren"],paired=TRUE)$p.value
#p-value 0.7499106

t.test(card_200A$Close.Resp.As.Number[card_200A$Type=="Bill.Williams"],card_200A$Close.Resp.As.Number[card_200A$Type=="Texas"],paired=TRUE)$p.value
# p-value 0.837717

t.test(card_200A$Close.Resp.As.Number[card_200A$Type=="Bill.Williams"],card_200A$Close.Resp.As.Number[card_200A$Type=="Portal"],paired=TRUE)$p.value
#p-value 0.001740264

t.test(card_200A$Close.Resp.As.Number[card_200A$Type=="Cactus.Wren"],card_200A$Close.Resp.As.Number[card_200A$Type=="Texas"],paired=TRUE)$p.value
# p-value 0.8668401

t.test(card_200A$Close.Resp.As.Number[card_200A$Type=="Cactus.Wren"],card_200A$Close.Resp.As.Number[card_200A$Type=="Portal"],paired=TRUE)$p.value
#  p-value 0.0009817141

t.test(card_200A$Close.Resp.As.Number[card_200A$Type=="Texas"],card_200A$Close.Resp.As.Number[card_200A$Type=="Portal"],paired=TRUE)$p.value
#p-value 0.001094854

## songs plus
#####
## plus songs

## PLOTS
#####
## PLOTS

## 100
#####
## 100

plus_mean_100A = c(mean(card_100A$Songs.24.Plus.Resp[card_100A$Type=="Cactus.Wren"]),
                    mean(card_100A$Songs.24.Plus.Resp[card_100A$Type=="Texas"]),
                    mean(card_100A$Songs.24.Plus.Resp[card_100A$Type=="Bill.Williams"]),
                    mean(card_100A$Songs.24.Plus.Resp[card_100A$Type=="Portal"]))
plus_sd_100A = c(sd(card_100A$Songs.24.Plus.Resp[card_100A$Type=="Cactus.Wren"]),
                  sd(card_100A$Songs.24.Plus.Resp[card_100A$Type=="Texas"]),
                  sd(card_100A$Songs.24.Plus.Resp[card_100A$Type=="Bill.Williams"]),
                  sd(card_100A$Songs.24.Plus.Resp[card_100A$Type=="Portal"]))
songs = c("Cactus.Wren","New.Mexico","Bill.Williams","Portal")
plus_plot_100A = barplot(plus_mean_100A,col=c("grey15","blue","green","purple"),
                          ylim=c(0,15),ylab="pluss",xlab="Song Locality",names=songs,
                          main="100A")
error.bar(plus_plot_100A,plus_mean_100A,plus_sd_100A/sqrt(length(card_100A$Type)/4))

plus_mean_100B = c(mean(card_100B$Songs.24.Plus.Resp[card_100B$Type=="Cactus.Wren"]),
                    mean(card_100B$Songs.24.Plus.Resp[card_100B$Type=="Texas"]),
                    mean(card_100B$Songs.24.Plus.Resp[card_100B$Type=="Bill.Williams"]),
                    mean(card_100B$Songs.24.Plus.Resp[card_100B$Type=="Portal"]))
plus_sd_100B = c(sd(card_100B$Songs.24.Plus.Resp[card_100B$Type=="Cactus.Wren"]),
                  sd(card_100B$Songs.24.Plus.Resp[card_100B$Type=="Texas"]),
                  sd(card_100B$Songs.24.Plus.Resp[card_100B$Type=="Bill.Williams"]),
                  sd(card_100B$Songs.24.Plus.Resp[card_100B$Type=="Portal"]))
songs = c("Cactus.Wren","New.Mexico","Bill.Williams","Portal")
plus_plot_100B = barplot(plus_mean_100B,col=c("grey15","blue","green","purple"),
                          ylim=c(0,15),ylab="pluss",xlab="Song Locality",names=songs,
                          main="100B")
error.bar(plus_plot_100B,plus_mean_100B,plus_sd_100B/sqrt(length(card_100B$Type)/4))

plus_mean_100C = c(mean(card_100C$Songs.24.Plus.Resp[card_100C$Type=="Cactus.Wren"]),
                    mean(card_100C$Songs.24.Plus.Resp[card_100C$Type=="Texas"]),
                    mean(card_100C$Songs.24.Plus.Resp[card_100C$Type=="Bill.Williams"]),
                    mean(card_100C$Songs.24.Plus.Resp[card_100C$Type=="Portal"]))
plus_sd_100C = c(sd(card_100C$Songs.24.Plus.Resp[card_100C$Type=="Cactus.Wren"]),
                  sd(card_100C$Songs.24.Plus.Resp[card_100C$Type=="Texas"]),
                  sd(card_100C$Songs.24.Plus.Resp[card_100C$Type=="Bill.Williams"]),
                  sd(card_100C$Songs.24.Plus.Resp[card_100C$Type=="Portal"]))
songs = c("Cactus.Wren","New.Mexico","Bill.Williams","Portal")
plus_plot_100C = barplot(plus_mean_100C,col=c("grey15","blue","green","purple"),
                          ylim=c(0,15),ylab="pluss",xlab="Song Locality",names=songs,
                          main="100C")
error.bar(plus_plot_100C,plus_mean_100C,plus_sd_100C/sqrt(length(card_100C$Type)/4))


## 150
#####
## 150

plus_mean_150A = c(mean(card_150A$Songs.24.Plus.Resp[card_150A$Type=="Cactus.Wren"]),
                    mean(card_150A$Songs.24.Plus.Resp[card_150A$Type=="Texas"]),
                    mean(card_150A$Songs.24.Plus.Resp[card_150A$Type=="Bill.Williams"]),
                    mean(card_150A$Songs.24.Plus.Resp[card_150A$Type=="Portal"]))
plus_sd_150A = c(sd(card_150A$Songs.24.Plus.Resp[card_150A$Type=="Cactus.Wren"]),
                  sd(card_150A$Songs.24.Plus.Resp[card_150A$Type=="Texas"]),
                  sd(card_150A$Songs.24.Plus.Resp[card_150A$Type=="Bill.Williams"]),
                  sd(card_150A$Songs.24.Plus.Resp[card_150A$Type=="Portal"]))
songs = c("Cactus.Wren","New.Mexico","Bill.Williams","Portal")
plus_plot_150A = barplot(plus_mean_150A,col=c("grey15","blue","green","purple"),
                          ylim=c(0,15),ylab="pluss",xlab="Song Locality",names=songs,
                          main="150A")
error.bar(plus_plot_150A,plus_mean_150A,plus_sd_150A/sqrt(length(card_150A$Type)/4))

plus_mean_150B = c(mean(card_150B$Songs.24.Plus.Resp[card_150B$Type=="Cactus.Wren"]),
                    mean(card_150B$Songs.24.Plus.Resp[card_150B$Type=="Texas"]),
                    mean(card_150B$Songs.24.Plus.Resp[card_150B$Type=="Bill.Williams"]),
                    mean(card_150B$Songs.24.Plus.Resp[card_150B$Type=="Portal"]))
plus_sd_150B = c(sd(card_150B$Songs.24.Plus.Resp[card_150B$Type=="Cactus.Wren"]),
                  sd(card_150B$Songs.24.Plus.Resp[card_150B$Type=="Texas"]),
                  sd(card_150B$Songs.24.Plus.Resp[card_150B$Type=="Bill.Williams"]),
                  sd(card_150B$Songs.24.Plus.Resp[card_150B$Type=="Portal"]))
songs = c("Cactus.Wren","New.Mexico","Bill.Williams","Portal")
plus_plot_150B = barplot(plus_mean_150B,col=c("grey15","blue","green","purple"),
                          ylim=c(0,15),ylab="pluss",xlab="Song Locality",names=songs,
                          main="150B")
error.bar(plus_plot_150B,plus_mean_150B,plus_sd_150B/sqrt(length(card_150B$Type)/4))

## 200
#####
## 200

plus_mean_200A = c(mean(card_200A$Songs.24.Plus.Resp[card_200A$Type=="Cactus.Wren"]),
                    mean(card_200A$Songs.24.Plus.Resp[card_200A$Type=="Texas"]),
                    mean(card_200A$Songs.24.Plus.Resp[card_200A$Type=="Bill.Williams"]),
                    mean(card_200A$Songs.24.Plus.Resp[card_200A$Type=="Portal"]))
plus_sd_200A = c(sd(card_200A$Songs.24.Plus.Resp[card_200A$Type=="Cactus.Wren"]),
                  sd(card_200A$Songs.24.Plus.Resp[card_200A$Type=="Texas"]),
                  sd(card_200A$Songs.24.Plus.Resp[card_200A$Type=="Bill.Williams"]),
                  sd(card_200A$Songs.24.Plus.Resp[card_200A$Type=="Portal"]))
songs = c("Cactus.Wren","New.Mexico","Bill.Williams","Portal")
plus_plot_200A = barplot(plus_mean_200A,col=c("grey15","blue","green","purple"),
                          ylim=c(0,15),ylab="pluss",xlab="Song Locality",names=songs,
                          main="200A")
error.bar(plus_plot_200A,plus_mean_200A,plus_sd_200A/sqrt(length(card_200A$Type)/4))

plus_mean_200B = c(mean(card_200B$Songs.24.Plus.Resp[card_200B$Type=="Cactus.Wren"]),
                    mean(card_200B$Songs.24.Plus.Resp[card_200B$Type=="Texas"]),
                    mean(card_200B$Songs.24.Plus.Resp[card_200B$Type=="Bill.Williams"]),
                    mean(card_200B$Songs.24.Plus.Resp[card_200B$Type=="Portal"]))
plus_sd_200B = c(sd(card_200B$Songs.24.Plus.Resp[card_200B$Type=="Cactus.Wren"]),
                  sd(card_200B$Songs.24.Plus.Resp[card_200B$Type=="Texas"]),
                  sd(card_200B$Songs.24.Plus.Resp[card_200B$Type=="Bill.Williams"]),
                  sd(card_200B$Songs.24.Plus.Resp[card_200B$Type=="Portal"]))
songs = c("Cactus.Wren","New.Mexico","Bill.Williams","Portal")
plus_plot_200B = barplot(plus_mean_200B,col=c("grey15","blue","green","purple"),
                          ylim=c(0,15),ylab="pluss",xlab="Song Locality",names=songs,
                          main="200B")
error.bar(plus_plot_200B,plus_mean_200B,plus_sd_200B/sqrt(length(card_200B$Type)/4))

## P VALUES
#####
## P VALUES

## 100
#####
## 100

t.test(card_100A$Songs.24.Plus.Resp[card_100A$Type=="Bill.Williams"],card_100A$Songs.24.Plus.Resp[card_100A$Type=="Cactus.Wren"],paired=TRUE)$p.value
#p-value = 0.3692504

t.test(card_100A$Songs.24.Plus.Resp[card_100A$Type=="Bill.Williams"],card_100A$Songs.24.Plus.Resp[card_100A$Type=="Texas"],paired=TRUE)$p.value
# p-value = 0.8765909

t.test(card_100A$Songs.24.Plus.Resp[card_100A$Type=="Bill.Williams"],card_100A$Songs.24.Plus.Resp[card_100A$Type=="Portal"],paired=TRUE)$p.value
#p-value =0.08620644

t.test(card_100A$Songs.24.Plus.Resp[card_100A$Type=="Cactus.Wren"],card_100A$Songs.24.Plus.Resp[card_100A$Type=="Texas"],paired=TRUE)$p.value
# p-value = 0.2021069

t.test(card_100A$Songs.24.Plus.Resp[card_100A$Type=="Cactus.Wren"],card_100A$Songs.24.Plus.Resp[card_100A$Type=="Portal"],paired=TRUE)$p.value
#  p-value = 0.385266

t.test(card_100A$Songs.24.Plus.Resp[card_100A$Type=="Texas"],card_100A$Songs.24.Plus.Resp[card_100A$Type=="Portal"],paired=TRUE)$p.value
#p-value = 0.03537784

## 150
#####
## 150
t.test(card_150A$Songs.24.Plus.Resp[card_150A$Type=="Bill.Williams"],card_150A$Songs.24.Plus.Resp[card_150A$Type=="Cactus.Wren"],paired=TRUE)$p.value
#p-value 0.4978853

t.test(card_150A$Songs.24.Plus.Resp[card_150A$Type=="Bill.Williams"],card_150A$Songs.24.Plus.Resp[card_150A$Type=="Texas"],paired=TRUE)$p.value
# p-value 0.3852213

t.test(card_150A$Songs.24.Plus.Resp[card_150A$Type=="Bill.Williams"],card_150A$Songs.24.Plus.Resp[card_150A$Type=="Portal"],paired=TRUE)$p.value
#p-value 0.9050392

t.test(card_150A$Songs.24.Plus.Resp[card_150A$Type=="Cactus.Wren"],card_150A$Songs.24.Plus.Resp[card_150A$Type=="Texas"],paired=TRUE)$p.value
# p-value 0.0255215

t.test(card_150A$Songs.24.Plus.Resp[card_150A$Type=="Cactus.Wren"],card_150A$Songs.24.Plus.Resp[card_150A$Type=="Portal"],paired=TRUE)$p.value
#  p-value 0.4760919

t.test(card_150A$Songs.24.Plus.Resp[card_150A$Type=="Texas"],card_150A$Songs.24.Plus.Resp[card_150A$Type=="Portal"],paired=TRUE)$p.value
#p-value 0.1913627

## 200
#####
## 200

t.test(card_200A$Songs.24.Plus.Resp[card_200A$Type=="Bill.Williams"],card_200A$Songs.24.Plus.Resp[card_200A$Type=="Cactus.Wren"],paired=TRUE)$p.value
#p-value 0.889728

t.test(card_200A$Songs.24.Plus.Resp[card_200A$Type=="Bill.Williams"],card_200A$Songs.24.Plus.Resp[card_200A$Type=="Texas"],paired=TRUE)$p.value
# p-value 0.5235415

t.test(card_200A$Songs.24.Plus.Resp[card_200A$Type=="Bill.Williams"],card_200A$Songs.24.Plus.Resp[card_200A$Type=="Portal"],paired=TRUE)$p.value
#p-value 0.3167081

t.test(card_200A$Songs.24.Plus.Resp[card_200A$Type=="Cactus.Wren"],card_200A$Songs.24.Plus.Resp[card_200A$Type=="Texas"],paired=TRUE)$p.value
# p-value 0.4377271

t.test(card_200A$Songs.24.Plus.Resp[card_200A$Type=="Cactus.Wren"],card_200A$Songs.24.Plus.Resp[card_200A$Type=="Portal"],paired=TRUE)$p.value
#  p-value 0.3902464

t.test(card_200A$Songs.24.Plus.Resp[card_200A$Type=="Texas"],card_200A$Songs.24.Plus.Resp[card_200A$Type=="Portal"],paired=TRUE)$p.value
#p-value 0.0839221

## Songs.Less.24S
#####
## Songs.Less.24S

## PLOTS
#####
## PLOTS

## 100
#####
## 100

less_mean_100A = c(mean(card_100A$Songs.Less.24.Resp[card_100A$Type=="Cactus.Wren"]),
                 mean(card_100A$Songs.Less.24.Resp[card_100A$Type=="Texas"]),
                 mean(card_100A$Songs.Less.24.Resp[card_100A$Type=="Bill.Williams"]),
                 mean(card_100A$Songs.Less.24.Resp[card_100A$Type=="Portal"]))
less_sd_100A = c(sd(card_100A$Songs.Less.24.Resp[card_100A$Type=="Cactus.Wren"]),
               sd(card_100A$Songs.Less.24.Resp[card_100A$Type=="Texas"]),
               sd(card_100A$Songs.Less.24.Resp[card_100A$Type=="Bill.Williams"]),
               sd(card_100A$Songs.Less.24.Resp[card_100A$Type=="Portal"]))
songs = c("Cactus.Wren","New.Mexico","Bill.Williams","Portal")
less_plot_100A = barplot(less_mean_100A,col=c("grey15","blue","green","purple"),
                       ylim=c(0,15),ylab="Songs.Less.24s",xlab="Song Locality",names=songs,
                       main="100A")
error.bar(less_plot_100A,less_mean_100A,less_sd_100A/sqrt(length(card_100A$Type)/4))

less_mean_100B = c(mean(card_100B$Songs.Less.24.Resp[card_100B$Type=="Cactus.Wren"]),
                 mean(card_100B$Songs.Less.24.Resp[card_100B$Type=="Texas"]),
                 mean(card_100B$Songs.Less.24.Resp[card_100B$Type=="Bill.Williams"]),
                 mean(card_100B$Songs.Less.24.Resp[card_100B$Type=="Portal"]))
less_sd_100B = c(sd(card_100B$Songs.Less.24.Resp[card_100B$Type=="Cactus.Wren"]),
               sd(card_100B$Songs.Less.24.Resp[card_100B$Type=="Texas"]),
               sd(card_100B$Songs.Less.24.Resp[card_100B$Type=="Bill.Williams"]),
               sd(card_100B$Songs.Less.24.Resp[card_100B$Type=="Portal"]))
songs = c("Cactus.Wren","New.Mexico","Bill.Williams","Portal")
less_plot_100B = barplot(less_mean_100B,col=c("grey15","blue","green","purple"),
                       ylim=c(0,15),ylab="Songs.Less.24s",xlab="Song Locality",names=songs,
                       main="100B")
error.bar(less_plot_100B,less_mean_100B,less_sd_100B/sqrt(length(card_100B$Type)/4))

less_mean_100C = c(mean(card_100C$Songs.Less.24.Resp[card_100C$Type=="Cactus.Wren"]),
                 mean(card_100C$Songs.Less.24.Resp[card_100C$Type=="Texas"]),
                 mean(card_100C$Songs.Less.24.Resp[card_100C$Type=="Bill.Williams"]),
                 mean(card_100C$Songs.Less.24.Resp[card_100C$Type=="Portal"]))
less_sd_100C = c(sd(card_100C$Songs.Less.24.Resp[card_100C$Type=="Cactus.Wren"]),
               sd(card_100C$Songs.Less.24.Resp[card_100C$Type=="Texas"]),
               sd(card_100C$Songs.Less.24.Resp[card_100C$Type=="Bill.Williams"]),
               sd(card_100C$Songs.Less.24.Resp[card_100C$Type=="Portal"]))
songs = c("Cactus.Wren","New.Mexico","Bill.Williams","Portal")
less_plot_100C = barplot(less_mean_100C,col=c("grey15","blue","green","purple"),
                       ylim=c(0,15),ylab="Songs.Less.24s",xlab="Song Locality",names=songs,
                       main="100C")
error.bar(less_plot_100C,less_mean_100C,less_sd_100C/sqrt(length(card_100C$Type)/4))


## 150
#####
## 150

less_mean_150A = c(mean(card_150A$Songs.Less.24.Resp[card_150A$Type=="Cactus.Wren"]),
                 mean(card_150A$Songs.Less.24.Resp[card_150A$Type=="Texas"]),
                 mean(card_150A$Songs.Less.24.Resp[card_150A$Type=="Bill.Williams"]),
                 mean(card_150A$Songs.Less.24.Resp[card_150A$Type=="Portal"]))
less_sd_150A = c(sd(card_150A$Songs.Less.24.Resp[card_150A$Type=="Cactus.Wren"]),
               sd(card_150A$Songs.Less.24.Resp[card_150A$Type=="Texas"]),
               sd(card_150A$Songs.Less.24.Resp[card_150A$Type=="Bill.Williams"]),
               sd(card_150A$Songs.Less.24.Resp[card_150A$Type=="Portal"]))
songs = c("Cactus.Wren","New.Mexico","Bill.Williams","Portal")
less_plot_150A = barplot(less_mean_150A,col=c("grey15","blue","green","purple"),
                       ylim=c(0,15),ylab="Songs.Less.24s",xlab="Song Locality",names=songs,
                       main="150A")
error.bar(less_plot_150A,less_mean_150A,less_sd_150A/sqrt(length(card_150A$Type)/4))

less_mean_150B = c(mean(card_150B$Songs.Less.24.Resp[card_150B$Type=="Cactus.Wren"]),
                 mean(card_150B$Songs.Less.24.Resp[card_150B$Type=="Texas"]),
                 mean(card_150B$Songs.Less.24.Resp[card_150B$Type=="Bill.Williams"]),
                 mean(card_150B$Songs.Less.24.Resp[card_150B$Type=="Portal"]))
less_sd_150B = c(sd(card_150B$Songs.Less.24.Resp[card_150B$Type=="Cactus.Wren"]),
               sd(card_150B$Songs.Less.24.Resp[card_150B$Type=="Texas"]),
               sd(card_150B$Songs.Less.24.Resp[card_150B$Type=="Bill.Williams"]),
               sd(card_150B$Songs.Less.24.Resp[card_150B$Type=="Portal"]))
songs = c("Cactus.Wren","New.Mexico","Bill.Williams","Portal")
less_plot_150B = barplot(less_mean_150B,col=c("grey15","blue","green","purple"),
                       ylim=c(0,15),ylab="Songs.Less.24s",xlab="Song Locality",names=songs,
                       main="150B")
error.bar(less_plot_150B,less_mean_150B,less_sd_150B/sqrt(length(card_150B$Type)/4))

## 200
#####
## 200

less_mean_200A = c(mean(card_200A$Songs.Less.24.Resp[card_200A$Type=="Cactus.Wren"]),
                 mean(card_200A$Songs.Less.24.Resp[card_200A$Type=="Texas"]),
                 mean(card_200A$Songs.Less.24.Resp[card_200A$Type=="Bill.Williams"]),
                 mean(card_200A$Songs.Less.24.Resp[card_200A$Type=="Portal"]))
less_sd_200A = c(sd(card_200A$Songs.Less.24.Resp[card_200A$Type=="Cactus.Wren"]),
               sd(card_200A$Songs.Less.24.Resp[card_200A$Type=="Texas"]),
               sd(card_200A$Songs.Less.24.Resp[card_200A$Type=="Bill.Williams"]),
               sd(card_200A$Songs.Less.24.Resp[card_200A$Type=="Portal"]))
songs = c("Cactus.Wren","New.Mexico","Bill.Williams","Portal")
less_plot_200A = barplot(less_mean_200A,col=c("grey15","blue","green","purple"),
                       ylim=c(0,15),ylab="Songs.Less.24s",xlab="Song Locality",names=songs,
                       main="200A")
error.bar(less_plot_200A,less_mean_200A,less_sd_200A/sqrt(length(card_200A$Type)/4))

less_mean_200B = c(mean(card_200B$Songs.Less.24.Resp[card_200B$Type=="Cactus.Wren"]),
                 mean(card_200B$Songs.Less.24.Resp[card_200B$Type=="Texas"]),
                 mean(card_200B$Songs.Less.24.Resp[card_200B$Type=="Bill.Williams"]),
                 mean(card_200B$Songs.Less.24.Resp[card_200B$Type=="Portal"]))
less_sd_200B = c(sd(card_200B$Songs.Less.24.Resp[card_200B$Type=="Cactus.Wren"]),
               sd(card_200B$Songs.Less.24.Resp[card_200B$Type=="Texas"]),
               sd(card_200B$Songs.Less.24.Resp[card_200B$Type=="Bill.Williams"]),
               sd(card_200B$Songs.Less.24.Resp[card_200B$Type=="Portal"]))
songs = c("Cactus.Wren","New.Mexico","Bill.Williams","Portal")
less_plot_200B = barplot(less_mean_200B,col=c("grey15","blue","green","purple"),
                       ylim=c(0,15),ylab="Songs.Less.24s",xlab="Song Locality",names=songs,
                       main="200B")
error.bar(less_plot_200B,less_mean_200B,less_sd_200B/sqrt(length(card_200B$Type)/4))

## P VALUES
#####
## P VALUES

## 100
#####
## 100

t.test(card_100A$Songs.Less.24.Resp[card_100A$Type=="Bill.Williams"],card_100A$Songs.Less.24.Resp[card_100A$Type=="Cactus.Wren"],paired=TRUE)$p.value
#p-value = 0.8435609

t.test(card_100A$Songs.Less.24.Resp[card_100A$Type=="Bill.Williams"],card_100A$Songs.Less.24.Resp[card_100A$Type=="Texas"],paired=TRUE)$p.value
# p-value = 0.3549008

t.test(card_100A$Songs.Less.24.Resp[card_100A$Type=="Bill.Williams"],card_100A$Songs.Less.24.Resp[card_100A$Type=="Portal"],paired=TRUE)$p.value
#p-value = 0.0004554319

t.test(card_100A$Songs.Less.24.Resp[card_100A$Type=="Cactus.Wren"],card_100A$Songs.Less.24.Resp[card_100A$Type=="Texas"],paired=TRUE)$p.value
# p-value = 0.1625444

t.test(card_100A$Songs.Less.24.Resp[card_100A$Type=="Cactus.Wren"],card_100A$Songs.Less.24.Resp[card_100A$Type=="Portal"],paired=TRUE)$p.value
#  p-value = 6.933654e-05

t.test(card_100A$Songs.Less.24.Resp[card_100A$Type=="Texas"],card_100A$Songs.Less.24.Resp[card_100A$Type=="Portal"],paired=TRUE)$p.value
#p-value = 0.001676701

## 150
#####
## 150
t.test(card_150A$Songs.Less.24.Resp[card_150A$Type=="Bill.Williams"],card_150A$Songs.Less.24.Resp[card_150A$Type=="Cactus.Wren"],paired=TRUE)$p.value
#p-value 0.7916545

t.test(card_150A$Songs.Less.24.Resp[card_150A$Type=="Bill.Williams"],card_150A$Songs.Less.24.Resp[card_150A$Type=="Texas"],paired=TRUE)$p.value
# p-value 0.7550447

t.test(card_150A$Songs.Less.24.Resp[card_150A$Type=="Bill.Williams"],card_150A$Songs.Less.24.Resp[card_150A$Type=="Portal"],paired=TRUE)$p.value
#p-value 0.001110279

t.test(card_150A$Songs.Less.24.Resp[card_150A$Type=="Cactus.Wren"],card_150A$Songs.Less.24.Resp[card_150A$Type=="Texas"],paired=TRUE)$p.value
# p-value 0.9451151

t.test(card_150A$Songs.Less.24.Resp[card_150A$Type=="Cactus.Wren"],card_150A$Songs.Less.24.Resp[card_150A$Type=="Portal"],paired=TRUE)$p.value
#  p-value 0.003401441

t.test(card_150A$Songs.Less.24.Resp[card_150A$Type=="Texas"],card_150A$Songs.Less.24.Resp[card_150A$Type=="Portal"],paired=TRUE)$p.value
#p-value 0.0006558847

## 200
#####
## 200

t.test(card_200A$Songs.Less.24.Resp[card_200A$Type=="Bill.Williams"],card_200A$Songs.Less.24.Resp[card_200A$Type=="Cactus.Wren"],paired=TRUE)$p.value
#p-value 0.2943572

t.test(card_200A$Songs.Less.24.Resp[card_200A$Type=="Bill.Williams"],card_200A$Songs.Less.24.Resp[card_200A$Type=="Texas"],paired=TRUE)$p.value
# p-value 0.04868656

t.test(card_200A$Songs.Less.24.Resp[card_200A$Type=="Bill.Williams"],card_200A$Songs.Less.24.Resp[card_200A$Type=="Portal"],paired=TRUE)$p.value
#p-value 0.0005329448

t.test(card_200A$Songs.Less.24.Resp[card_200A$Type=="Cactus.Wren"],card_200A$Songs.Less.24.Resp[card_200A$Type=="Texas"],paired=TRUE)$p.value
# p-value 0.5083037

t.test(card_200A$Songs.Less.24.Resp[card_200A$Type=="Cactus.Wren"],card_200A$Songs.Less.24.Resp[card_200A$Type=="Portal"],paired=TRUE)$p.value
#  p-value 0.008093849

t.test(card_200A$Songs.Less.24.Resp[card_200A$Type=="Texas"],card_200A$Songs.Less.24.Resp[card_200A$Type=="Portal"],paired=TRUE)$p.value
#p-value 0.006386099

## FLYBYS
#####
## FLYBYS

## PLOTS
#####
## PLOTS

## 100
#####
## 100

fb_mean_100A = c(mean(card_100A$Flyby.Resp[card_100A$Type=="Cactus.Wren"]),
                 mean(card_100A$Flyby.Resp[card_100A$Type=="Texas"]),
                 mean(card_100A$Flyby.Resp[card_100A$Type=="Bill.Williams"]),
                 mean(card_100A$Flyby.Resp[card_100A$Type=="Portal"]))
fb_sd_100A = c(sd(card_100A$Flyby.Resp[card_100A$Type=="Cactus.Wren"]),
               sd(card_100A$Flyby.Resp[card_100A$Type=="Texas"]),
               sd(card_100A$Flyby.Resp[card_100A$Type=="Bill.Williams"]),
               sd(card_100A$Flyby.Resp[card_100A$Type=="Portal"]))
songs = c("Cactus.Wren","New.Mexico","Bill.Williams","Portal")
fb_plot_100A = barplot(fb_mean_100A,col=c("grey15","blue","green","purple"),
                       ylim=c(0,7),ylab="Flybys",xlab="Song Locality",names=songs,
                       main="100A")
error.bar(fb_plot_100A,fb_mean_100A,fb_sd_100A/sqrt(length(card_100A$Type)/4))

fb_mean_100B = c(mean(card_100B$Flyby.Resp[card_100B$Type=="Cactus.Wren"]),
                 mean(card_100B$Flyby.Resp[card_100B$Type=="Texas"]),
                 mean(card_100B$Flyby.Resp[card_100B$Type=="Bill.Williams"]),
                 mean(card_100B$Flyby.Resp[card_100B$Type=="Portal"]))
fb_sd_100B = c(sd(card_100B$Flyby.Resp[card_100B$Type=="Cactus.Wren"]),
               sd(card_100B$Flyby.Resp[card_100B$Type=="Texas"]),
               sd(card_100B$Flyby.Resp[card_100B$Type=="Bill.Williams"]),
               sd(card_100B$Flyby.Resp[card_100B$Type=="Portal"]))
songs = c("Cactus.Wren","New.Mexico","Bill.Williams","Portal")
fb_plot_100B = barplot(fb_mean_100B,col=c("grey15","blue","green","purple"),
                       ylim=c(0,8),ylab="Flybys",xlab="Song Locality",names=songs,
                       main="100B")
error.bar(fb_plot_100B,fb_mean_100B,fb_sd_100B/sqrt(length(card_100B$Type)/4))

fb_mean_100C = c(mean(card_100C$Flyby.Resp[card_100C$Type=="Cactus.Wren"]),
                 mean(card_100C$Flyby.Resp[card_100C$Type=="Texas"]),
                 mean(card_100C$Flyby.Resp[card_100C$Type=="Bill.Williams"]),
                 mean(card_100C$Flyby.Resp[card_100C$Type=="Portal"]))
fb_sd_100C = c(sd(card_100C$Flyby.Resp[card_100C$Type=="Cactus.Wren"]),
               sd(card_100C$Flyby.Resp[card_100C$Type=="Texas"]),
               sd(card_100C$Flyby.Resp[card_100C$Type=="Bill.Williams"]),
               sd(card_100C$Flyby.Resp[card_100C$Type=="Portal"]))
songs = c("Cactus.Wren","New.Mexico","Bill.Williams","Portal")
fb_plot_100C = barplot(fb_mean_100C,col=c("grey15","blue","green","purple"),
                       ylim=c(0,8),ylab="Flybys",xlab="Song Locality",names=songs,
                       main="100C")
error.bar(fb_plot_100C,fb_mean_100C,fb_sd_100C/sqrt(length(card_100C$Type)/4))


## 150
#####
## 150

fb_mean_150A = c(mean(card_150A$Flyby.Resp[card_150A$Type=="Cactus.Wren"]),
                 mean(card_150A$Flyby.Resp[card_150A$Type=="Texas"]),
                 mean(card_150A$Flyby.Resp[card_150A$Type=="Bill.Williams"]),
                 mean(card_150A$Flyby.Resp[card_150A$Type=="Portal"]))
fb_sd_150A = c(sd(card_150A$Flyby.Resp[card_150A$Type=="Cactus.Wren"]),
               sd(card_150A$Flyby.Resp[card_150A$Type=="Texas"]),
               sd(card_150A$Flyby.Resp[card_150A$Type=="Bill.Williams"]),
               sd(card_150A$Flyby.Resp[card_150A$Type=="Portal"]))
songs = c("Cactus.Wren","New.Mexico","Bill.Williams","Portal")
fb_plot_150A = barplot(fb_mean_150A,col=c("grey15","blue","green","purple"),
                       ylim=c(0,8),ylab="Flybys",xlab="Song Locality",names=songs,
                       main="150A")
error.bar(fb_plot_150A,fb_mean_150A,fb_sd_150A/sqrt(length(card_150A$Type)/4))

fb_mean_150B = c(mean(card_150B$Flyby.Resp[card_150B$Type=="Cactus.Wren"]),
                 mean(card_150B$Flyby.Resp[card_150B$Type=="Texas"]),
                 mean(card_150B$Flyby.Resp[card_150B$Type=="Bill.Williams"]),
                 mean(card_150B$Flyby.Resp[card_150B$Type=="Portal"]))
fb_sd_150B = c(sd(card_150B$Flyby.Resp[card_150B$Type=="Cactus.Wren"]),
               sd(card_150B$Flyby.Resp[card_150B$Type=="Texas"]),
               sd(card_150B$Flyby.Resp[card_150B$Type=="Bill.Williams"]),
               sd(card_150B$Flyby.Resp[card_150B$Type=="Portal"]))
songs = c("Cactus.Wren","New.Mexico","Bill.Williams","Portal")
fb_plot_150B = barplot(fb_mean_150B,col=c("grey15","blue","green","purple"),
                       ylim=c(0,8),ylab="Flybys",xlab="Song Locality",names=songs,
                       main="150B")
error.bar(fb_plot_150B,fb_mean_150B,fb_sd_150B/sqrt(length(card_150B$Type)/4))

## 200
#####
## 200

fb_mean_200A = c(mean(card_200A$Flyby.Resp[card_200A$Type=="Cactus.Wren"]),
                 mean(card_200A$Flyby.Resp[card_200A$Type=="Texas"]),
                 mean(card_200A$Flyby.Resp[card_200A$Type=="Bill.Williams"]),
                 mean(card_200A$Flyby.Resp[card_200A$Type=="Portal"]))
fb_sd_200A = c(sd(card_200A$Flyby.Resp[card_200A$Type=="Cactus.Wren"]),
               sd(card_200A$Flyby.Resp[card_200A$Type=="Texas"]),
               sd(card_200A$Flyby.Resp[card_200A$Type=="Bill.Williams"]),
               sd(card_200A$Flyby.Resp[card_200A$Type=="Portal"]))
songs = c("Cactus.Wren","New.Mexico","Bill.Williams","Portal")
fb_plot_200A = barplot(fb_mean_200A,col=c("grey15","blue","green","purple"),
                       ylim=c(0,7),ylab="Flybys",xlab="Song Locality",names=songs,
                       main="200A")
error.bar(fb_plot_200A,fb_mean_200A,fb_sd_200A/sqrt(length(card_200A$Type)/4))

fb_mean_200B = c(mean(card_200B$Flyby.Resp[card_200B$Type=="Cactus.Wren"]),
                 mean(card_200B$Flyby.Resp[card_200B$Type=="Texas"]),
                 mean(card_200B$Flyby.Resp[card_200B$Type=="Bill.Williams"]),
                 mean(card_200B$Flyby.Resp[card_200B$Type=="Portal"]))
fb_sd_200B = c(sd(card_200B$Flyby.Resp[card_200B$Type=="Cactus.Wren"]),
               sd(card_200B$Flyby.Resp[card_200B$Type=="Texas"]),
               sd(card_200B$Flyby.Resp[card_200B$Type=="Bill.Williams"]),
               sd(card_200B$Flyby.Resp[card_200B$Type=="Portal"]))
songs = c("Cactus.Wren","New.Mexico","Bill.Williams","Portal")
fb_plot_200B = barplot(fb_mean_200B,col=c("grey15","blue","green","purple"),
                       ylim=c(0,8),ylab="Flybys",xlab="Song Locality",names=songs,
                       main="200B")
error.bar(fb_plot_200B,fb_mean_200B,fb_sd_200B/sqrt(length(card_200B$Type)/4))

## P VALUES
#####
## P VALUES

## 100
#####
## 100

t.test(card_100A$Flyby.Resp[card_100A$Type=="Bill.Williams"],card_100A$Flyby.Resp[card_100A$Type=="Cactus.Wren"],paired=TRUE)$p.value
#p-value = 0.0131426

t.test(card_100A$Flyby.Resp[card_100A$Type=="Bill.Williams"],card_100A$Flyby.Resp[card_100A$Type=="Texas"],paired=TRUE)$p.value
# p-value = 0.3675552

t.test(card_100A$Flyby.Resp[card_100A$Type=="Bill.Williams"],card_100A$Flyby.Resp[card_100A$Type=="Portal"],paired=TRUE)$p.value
#p-value = 9.264653e-06

t.test(card_100A$Flyby.Resp[card_100A$Type=="Cactus.Wren"],card_100A$Flyby.Resp[card_100A$Type=="Texas"],paired=TRUE)$p.value
# p-value = 0.09133883

t.test(card_100A$Flyby.Resp[card_100A$Type=="Cactus.Wren"],card_100A$Flyby.Resp[card_100A$Type=="Portal"],paired=TRUE)$p.value
#  p-value = 2.299083e-07

t.test(card_100A$Flyby.Resp[card_100A$Type=="Texas"],card_100A$Flyby.Resp[card_100A$Type=="Portal"],paired=TRUE)$p.value
#p-value = 2.430298e-06

## 150
#####
## 150
t.test(card_150A$Flyby.Resp[card_150A$Type=="Bill.Williams"],card_150A$Flyby.Resp[card_150A$Type=="Cactus.Wren"],paired=TRUE)$p.value
#p-value 0.2113142

t.test(card_150A$Flyby.Resp[card_150A$Type=="Bill.Williams"],card_150A$Flyby.Resp[card_150A$Type=="Texas"],paired=TRUE)$p.value
# p-value 0.05743856

t.test(card_150A$Flyby.Resp[card_150A$Type=="Bill.Williams"],card_150A$Flyby.Resp[card_150A$Type=="Portal"],paired=TRUE)$p.value
#p-value 1.040319e-05

t.test(card_150A$Flyby.Resp[card_150A$Type=="Cactus.Wren"],card_150A$Flyby.Resp[card_150A$Type=="Texas"],paired=TRUE)$p.value
# p-value 0.9051961

t.test(card_150A$Flyby.Resp[card_150A$Type=="Cactus.Wren"],card_150A$Flyby.Resp[card_150A$Type=="Portal"],paired=TRUE)$p.value
#  p-value 8.541745e-06

t.test(card_150A$Flyby.Resp[card_150A$Type=="Texas"],card_150A$Flyby.Resp[card_150A$Type=="Portal"],paired=TRUE)$p.value
#p-value 2.057216e-06

## 200
#####
## 200

t.test(card_200A$Flyby.Resp[card_200A$Type=="Bill.Williams"],card_200A$Flyby.Resp[card_200A$Type=="Cactus.Wren"],paired=TRUE)$p.value
#p-value 0.4169904

t.test(card_200A$Flyby.Resp[card_200A$Type=="Bill.Williams"],card_200A$Flyby.Resp[card_200A$Type=="Texas"],paired=TRUE)$p.value
# p-value 1

t.test(card_200A$Flyby.Resp[card_200A$Type=="Bill.Williams"],card_200A$Flyby.Resp[card_200A$Type=="Portal"],paired=TRUE)$p.value
#p-value 0.0004878235

t.test(card_200A$Flyby.Resp[card_200A$Type=="Cactus.Wren"],card_200A$Flyby.Resp[card_200A$Type=="Texas"],paired=TRUE)$p.value
# p-value 0.4169904

t.test(card_200A$Flyby.Resp[card_200A$Type=="Cactus.Wren"],card_200A$Flyby.Resp[card_200A$Type=="Portal"],paired=TRUE)$p.value
#  p-value 0.0003276955

t.test(card_200A$Flyby.Resp[card_200A$Type=="Texas"],card_200A$Flyby.Resp[card_200A$Type=="Portal"],paired=TRUE)$p.value
#p-value 0.0002663934

## chips
#####
## chipss

## PLOTS
#####
## PLOTS

## 100
#####
## 100

chips_mean_100A = c(mean(card_100A$Chips.Pres.Abs.Resp[card_100A$Type=="Cactus.Wren"]),
                 mean(card_100A$Chips.Pres.Abs.Resp[card_100A$Type=="Texas"]),
                 mean(card_100A$Chips.Pres.Abs.Resp[card_100A$Type=="Bill.Williams"]),
                 mean(card_100A$Chips.Pres.Abs.Resp[card_100A$Type=="Portal"]))
chips_sd_100A = c(sd(card_100A$Chips.Pres.Abs.Resp[card_100A$Type=="Cactus.Wren"]),
               sd(card_100A$Chips.Pres.Abs.Resp[card_100A$Type=="Texas"]),
               sd(card_100A$Chips.Pres.Abs.Resp[card_100A$Type=="Bill.Williams"]),
               sd(card_100A$Chips.Pres.Abs.Resp[card_100A$Type=="Portal"]))
songs = c("Cactus.Wren","New.Mexico","Bill.Williams","Portal")
chips_plot_100A = barplot(chips_mean_100A,col=c("grey15","blue","green","purple"),
                       ylim=c(0,1),ylab="chipss",xlab="Song Locality",names=songs,
                       main="100A")
error.bar(chips_plot_100A,chips_mean_100A,chips_sd_100A/sqrt(length(card_100A$Type)/4))

chips_mean_100B = c(mean(card_100B$Chips.Pres.Abs.Resp[card_100B$Type=="Cactus.Wren"]),
                 mean(card_100B$Chips.Pres.Abs.Resp[card_100B$Type=="Texas"]),
                 mean(card_100B$Chips.Pres.Abs.Resp[card_100B$Type=="Bill.Williams"]),
                 mean(card_100B$Chips.Pres.Abs.Resp[card_100B$Type=="Portal"]))
chips_sd_100B = c(sd(card_100B$Chips.Pres.Abs.Resp[card_100B$Type=="Cactus.Wren"]),
               sd(card_100B$Chips.Pres.Abs.Resp[card_100B$Type=="Texas"]),
               sd(card_100B$Chips.Pres.Abs.Resp[card_100B$Type=="Bill.Williams"]),
               sd(card_100B$Chips.Pres.Abs.Resp[card_100B$Type=="Portal"]))
songs = c("Cactus.Wren","New.Mexico","Bill.Williams","Portal")
chips_plot_100B = barplot(chips_mean_100B,col=c("grey15","blue","green","purple"),
                       ylim=c(0,1),ylab="chipss",xlab="Song Locality",names=songs,
                       main="100B")
error.bar(chips_plot_100B,chips_mean_100B,chips_sd_100B/sqrt(length(card_100B$Type)/4))

chips_mean_100C = c(mean(card_100C$Chips.Pres.Abs.Resp[card_100C$Type=="Cactus.Wren"]),
                 mean(card_100C$Chips.Pres.Abs.Resp[card_100C$Type=="Texas"]),
                 mean(card_100C$Chips.Pres.Abs.Resp[card_100C$Type=="Bill.Williams"]),
                 mean(card_100C$Chips.Pres.Abs.Resp[card_100C$Type=="Portal"]))
chips_sd_100C = c(sd(card_100C$Chips.Pres.Abs.Resp[card_100C$Type=="Cactus.Wren"]),
               sd(card_100C$Chips.Pres.Abs.Resp[card_100C$Type=="Texas"]),
               sd(card_100C$Chips.Pres.Abs.Resp[card_100C$Type=="Bill.Williams"]),
               sd(card_100C$Chips.Pres.Abs.Resp[card_100C$Type=="Portal"]))
songs = c("Cactus.Wren","New.Mexico","Bill.Williams","Portal")
chips_plot_100C = barplot(chips_mean_100C,col=c("grey15","blue","green","purple"),
                       ylim=c(0,1),ylab="chipss",xlab="Song Locality",names=songs,
                       main="100C")
error.bar(chips_plot_100C,chips_mean_100C,chips_sd_100C/sqrt(length(card_100C$Type)/4))


## 150
#####
## 150

chips_mean_150A = c(mean(card_150A$Chips.Pres.Abs.Resp[card_150A$Type=="Cactus.Wren"]),
                 mean(card_150A$Chips.Pres.Abs.Resp[card_150A$Type=="Texas"]),
                 mean(card_150A$Chips.Pres.Abs.Resp[card_150A$Type=="Bill.Williams"]),
                 mean(card_150A$Chips.Pres.Abs.Resp[card_150A$Type=="Portal"]))
chips_sd_150A = c(sd(card_150A$Chips.Pres.Abs.Resp[card_150A$Type=="Cactus.Wren"]),
               sd(card_150A$Chips.Pres.Abs.Resp[card_150A$Type=="Texas"]),
               sd(card_150A$Chips.Pres.Abs.Resp[card_150A$Type=="Bill.Williams"]),
               sd(card_150A$Chips.Pres.Abs.Resp[card_150A$Type=="Portal"]))
songs = c("Cactus.Wren","New.Mexico","Bill.Williams","Portal")
chips_plot_150A = barplot(chips_mean_150A,col=c("grey15","blue","green","purple"),
                       ylim=c(0,1),ylab="chipss",xlab="Song Locality",names=songs,
                       main="150A")
error.bar(chips_plot_150A,chips_mean_150A,chips_sd_150A/sqrt(length(card_150A$Type)/4))

chips_mean_150B = c(mean(card_150B$Chips.Pres.Abs.Resp[card_150B$Type=="Cactus.Wren"]),
                 mean(card_150B$Chips.Pres.Abs.Resp[card_150B$Type=="Texas"]),
                 mean(card_150B$Chips.Pres.Abs.Resp[card_150B$Type=="Bill.Williams"]),
                 mean(card_150B$Chips.Pres.Abs.Resp[card_150B$Type=="Portal"]))
chips_sd_150B = c(sd(card_150B$Chips.Pres.Abs.Resp[card_150B$Type=="Cactus.Wren"]),
               sd(card_150B$Chips.Pres.Abs.Resp[card_150B$Type=="Texas"]),
               sd(card_150B$Chips.Pres.Abs.Resp[card_150B$Type=="Bill.Williams"]),
               sd(card_150B$Chips.Pres.Abs.Resp[card_150B$Type=="Portal"]))
songs = c("Cactus.Wren","New.Mexico","Bill.Williams","Portal")
chips_plot_150B = barplot(chips_mean_150B,col=c("grey15","blue","green","purple"),
                       ylim=c(0,1),ylab="chipss",xlab="Song Locality",names=songs,
                       main="150B")
error.bar(chips_plot_150B,chips_mean_150B,chips_sd_150B/sqrt(length(card_150B$Type)/4))

## 200
#####
## 200

chips_mean_200A = c(mean(card_200A$Chips.Pres.Abs.Resp[card_200A$Type=="Cactus.Wren"]),
                 mean(card_200A$Chips.Pres.Abs.Resp[card_200A$Type=="Texas"]),
                 mean(card_200A$Chips.Pres.Abs.Resp[card_200A$Type=="Bill.Williams"]),
                 mean(card_200A$Chips.Pres.Abs.Resp[card_200A$Type=="Portal"]))
chips_sd_200A = c(sd(card_200A$Chips.Pres.Abs.Resp[card_200A$Type=="Cactus.Wren"]),
               sd(card_200A$Chips.Pres.Abs.Resp[card_200A$Type=="Texas"]),
               sd(card_200A$Chips.Pres.Abs.Resp[card_200A$Type=="Bill.Williams"]),
               sd(card_200A$Chips.Pres.Abs.Resp[card_200A$Type=="Portal"]))
songs = c("Cactus.Wren","New.Mexico","Bill.Williams","Portal")
chips_plot_200A = barplot(chips_mean_200A,col=c("grey15","blue","green","purple"),
                       ylim=c(0,1),ylab="chipss",xlab="Song Locality",names=songs,
                       main="200A")
error.bar(chips_plot_200A,chips_mean_200A,chips_sd_200A/sqrt(length(card_200A$Type)/4))

chips_mean_200B = c(mean(card_200B$Chips.Pres.Abs.Resp[card_200B$Type=="Cactus.Wren"]),
                 mean(card_200B$Chips.Pres.Abs.Resp[card_200B$Type=="Texas"]),
                 mean(card_200B$Chips.Pres.Abs.Resp[card_200B$Type=="Bill.Williams"]),
                 mean(card_200B$Chips.Pres.Abs.Resp[card_200B$Type=="Portal"]))
chips_sd_200B = c(sd(card_200B$Chips.Pres.Abs.Resp[card_200B$Type=="Cactus.Wren"]),
               sd(card_200B$Chips.Pres.Abs.Resp[card_200B$Type=="Texas"]),
               sd(card_200B$Chips.Pres.Abs.Resp[card_200B$Type=="Bill.Williams"]),
               sd(card_200B$Chips.Pres.Abs.Resp[card_200B$Type=="Portal"]))
songs = c("Cactus.Wren","New.Mexico","Bill.Williams","Portal")
chips_plot_200B = barplot(chips_mean_200B,col=c("grey15","blue","green","purple"),
                       ylim=c(0,1),ylab="chipss",xlab="Song Locality",names=songs,
                       main="200B")
error.bar(chips_plot_200B,chips_mean_200B,chips_sd_200B/sqrt(length(card_200B$Type)/4))

## P VALUES
#####
## P VALUES

## 100
#####
## 100

t.test(card_100A$Chips.Pres.Abs.Resp[card_100A$Type=="Bill.Williams"],card_100A$Chips.Pres.Abs.Resp[card_100A$Type=="Cactus.Wren"],paired=TRUE)$p.value
#p-value = 0.4111044

t.test(card_100A$Chips.Pres.Abs.Resp[card_100A$Type=="Bill.Williams"],card_100A$Chips.Pres.Abs.Resp[card_100A$Type=="Texas"],paired=TRUE)$p.value
# p-value = 0.7847766

t.test(card_100A$Chips.Pres.Abs.Resp[card_100A$Type=="Bill.Williams"],card_100A$Chips.Pres.Abs.Resp[card_100A$Type=="Portal"],paired=TRUE)$p.value
#p-value = 3.646406e-05

t.test(card_100A$Chips.Pres.Abs.Resp[card_100A$Type=="Cactus.Wren"],card_100A$Chips.Pres.Abs.Resp[card_100A$Type=="Texas"],paired=TRUE)$p.value
# p-value = 0.2897839

t.test(card_100A$Chips.Pres.Abs.Resp[card_100A$Type=="Cactus.Wren"],card_100A$Chips.Pres.Abs.Resp[card_100A$Type=="Portal"],paired=TRUE)$p.value
#  p-value = 1.112183e-06

t.test(card_100A$Chips.Pres.Abs.Resp[card_100A$Type=="Texas"],card_100A$Chips.Pres.Abs.Resp[card_100A$Type=="Portal"],paired=TRUE)$p.value
#p-value = 0.0001717176

## 150
#####
## 150
t.test(card_150A$Chips.Pres.Abs.Resp[card_150A$Type=="Bill.Williams"],card_150A$Chips.Pres.Abs.Resp[card_150A$Type=="Cactus.Wren"],paired=TRUE)$p.value
#p-value 0.4888356

t.test(card_150A$Chips.Pres.Abs.Resp[card_150A$Type=="Bill.Williams"],card_150A$Chips.Pres.Abs.Resp[card_150A$Type=="Texas"],paired=TRUE)$p.value
# p-value 0.6623618

t.test(card_150A$Chips.Pres.Abs.Resp[card_150A$Type=="Bill.Williams"],card_150A$Chips.Pres.Abs.Resp[card_150A$Type=="Portal"],paired=TRUE)$p.value
#p-value 0.000539191

t.test(card_150A$Chips.Pres.Abs.Resp[card_150A$Type=="Cactus.Wren"],card_150A$Chips.Pres.Abs.Resp[card_150A$Type=="Texas"],paired=TRUE)$p.value
# p-value 0.7122294

t.test(card_150A$Chips.Pres.Abs.Resp[card_150A$Type=="Cactus.Wren"],card_150A$Chips.Pres.Abs.Resp[card_150A$Type=="Portal"],paired=TRUE)$p.value
#  p-value 2.286408e-05

t.test(card_150A$Chips.Pres.Abs.Resp[card_150A$Type=="Texas"],card_150A$Chips.Pres.Abs.Resp[card_150A$Type=="Portal"],paired=TRUE)$p.value
#p-value 0.0002474051

## 200
#####
## 200

t.test(card_200A$Chips.Pres.Abs.Resp[card_200A$Type=="Bill.Williams"],card_200A$Chips.Pres.Abs.Resp[card_200A$Type=="Cactus.Wren"],paired=TRUE)$p.value
#p-value 1

t.test(card_200A$Chips.Pres.Abs.Resp[card_200A$Type=="Bill.Williams"],card_200A$Chips.Pres.Abs.Resp[card_200A$Type=="Texas"],paired=TRUE)$p.value
# p-value 0.664306

t.test(card_200A$Chips.Pres.Abs.Resp[card_200A$Type=="Bill.Williams"],card_200A$Chips.Pres.Abs.Resp[card_200A$Type=="Portal"],paired=TRUE)$p.value
#p-value 0.0008848959

t.test(card_200A$Chips.Pres.Abs.Resp[card_200A$Type=="Cactus.Wren"],card_200A$Chips.Pres.Abs.Resp[card_200A$Type=="Texas"],paired=TRUE)$p.value
# p-value 0.7465708

t.test(card_200A$Chips.Pres.Abs.Resp[card_200A$Type=="Cactus.Wren"],card_200A$Chips.Pres.Abs.Resp[card_200A$Type=="Portal"],paired=TRUE)$p.value
#  p-value 0.0008848959

t.test(card_200A$Chips.Pres.Abs.Resp[card_200A$Type=="Texas"],card_200A$Chips.Pres.Abs.Resp[card_200A$Type=="Portal"],paired=TRUE)$p.value
#p-value 0.001938806