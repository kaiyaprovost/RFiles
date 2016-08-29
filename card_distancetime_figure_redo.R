card_sec_as_num <- read.csv("C:/Users/Kaiya/Dropbox/Docs for Brian/THESIS/Field Work/Cardinal Data Analysis/Time Scaled Data/card_minbymin_18aug2015.csv")

sterr <- function(x) sd(x)/sqrt(length(x))

## loop of means
N = 54
mean.matrix = as.data.frame(matrix(0,ncol=N,nrow=4))
for (i in 1:N) {
  x = c(mean(card_sec_as_num[card_sec_as_num$Type=="Bill.Williams",(i+7)]),
        mean(card_sec_as_num[card_sec_as_num$Type=="Cactus.Wren",(i+7)]),
        mean(card_sec_as_num[card_sec_as_num$Type=="Texas",(i+7)]),
        mean(card_sec_as_num[card_sec_as_num$Type=="Portal",(i+7)]))  
  mean.matrix[,i] = as.numeric(x)
  colnames(mean.matrix)=colnames(card_sec_as_num[(8):(N+7)])
}
mean.matrix = cbind(Test = c("BW","CW","TX","PT"),mean.matrix)

bw.mean = as.numeric((mean.matrix[mean.matrix$Test=="BW",-1])) ## gives values for BW
cw.mean = as.numeric((mean.matrix[mean.matrix$Test=="CW",-1])) ## gives values for CW
tx.mean = as.numeric((mean.matrix[mean.matrix$Test=="TX",-1])) ## gives values for TX
pt.mean = as.numeric((mean.matrix[mean.matrix$Test=="PT",-1])) ## gives values for PT

## loop of standard errors
N = 54
sterr.matrix = as.data.frame(matrix(0,ncol=N,nrow=4))
for (i in 1:N) {
  z = c(sterr(card_sec_as_num[card_sec_as_num$Type=="Bill.Williams",(i+7)]),
        sterr(card_sec_as_num[card_sec_as_num$Type=="Cactus.Wren",(i+7)]),
        sterr(card_sec_as_num[card_sec_as_num$Type=="Texas",(i+7)]),
        sterr(card_sec_as_num[card_sec_as_num$Type=="Portal",(i+7)]))  
  sterr.matrix[,i] = as.numeric(z)
  colnames(sterr.matrix)=colnames(card_sec_as_num[(8):(N+7)])
}
sterr.matrix = cbind(Test = c("BW","CW","TX","PT"),sterr.matrix)

bw.sterr = as.numeric((sterr.matrix[sterr.matrix$Test=="BW",-1])) ## gives values for BW
cw.sterr = as.numeric((sterr.matrix[sterr.matrix$Test=="CW",-1])) ## gives values for CW
tx.sterr = as.numeric((sterr.matrix[sterr.matrix$Test=="TX",-1])) ## gives values for TX
pt.sterr = as.numeric((sterr.matrix[sterr.matrix$Test=="PT",-1])) ## gives values for PT

## average plot
seconds = seq(10,540,10)
epsilon=0.02
png(filename="mean_cardinalis_distance_plots_by_second_with_errors_9apr2016FIX.png",
    width=850,height=550,units="px",pointsize=16)
plot(seconds+1,tx.mean,col="blue",ylim=c(13,24),pch=15,
       xlab="Time (s)",ylab="Mean (± SE) Distance from speaker (m)",
       main="Mean ± SE Dist from Speaker vs Time")
for(i in 1:N) {
  epsilon = 0.02
  up = tx.mean[i] + tx.sterr[i]
  down = tx.mean[i] - tx.sterr[i]
  segments(seconds[i]+1,down,seconds[i]+1,up,col="blue")
}
points(seconds+2,bw.mean,col="green",ylim=c(13,24),pch=16)
for(i in 1:N) {
  epsilon = 0.02
  up = bw.mean[i] + bw.sterr[i]
  down = bw.mean[i] - bw.sterr[i]
  segments(seconds[i]+2,down,seconds[i]+2,up,col="green")
}
points(seconds,pt.mean,col="purple",pch=1,ylim=c(13,24))
for(i in 1:N) {
  epsilon = 0.02
  up = pt.mean[i] + pt.sterr[i]
  down = pt.mean[i] - pt.sterr[i]
  segments(seconds[i],down,seconds[i],up,col="purple")
}
points(seconds+3,cw.mean, ylim=c(13,24),pch=0)
for(i in 1:N) {
  epsilon = 0.02
  up = cw.mean[i] + cw.sterr[i]
  down = cw.mean[i] - cw.sterr[i]
  segments(seconds[i]+3,down,seconds[i]+3,up)
}
abline(v=187,col="gray")
abline(v=367,col="gray")
legend(10,18,c("Null","Across-Barrier","Distant","Local"),
       col=c("black","blue","green","purple"),
       pch=c(0,15,16,1))
dev.off()

## average plot ZOOMED IN
seconds = seq(10,540,10)
epsilon=0.02
png(filename="mean_cardinalis_distance_plots_by_second_with_errors_ZOOM_6may2016.png",
    width=850,height=550,units="px",pointsize=16)

plot(1, type="n",, xlab="Time (s)", 
     ylab="Mean (± SE) Distance from speaker (m)",ylim=c(20,24),xlim=c(190,540))
abline(v=187,col="gray",lwd=3)
abline(v=367,col="gray",lwd=3)
points(seconds+2,bw.mean,col="green",ylim=c(20,24),xlim=c(190,540),pch=16)
for(i in 1:N) {
  epsilon = 0.02
  up = bw.mean[i] + bw.sterr[i]
  down = bw.mean[i] - bw.sterr[i]
  segments(seconds[i]+2,down,seconds[i]+2,up,col="green")
}
points(seconds,tx.mean,col="blue",ylim=c(20,24),xlim=c(190,540),pch=15)
for(i in 1:N) {
  epsilon = 0.02
  up = tx.mean[i] + tx.sterr[i]
  down = tx.mean[i] - tx.sterr[i]
  segments(seconds[i],down,seconds[i],up,col="blue")
}
#points(seconds,pt.mean,col="purple",pch=1,ylim=c(20,24))
#for(i in 1:N) {
#  epsilon = 0.02
#  up = pt.mean[i] + pt.sterr[i]
#  down = pt.mean[i] - pt.sterr[i]
#  segments(seconds[i],down,seconds[i],up,col="purple")
#}
points(seconds+4,cw.mean, ylim=c(20,24),xlim=c(190,540),pch=0)
for(i in 1:N) {
  epsilon = 0.02
  up = cw.mean[i] + cw.sterr[i]
  down = cw.mean[i] - cw.sterr[i]
  segments(seconds[i]+4,down,seconds[i]+4,up)
}
legend(10,18,c("Null","Across-Barrier","Distant","Local"),
       col=c("black","blue","green","purple"),
       pch=c(0,15,16,1))
dev.off()