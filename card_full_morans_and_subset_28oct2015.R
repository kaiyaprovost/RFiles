onepoint_data <- 
  read.csv("C:/Users/Kaiya/Dropbox/Docs for Brian/Field Work/Cardinal Data Analysis/Master Files/CSVs/onepoint_csv_28Oct2015.csv")

## SPATIAL AUTOCORRELATION

## calculate moran's I

## make an inverse distance matrix
onepoint_dists <- as.matrix(dist(cbind(onepoint_data$X0.Longitude, onepoint_data$X0.Latitude)))

onepoint_dists_inv <- 1/onepoint_dists
diag(onepoint_dists_inv) <- 0

onepoint_dists_inv[1:5, 1:5]

## calculate for each variable
  ## first one, portal flyby resps
library(ape)
Moran.I(onepoint_data[,1], onepoint_dists_inv)

## iterate over each column
## moran's I does not like columns with all identical data, so they are removed

library(ape)
reduced = onepoint_data[,c(1:94,97,99:198,200:340)]
N = 1
len = length(reduced)
moran_vector=vector()
for (i in 1:len) {
  moran_vector[i] = as.numeric(Moran.I(as.numeric(reduced[,i]),onepoint_dists_inv)$p.value)
}
moran_vector=as.numeric(moran_vector)
names = colnames(reduced)
moran_matrix = as.data.frame(matrix(moran_vector))
colnames(moran_matrix) = "Moran's I"
rownames(moran_matrix) = names

len = length(reduced)
moran_strength = vector()
for (i in 1:len) {
  moran_strength[i] = as.numeric(Moran.I(as.numeric(reduced[,i]),onepoint_dists_inv)$observed)
}
moran_matrix = cbind(moran_matrix,moran_strength)
colnames(moran_matrix) = c("Morans.I.Pval","Observed")

moran_result = vector()
for(i in 1:len) {
  if(moran_matrix[i,1] > 0.05){
    moran_result[i] = "n.s."
  } else if (moran_matrix[i,2] > 0) { 
    moran_result[i] = "pos"
  } else if (moran_matrix[i,2] < 0) {
    moran_result[i] = "neg"
  } else if (moran_matrix[i,2] == 0) { 
    moran_result[i] = "zero"
  } else { 
    moran_result[i] = "error"
    warning("something went wrong")
  }
}
moran_matrix = cbind(moran_matrix,moran_result)
colnames(moran_matrix) = c("Morans.I.Pval","Observed","Result")

write.csv(moran_matrix,
          file="C:/Users/Kaiya/Dropbox/Docs for Brian/Field Work/Cardinal Data Analysis/moran_matrix_csv.csv")

### ### 

## actually plotting?

# packages used for the data generation
library(raster)
library(colorRamps) # for some crispy colors
library(vegan) # will be used for PCNM

# empty matrix and spatial coordinates of its cells
side=30
my.mat <- matrix(NA, nrow=side, ncol=side)
x.coord <- rep(1:side, each=side)
y.coord <- rep(1:side, times=side)
xy <- data.frame(x.coord, y.coord)

# all paiwise euclidean distances between the cells
xy.dist <- dist(xy)

# PCNM axes of the dist. matrix (from 'vegan' package)
pcnm.axes <- pcnm(xy.dist)$vectors

# using 8th PCNM axis as my atificial z variable
z.value <- pcnm.axes[,8]*200 + rnorm(side*side, 0, 1)

# plotting the artificial spatial data
my.mat[] <- z.value
r <- raster(my.mat)
plot(r, axes=F, col=matlab.like(20))

library(ncf)
ncf.cor <- correlog(x.coord, y.coord, z.value,
                    increment=2, resamp=500)

library(pgirmess)
pgi.cor <- correlog(coords=xy, z=z.value, method="Moran", nbclass=21)

library(spdep)
# 'nb' - neighbourhood of each cell
r.nb <- dnearneigh(as.matrix(xy), d1=0.5, d2=1.5)
# 'nb' - an alternative way to specify the neighbourhood
# r.nb <- cell2nb(nrow=side, ncol=side, type="queen")
sp.cor <- sp.correlogram(r.nb, z.value, order=15,
                         method="I", randomisation=FALSE)



### instead keep all 100 and cull the ones above the minimum? then do unique function? 

#####

## subset of points more than 100 m from each other

card_distance_pivot <- 
  read.csv("C:/Users/Kaiya/Dropbox/Docs for Brian/Field Work/Point and Dist Data/card_waypoints_all_utm_distance_pivot.csv")
rownames(card_distance_pivot) = card_distance_pivot[,1]
card_distance_pivot = card_distance_pivot[,-1]
colnames(card_distance_pivot) = rownames(card_distance_pivot)
#diag(card_distance_pivot) = 0


## START HERE
points.subset = function(data, N = 100, DropMode = "rand") {
  print("start")
  starttime = Sys.time()
  # "rand" "mode" "min"
  # Dropmode chooses whether you want to drop the points randomly,
  # by whichever has the highest mode and THEN randomly,
  # or by whichever has the lowest minimum
  card_dist_reduced = data
  not_pair = data.frame()
  not_pair_rev = data.frame()
  not_pair_both = data.frame()
  okay = c()
  okay_pair = data.frame()
  not = c()
  badlist = c()
  minlist = c()
  replist = c()
  while (min(card_dist_reduced,na.rm=TRUE)<N) {
    repeat {
      #print("repeat")
      not_pair = data.frame()
      not_pair_rev = data.frame()
      not_pair_both = data.frame()
      okay = c()
      okay_pair = data.frame()
      not = c()
      #j = c()
      #i = c()
      #k = c()
      #print("first for loop")
      for (i in 1:length(card_dist_reduced)) {
        for (j in 1:length(card_dist_reduced)) {
          if (as.numeric(rownames(card_dist_reduced[i,])) >= 
                as.numeric(colnames(card_dist_reduced[j]))) {
            okay = c(okay)
            not = c(not)
          }
          else if (card_dist_reduced[i,j] >= N) {
            okay = c(okay,paste(as.numeric(rownames(card_dist_reduced[i,]))
                                
                                ,",",as.numeric(colnames(card_dist_reduced[j])),sep=""))
            okay_pair = rbind(okay_pair,c(as.numeric(rownames(card_dist_reduced[i,])),
                                          as.numeric(colnames(card_dist_reduced[j]))))
          }
          else if (card_dist_reduced[i,j] < N) {
            not = c(not,paste(as.numeric(rownames(card_dist_reduced[i,]))
                              
                              ,",",as.numeric(colnames(card_dist_reduced[j])),sep=""))
            not_pair = rbind(not_pair,c(as.numeric(rownames(card_dist_reduced[i,])),
                                        as.numeric(colnames(card_dist_reduced[j]))))
          }
          else {
            print("ERROR")
          }
        }
      }
      okay_pair = unique(okay_pair)
      not_pair = unique(not_pair)
      if (length(not_pair) == 0) {
        break()
        print("no more pairs")
      }
      #print("finding the frequencies") ## finding the frequencies
      unname(not_pair)
      colnames(not_pair) =c("one","two")
      not_pair_rev = as.data.frame(cbind(not_pair[,2],not_pair[,1]))
      unname(not_pair_rev)
      colnames(not_pair_rev) =c("one","two")
      not_pair_both = rbind(not_pair,not_pair_rev)
      sorted = as.data.frame(sort(table(not_pair_both[1]),decreasing=TRUE))
      sorted = cbind(as.numeric(rownames(sorted)),sorted)
      sorted = as.data.frame(sorted)
      colnames(sorted) = c("Point","Freq")
      rownames(sorted) = seq(1:length(sorted[,1]))
      print(c("length sorted",length(sorted[,1])))
      #print("mode loop") ## running the loop to find the modes
      repeats = c()
      if ((as.numeric(sorted$Freq[1])) != (as.numeric(sorted$Freq[2]))) {
        repeats = 1
      } else {
        for (k in 1:length(sorted[,1])) {
          if ((as.numeric(sorted$Freq[1])) == (as.numeric(sorted$Freq[k])))  {
            repeats = k
          } else {
            repeats = repeats
          }
        }
      }
      print(c("repeats",repeats))
      replist = c(replist,repeats)
      
      if (DropMode == "mode") {
        ## removing the modes
        #print("mode removal")
        bad = as.numeric(sorted$Point[1:repeats])
        if (repeats != 1) {
          bad = sample(bad,1)
          print("rand")
        } else {
          bad = bad
        }
      } else if (DropMode == "rand") {
        bad = sample(as.numeric(sorted$Point),1)
      } else if (DropMode == "min") {
        mintable = data.frame()
        for (i in 1:length(sorted$Point)){
          minnum = which(names(card_dist_reduced)==sorted$Point[i])
          mintable[i,1] = names(card_dist_reduced[minnum])
          mintable[i,2] = min(card_dist_reduced[minnum],na.rm=TRUE)
        }
        colnames(mintable) = c("Point","Min")
        mintable = mintable[order(mintable$Min),]
        if (mintable$Min[1] == mintable$Min[2]) {
          bad = as.numeric(sample(c(mintable$Point[1],mintable$Point[2]),1))
        } else {
          bad = as.numeric(mintable$Point[1])
        }
      } else {
        print("Invalid Type")
      }
      print(c("bad",bad))
      badnum = which(names(card_dist_reduced)==bad)
      print(c("badnum",badnum))
      card_dist_reduced = card_dist_reduced[,-badnum]
      card_dist_reduced = card_dist_reduced[-badnum,]
      print(names(card_dist_reduced))
      print(c("min",min(card_dist_reduced,na.rm=TRUE)))
      badlist = c(badlist,as.numeric(bad))
      minlist = c(minlist,as.numeric(min(card_dist_reduced,na.rm=TRUE)))
    } 
  }
  fully_reduced = names(card_dist_reduced)
  print("FULLY REDUCED")
  print(fully_reduced)
  par(mfrow=c(3,1))
  plot(badlist)
  plot(minlist)
  plot(replist)
  endtime = Sys.time()
  result = list(removed=badlist,modes=replist,minimums=minlist,subset=fully_reduced)
  return(result)
  print("start time",starttime,"end time",endtime,"elapsed",(endtime-starttime))
}
test1.1 = points.subset(data = card_distance_pivot, N = 100,DropMode = "rand")
test
test1.1
test2
test2.1
test1.5
test1.5.1
# do this 100 times and then record if higher number points?
n = 100
best100.2 = data.frame()
bestlist100.2 = c()
length=c()
startbest = Sys.time()
for (i in 1:n) {
  print(c("round",i,"of",n))
  x = points.subset(data=card_distance_pivot,N=100,DropMode="rand")
  if (length(x$subset) > max(bestlist100.2)) {
    best100.2 = x$subset
    bestlist100.2 = c(bestlist100.2,as.numeric(length(x$subset)))
    length = c(length,length(best100.2))
  } else if (length(x$subset) < max(bestlist100.2)) {
    best100.2 = best100.2
    bestlist100.2 = c(bestlist100.2,as.numeric(length(x$subset)))
    length = c(length,length(best100.2))
  } else if (length(x$subset) == max(bestlist100.2)) {
    best100.2 = cbind(best100.2,x$subset)
    bestlist100.2 = c(bestlist100.2,as.numeric(length(x$subset)))
    length = c(length,length(best100.2))
  } 
  par(mfrow=c(2,1))
  plot(bestlist100.2)
  plot(length)
} 
best100.2
endbest = Sys.time()
print(c("start",startbest,"end",endbest,"elapsed",endbest-startbest))

n = 100
best150.2 = data.frame()
bestlist150.2 = c()
length = c()
startbest = Sys.time()
for (i in 1:n) {
  print(c("round",i,"of",n))
  x = points.subset(data=card_distance_pivot,N=150,DropMode="rand")
  if (length(x$subset) > max(bestlist150.2)) {
    best150.2 = x$subset
    bestlist150.2 = c(bestlist150.2,as.numeric(length(x$subset)))
    length = c(length,length(best150.2))
  } else if (length(x$subset) < max(bestlist150.2)) {
    best150.2 = best150.2
    bestlist150.2 = c(bestlist150.2,as.numeric(length(x$subset)))
    length = c(length,length(best150.2))
  } else if (length(x$subset) == max(bestlist150.2)) {
    best150.2 = cbind(best150.2,x$subset)
    bestlist150.2 = c(bestlist150.2,as.numeric(length(x$subset)))
    length = c(length,length(best150.2))
  } 
  par(mfrow=c(2,1))
  plot(length)
  plot(bestlist150.2)
} 
best150.2
endbest = Sys.time()
print(c("start",startbest,"end",endbest,"elapsed",endbest-startbest))

n = 100
best200.2 = data.frame()
bestlist200.2 = c()
length = c()
startbest = Sys.time()
for (i in 1:n) {
  print(c("round",i,"of",n))
  x = points.subset(data=card_distance_pivot,N=200,DropMode="rand")
  if (length(x$subset) > max(bestlist200.2)) {
    best200.2 = x$subset
    bestlist200.2 = c(bestlist200.2,as.numeric(length(x$subset)))
    length = c(length,length(best200.2))
  } else if (length(x$subset) < max(bestlist200.2)) {
    best200.2 = best200.2
    bestlist200.2 = c(bestlist200.2,as.numeric(length(x$subset)))
    length = c(length,length(best200.2))
  } else if (length(x$subset) == max(bestlist200.2)) {
    best200.2 = cbind(best200.2,x$subset)
    bestlist200.2 = c(bestlist200.2,as.numeric(length(x$subset)))
    length = c(length,length(best200.2))
  } 
  par(mfrow=c(2,1))
  plot(bestlist200.2)
  plot(length)
} 
best200.2
endbest = Sys.time()
print(c("start",startbest,"end",endbest,"elapsed",endbest-startbest))

best200.2
bestlist200.2
best150.2
bestlist150.2
best100.2
bestlist100.2



bestlist100
bestlist200
best100
best200

best100.1
plot(bestlist100.1)
best150.1
plot(bestlist150.1)
best200.1
plot(bestlist200.1)

fully_reduced_100_1 
fully_reduced_100_min 
fully_reduced_100_rand
