## analyzing the spatial subset data


## 150 B!!!
one_150B = read.csv("C:/Users/Kaiya/Dropbox/Docs for Brian/Field Work/Cardinal Data Analysis/Master Files/CSVs/spatial subsets/one_point_150B_6nov2015.csv")
## 200 B!!!
one_200B = read.csv("C:/Users/Kaiya/Dropbox/Docs for Brian/Field Work/Cardinal Data Analysis/Master Files/CSVs/spatial subsets/one_point_200B_6nov2015.csv")
## 150 A!!!
one_150A = read.csv("C:/Users/Kaiya/Dropbox/Docs for Brian/Field Work/Cardinal Data Analysis/Master Files/CSVs/spatial subsets/one_point_150A_6nov2015.csv")
## 200 A!!!
one_200A = read.csv("C:/Users/Kaiya/Dropbox/Docs for Brian/Field Work/Cardinal Data Analysis/Master Files/CSVs/spatial subsets/one_point_200A_6nov2015.csv")
## 100B!!!
one_100B = read.csv("C:/Users/Kaiya/Dropbox/Docs for Brian/Field Work/Cardinal Data Analysis/Master Files/CSVs/spatial subsets/one_point_100B_6nov2015.csv")
## 100A!!!
one_100A = read.csv("C:/Users/Kaiya/Dropbox/Docs for Brian/Field Work/Cardinal Data Analysis/Master Files/CSVs/spatial subsets/one_point_100A_6nov2015.csv")
## 100C!!!
one_100C = read.csv("C:/Users/Kaiya/Dropbox/Docs for Brian/Field Work/Cardinal Data Analysis/Master Files/CSVs/spatial subsets/one_point_100C_6nov2015.csv")



## spatial autocorrelation measures 150B
## calculate moran's I
## make an inverse distance matrix
one_150B_dists <- as.matrix(dist(cbind(one_150B$X0.Longitude, one_150B$X0.Latitude)))
one_150B_dists_inv <- 1/one_150B_dists
diag(one_150B_dists_inv) <- 0
one_150B_dists_inv[1:5, 1:5]
## calculate for each variable
## first one, portal flyby resps
library(ape)
Moran.I(one_150B[,1], one_150B_dists_inv)
## iterate over each column
## moran's I does not like columns with all identical data, so they are removed
reduced = one_150B[,apply(one_150B,2,function(x) any(c(FALSE,x[-length(x)]!=x[-1])))] 
library(ape)
N = 1
len = length(reduced)
moran_vector_150B=vector()
for (i in 1:len) {
  moran_vector_150B[i] = as.numeric(Moran.I(as.numeric(reduced[,i]),one_150B_dists_inv)$p.value)
}
moran_vector_150B=as.numeric(moran_vector_150B)
names = colnames(reduced)
moran_matrix_150B = as.data.frame(matrix(moran_vector_150B))
colnames(moran_matrix_150B) = "Moran's I"
rownames(moran_matrix_150B) = names
len = length(reduced)
moran_strength = vector()
for (i in 1:len) {
  moran_strength[i] = as.numeric(Moran.I(as.numeric(reduced[,i]),one_150B_dists_inv)$observed)
}
moran_matrix_150B = cbind(moran_matrix_150B,moran_strength)
colnames(moran_matrix_150B) = c("Morans.I.Pval","Observed")
moran_result = vector()
for(i in 1:len) {
  if(moran_matrix_150B[i,1] > 0.05){
    moran_result[i] = "n.s."
  } else if (moran_matrix_150B[i,2] > 0) { 
    moran_result[i] = "pos"
  } else if (moran_matrix_150B[i,2] < 0) {
    moran_result[i] = "neg"
  } else if (moran_matrix_150B[i,2] == 0) { 
    moran_result[i] = "zero"
  } else { 
    moran_result[i] = "error"
    warning("something went wrong")
  }
}
moran_matrix_150B = cbind(moran_matrix_150B,moran_result)
colnames(moran_matrix_150B) = c("Morans.I.Pval","Observed","Result")
write.csv(moran_matrix_150B,
          file="C:/Users/Kaiya/Dropbox/Docs for Brian/Field Work/Cardinal Data Analysis/moran_matrix_150B_csv.csv")
#####

## spatial autocorrelation measures 200B
## calculate moran's I
## make an inverse distance matrix
one_200B_dists <- as.matrix(dist(cbind(one_200B$X0.Longitude, one_200B$X0.Latitude)))
one_200B_dists_inv <- 1/one_200B_dists
diag(one_200B_dists_inv) <- 0
one_200B_dists_inv[1:5, 1:5]
## calculate for each variable
## first one, portal flyby resps
library(ape)
Moran.I(one_200B[,1], one_200B_dists_inv)
## iterate over each column
## moran's I does not like columns with all identical data, so they are removed
reduced = one_200B[,apply(one_200B,2,function(x) any(c(FALSE,x[-length(x)]!=x[-1])))] 
library(ape)
N = 1
len = length(reduced)
moran_vector_200B=vector()
for (i in 1:len) {
  moran_vector_200B[i] = as.numeric(Moran.I(as.numeric(reduced[,i]),one_200B_dists_inv)$p.value)
}
moran_vector_200B=as.numeric(moran_vector_200B)
names = colnames(reduced)
moran_matrix_200B = as.data.frame(matrix(moran_vector_200B))
colnames(moran_matrix_200B) = "Moran's I"
rownames(moran_matrix_200B) = names
len = length(reduced)
moran_strength = vector()
for (i in 1:len) {
  moran_strength[i] = as.numeric(Moran.I(as.numeric(reduced[,i]),one_200B_dists_inv)$observed)
}
moran_matrix_200B = cbind(moran_matrix_200B,moran_strength)
colnames(moran_matrix_200B) = c("Morans.I.Pval","Observed")
moran_result = vector()
for(i in 1:len) {
  if(moran_matrix_200B[i,1] > 0.05){
    moran_result[i] = "n.s."
  } else if (moran_matrix_200B[i,2] > 0) { 
    moran_result[i] = "pos"
  } else if (moran_matrix_200B[i,2] < 0) {
    moran_result[i] = "neg"
  } else if (moran_matrix_200B[i,2] == 0) { 
    moran_result[i] = "zero"
  } else { 
    moran_result[i] = "error"
    warning("something went wrong")
  }
}
moran_matrix_200B = cbind(moran_matrix_200B,moran_result)
colnames(moran_matrix_200B) = c("Morans.I.Pval","Observed","Result")
write.csv(moran_matrix_200B,
          file="C:/Users/Kaiya/Dropbox/Docs for Brian/Field Work/Cardinal Data Analysis/moran_matrix_200B_csv.csv")
#####

## spatial autocorrelation measures 150A
## calculate moran's I
## make an inverse distance matrix
one_150A_dists <- as.matrix(dist(cbind(one_150A$X0.Longitude, one_150A$X0.Latitude)))
one_150A_dists_inv <- 1/one_150A_dists
diag(one_150A_dists_inv) <- 0
one_150A_dists_inv[1:5, 1:5]
## calculate for each variable
## first one, portal flyby resps
library(ape)
Moran.I(one_150A[,1], one_150A_dists_inv)
## iterate over each column
## moran's I does not like columns with all identical data, so they are removed
reduced = one_150A[,apply(one_150A,2,function(x) any(c(FALSE,x[-length(x)]!=x[-1])))] 
library(ape)
N = 1
len = length(reduced)
moran_vector_150A=vector()
for (i in 1:len) {
  moran_vector_150A[i] = as.numeric(Moran.I(as.numeric(reduced[,i]),one_150A_dists_inv)$p.value)
}
moran_vector_150A=as.numeric(moran_vector_150A)
names = colnames(reduced)
moran_matrix_150A = as.data.frame(matrix(moran_vector_150A))
colnames(moran_matrix_150A) = "Moran's I"
rownames(moran_matrix_150A) = names
len = length(reduced)
moran_strength = vector()
for (i in 1:len) {
  moran_strength[i] = as.numeric(Moran.I(as.numeric(reduced[,i]),one_150A_dists_inv)$observed)
}
moran_matrix_150A = cbind(moran_matrix_150A,moran_strength)
colnames(moran_matrix_150A) = c("Morans.I.Pval","Observed")
moran_result = vector()
for(i in 1:len) {
  if(moran_matrix_150A[i,1] > 0.05){
    moran_result[i] = "n.s."
  } else if (moran_matrix_150A[i,2] > 0) { 
    moran_result[i] = "pos"
  } else if (moran_matrix_150A[i,2] < 0) {
    moran_result[i] = "neg"
  } else if (moran_matrix_150A[i,2] == 0) { 
    moran_result[i] = "zero"
  } else { 
    moran_result[i] = "error"
    warning("something went wrong")
  }
}
moran_matrix_150A = cbind(moran_matrix_150A,moran_result)
colnames(moran_matrix_150A) = c("Morans.I.Pval","Observed","Result")
write.csv(moran_matrix_150A,
          file="C:/Users/Kaiya/Dropbox/Docs for Brian/Field Work/Cardinal Data Analysis/moran_matrix_150A_csv.csv")
#####

## spatial autocorrelation measures 200A
## calculate moran's I
## make an inverse distance matrix
one_200A_dists <- as.matrix(dist(cbind(one_200A$X0.Longitude, one_200A$X0.Latitude)))
one_200A_dists_inv <- 1/one_200A_dists
diag(one_200A_dists_inv) <- 0
one_200A_dists_inv[1:5, 1:5]
## calculate for each variable
## first one, portal flyby resps
library(ape)
Moran.I(one_200A[,1], one_200A_dists_inv)
## iterate over each column
## moran's I does not like columns with all identical data, so they are removed
reduced = one_200A[,apply(one_200A,2,function(x) any(c(FALSE,x[-length(x)]!=x[-1])))] 
library(ape)
N = 1
len = length(reduced)
moran_vector_200A=vector()
for (i in 1:len) {
  moran_vector_200A[i] = as.numeric(Moran.I(as.numeric(reduced[,i]),one_200A_dists_inv)$p.value)
}
moran_vector_200A=as.numeric(moran_vector_200A)
names = colnames(reduced)
moran_matrix_200A = as.data.frame(matrix(moran_vector_200A))
colnames(moran_matrix_200A) = "Moran's I"
rownames(moran_matrix_200A) = names
len = length(reduced)
moran_strength = vector()
for (i in 1:len) {
  moran_strength[i] = as.numeric(Moran.I(as.numeric(reduced[,i]),one_200A_dists_inv)$observed)
}
moran_matrix_200A = cbind(moran_matrix_200A,moran_strength)
colnames(moran_matrix_200A) = c("Morans.I.Pval","Observed")
moran_result = vector()
for(i in 1:len) {
  if(moran_matrix_200A[i,1] > 0.05){
    moran_result[i] = "n.s."
  } else if (moran_matrix_200A[i,2] > 0) { 
    moran_result[i] = "pos"
  } else if (moran_matrix_200A[i,2] < 0) {
    moran_result[i] = "neg"
  } else if (moran_matrix_200A[i,2] == 0) { 
    moran_result[i] = "zero"
  } else { 
    moran_result[i] = "error"
    warning("something went wrong")
  }
}
moran_matrix_200A = cbind(moran_matrix_200A,moran_result)
colnames(moran_matrix_200A) = c("Morans.I.Pval","Observed","Result")
write.csv(moran_matrix_200A,
          file="C:/Users/Kaiya/Dropbox/Docs for Brian/Field Work/Cardinal Data Analysis/moran_matrix_200A_csv.csv")
#####

## spatial autocorrelation measures 100B
## calculate moran's I
## make an inverse distance matrix
one_100B_dists <- as.matrix(dist(cbind(one_100B$X0.Longitude, one_100B$X0.Latitude)))
one_100B_dists_inv <- 1/one_100B_dists
diag(one_100B_dists_inv) <- 0
one_100B_dists_inv[1:5, 1:5]
## calculate for each variable
## first one, portal flyby resps
library(ape)
Moran.I(one_100B[,1], one_100B_dists_inv)
## iterate over each column
## moran's I does not like columns with all identical data, so they are removed
reduced = one_100B[,apply(one_100B,2,function(x) any(c(FALSE,x[-length(x)]!=x[-1])))] 
library(ape)
N = 1
len = length(reduced)
moran_vector_100B=vector()
for (i in 1:len) {
  moran_vector_100B[i] = as.numeric(Moran.I(as.numeric(reduced[,i]),one_100B_dists_inv)$p.value)
}
moran_vector_100B=as.numeric(moran_vector_100B)
names = colnames(reduced)
moran_matrix_100B = as.data.frame(matrix(moran_vector_100B))
colnames(moran_matrix_100B) = "Moran's I"
rownames(moran_matrix_100B) = names
len = length(reduced)
moran_strength = vector()
for (i in 1:len) {
  moran_strength[i] = as.numeric(Moran.I(as.numeric(reduced[,i]),one_100B_dists_inv)$observed)
}
moran_matrix_100B = cbind(moran_matrix_100B,moran_strength)
colnames(moran_matrix_100B) = c("Morans.I.Pval","Observed")
moran_result = vector()
for(i in 1:len) {
  if(moran_matrix_100B[i,1] > 0.05){
    moran_result[i] = "n.s."
  } else if (moran_matrix_100B[i,2] > 0) { 
    moran_result[i] = "pos"
  } else if (moran_matrix_100B[i,2] < 0) {
    moran_result[i] = "neg"
  } else if (moran_matrix_100B[i,2] == 0) { 
    moran_result[i] = "zero"
  } else { 
    moran_result[i] = "error"
    warning("something went wrong")
  }
}
moran_matrix_100B = cbind(moran_matrix_100B,moran_result)
colnames(moran_matrix_100B) = c("Morans.I.Pval","Observed","Result")
write.csv(moran_matrix_100B,
          file="C:/Users/Kaiya/Dropbox/Docs for Brian/Field Work/Cardinal Data Analysis/moran_matrix_100B_csv.csv")
#####

## spatial autocorrelation measures 100C
## calculate moran's I
## make an inverse distance matrix
one_100C_dists <- as.matrix(dist(cbind(one_100C$X0.Longitude, one_100C$X0.Latitude)))
one_100C_dists_inv <- 1/one_100C_dists
diag(one_100C_dists_inv) <- 0
one_100C_dists_inv[1:5, 1:5]
## calculate for each variable
## first one, portal flyby resps
library(ape)
Moran.I(one_100C[,1], one_100C_dists_inv)
## iterate over each column
## moran's I does not like columns with all identical data, so they are removed
reduced = one_100C[,apply(one_100C,2,function(x) any(c(FALSE,x[-length(x)]!=x[-1])))] 
library(ape)
N = 1
len = length(reduced)
moran_vector_100C=vector()
for (i in 1:len) {
  moran_vector_100C[i] = as.numeric(Moran.I(as.numeric(reduced[,i]),one_100C_dists_inv)$p.value)
}
moran_vector_100C=as.numeric(moran_vector_100C)
names = colnames(reduced)
moran_matrix_100C = as.data.frame(matrix(moran_vector_100C))
colnames(moran_matrix_100C) = "Moran's I"
rownames(moran_matrix_100C) = names
len = length(reduced)
moran_strength = vector()
for (i in 1:len) {
  moran_strength[i] = as.numeric(Moran.I(as.numeric(reduced[,i]),one_100C_dists_inv)$observed)
}
moran_matrix_100C = cbind(moran_matrix_100C,moran_strength)
colnames(moran_matrix_100C) = c("Morans.I.Pval","Observed")
moran_result = vector()
for(i in 1:len) {
  if(moran_matrix_100C[i,1] > 0.05){
    moran_result[i] = "n.s."
  } else if (moran_matrix_100C[i,2] > 0) { 
    moran_result[i] = "pos"
  } else if (moran_matrix_100C[i,2] < 0) {
    moran_result[i] = "neg"
  } else if (moran_matrix_100C[i,2] == 0) { 
    moran_result[i] = "zero"
  } else { 
    moran_result[i] = "error"
    warning("something went wrong")
  }
}
moran_matrix_100C = cbind(moran_matrix_100C,moran_result)
colnames(moran_matrix_100C) = c("Morans.I.Pval","Observed","Result")
write.csv(moran_matrix_100C,
          file="C:/Users/Kaiya/Dropbox/Docs for Brian/Field Work/Cardinal Data Analysis/moran_matrix_100C_csv.csv")
#####

## spatial autocorrelation measures 100A
## calculate moran's I
## make an inverse distance matrix
one_100A_dists <- as.matrix(dist(cbind(one_100A$X0.Longitude, one_100A$X0.Latitude)))
one_100A_dists_inv <- 1/one_100A_dists
diag(one_100A_dists_inv) <- 0
one_100A_dists_inv[1:5, 1:5]
## calculate for each variable
## first one, portal flyby resps
library(ape)
Moran.I(one_100A[,1], one_100A_dists_inv)
## iterate over each column
## moran's I does not like columns with all identical data, so they are removed
reduced = one_100A[,apply(one_100A,2,function(x) any(c(FALSE,x[-length(x)]!=x[-1])))] 
library(ape)
N = 1
len = length(reduced)
moran_vector_100A=vector()
for (i in 1:len) {
  moran_vector_100A[i] = as.numeric(Moran.I(as.numeric(reduced[,i]),one_100A_dists_inv)$p.value)
}
moran_vector_100A=as.numeric(moran_vector_100A)
names = colnames(reduced)
moran_matrix_100A = as.data.frame(matrix(moran_vector_100A))
colnames(moran_matrix_100A) = "Moran's I"
rownames(moran_matrix_100A) = names
len = length(reduced)
moran_strength = vector()
for (i in 1:len) {
  moran_strength[i] = as.numeric(Moran.I(as.numeric(reduced[,i]),one_100A_dists_inv)$observed)
}
moran_matrix_100A = cbind(moran_matrix_100A,moran_strength)
colnames(moran_matrix_100A) = c("Morans.I.Pval","Observed")
moran_result = vector()
for(i in 1:len) {
  if(moran_matrix_100A[i,1] > 0.05){
    moran_result[i] = "n.s."
  } else if (moran_matrix_100A[i,2] > 0) { 
    moran_result[i] = "pos"
  } else if (moran_matrix_100A[i,2] < 0) {
    moran_result[i] = "neg"
  } else if (moran_matrix_100A[i,2] == 0) { 
    moran_result[i] = "zero"
  } else { 
    moran_result[i] = "error"
    warning("something went wrong")
  }
}
moran_matrix_100A = cbind(moran_matrix_100A,moran_result)
colnames(moran_matrix_100A) = c("Morans.I.Pval","Observed","Result")
write.csv(moran_matrix_100A,
          file="C:/Users/Kaiya/Dropbox/Docs for Brian/Field Work/Cardinal Data Analysis/moran_matrix_100A_csv.csv")
#####