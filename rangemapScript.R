library(sp)
library(maptools)
library(rgeos)
library(raster)
library(rworldmap)
library(RColorBrewer)
#library(rgdal) ## use shapespatial fxn - rp? rgdal is bunk

## get the names of all the files

path = "C:/Users/Kaiya/Dropbox/BirdlifeRangeMaps/BirdlifeRangeMaps/"
setwd(path)
file.names = dir(path, pattern =".shp")
#full.files=c()
#for(i in 1:length(file.names)){
  #print(file.names[i])
  #lens = nchar(file.names[i])
  #print(substring(file.names[i],1,lens-4))
  #full.files = append(full.files,paste(path,file.names[i],sep=""))
#}

#print( full.files[1:5])

## RENAME files
rename.names = dir(path,pattern=".shpRASTER.asc")
print(rename.names[1:5])
for (i in 1:length(rename.names)){
  file = paste(path,rename.names[i],sep="")
  subs = paste(substring(file,1,nchar(file)-nchar(".shpRASTER.asc")),".asc",sep="")
  file.rename(file,subs)
}

#testshp = readOGR(full.files[1], 'shapefile')

## import a shpfile
testshp = readShapeSpatial(file.names[1])
proj4string(testshp) <- "+proj=longlat +datum=WGS84"
plot(testshp, axes=TRUE, col=rgb(0.3, 0.5, 0.5, 0.3),
     border=rgb(0,0,0,0),
     xlim=c(-90,-10),ylim=c(-90,90))
  
for(i in 2:20){
  shp = readShapeSpatial(file.names[i])
  proj4string(shp) <- "+proj=longlat +datum=WGS84"
  plot(shp, axes=TRUE, col=rgb(0.3, 0.5, 0.5, 0.3),
       border=rgb(0,0,0,0),
       xlim=c(-90,-10),ylim=c(-90,90),add=TRUE)
}

## make rasters
shp2 = readShapeSpatial(file.names[2])
shp8 = readShapeSpatial(file.names[8])

## Set up a raster "template" to use in rasterize()
ext =  extent (-90, -10, -90, 90)
xy = abs(apply(as.matrix(bbox(ext)), 1, diff))
n = 5
r = raster(ext, ncol=xy[1]*n, nrow=xy[2]*n)

## Rasterize the shapefile
rr = rasterize(shp2, r)
r8 = rasterize(shp8, r)
rlist = c(rr,r8)
mos(rr,r8,fun=sum)

mos1 = mosaic(rlist,fun=sum) ## does not work, need to feed in one at a time

rast.mosargs <- rlist
rast.mosargs$fun <- sum
mos2 <- do.call(mosaic, rast.mosargs)
stopifnot(identical(mos, mos2))

plot(mos)
plot(mos2)

## A couple of outputs
writeRaster(rr, "shp2.asc",overwrite=TRUE)
plot(rr)
plot(r8,add=TRUE)
plot(rr,r8,col = topo.colors(20),
     zlim = c(0,2))

brks <- seq(0,30,by=0.3)
nbrks <- length(brks)-1
plot(rr,breaks=brks,col=rev(terrain.colors(nbrks)),lab.breaks=brks,zlim=c(0,1))
plot(r8,breaks=brks,col=rev(terrain.colors(nbrks)),lab.breaks=brks,zlim=c(0,1))


## convert all to raster?
for(i in 2:20){
  shp = readShapeSpatial(file.names[i])
  proj4string(shp) <- "+proj=longlat +datum=WGS84"
  plot(shp, axes=TRUE, col=rgb(0.3, 0.5, 0.5, 0.3),
       border=rgb(0,0,0,0),
       xlim=c(-180,180),ylim=c(-90,90),add=TRUE)
}

path = "C:/Users/Kaiya/Dropbox/BirdlifeRangeMaps/BirdlifeRangeMaps/"
setwd(path)
ext =  extent (-150, -25, -90, 90)
xy = abs(apply(as.matrix(bbox(ext)), 1, diff))
n = 5
r = raster(ext, ncol=xy[1]*n, nrow=xy[2]*n)
for(i in file.names){
  shp = readShapeSpatial(i)
  proj4string(shp) <- "+proj=longlat +datum=WGS84"
  rst = rasterize(shp,r)
  writeRaster(rst, paste(i,"RASTER.asc",sep=""),overwrite=TRUE)
}


##browseURL("https://www.youtube.com/watch?v=WZ_8HhDvgbQ")

## re-import the rasters one by one?
path = "C:/Users/Kaiya/Dropbox/BirdlifeRangeMaps/BirdlifeRangeMaps/"
setwd(path)
rast.names = dir(path, pattern =".asc")
full.rast=c()
for(i in 2:length(rast.names)){
  #full.rast = append(full.rast,paste(path,rast.names[i],sep=""))
  if(i == 2){
    r1 = raster(rast.names[1])
  } 
  r2 = raster(rast.names[i])
  mosSum = mosaic(r1,r2,fun=sum)
  r1 = mosSum
}

plot(mosSum,col=rev(terrain.colors(nbrks)),lab.breaks=brks)
writeRaster(mosSum,"mosSumRASTER.asc",overwrite=TRUE)

cols = colorRampPalette(brewer.pal(nbrks,"Blues"))
# display.brewer.all()

mosSum2 = raster(paste(path,"mosSumRASTER.asc",sep=""))
#plot(mosSum2,col=topo.colors((nbrks)))
plot(mosSum2,col=cols(nbrks))



## extract the taxon name of a raster/shp
lookupCsv = c("Taxon","Suffix","Shapefile","Raster")

for(i in 1:length(file.names)){
  #print(file.names[i])
  x = unlist(strsplit(file.names[i],c("_")))
  #print(x[2])
  sppName = paste(x[1],x[2],sep="_")
  endName = paste(x[3],substr(x[4],0,nchar(x[4])-4),sep="_")
  #fileEnd = substr(x[4],nchar(x[4])-3,nchar(x[4]))
  #print(sppName)
  #print(endName)
  #print(fileEnd)
  lookupCsv = rbind(lookupCsv,c(sppName,endName,file.names[i],
                    paste(paste(sppName,endName,sep="_"),".asc",sep="")))
}

rownames(lookupCsv) = NULL
colnames(lookupCsv) = c("Taxon","Suffix","Shapefile","Raster")
lookupCsv = lookupCsv[-1,]
write.csv(lookupCsv,file=paste(path,"lookupCsv.csv",sep=""))
