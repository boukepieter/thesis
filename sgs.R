library(sp)
library(rgdal)
library(raster)
library(gstat)
load("vgmf.RData")
clippedDEM <- raster("clippedDEM.tif")
mask <- clippedDEM
mask <- as(mask, 'SpatialGridDataFrame')
mask[!is.na(mask@data)] <- 1
points <- rasterToPoints(clippedDEM, fun=function(x){!is.na(x)}, spatial=TRUE)
#test <- krige(DEM~1, points, newdata=mask, model=vgmf, nsim=1, nmax=3)
gpb <- gstat(id = c("DEM"), formula = DEM~1, dummy = T, beta = 0, model=vgmf, nmax=25)
#projection(mask) <- NA
DEM.sim <- predict.gstat(gpb, mask, nsim=3, debug.level=-1)


set.seed(2373)
DEM.sim2 <- predict.gstat(gpb, mask, nsim=50, debug.level=-1)
for (i in 1:50){
  newDEM <- clippedDEM + raster(DEM.sim2[i])
  writeRaster(newDEM,sprintf("pos_DEMs/DEM%03d.tif",i),format="GTiff", overwrite=T)
}
set.seed(2374)
DEM.sim2 <- predict.gstat(gpb, mask, nsim=50, debug.level=-1)
for (i in 51:100){
  newDEM <- clippedDEM + raster(DEM.sim2[i-50])
  writeRaster(newDEM,sprintf("pos_DEMs/DEM%03d.tif",i),format="GTiff", overwrite=T)
}
set.seed(2375)
DEM.sim2 <- predict.gstat(gpb, mask, nsim=50, debug.level=-1)
for (i in 101:150){
  newDEM <- clippedDEM + raster(DEM.sim2[i-100])
  writeRaster(newDEM,sprintf("pos_DEMs/DEM%03d.tif",i),format="GTiff", overwrite=T)
}
set.seed(2376)
DEM.sim2 <- predict.gstat(gpb, mask, nsim=50, debug.level=-1)
for (i in 151:200){
  newDEM <- clippedDEM + raster(DEM.sim2[i-150])
  writeRaster(newDEM,sprintf("pos_DEMs/DEM%03d.tif",i),format="GTiff", overwrite=T)
}

setwd("E:/thesis/workspace/MCAnalyse")
noSteps <- 12
get.suitables <- function(nr) {
  names <- lapply(1:3,FUN=function(x){sprintf("run%03d/output/highPotential%i_%02d.tif", nr, x, 1:noSteps)})
  highPotentials <- lapply(X=names, FUN=stack)
  suitable <- lapply(highPotentials, FUN=reclassify, c(-1,9999,0,10000,Inf,1))
  suitnr <- mapply(FUN=calc, suitable, MoreArgs=list(fun=sum, na.rm=T))
  suit <- lapply(suitnr, FUN=reclassify, c(-1,9,0,9,12,1))
  suitables <- stack(suit)
}
get.suitables <- function(nr, aggregate=0) {
  names <- lapply(1:3,FUN=function(x){sprintf("run%03d/output/highPotential%i_%02d.tif", nr, x, 1:noSteps)})
  highPotentials <- lapply(X=names, FUN=stack)
  suitable <- lapply(highPotentials, FUN=reclassify, c(-1,9999,0,10000,Inf,1))
  suitnr <- mapply(FUN=calc, suitable, MoreArgs=list(fun=sum, na.rm=T))
  suit <- lapply(suitnr, FUN=reclassify, c(-1,9,0,9,12,1))
  if (aggregate > 0) {suit <- lapply(suit, FUN=aggregate, aggregate, max)}
  suitables <- stack(suit)
}
suitables <- get.suitables(101)
for (i in 102:200) {
  suitables2 <- get.suitables(i)
  suitables <- suitables + suitables2
  print(i)
}
writeRaster(suitables, c("suitables11.tif", "suitables22.tif", "suitables33.tif"), 
            bylayer=TRUE, format="GTiff", overwrite=TRUE)
suitables <- get.suitables(101, 10)
for (i in 102:200) {
  suitables2 <- get.suitables(i, 10)
  suitables <- suitables + suitables2
  print(i)
}
writeRaster(suitables, c("suitables11Aggr.tif", "suitables22Aggr.tif", "suitables33Aggr.tif"), 
            bylayer=TRUE, format="GTiff", overwrite=TRUE)
suitables <- stack(c("suitables1.tif", "suitables2.tif", "suitables3.tif"))
library(plotKML)
plotKML(suitables[[2]])
suitables[[3]][suitables[[3]] < 1] <- NA
plotKML(suitables[[3]])

### analyzes
suitables1 <- stack(c("suitables1.tif", "suitables2.tif", "suitables3.tif"))
suitables2 <- stack(c("suitables11.tif", "suitables22.tif", "suitables33.tif"))
diff <- suitables200 - suitables100
plot(diff)

suitables3 <- (suitables1+suitables2) / 2
writeRaster(suitables3, c("suitables111.tif", "suitables222.tif", "suitables333.tif"), 
            bylayer=TRUE, format="GTiff", overwrite=TRUE)

plot(x=suitables1[[2]][], y=suitables2[[2]][], pch=20, col=rgb(0,0,0,0.05))
lines(x=0:70, y=0:70)

suitables1a <- stack(c("suitables1Aggr.tif", "suitables2Aggr.tif", "suitables3Aggr.tif"))
suitables2a <- stack(c("suitables11Aggr.tif", "suitables22Aggr.tif", "suitables33Aggr.tif"))
diff <- suitables2a - suitables1a
plot(diff)
plot(x=suitables1a[[2]][], y=suitables2a[[2]][], pch=20, col=rgb(0,0,0,0.1))
lines(x=0:100, y=0:100)

suitables3a <- (suitables1a+suitables2a) / 2
writeRaster(suitables3a, c("suitables111Aggr.tif", "suitables222Aggr.tif", "suitables333Aggr.tif"), 
            bylayer=TRUE, format="GTiff", overwrite=TRUE)
