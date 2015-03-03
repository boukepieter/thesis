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