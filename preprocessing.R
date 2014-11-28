library(raster)
library(rgdal)
library(sp)

# files
setwd("/media/boukepieter/schijfje_ottow/thesis/workspace")
mod16filename = "data/ET.tif"
trmmfilename = "data/trmm2b31_annual_mm_per_year.tif"
srtmfilename = "data/srtm.tif"

utm51 = CRS('+proj=utm +zone=51 +ellps=WGS84 +datum=WGS84 +units=m +no_defs')
wgs84 = CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')

# projectarea
xym <- matrix(c(5.46836, 124, 7, 124, 7, 126, 5.46836, 126, 5.46836, 124), ncol=2, byrow=T)
xym2 <- xym
xym2[,1] = xym[,2]
xym2[,2] = xym[,1]
p = Polygon(xym2)
ps = Polygons(list(p),1)
sps = SpatialPolygons(list(ps))
proj4string(sps) <- wgs84
projectarea <- spTransform(sps, utm51)

xym <- matrix(c(5, 123, 8, 123, 8, 127, 5, 127, 5, 123), ncol=2, byrow=T)
xym2 <- xym
xym2[,1] = xym[,2]
xym2[,2] = xym[,1]
p = Polygon(xym2)
ps = Polygons(list(p),1)
arealarge = SpatialPolygons(list(ps))
proj4string(arealarge) <- wgs84

# modis preprocessing
mod16 <- raster(mod16filename)
mod16utm = projectRaster(mod16, crs=utm51)
ET <- crop(mod16utm, projectarea)
ET[ET>65500] <- NA
ET@data@values <- ET@data@values * 0.1 / 365
plot(ET, col=rainbow(12))

#trmm preprocessing
trmm <- raster(trmmfilename)
projection(trmm) <- wgs84
trmm <- crop(trmm, arealarge)
trmmutm = projectRaster(trmm, crs=utm51)
P <- crop(trmmutm, projectarea)
P@data@values <- P@data@values / 365
plot(P, col=rainbow(12))

#srtm preprocessing
# srtm <- raster(srtmfilename)
# srtm <- crop(srtm, arealarge)
# srtmutm <- projectRaster(srtm, crs=utm51)
# 
# DEM <- crop(srtmutm, projectarea)

#Aster preprocessing
name1="data/ASTGTM2_N05E124_dem.tif"
name2="data/ASTGTM2_N05E125_dem.tif"
name3="data/ASTGTM2_N06E124_dem.tif"
name4="data/ASTGTM2_N06E125_dem.tif"
aster1 <- raster(name1)
aster2 <- raster(name2)
aster3 <- raster(name3)
aster4 <- raster(name4)
aster <- merge(aster1, aster2, aster3, aster4, filename="data/aster_merged.tif")
system(paste("gdalwarp -overwrite -s_srs EPSG:4326 -t_srs EPSG:32651 -of GTiff ", getwd(), "/data/aster_merged.tif ",
              getwd(), "/data/aster_utm51.tif", sep=""), intern=TRUE)
aster <- raster("data/aster_utm51.tif")
DEM <- crop(aster, projectarea)

# Aster 270m
DEMsmall <- aggregate(DEM, 9, fun=mean, expand=FALSE, na.rm=TRUE, filename="source/DEMsmall.tif")


# save result
writeRaster(ET, filename="source/ET.tif", format="GTiff", overwrite=TRUE)
writeRaster(P, filename="source/P.tif", format="GTiff", overwrite=TRUE)
writeRaster(DEM, filename="source/DEM.tif", format="GTiff", overwrite=TRUE)


