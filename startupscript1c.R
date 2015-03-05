# Author: B.P. Ottow

# installs
install.packages("raster")
install.packages("sp")
install.packages("gdistance")
install.packages("maptools")
install.packages("RSAGA")
install.packages("rgeos")
install.packages("rgdal")
install.packages("topmodel")
install.packages("spacetime")
install.packages("doParallel")
install.packages("foreach")
install.packages("gdalUtils")
install.packages("animation")
install.packages("plotKML")

# parameters
channelLength = 500
minimumHead = 25
minimumDebiet = 0.1
minimumPotential = 100000
coord <- matrix(c(125.175278, 6.383889), nrow=1)
require(RSAGA)
work_env = rsaga.env(path="C:/Program Files (x86)/SAGA_GIS") # windows

# files
#setwd("/media/boukepieter/schijfje_ottow/thesis/workspace") # linux
sourceCode <- "E:/thesis/workspace/thesis" # gaia windows
setwd("E:/thesis/testws")
ETdir <- "input/ET"
Pdir <- "input/P"
DEMfile <- "input/DEM.tif"
flist <- list.files(sourceCode, ".+[.]R$", full.names = TRUE)
dir.create(paste(getwd(), "scripts", sep="/"))
file.copy(flist, paste(getwd(), "scripts", sep="/"))

# model
source("scripts/model1c.R")
HydroPowerMonthly(DEMfile, Pdir, ETdir, coord=coord, minimumPotential = minimumPotential, minimumDebiet = minimumDebiet,
           minimumHead = minimumHead, channelLength = channelLength, work_env=work_env, plotMethod="None")


####################################-Preprosessing-##################################################
library(raster)
install.packages("gdalUtils")
library(gdalUtils)

utm51 = CRS('+proj=utm +zone=51 +ellps=WGS84 +datum=WGS84 +units=m +no_defs')
wgs84 = CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')

files <- list.files("Data/MOD16")
dir.create("monthly")

hdf4_dataset <- system.file(paste("Data/MOD16/", files[1], sep=""), package="gdalUtils")
for (i in 1:length(files)){
  gdal_translate(paste("Data/MOD16/", files[i], sep=""), 
                 paste("Data/ET/", substring(files[i],1,42), "tif",sep=""), sd_index=1)
  print(i)
}

files <- list.files("Data/ET")

days <- c(31,28,31,30,31,30,31,31,30,31,30,31)
for (i in 1:12){
  mod2010 <- raster(paste("Data/ET/", files[i], sep=""))
  mod2011 <- raster(paste("Data/ET/", files[i+12], sep=""))
  mod2012 <- raster(paste("Data/ET/", files[i+24], sep=""))
  mod2013 <- raster(paste("Data/ET/", files[i+36], sep=""))
  #mod2010s <- crop(mod2010, arealarge)
  mod2010[mod2010>3000] <- NA
  #mod2011s <- crop(mod2011, arealarge)
  mod2011[mod2011>3000] <- NA
  #mod2012s <- crop(mod2012, arealarge)
  mod2012[mod2012>3000] <- NA
  #mod2013s <- crop(mod2013, arealarge)
  mod2013[mod2013>3000] <- NA
  mod16 <- (mod2010 + mod2011 + mod2012 + mod2013) / 4 / days[i]
  mod16utm = projectRaster(mod16, crs=utm51)
  ET <- crop(mod16utm, projectarea)
  writeRaster(ET, filename=paste("input/ET", i, ".tif", sep=""), format="GTiff", overwrite=TRUE)
  print(i)
}

# TRMM
setwd("/media/boukepieter/schijfje_ottow/thesis/data/TRMM/3-hourly-tiff")
days <- c(31,28,31,30,31,30,31,31,30,31,30,31)
files <- list.files("monthly")
for (i in 1:12){
  setwd("/media/boukepieter/schijfje_ottow/thesis/data/TRMM/3-hourly-tiff")
  trmm2010 <- raster(paste("monthly/", files[i], sep=""))
  trmm2011 <- raster(paste("monthly/", files[i+12], sep=""))
  trmm2012 <- raster(paste("monthly/", files[i+24], sep=""))
  trmm2013 <- raster(paste("monthly/", files[i+36], sep=""))
  trmm <- (trmm2010 + trmm2011 + trmm2012 + trmm2013) / 40 * 3 / days[i]
  projection(trmm) <- wgs84
  trmmutm = projectRaster(trmm, crs=utm51)
  P <- crop(trmmutm, projectarea)
  setwd("/media/boukepieter/schijfje_ottow/thesis/workspace1b")
  writeRaster(P, filename=paste("input/P", i, ".tif", sep=""), format="GTiff", overwrite=TRUE)
  print(i)
}

DEM <- raster("Data/DEM.tif")
test <- raster("input/P1.tif")
plot(test, main="August", col=rainbow(12))

for (i in 2:12) {
  m <- raster(paste("input/P", i, ".tif", sep=""))
  test <- test + m
}
test <- test / 12
plot(test)
plot(P)



