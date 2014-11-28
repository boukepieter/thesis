library(raster)
library(rgdal)
library(sp)
library(RSAGA)

# files
setwd("/media/boukepieter/schijfje_ottow/thesis/workspace")
ETfile <- "source/ET.tif"
Pfile <- "source/P.tif"
DEMfile <- "source/DEM.tif"
DEMsmallfile <- "source/DEMsmall.tif"

# load in input
ET <- raster(ETfile)
P <- raster(Pfile)
DEM <- raster(DEMfile)
DEMsmall <- raster(DEMsmallfile)

# runoff calc
Presample <- resample(P, ET, method='bilinear')
runoff <- Presample - ET
runoff[runoff < 0] <- 0
plot(runoff, col=rainbow(12))

# Fill DEM:
rsaga.geoprocessor(lib="io_gdal", module=0,param=list(GRIDS=paste(getwd(),"RSAGA/DEMsmall.sgrd",sep="/"), 
                                                      FILES=paste(getwd(),"source/DEMsmall.tif",sep="/"), 
                                                      TRANSFORM=TRUE, INTERPOL=1))
rsaga.geoprocessor(lib="ta_preprocessor", module=3, 
                   param=list(DEM="RSAGA/DEMsmall.sgrd", RESULT="RSAGA/filledDEM.sgrd", MINSLOPE=0.05))
filledDEM <- raster("RSAGA/filledDEM.sdat")

# Resample and round runoff
runoffRes <- resample(runoff, filledDEM, method='bilinear')
plot(runoffRes, col=rainbow(12))
runoffRounded <- runoffRes
runoffRounded[] <- round(runoffRes[])
writeRaster(runoffRounded, filename="RSAGA/runoffRounded.tif", format="GTiff", overwrite=TRUE)
writeRaster(filledDEM, filename="RSAGA/filledDEM.tif", format="GTiff", overwrite=TRUE)

# Flow accumulation
rsaga.geoprocessor(lib="io_gdal", module=0,param=list(GRIDS=paste(getwd(),"RSAGA/runoffRounded.sgrd",sep="/"), 
                                                      FILES=paste(getwd(),"RSAGA/runoffRounded.tif",sep="/"), 
                                                      TRANSFORM=TRUE, INTERPOL=1))
rsaga.geoprocessor(lib="io_gdal", module=0, param=list(GRIDS="RSAGA/filledDEMnew.sgrd",
                                                       FILES="RSAGA/filledDEM.tif",
                                                       TRANSFORM=TRUE, INTERPOL=1))
rsaga.geoprocessor(lib="grid_analysis", module=18, 
                   param=list(SURFACE="RSAGA/filledDEMnew.sgrd", INPUT="RSAGA/runoffRounded.sgrd",
                              FLUX="RSAGA/cell_acc.sgrd", OPERATION=0))
cellAcc <- raster("RSAGA/cell_acc.sdat")

# head
head <- focal(filledDEM, w=matrix(1,3,3), fun=function(x){x[5] - min(x)})
debiet <- cellAcc / 1000 / 24 / 3600 * res(cellAcc)[1] ^ 2

# hydro potential
potential <- debiet * head * 9.81 # Joule / second
writeRaster(potential, filename="source/potential.tif", format="GTiff", overwrite=TRUE)
highPotential <- potential
highPotential[potential < 10000] <- NA
writeRaster(highPotential, filename="source/highPotential.tif", format="GTiff", overwrite=TRUE)

# plot