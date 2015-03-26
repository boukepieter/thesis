library(raster)
library(rgdal)
library(sp)
library(RSAGA)

# files
setwd("E:/thesis/workspace/results/1a")
ETfile <- "input/ET.tif"
Pfile <- "input/P.tif"
DEMfile <- "input/DEM.tif"
work_env = rsaga.env(path="C:/Program Files (x86)/SAGA_GIS") # windows
dir.create("step")
dir.create("output")

# load in input
ET <- raster(ETfile)
P <- raster(Pfile)
DEM <- raster(DEMfile)
DEM <- aggregate(DEM, 9, mean)
writeRaster(DEM, "step/DEM.tif", format="GTiff", overwrite=TRUE)

# runoff calc
Presample <- resample(P, ET, method='bilinear')
runoff <- Presample - ET
runoff[runoff < 0] <- 0
#plot(runoff, col=rainbow(12))

# Fill DEM:
rsaga.geoprocessor(lib="io_gdal", module=0,param=list(GRIDS=paste(getwd(),"step/DEM.sgrd",sep="/"), 
                                                      FILES=paste(getwd(),"step/DEM.tif",sep="/"), 
                                                      TRANSFORM=TRUE, INTERPOL=1),
                   env=work_env)
rsaga.geoprocessor(lib="ta_preprocessor", module=3, 
                   param=list(DEM="step/DEM.sgrd", RESULT="step/filledDEM.sgrd", MINSLOPE=0.05),
                   env=work_env)
filledDEM <- raster("step/filledDEM.sdat")

# Resample and round runoff
runoffRes <- resample(runoff, filledDEM, method='bilinear')
#plot(runoffRes, col=rainbow(12))
writeRaster(runoffRes, filename="step/runoffRes.tif", format="GTiff", overwrite=TRUE)
writeRaster(filledDEM, filename="step/filledDEM.tif", format="GTiff", overwrite=TRUE)

# Flow accumulation
rsaga.geoprocessor(lib="io_gdal", module=0,param=list(GRIDS=paste(getwd(),"step/runoffRes.sgrd",sep="/"), 
                                                      FILES=paste(getwd(),"step/runoffRes.tif",sep="/"), 
                                                      TRANSFORM=TRUE, INTERPOL=1),
                   env=work_env)
rsaga.geoprocessor(lib="io_gdal", module=0, param=list(GRIDS="step/filledDEMnew.sgrd",
                                                       FILES="step/filledDEM.tif",
                                                       TRANSFORM=TRUE, INTERPOL=1),
                   env=work_env)
rsaga.geoprocessor(lib="grid_analysis", module=18, 
                   param=list(SURFACE="step/filledDEMnew.sgrd", INPUT="step/runoffRes.sgrd",
                              FLUX="step/cell_acc.sgrd", OPERATION=0),
                   env=work_env)
cellAcc <- raster("step/cell_acc.sdat")

# head
head <- focal(filledDEM, w=matrix(1,3,3), fun=function(x){x[5] - min(x)})
debiet <- cellAcc / 1000 / 24 / 3600 * res(cellAcc)[1] ^ 2

# hydro potential
potential <- debiet * head * 9.81 * 1000 # Joule / second
writeRaster(potential, filename="output/potentialA.tif", format="GTiff", overwrite=TRUE)
highPotential <- potential
highPotential[potential < 10000] <- NA
writeRaster(highPotential, filename="output/highPotentialA.tif", format="GTiff", overwrite=TRUE)

# plot
highPotential <- raster("output/highPotential.tif")
potential <- raster("output/potential.tif")
plot(highPotential)
potential
plot(head)
plot(highPotential, main="Potential in Watt with increased spatial resolution", 
     zlim=c(0,3000))
highPotential
