library(raster)
library(rgdal)
library(sp)
library(RSAGA)

# parameters
utm51 = CRS('+proj=utm +zone=51 +ellps=WGS84 +datum=WGS84 +units=m +no_defs')
wgs84 = CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')
setwd("/media/boukepieter/schijfje_ottow/thesis/workspace")
channelLength = 500
minimumHead = 25
minimumDebiet = 0.1
minimumPotential = 500000

# files

ETfile <- "input/ET.tif"
Pfile <- "input/P.tif"
DEMfile <- "input/DEM.tif"

# load in input
ET <- raster(ETfile)
P <- raster(Pfile)
DEM <- raster(DEMfile)

# runoff calc
Presample <- resample(P, ET, method='bilinear')
runoff <- Presample - ET
runoff[runoff < 0] <- 0
plot(runoff, col=rainbow(12))

# Fill DEM:
rsaga.geoprocessor(lib="io_gdal", module=0,param=list(GRIDS=paste(getwd(),"step/DEM.sgrd",sep="/"), 
                                                      FILES=paste(getwd(),"input/DEM.tif",sep="/"), 
                                                      TRANSFORM=TRUE, INTERPOL=1))
rsaga.geoprocessor(lib="ta_preprocessor", module=3, 
                   param=list(DEM="step/DEM.sgrd", RESULT="step/filledDEM.sgrd", MINSLOPE=0.01))
filledDEM <- raster("step/filledDEM.sdat")

# Resample and round runoff
runoffRes <- resample(runoff, filledDEM, method='bilinear')

## intermezzo, making area smaller
runoffRessmall <- crop(runoffRes, projectareasmall)
filledDEMsmall <- crop(filledDEM, projectareasmall)
##

writeRaster(runoffRessmall, filename="step/runoffRes.tif", format="GTiff", overwrite=TRUE)
writeRaster(filledDEMsmall, filename="step/filledDEM.tif", format="GTiff", overwrite=TRUE)

# Flow accumulation
rsaga.geoprocessor(lib="io_gdal", module=0,param=list(GRIDS=paste(getwd(),"step/runoffRes.sgrd",sep="/"), 
                                                      FILES=paste(getwd(),"step/runoffRes.tif",sep="/"), 
                                                      TRANSFORM=TRUE, INTERPOL=1))
rsaga.geoprocessor(lib="io_gdal", module=0, param=list(GRIDS="step/filledDEMnew.sgrd",
                                                       FILES="step/filledDEM.tif",
                                                       TRANSFORM=TRUE, INTERPOL=1))
rsaga.geoprocessor(lib="grid_analysis", module=18, 
                   param=list(SURFACE="step/filledDEMnew.sgrd", INPUT="step/runoffRes.sgrd",
                              FLUX="step/cell_acc.sgrd", OPERATION=0, LINEAR=TRUE, THRES_LINEAR=0))
cellAcc <- raster("step/cell_acc.sdat")
debiet <- cellAcc / 1000 / 24 / 3600 * res(cellAcc)[1] ^ 2
writeRaster(debiet, "step/debiet.tif")

filledDEMsmall <- raster("step/filledDEMnew.sdat")


# head
source("costrasters.R")
head <- HeadOnRiver.large(filledDEMsmall, debiet, minimumDebiet=minimumDebiet, 
                    channelLength=channelLength)
writeRaster(head, "step/head.tif", format="GTiff", overwrite=TRUE)
head <- raster("step/head.tif")

minneighbor <- focal(filledDEMsmall, w=matrix(1,33,33), fun=min)
headtest <- filledDEMsmall - minneighbor

# hydro potential
potential <- debiet * head * 9.81 * 1000 # Joule / second

# filters
potential[debiet < minimumDebiet] <- NA # too low runoff
potential[head < minimumHead] <- NA # too low head

highPotential <- potential
highPotential[potential < minimumPotential] <- NA # highpotential spots
noOfPoints <- ncell(highPotential) - summary(highPotential)[6]
print(paste("Number of points:", noOfPoints))

# output
points <- rasterToPoints(highPotential, spatial=TRUE)
pointswgs <- spTransform(points, CRS=(wgs84))
writeOGR(pointswgs, dsn="output/points", layer= "output/points", 
         driver="KML", dataset_options=c("NameField=layer"))
writeRaster(potential, filename="output/potential.tif", format="GTiff", overwrite=TRUE)
writeRaster(highPotential, filename="output/highPotential.tif", format="GTiff", overwrite=TRUE)
writeRaster(head, filename="step/head.tif", format="GTiff", overwrite=TRUE)
writeRaster(debiet, filename="step/debiet.tif", format="GTiff", overwrite=TRUE)

# plot
require(plotKML)
plotKML(pointswgs, folder.name="output", file.name="output/potentialnew.kml")
