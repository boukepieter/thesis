# Author: B.P. Ottow

################################# Model starts ############################################
HydroPower <- function(DEMfile = "input/DEM.tif", Pfile = "input/P.tif", ETfile = "input/ET.tif",
                       channelLength = 500, minimumHead = 25, minimumDebiet = 0.1, minimumPotential = 1000000,
                       plotToGE = TRUE, work_env = "", plotMethod = "raster") {
# load in input and packages
require(raster)
require(rgdal)
require(sp)
require(RSAGA)

if(work_env == ""){
  work_env <- rsaga.env()
}


dir.create("step")
dir.create("output")
ET <- raster(ETfile)
P <- raster(Pfile)
DEM <- raster(DEMfile)

# runoff calc
print("calculating runoff...")
Presample <- resample(P, ET, method='bilinear')
runoff <- Presample - ET
runoff[runoff < 0] <- 0
runoffRes <- resample(runoff, DEM, method='bilinear')
writeRaster(runoffRes, filename="step/runoffRes.tif", format="GTiff", overwrite=TRUE)

# Fill DEM:
print("filling DEM...")
rsaga.geoprocessor(lib="io_gdal", module=0,param=list(GRIDS=paste(getwd(),"step/DEM.sgrd",sep="/"), 
                                                      FILES=paste(getwd(), DEMfile,sep="/"), 
                                                      TRANSFORM=TRUE, INTERPOL=1),
                   env=work_env, show.output.on.console = FALSE, warn = FALSE)
rsaga.geoprocessor(lib="ta_preprocessor", module=3, 
                   param=list(DEM="step/DEM.sgrd", RESULT="step/filledDEM.sgrd", MINSLOPE=0.01),
                   env=work_env, show.output.on.console = FALSE, warn = FALSE)
filledDEM <- raster("step/filledDEM.sdat")

# Flow accumulation
print("calculating flow accumulation...")
rsaga.geoprocessor(lib="io_gdal", module=0,param=list(GRIDS=paste(getwd(),"step/runoffRes.sgrd",sep="/"), 
                                                      FILES=paste(getwd(),"step/runoffRes.tif",sep="/"), 
                                                      TRANSFORM=TRUE, INTERPOL=1),
                   env=work_env, show.output.on.console = FALSE, warn = FALSE)
file.copy(c("step/filledDEM.sgrd", "step/filledDEM.sdat", "step/filledDEM.prj", "step/filledDEM.mgrd"),
          c("step/runoffResa.sgrd", "step/runoffResa.sdat", "step/runoffResa.prj", "step/runoffResa.mgrd"))
rsaga.geoprocessor(lib="grid_tools", module=0,
                   param=list(INPUT="step/runoffRes.sgrd", KEEP_TYPE=TRUE,
                              TARGET=1, SCALE_UP_METHOD=1, SCALE_DOWN_METHOD=1,
                              GRID_GRID="step/runoffResa.sgrd"),
                   env=work_env, show.output.on.console = FALSE, warn = FALSE)
rsaga.geoprocessor(lib="grid_analysis", module=18, 
                   param=list(SURFACE="step/filledDEM.sgrd", INPUT="step/runoffResa.sgrd",
                              FLUX="step/cell_acc.sgrd", OPERATION=0),
                   env=work_env, show.output.on.console = FALSE, warn = FALSE)
cellAcc <- raster("step/cell_acc.sdat")
debiet <- cellAcc / 1000 / 24 / 3600 * res(cellAcc)[1] ^ 2


# head
print("calculating head...")
source("costrasters.R")
head <- HeadOnRiver.large(filledDEM, debiet, minimumDebiet=minimumDebiet, 
                    channelLength=channelLength)

# hydro potential
print("calculating potential...")
potential <- debiet * head * 9.81 * 1000 # Joule / second

# filters
potential[debiet < minimumDebiet] <- NA # too low runoff
potential[head < minimumHead] <- NA # too low head

highPotential <- potential
highPotential[potential < minimumPotential] <- NA # highpotential spots
noOfPoints <- ncell(highPotential) - summary(highPotential)[6]
print(paste("Number of points:", noOfPoints))

# output
print("saving output...")
wgs84 = CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')
writeRaster(potential, filename="output/potential.tif", format="GTiff", overwrite=TRUE)
writeRaster(highPotential, filename="output/highPotential.tif", format="GTiff", overwrite=TRUE)
#writeRaster(head, filename="step/head.tif", format="GTiff", overwrite=TRUE)
#writeRaster(debiet, filename="step/debiet.tif", format="GTiff", overwrite=TRUE)
if(noOfPoints == 0) stop(paste("No points found with a potential higher than", minimumPotential))
points <- rasterToPoints(highPotential, spatial=TRUE)
pointswgs <- spTransform(points, CRS=(wgs84))
writeOGR(pointswgs, dsn="output/points", layer= "output/points", 
         driver="ESRI Shapefile")



# plot
require(plotKML)
names(potential) <- "potentialRaster"
names(pointswgs) <- "potentialVector"
data(SAGA_pal)
setwd("output")
if (plotMethod == "raster"){
  kml(potential / 1000, colour_scale = SAGA_pal[[1]], colour = potentialRaster, 
      file.name="potentialRaster.kml") 
} else if (plotMethod == "vector"){
  kml(pointswgs, shape = "http://maps.google.com/mapfiles/kml/pal2/icon18.png", 
      size = potentialVector, colour = potentialVector, 
      labels = paste(round(pointswgs$potentialVector / 1000), "kW"), file.name="potentialVector.kml")
} else if (plotMethod == "both") {
  kml(potential / 1000, colour_scale = SAGA_pal[[1]], colour = potentialRaster, 
      file.name="potentialRaster.kml") 
  kml(pointswgs, shape = "http://maps.google.com/mapfiles/kml/pal2/icon18.png", 
      size = potentialVector, colour = potentialVector, 
      labels = paste(round(pointswgs$potentialVector / 1000), "kW"), file.name="potentialVector.kml")
}

}

