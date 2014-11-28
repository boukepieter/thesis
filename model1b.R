# Author: B.P. Ottow

################################# Model starts ############################################
HydroPowerMonthly <- function(DEMfile = "input/DEM.tif", Pdir = "input/P", ETdir = "input/ET",
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

ETfiles <- list.files(ETdir, pattern=".tif", full.names=TRUE)
Pfiles <- list.files(Pdir, pattern=".tif", full.names=TRUE)
noSteps <- min(length(ETfiles), length(Pfiles))

ET <- stack(ETfiles[1:noSteps])
P <- stack(Pfiles[1:noSteps])
DEM <- raster(DEMfile)

# runoff calc
print("calculating runoff...")
Presample <- resample(P, ET, method='bilinear')
runoff <- Presample - ET
runoffRes <- resample(runoff, DEM, method='bilinear')
for (i in 1:noSteps){
  writeRaster(runoffRes[[i]], filename=sprintf("step/runoffRes%02d.tif",i), format="GTiff", overwrite=TRUE)  
}

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
for (i in 1:noSteps) {
  rsaga.geoprocessor(lib="io_gdal", module=0,param=list(GRIDS=paste(getwd(),sprintf("step/runoffRes%02d.tif",i),sep="/"), # moet zonder getwd kunnen
                                                        FILES=paste(getwd(),sprintf("step/runoffRes%02d.tif",i),sep="/"), 
                                                        TRANSFORM=TRUE, INTERPOL=1),
                     env=work_env, show.output.on.console = FALSE, warn = FALSE)
  file.copy(c("step/filledDEM.sgrd", "step/filledDEM.sdat", "step/filledDEM.prj", "step/filledDEM.mgrd"),
            c("step/runoffResa.sgrd", "step/runoffResa.sdat", "step/runoffResa.prj", "step/runoffResa.mgrd"))
  rsaga.geoprocessor(lib="grid_tools", module=0,
                     param=list(INPUT=sprintf("step/runoffRes%02d.sgrd",i), KEEP_TYPE=TRUE,
                                TARGET=1, SCALE_UP_METHOD=1, SCALE_DOWN_METHOD=1,
                                GRID_GRID="step/runoffResa.sgrd"),
                     env=work_env, show.output.on.console = FALSE, warn = FALSE)
  rsaga.geoprocessor(lib="grid_analysis", module=18, 
                     param=list(SURFACE="step/filledDEM.sgrd", INPUT="step/runoffResa.sgrd",
                                FLUX=sprintf("step/cell_acc%02d.sgrd",i), OPERATION=0),
                     env=work_env, show.output.on.console = FALSE, warn = FALSE)
  print(sprintf("%d step(s) finished", i))
  file.remove(c("step/runoffResa.sgrd", "step/runoffResa.sdat", "step/runoffResa.prj", "step/runoffResa.mgrd"))
}

cellAcc <- stack(sprintf("step/cell_acc%02d.sdat", 1:noSteps))
debiet <- cellAcc / 1000 / 24 / 3600 * res(cellAcc)[1] ^ 2

# head
print("calculating head...")
source("costrasters.R")
head <- HeadOnRiver.large(filledDEM, max(debiet), minimumDebiet=minimumDebiet, 
                    channelLength=channelLength)

# hydro potential
print("calculating potential...")
potential <- debiet * head * 9.81 * 1000 # Joule / second

# filters
for (i in 1:noSteps){
  potential[[i]][debiet[[i]] < minimumDebiet] <- NA
  potential[[i]][head < minimumHead] <- NA
}

highPotential <- potential
for (i in 1:noSteps){
  highPotential[[i]][potential[[i]] < minimumPotential] <- NA # highpotential spots  
}
noOfPoints <- ncell(highPotential) - summary(highPotential)[6,]
print("Number of points:")
print(noOfPoints)

# output
print("saving output...")
data(SAGA_pal)
setwd("output")
for (i in 1:noSteps){
  writeRaster(potential[[i]], filename=sprintf("potential%02d.tif", i), format="GTiff", overwrite=TRUE)
  writeRaster(highPotential[[i]], filename=sprintf("highPotential%02d.tif", i), format="GTiff", overwrite=TRUE)
  #writeRaster(head, filename="step/head.tif", format="GTiff", overwrite=TRUE)
  #writeRaster(debiet, filename="step/debiet.tif", format="GTiff", overwrite=TRUE)
  if(noOfPoints[i] == 0) {
    warning(paste("No points found with a potential higher than", minimumPotential, "in step", i))
    next
  }
}

# plot
source("../plotPotential.R")
names(potential) <- sprintf("potentialRaster%02d", seq(1,12))
if (plotMethod == "raster"){
  PlotTimeRaster()
} else if (plotMethod == "vector"){
  PlotTimeVector()
} else if (plotMethod == "both") {
  PlotTimeRaster()
  PlotTimeVector()
}


}



