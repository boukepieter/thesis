# Author: B.P. Ottow

################################# Model starts ############################################
HydroPowerMonthly <- function(DEMfile = "input/DEM.tif", Pdir = "input/P", ETdir = "input/ET", coord,
                              channelLength = 500, minimumHead = 25, minimumDebiet = 0.1, minimumPotential = 100000,
                              plotToGE = TRUE, work_env = "", plotMethod = "raster") {
  # load in input and packages
  require(raster)
  require(rgdal)
  require(sp)
  require(RSAGA)
  
  if(type(work_env) == "character"){
    work_env <- rsaga.env()
  }
  
  dir.create("step")
  dir.create("output")
  rasterOptions(tmpdir=paste(getwd(),"step", sep="/")) 
  
  ETfiles <- list.files(ETdir, pattern=".tif", full.names=TRUE)
  Pfiles <- list.files(Pdir, pattern=".tif", full.names=TRUE)
  noSteps <- min(length(ETfiles), length(Pfiles))
  
  ET <- stack(ETfiles[1:noSteps])
  P <- stack(Pfiles[1:noSteps])
  
  # Fill and clip DEM:
  source("scripts/watershed.R")
  filledDEM <- getCatchment(DEMfile, coord, work_env)
  writeRaster(filledDEM, filename="step/filledDEM.tif", format="GTiff", overwrite=TRUE)
  rsaga.geoprocessor(lib="io_gdal", module=0,param=list(GRIDS=paste(getwd(),"step/filledDEM.sgrd",sep="/"), 
                                                        FILES=paste(getwd(), "step/filledDEM.tif",sep="/"), 
                                                        TRANSFORM=TRUE, INTERPOL=1),
                     env=work_env, show.output.on.console = FALSE, warn = FALSE)
  
  # runoff calc
  print("calculating runoff...")
  Presample <- resample(P, ET, method='bilinear')
  runoff <- Presample - ET
  runoffRes <- resample(runoff, filledDEM, method='bilinear')
  for (i in 1:noSteps){
    writeRaster(runoffRes[[i]], filename=sprintf("step/runoffRes%02d.tif",i), format="GTiff", overwrite=TRUE)  
  }
  
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
  debiet <- calc(cellAcc, fun=function(x) x / 1000 / 24 / 3600 * res(cellAcc)[1] ^ 2)
  
  # head
  print("calculating head...")
  source("scripts/costrasters.R")
  head <- HeadOnRiver.large(filledDEM, max(debiet), minimumDebiet=minimumDebiet, 
                            channelLength=channelLength)
  writeRaster(head, filename="step/head.tif", format="GTiff", overwrite=TRUE)
  
  # hydro potential
  print("calculating potential...")
  debiet <- crop(debiet, potential)
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
  source("scripts/plotPotential.R")
  data(SAGA_pal)
  setwd("output")
  for (i in 1:noSteps){
    writeRaster(potential[[i]], filename=sprintf("potential%02d.tif", i), format="GTiff", overwrite=TRUE)
    #writeRaster(highPotential[[i]], filename=sprintf("highPotential%02d.tif", i), format="GTiff", overwrite=TRUE)
    writeRaster(debiet[[i]], filename=sprintf("../step/debiet%02d.tif", i), format="GTiff", overwrite=TRUE)
    #   if(noOfPoints[i] == 0) {
    #     warning(paste("No points found with a potential higher than", minimumPotential, "in step", i))
    #     next
    #   }
  }
  # potential <- stack(sprintf("potential%02d.tif", 1:noSteps))
  
  
  # plot
  names(potential) <- sprintf("potentialRaster%02d", seq(1,12))
  names(highPotential) <- sprintf("potentialRaster%02d", seq(1,12))
  if (plotMethod == "raster"){
    PlotTimeRaster()
  } else if (plotMethod == "vector"){
    PlotTimeVector()
  } else if (plotMethod == "both") {
    PlotTimeRaster()
    PlotTimeVector()
  }
  
  
}

require(raster)
library(topmodel)
setwd("/media/boukepieter/schijfje_ottow/thesis/workspace1b")
noSteps <- 12
head <- raster("step/head.tif")
filledDEM <- raster("step/filledDEM.sdat")
cellAcc <- raster("step/cell_acc01.sdat")
debiet <- cellAcc / 1000 / 24 / 3600 * res(cellAcc)[1] ^ 2
potential <- debiet * head * 9.81 * 1000 # Joule / second

# define point
test <- potential
test[test < 750000] <- NA
point <- rasterToPoints(test, spatial=TRUE)
coord <- point@coords
coord <- c(rowFromY(test, coord[2]), colFromX(test, coord[1]))

# catchment
catch <- subcatch(as.matrix(filledDEM), coord)
cat <- filledDEM
cat[] <- catch

# topographic index
topi <- topidx(as.matrix(filledDEM), res(filledDEM)[1])
atb <- filledDEM
atb[] <- topi$atb
atb <- mask(atb, cat, maskvalue=0)
topix <- make.classes(atb[],10)
area <- filledDEM
area[] <- topi$area
plot(area)

# parameters & delay
data(huagrahuma)
attach(huagrahuma)
parameters["qs0"] <- 2e-4
parameters["lnTe"] <- 25
parameters["m"] <- 0.06
parameters["dt"] <- 24 * 30
parameters["Sr0"] <- 0.15
parameters["k0"] <- 2
parameters["td"] <- 109
parameters["CD"] <- 6.5
parameters["Srmax"] <- 0.0022
parameters["vr"] <- 0.06

delay

# rain
Pres <- resample(P, filledDEM, method="bilinear")
r <- zonal(Pres, cat, 'mean')
rain <- as.vector(r[2,2:length(r[2,])])
rain <- rain / 1000 * 30

# ET0
ETres <- resample(ET, filledDEM, method="bilinear")
et <- zonal(ETres, cat, 'mean')
et0 <- as.vector(et[2,2:length(et[2,])]) + 0.5
et0 <- et0 / 1000 * 30

save.image("/media/boukepieter/schijfje_ottow/thesis/workspace1b/topmodelEnvi.RData")
load("/media/boukepieter/schijfje_ottow/thesis/workspace1b/topmodelEnvi.RData")
# topmodel
Qsim <- topmodel(parameters, topix, delay, rain, et0, verbose = TRUE)
plot(as.vector(debiet[coord[1],coord[2]]), col="blue", type="l", ylim=c(0,4))
points(Qsim$Q * topi$area[coord[1],coord[2]] / 30 / 24 / 3600, col="red", type="l")

as.vector(debiet[coord[1],coord[2]])

summary(cellAccS[[1]])[5]

debiet <- cellAccS
function(){
  for (i in seq(1,12)){
    debiet[[i]][] <- 0
    for (j in seq(0,11)){
      k <- ifelse((i - j) < 1, i - j + 12, i - j)
      print(k)
      debiet[[i]] <- debiet[[i]] + cellAccS[[k]] * 0.5 ^ j
      print(paste(i,j))
    }
  }
}
