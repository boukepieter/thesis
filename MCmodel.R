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
  
  if(class(work_env) == "character"){
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
  
  # Fill DEM:
  rsaga.geoprocessor(lib="io_gdal", module=0,param=list(GRIDS=paste(getwd(),"step/DEM.sgrd",sep="/"), 
                                                        FILES=paste(getwd(), DEMfile,sep="/"), 
                                                        TRANSFORM=TRUE, INTERPOL=1),
                     env=work_env, show.output.on.console = FALSE, warn = FALSE)
  print("filling DEM")
  rsaga.geoprocessor(lib="ta_preprocessor", module=3, 
                     param=list(DEM="step/DEM.sgrd", RESULT="step/filledDEM.sgrd", MINSLOPE=0.01),
                     env=work_env, show.output.on.console = FALSE, warn = FALSE)
  filledDEM <- raster("step/filledDEM.sdat")
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
  rm(P,ET)
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
  
  rm(Presample,runoff, cellAcc)
  # Runoff delay scenarios
  print("calculating runoff scenario's")
  source("scripts/runoffStorage.R")
  factors <- list(0.4,0.7)
  runoffs <- lapply(factors,FUN=storage.fun,debiet=debiet,noSteps=noSteps)
  poi <- SpatialPoints(matrix(c(737522.424973, 706557.393475),nrow=1),
                       proj4string=CRS(projection(debiet[[1]])))
  #testplot.storages(runoffs,debiet,poi, factors)
  runoffs <- c(debiet, runoffs)
  
  # head
  print("calculating head...")
  source("scripts/costrasters.R")
  head <- HeadOnRiver.large(filledDEM, max(debiet), minimumDebiet=minimumDebiet, 
                            channelLength=channelLength,cores=6)
  writeRaster(head, filename="step/head.tif", format="GTiff", overwrite=TRUE)
  #head <- raster("step/head.tif")
  
  # hydro potential
  print("calculating potential...")
  runoffs <- lapply(runoffs, FUN=crop, y=head)
  potentials <- lapply(runoffs, FUN=function(x, head) {x * head * 9.81 * 1000}, head=head) # Joule / second
  
  
  
  # filters
  print("applying filters")
  filter.fun <- function(pot, deb, head, minDeb, minHead){
    pot[deb < minDeb] <- NA
    pot[head < minHead] <- NA
    pot
  }
  potentials <- mapply(FUN=filter.fun, pot=potentials, deb=runoffs, 
                       MoreArgs=list(head=head, minDeb=minimumDebiet, minHead=minimumHead),
                       SIMPLIFY=FALSE)
  #names <- lapply(1:3,FUN=function(x){sprintf("output/potential%i_%02d.tif", x, 1:noSteps)})
  #mapply(writeRaster, potentials, names, MoreArgs=list(bylayer=TRUE, format="GTiff", 
  #                                                     overwrite=TRUE))
  #potentials <- lapply(X=names, FUN=stack)
  highPotentials <- potentials
  print("calculating high potentials")
  high.pot <- function(x){
    x[x < minimumPotential] <- NA
    return(x)
  }  
  highPotentials <- mapply(FUN=calc, highPotentials, MoreArgs=list(fun=high.pot))
  
  points.fun <-function(x){
    y <- ncell(x) - summary(x)[6,]
    return(y)
  } 
  noOfPoints <- lapply(highPotentials, FUN=points.fun)
  print("Number of points:")
  print(noOfPoints)
  
  # output
  print("saving output...")
  source("scripts/plotPotential.R")
  setwd("output")
  names <- lapply(1:3,FUN=function(x){sprintf("potential%i_%02d.tif", x, 1:noSteps)})
  mapply(writeRaster, potentials, names, MoreArgs=list(bylayer=TRUE, format="GTiff", 
                                                       overwrite=TRUE))
  names <- lapply(1:3,FUN=function(x){sprintf("highPotential%i_%02d.tif", x, 1:noSteps)})
  mapply(writeRaster, highPotentials, names, MoreArgs=list(bylayer=TRUE, format="GTiff", 
                                                           overwrite=TRUE))
  #writeRaster(debiet, filename=sprintf("../step/debiet%02d.tif", 1:noSteps),
  #            bylayer=TRUE, format="GTiff", overwrite=TRUE)
  
  # tot hierrrrr....
  # plot
#   names(potential) <- sprintf("potentialRaster%02d", seq(1,12))
#   names(highPotential) <- sprintf("potentialRaster%02d", seq(1,12))
#   if (plotMethod == "raster"){
#     PlotTimeRaster()
#   } else if (plotMethod == "vector"){
#     PlotTimeVector()
#   } else if (plotMethod == "both") {
#     PlotTimeRaster()
#     PlotTimeVector()
#   }
  
  
}
