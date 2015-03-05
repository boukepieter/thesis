require(RSAGA)
require(sp)
require(raster)
require(rgdal)

#TODO(projection from input)

getCatchment <- function(DEMfile, coord, work_env){
  rsaga.geoprocessor(lib="io_gdal", module=0,param=list(GRIDS=paste(getwd(),"step/DEM.sgrd",sep="/"), 
                                                        FILES=paste(getwd(), DEMfile,sep="/"), 
                                                        TRANSFORM=TRUE, INTERPOL=1),
                     env=work_env, show.output.on.console = FALSE, warn = FALSE)
  print("filling DEM")
  # fill DEM
  rsaga.geoprocessor(lib="ta_preprocessor", module=3, 
                     param=list(DEM="step/DEM.sgrd", RESULT="step/filledDEM.sgrd", MINSLOPE=0.01),
                     env=work_env, show.output.on.console = FALSE, warn = FALSE)
  print("calculating rivers")
  # flow accum
  rsaga.geoprocessor(lib="ta_hydrology", module=0, 
                     param=list(ELEVATION=sprintf("%s/step/filledDEM.sgrd", getwd()), 
                                CAREA=sprintf("%s/step/carea.sgrd", getwd())),
                     env=work_env, show.output.on.console = FALSE, warn = FALSE)
  print("calculating strahler")
  # Strahler
  rsaga.geoprocessor(lib="ta_channels", module=6, 
                     param=list(DEM=sprintf("%s/step/filledDEM.sgrd", getwd()), 
                                STRAHLER=sprintf("%s/step/strahler.sgrd", getwd())),
                     env=work_env, show.output.on.console = FALSE, warn = FALSE)
  
  # Strahler calc
  rsaga.geoprocessor(lib="grid_calculus", module=1, 
                     param=list(GRIDS=sprintf("%s/step/strahler.sgrd", getwd()), 
                                RESULT=sprintf("%s/step/strahler8.sgrd", getwd()),
                                FORMULA="ifelse(g1>7,1,-99999)"),
                     env=work_env, show.output.on.console = FALSE, warn = FALSE)
  print("clipping on land")
  # Land clip
  rsaga.geoprocessor(lib="grid_calculus", module=1, 
                     param=list(GRIDS=sprintf("%s/step/strahler8.sgrd;%s/step/DEM.sgrd", getwd(), getwd()), 
                                RESULT=sprintf("%s/step/strahlerLand.sgrd", getwd()),
                                FORMULA="ifelse(g2>0,g1,-99999)"),
                     env=work_env, show.output.on.console = FALSE, warn = FALSE)
  
  # Land clip for DEM
  rsaga.geoprocessor(lib="grid_calculus", module=1, 
                     param=list(GRIDS=sprintf("%s/step/filledDEM.sgrd;%s/step/DEM.sgrd", getwd(), getwd()), 
                                RESULT=sprintf("%s/step/DEMLand.sgrd", getwd()),
                                FORMULA="ifelse(g2>0,g1,-99999)"),
                     env=work_env, show.output.on.console = FALSE, warn = FALSE)
  print("calculating watersheds")
  # Watershed
  rsaga.geoprocessor(lib="ta_channels", module=2, 
                     param=list(DEM=sprintf("%s/step/DEMland.sgrd", getwd()), 
                                CHANNELS=sprintf("%s/step/strahlerLand.sgrd", getwd()), 
                                BASINS=sprintf("%s/step/basins.sgrd", getwd())),
                     env=work_env, show.output.on.console = FALSE, warn = FALSE)
  print("selecting watershed")
  ws <- raster("step/basins.sdat")
  wgs84 = CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')
  utm51 = CRS('+proj=utm +zone=51 +ellps=WGS84 +datum=WGS84 +units=m +no_defs')
  point <- SpatialPoints(coord, proj4string=wgs84)
  pointUTM <- spTransform(point, utm51)
  wsNumber <- extract(ws, pointUTM)
  rsaga.geoprocessor(lib="shapes_grid", module=6, 
                     param=list(GRID=sprintf("%s/step/basins.sgrd", getwd()), 
                                POLYGONS=sprintf("%s/step/watershed.shp", getwd()), 
                                CLASS_ALL="0", CLASS_ID=wsNumber),
                     env=work_env, show.output.on.console = FALSE, warn = FALSE)
  wsSHP <- readOGR("step", layer="watershed")
  filledDEM <- raster("step/filledDEM.sdat") 
  filledDEMsmall <- crop(filledDEM, extent(wsSHP))
  print("catchment clipping done")
  return(filledDEMsmall)
}

# TEST
if (FALSE){
  setwd("E:/thesis/workspace1b")
  DEMfile <- "input/DEM.tif"
  require(RSAGA)
  work_env = rsaga.env(path="C:/Program Files (x86)/SAGA_GIS")
  
  filledDEM <- raster("step/filledDEM.sdat")  
  test <- raster("step/basinsExt3.sdat")
  plot(test)
  plotKML(test)
  eDEM <- raster(DEMfile)
  
  land <- calc(DEM, fun=function(x){elseif(x>0,1,NA)})
  
  test2[DEM == 0] <- NA
  test2 <- test
  test2[which(DEM == 0)] <- NA
  
  coord <- matrix(c(125.175278, 6.383889), nrow=1)
  
}