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
minimumPotential = 10000
coord <- matrix(c(125.175278, 6.383889), nrow=1)
require(RSAGA)
work_env = rsaga.env(path="C:/Program Files (x86)/SAGA_GIS") # windows

# files
#setwd("/media/boukepieter/schijfje_ottow/thesis/workspace") # linux
i <- 1
sourceCode <- "E:/thesis/workspace/thesis" # gaia windows
sourceCode <- "D:/MCAnalyse/scripts"
setwd("E:/thesis/workspace/MCAnalyse/")
dir.create(sprintf("run%03d", i))
setwd(sprintf("run%03d", i))
ETdir <- "../input/ET"
Pdir <- "../input/P"
DEMfile <- "../input/DEM/DEM001.tif"
flist <- list.files(sourceCode, ".+[.]R$", full.names = TRUE)
dir.create(paste(getwd(), "scripts", sep="/"))
file.copy(flist, paste(getwd(), "scripts", sep="/"))

# model
source("scripts/MCmodel.R")
#HydroPowerMonthly(DEMfile, Pdir, ETdir, coord=coord, minimumPotential = minimumPotential, minimumDebiet = minimumDebiet,
#           minimumHead = minimumHead, channelLength = channelLength, work_env=work_env, plotMethod="None")

for (i in 1:20){
  setwd("D:/MCAnalyse")
  dir.create(sprintf("run%03d", i))
  setwd(sprintf("run%03d", i))
  DEMfile <- sprintf("../input/DEM/DEM%03d.tif", i)
  dir.create(paste(getwd(), "scripts", sep="/"))
  file.copy(flist, paste(getwd(), "scripts", sep="/"))
  HydroPowerMonthly(DEMfile, Pdir, ETdir, coord=coord, minimumPotential = minimumPotential, minimumDebiet = minimumDebiet,
                    minimumHead = minimumHead, channelLength = channelLength, work_env=work_env, plotMethod="None")
  unlink("../step", recursive=TRUE)
  unlink("../scripts", recursive=TRUE)
}

