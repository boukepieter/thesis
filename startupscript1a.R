# Author: B.P. Ottow

# parameters
#utm51 = CRS('+proj=utm +zone=51 +ellps=WGS84 +datum=WGS84 +units=m +no_defs')
setwd("E:/thesis/workspace/results/1a")
sourceCode <- "E:/thesis/workspace/thesis" # gaia windows
channelLength = 500
minimumHead = 25
minimumDebiet = 0.1
minimumPotential = 10000

# files
ETfile <- "input/ET.tif"
Pfile <- "input/P.tif"
DEMfile <- "input/DEM.tif"

# scripts
flist <- list.files(sourceCode, ".+[.]R$", full.names = TRUE)
dir.create(paste(getwd(), "scripts", sep="/"))
file.copy(flist, paste(getwd(), "scripts", sep="/"))

require(RSAGA)
work_env = rsaga.env(path="C:/Program Files (x86)/SAGA_GIS") # windows

source("scripts/model1a.R")
HydroPower(DEMfile, Pfile, ETfile, minimumPotential = minimumPotential, minimumDebiet = minimumDebiet,
           minimumHead = minimumHead, channelLength = channelLength, work_env=work_env)
