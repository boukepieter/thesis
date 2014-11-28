# Author: B.P. Ottow

# parameters
#utm51 = CRS('+proj=utm +zone=51 +ellps=WGS84 +datum=WGS84 +units=m +no_defs')
setwd("/media/boukepieter/schijfje_ottow/thesis/workspace/test")
channelLength = 500
minimumHead = 25
minimumDebiet = 0.1
minimumPotential = 100000

# files
ETfile <- "input/ETs.tif"
Pfile <- "input/Ps.tif"
DEMfile <- "input/DEMs.tif"

source("model1a.R")
HydroPower(DEMfile, Pfile, ETfile, minimumPotential = minimumPotential, minimumDebiet = minimumDebiet,
           minimumHead = minimumHead, channelLength = channelLength)
