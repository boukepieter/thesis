library(sp)
library(rgdal)
library(plotKML)
library(raster)
library(gstat)

setwd("E:/thesis/data/reference dem")

utm10 <- CRS("+proj=utm +zone=10 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
lasFiles <- list.files(paste(getwd(), "data", sep="/"), pattern=".laz")#, full.names=TRUE)
process.laz <- function(file){
  system(sprintf('E:/thesis/LAStools/bin/lasground -i "%s" -olas', file))
  system(sprintf('E:/thesis/LAStools/bin/las2txt -i "%s_1.las" -otxt -parse xyzc -sep comma', 
                 substr(file, 1, nchar(file)-4)))
  res <- read.csv(sprintf("%s_1.txt", substr(file, 1, nchar(file)-4)), header=FALSE)
  res <- res[res[4]==2,]
  print(file)
  points <- res[sample(1:nrow(res),25),]
}
selected <- lapply(lasFiles, FUN=process.laz)
merged <- Reduce(function(...) merge(..., all=T), selected)
points <- SpatialPointsDataFrame(merged[,1:2], merged, proj4string=utm10)

aster <- raster("ASTER/ASTGTM2_N45W122_dem.tif")
asterUTM <- projectRaster(aster, crs=utm10)
aster2 <- raster("ASTER/ASTGTM2_N46W122_dem.tif")
asterUTM2 <- projectRaster(aster2, crs=utm10)

plot(asterUTM2)
plot(points, add=T)
extent(asterUTM)

asterValues <- extract(asterUTM2,points)
points$asterValues <- asterValues
points$difference <- points$V3 - points$asterValues

save(points,file="points.Rda")
load("points.Rda")
writeRaster(asterUTM2, filename="asterUTM.tif" , format="GTiff", overwrite=TRUE)
writeOGR(points, dsn=getwd(), layer= "points", 
         driver="ESRI Shapefile", overwrite=TRUE)

## PLOT
aster <- crop(asterUTM2, extent(points))

plot(aster, main="ASTER GDEM and location and error of the lidar points")
plot(points, add=T, pch=21, cex=0.5, col="black", bg="green")
plot(errorpoints3, add=T, pch=21, cex=0.5, bg="yellow")
plot(errorpoints2, add=T, pch=21, cex=0.5, bg="red")
plot(errorpoints, add=T, pch=21, cex=0.5, bg='purple')
legend( "bottomright"
        #, inset = c(0,0.4), 
        , cex = 1, 
        #, bty = "n", 
        , legend = c("> 1000 m", "> 50 m", "> 20 m", "< 20 m"), 
        #, text.col = c("red", "blue"),
        , col = rep("black", 4), 
        , pt.bg = c("purple", "red","yellow", "green")
        , pch = c(21,21,21)
      , title= "DEM error in points"
)

errorpoints3 <- points[abs(points$difference) > 20,]

points <- readOGR(getwd(), "points")
points <- points[abs(points$difference) < 1000,]

points@data[abs(points$difference) > 100,]

m <- mean(points$difference)
sd <- sd(points$difference)
h <- hist(points$difference, breaks=100, xlim=c(-100,100), 
     main="Histogram of points elevation - ASTER GDEM elevation",
     xlab="reference points - ASTER GDEM", col="grey", border="grey")
segments(m, 0, m, 150, lwd = 2, lty=2)
segments(m-sd, 0, m-sd, 150, lwd = 2, lty=3)
segments(m-2*sd, 0, m-2*sd, 150, lwd = 2, lty=3)
segments(m+sd, 0, m+sd, 150, lwd = 2, lty=3)
segments(m+2*sd, 0, m+2*sd, 150, lwd = 2, lty=3)

xfit<-seq(min(points$difference),max(points$difference),length=400) 
yfit<-dnorm(xfit,mean=m,sd=sd)
yfit <- yfit*diff(h$mids[1:2])*length(points$difference) 
lines(xfit, yfit, col="black", lwd=1)

# gstat
g <- gstat(formula = difference~1, data = points)
variogram <- variogram(g, boundaries = c(0, 1:10*50, seq(600,1200,by=100)))
vgm <- vgm(nugget = 0, psill = 300, range = 250, model = "Exp")
vgmf <- fit.variogram(variogram, vgm, fit.method=7)

plot(variogram, vgmf, plot.nu = T, main="Semivariogram for the DEM error")
vgmf

# SSErr attribute is (weighted) sum of squared errors according to fit criterion
attr(vgmf, "SSErr")


#simulation of maps:
load("vgmf.RData")
DEM <- raster("E:/thesis/workspace1b/input/DEM.tif")
filledDEM <- raster("E:/thesis/workspace1b/step/filledDEM.tif")
clippedDEM <- crop(DEM, filledDEM)
mask <- clippedDEM
mask <- as(mask, 'SpatialGridDataFrame')
mask[!is.na(mask@data)] <- 1
points <- rasterToPoints(clippedDEM, fun=function(x){!is.na(x)}, spatial=TRUE)
test <- krige(DEM~1, points, newdata=mask, model=vgmf, nsim=1, nmax=3)

gpb <- gstat(id = c("DEM"), formula = DEM~1, data = points, model=vgmf, nmax=25)
projection(mask) <- NA
DEM.sim <- predict.gstat(gpb, mask, nsim=3, debug.level=-1)

