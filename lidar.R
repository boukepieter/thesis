setwd("E:/thesis/data/reference dem")
library(sp)
library(rgdal)
library(plotKML)
library(raster)
library(gstat)

utm10 <- CRS("+proj=utm +zone=10 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
lasFiles <- list.files(getwd(), pattern=".laz")#, full.names=TRUE)

file <- lasFiles[1]
process.laz <- function(file){
  system(sprintf('E:/thesis/LAStools/bin/lasground -i "%s" -olas', file))
  system(sprintf('E:/thesis/LAStools/bin/las2txt -i "%s_1.las" -otxt -parse xyzc -sep comma', substr(file, 1, nchar(file)-4)))
  res <- read.csv(sprintf("%s_1.txt", substr(file, 1, nchar(file)-4)), header=FALSE)
  res <- res[res[4]==2,]
  print(file)
  points <- res[sample(1:nrow(res),25),]
}

selected <- lapply(lasFiles, FUN=process.laz)
test <- Reduce(function(...) merge(..., all=T), selected)
points <- SpatialPointsDataFrame(test[,1:2], test, proj4string=utm10)

setwd("..")
aster <- raster("ASTER/ASTGTM2_N45W122_dem.tif")
asterUTM <- projectRaster(aster, crs=utm10)
aster2 <- raster("ASTER/ASTGTM2_N46W122_dem.tif")
asterUTM2 <- projectRaster(aster2, crs=utm10)

plot(asterUTM)
plot(points, add=T)
extent(asterUTM)

asterValues <- extract(asterUTM2,points)
points$asterValues <- asterValues
points$difference <- points$V3 - points$asterValues

save(points,file="points.Rda")
writeRaster(asterUTM2, filename="asterUTM.tif" , format="GTiff", overwrite=TRUE)
writeOGR(points, dsn=getwd(), layer= "points", 
         driver="ESRI Shapefile", overwrite=TRUE)

## after reading
points <- readOGR(getwd(), "points")
points <- points[abs(points$diffrnc) < 1000,]

points@data[abs(points$diffrnc) > 100,]
hist(points$diffrnc, breaks=100, xlim=c(-100,100))
mean(points$diffrnc)
sd(points$diffrnc)

# gstat
g <- gstat(formula = diffrnc~1, data = points)
variogram <- variogram(g)
variogram <- variogram(g, boundaries = c(0, 1:10*50, seq(600,1200,by=100)))
plot(variogram)

vgm <- vgm(nugget = 0, psill = 300, range = 250, model = "Exp")
plot(variogram, vgm, plot.nu = T)
vgmf <- fit.variogram(variogram, vgm, fit.method=7)
plot(variogram, vgmf, plot.nu = T)
vgmf

# SSErr attribute is (weighted) sum of squared errors according to fit criterion
attr(vgmf, "SSErr")

# simulation of maps
geul <- read.table("SMS/geuldata.txt", header = TRUE)
coordinates(geul) <- ~x+y
mask <- readGDAL("SMS/geul_mask.txt")
gpb <- gstat(id = c("pb"), formula = pb~1, data = geul)
vgpb <- variogram(gpb, boundaries = c(50,100,150,200,300,400,600,800,1000))
vgmpb <- vgm(psill = 15000, range = 400, model = "Sph", add.to = vgm(psill = 1000, range = 100, model = "Sph"))
vgmpb <- fit.variogram(vgpb,vgmpb)
plot(vgpb,vgmpb, plot.numbers = TRUE)

geul.krig <- krige(pb~1, geul, newdata = mask, vgmpb)
MC <- 100
set.seed(2373)
geul.sim <- krige(pb~1, geul, newdata = mask, vgmpb, nsim = 100, nmax = 24)

plot(asterUTM2)
ext <- drawExtent()
test <- crop(asterUTM2, ext)
test <- as(test, 'SpatialGridDataFrame')
test[!is.na(test@data)] <- 1
projection(test) <- NA
set.seed(2373)
aster.sim <- krige(V3~1, points, newdata=test, model=vgmf, nsim = 3, nmax = 3)
spplot (DEM.sim, zcol = c("sim1", "sim2", "sim3"), #xlim=c(190200,191300), ylim=c(314300,315600),
        col.regions = bpy.colors())
save(vgmf,file="vgmf.RData")

spplot (DEM.sim[1], zcol = "sim1", #xlim=c(190200,191300), ylim=c(314300,315600),
        col.regions = bpy.colors())

#now for my study area:
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

