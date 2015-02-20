setwd("E:/thesis/data/reference dem/data")
library(sp)
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

points <- points[abs(points$difference) < 1000,]

points@data[abs(points$difference) > 100,]
hist(points$difference, breaks=100, xlim=c(-100,100))
mean(points$difference)
sd(points$difference)

# gstat
g <- gstat(formula = difference~1, data = points)
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
set.seed(2373)
geul.sim <- krige(difference~1, points, newdata = asterUTM2, vgmf, nsim = 3, nmax = 24)
spplot (geul.sim, zcol = c("sim1", "sim3", "sim8", "sim15"), xlim=c(190200,191300), ylim=c(314300,315600),
        col.regions = bpy.colors())
