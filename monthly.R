
setwd("/media/boukepieter/schijfje_ottow/thesis/data/TRMM/2b31/monthly")
library(raster)

SumToDayold <- function(year, month, day){
  dayras <- raster(sprintf("3B42.%d%02d%02d.00.7A.tif", year, month, day))
  extent(dayras) <- extent(-180,180,-50,50)
  dayras <- crop(dayras, arealarge)
  for (i in seq(3,21,3)){
    hour3ras <- raster(sprintf("3B42.%d%02d%02d.%02d.7A.tif", year, month, day, i))
    extent(hour3ras) <- extent(-180,180,-50,50)
    hour3ras <- crop(hour3ras, arealarge)
    dayras <- dayras + hour3ras
  }
  writeRaster(dayras, filename=sprintf("daily/%d%02d%02d.tif", year, month, day), format="GTiff", overwrite=TRUE)
}

SumToDaynew <- function(year, month, day){
  dayras <- raster(sprintf("3B42.%d%02d%02d.00.7.tif", year, month, day))
  extent(dayras) <- extent(-180,180,-50,50)
  dayras <- crop(dayras, arealarge)
  for (i in seq(3,21,3)){    
    hour3ras <- raster(sprintf("3B42.%d%02d%02d.%02d.7.tif", year, month, day, i))
    extent(hour3ras) <- extent(-180,180,-50,50)
    hour3ras <- crop(hour3ras, arealarge)
    dayras <- dayras + hour3ras
  }
  writeRaster(dayras, filename=sprintf("daily/%d%02d%02d.tif", year, month, day), format="GTiff", overwrite=TRUE)
}

SumToMonth <- function(year, month, nrd){
  monthras <- raster(sprintf("daily/%d%02d%02d.tif", year, month, 1))
  for (i in 2:nrd){
    dayras <- raster(sprintf("daily/%d%02d%02d.tif", year, month, i))
    monthras <- monthras + dayras
  }
  writeRaster(monthras, filename=sprintf("monthly/%d%02d.tif", year, month), format="GTiff", overwrite=TRUE)
}

SumToYear <- function(year) {
  yearras <- raster(sprintf("monthly/%d%02d.tif", year, 1))
  for (i in 2:12){
    monthras <- raster(sprintf("monthly/%d%02d.tif", year, i))
    yearras <- yearras + monthras
  }
  return(yearras)
}

year <- 2013
days <- c(31,28,31,30,31,30,31,31,30,31,30,31)
for (i in 1:9){
  for (j in 1:days[i]){
    SumToDayold(year, i, j)
    print(sprintf("%d-%02d-%02d", year, i, j))
  }
}
for (i in 1:12){
  for (j in 1:days[i]){
    SumToDaynew(year, i, j)
    print(sprintf("%d-%02d-%02d", year, i, j))
  }
}

for (i in 1:12){
  SumToMonth(year, i, days[i])
  print(sprintf("%d-%02d", year, i))
}

test2011 <- SumToYear(2012)
test2010
plot(test2010)

##################################### new #####################################
setwd("/media/boukepieter/schijfje_ottow/thesis/workspace1b")
source("projectarea.R")

setwd("/media/boukepieter/schijfje_ottow/thesis/data/TRMM/2b31/monthly")
library(raster)
files <- list.files(pattern=".tif")
months <- c("jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec")
days <- c(31,28,31,30,31,30,31,31,30,31,30,31)

for (i in 1:length(files)){
  setwd("/media/boukepieter/schijfje_ottow/thesis/data/TRMM/2b31/monthly")
  P <- raster(files[i])
  P <- crop(P, arealarge)
  projection(P) <- wgs84
  P <- P / days[i]
  P <- projectRaster(P, crs=utm51)
  P <- crop(P, projectarea)
  setwd("/media/boukepieter/schijfje_ottow/thesis/workspace1b/Data/P")
  writeRaster(P, filename=sprintf("P%02d.tif", match(months[sapply(months, grepl, files[i])], months)), 
              format="GTiff", overwrite=TRUE)
}