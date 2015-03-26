require(raster)
require(RColorBrewer)
library(dismo)

resetPar <- function() {
  dev.new()
  op <- par(no.readonly = TRUE)
  dev.off()
  op
}

cols <- rev(brewer.pal(11,"Spectral")[c(1,2,3,4,5,8,9,10,11)])
brks <- c(10000,20000,50000,100000,200000,400000,750000,1500000,3000000)
nms <- c(">10 kW", ">20 kW", ">50 kW", ">100 kW", ">200 kW", ">400 kW", ">750 kW", ">1.5 MW", ">3.0 MW")
mymap <- gmap(crwgs, type='satellite',rgb=T, lonlat=T, zoom=12)
ext <- c(736411, 741789, 704418,707888)
months <- c("January", "February", "March", "April", "May", "June", "July", "August",
            "September", "October", "November", "December")

### testPLOT

plot(highPotential, main="Potential in Watt with increased spatial resolution",
     zlim=c(0,2000000), col=cols, breaks=brks,
     xlim=c(736002, 744138), ylim=c(700691, 709433), legend=F)
high <- 4
legend("bottomleft", legend = nms[1:high], fill = cols[1:high])
setwd("E:/thesis/workspace/results/1a")
highPotential <- raster("output/highPotential.tif")
cr <- crop(highPotential, extent(c(736002, 744138, 700691, 709433)))
wgs84 = CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')
crwgs <- projectRaster(cr, crs=wgs84)
mymap <- gmap(crwgs, type='satellite',rgb=T, lonlat=T, zoom=12)
plot(crwgs, main="Potential in Watt with increased spatial resolution",
     zlim=c(0,2000000), col=cols, breaks=brks,
     xlim=c(736002, 744138), ylim=c(700691, 709433), legend=F)
plotRGB(mymap, alpha=150, add=T)
legend("bottomleft", legend = nms[1:high], fill = cols[1:high])
text(x=125.227, y=6.335, labels="background: GoogleMaps", cex=0.7)

#### Potential with increased spatial resolution
setwd("E:/thesis/workspace/results/1a")
highPotential <- raster("output/highPotential.tif")
high <- 6
cr <- crop(highPotential, extent(ext))
crwgs <- projectRaster(cr, crs=wgs84)
plot(crwgs, main="Potential with increased spatial resolution",
     col=cols, breaks=brks,
     xlim=ext[1:2], ylim=ext[3:4], legend=F)
plotRGB(mymap, alpha=150, add=T)
plot(crwgs, col=cols, breaks=brks, add=T, legend=F)
legend("bottomleft", legend = nms[1:high], fill = cols[1:high])
text(x=125.184, y=6.368, labels="background: GoogleMaps", cex=0.7)

#### Default Model
setwd("E:/thesis/workspace/results/1a")
res <- raster("output/highPotentialA.tif")
high <- 7
cr <- crop(res, extent(ext))
crwgs <- projectRaster(cr, crs=wgs84)
plot(crwgs, main="Potential following the default model",
     col=cols, breaks=brks,
     xlim=ext[1:2], ylim=ext[3:4], legend=F)
plotRGB(mymap, alpha=150, add=T)
plot(crwgs, col=cols, breaks=brks, add=T, legend=F)
legend("bottomleft", legend = nms[1:high], fill = cols[1:high])
text(x=125.186, y=6.366, labels="background: GoogleMaps", cex=0.7)

### Costrasters and monthly
setwd("E:/thesis/workspace1b/output")
names <- lapply(1:3,FUN=function(x){sprintf("highPotential%i_%02d.tif", x, 1:12)})
res <- lapply(X=names, FUN=stack)
high <- 8

for (i in 1:12){
  cr <- crop(res[[1]][[i]], extent(ext))
  crwgs <- projectRaster(cr, crs=wgs84)
  png(sprintf("E:/thesis/reports/Thesis_report/figures/3_%02d.png", i),
      width = 783, height = 513, units = "px", pointsize = 16)
  plot(crwgs, main=sprintf("Potential for %s", months[i]),
       col=cols, breaks=brks,
       xlim=ext[1:2], ylim=ext[3:4], legend=F)
  plotRGB(mymap, alpha=150, add=T)
  plot(crwgs, col=cols, breaks=brks, add=T, legend=F)
  legend("bottomleft", legend = nms[1:high], fill = cols[1:high])
  text(x=125.184, y=6.368, labels="background: GoogleMaps", cex=0.7)
  dev.off()
}

### Scenario's
for (i in 1:12){
  cr <- crop(res[[2]][[i]], extent(ext))
  crwgs <- projectRaster(cr, crs=wgs84)
  png(sprintf("E:/thesis/reports/Thesis_report/figures/4_%02d.png", i),
      width = 783, height = 513, units = "px", pointsize = 16)
  plot(crwgs, main=sprintf("Potential for %s with storage factor 0.6", months[i]),
       col=cols, breaks=brks,
       xlim=ext[1:2], ylim=ext[3:4], legend=F)
  plotRGB(mymap, alpha=150, add=T)
  plot(crwgs, col=cols, breaks=brks, add=T, legend=F)
  legend("bottomleft", legend = nms[1:high], fill = cols[1:high])
  text(x=125.184, y=6.368, labels="background: GoogleMaps", cex=0.7)
  dev.off()
}
for (i in 1:12){
  cr <- crop(res[[3]][[i]], extent(ext))
  crwgs <- projectRaster(cr, crs=wgs84)
  png(sprintf("E:/thesis/reports/Thesis_report/figures/5_%02d.png", i),
      width = 783, height = 513, units = "px", pointsize = 16)
  plot(crwgs, main=sprintf("Potential for %s with storage factor 0.3", months[i]),
       col=cols, breaks=brks,
       xlim=ext[1:2], ylim=ext[3:4], legend=F)
  plotRGB(mymap, alpha=150, add=T)
  plot(crwgs, col=cols, breaks=brks, add=T, legend=F)
  legend("bottomleft", legend = nms[1:high], fill = cols[1:high])
  text(x=125.184, y=6.368, labels="background: GoogleMaps", cex=0.7)
  dev.off()
}

### LINE for one point

poi <- SpatialPoints(matrix(c(740259.159884, 706064.023617),nrow=1),
                                          proj4string=CRS(projection(res[[1]])))
nul <- extract(res[[1]],poi)
facts <- lapply(res,FUN=extract,y=poi)
for (i in 1:3){
  facts[[i]][is.na(facts[[i]])] <- 0
}
plot(facts[[1]][1,]/1000,col="red", type='b', xlab="step", ylab="power potential (kW)", xlim=c(1,12),
     ylim=c(0,2500))
cols <- c("red", "blue", "green")
for(i in 2:3){
  lines(facts[[i]][1,]/1000, col=cols[i], type='b')
}
lines(rep(0,12), col="black", type='l', lty=2)
legend("topleft", legend=c("1.0", "0.7","0.4"), col=c("red", "green", "blue"), pch=1)
title(paste(sprintf("Runoff at point %d, %d for storage factors", 
                    as.integer(poi@coords)[1], as.integer(poi@coords)[2]), 
            paste("1.0", "0.7", "0.4",sep=", ")))
