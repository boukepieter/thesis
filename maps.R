require(raster)
require(RColorBrewer)
library(dismo)
library(plotrix)

resetPar <- function() {
  dev.new()
  op <- par(no.readonly = TRUE)
  dev.off()
  op
}

wgs84 = CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')
cols <- rev(brewer.pal(11,"Spectral")[c(1,2,3,4,5,8,9,10,11)])
brks <- c(10000,20000,50000,100000,200000,400000,750000,1500000,3000000)
nms <- c(">10 kW", ">20 kW", ">50 kW", ">100 kW", ">200 kW", ">400 kW", ">750 kW", ">1.5 MW", ">3.0 MW")
ext <- c(736411, 741789, 704418,707888)
mymap <- gmap(crwgs, type='satellite',rgb=T, lonlat=T, zoom=12)
months <- c("January", "February", "March", "April", "May", "June", "July", "August",
            "September", "October", "November", "December")
scenarios <- c("0.0", "0.6", "0.3")

### testPLOT

plot(highPotential, main="Potential in Watt with increased spatial resolution",
     zlim=c(0,2000000), col=cols, breaks=brks,
     xlim=c(736002, 744138), ylim=c(700691, 709433), legend=F)
high <- 4
legend("bottomleft", legend = nms[1:high], fill = cols[1:high])
setwd("E:/thesis/workspace/results/1a")
highPotential <- raster("output/highPotential.tif")
cr <- crop(highPotential, extent(c(736002, 744138, 700691, 709433)))
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

### QA file
num <- raster("E:/thesis/data/dem/ASTGTM2_N06E125/ASTGTM2_N06E125_num.tif")
shed <- readOGR(dsn="E:/thesis/workspace1b/step/watershed.shp", layer="watershed")
shed <- spTransform(shed, wgs84)
num <- crop(num,shed)
numM <- mask(num,shed)
plot(numM, col=c("red","yellow","lightgreen","darkgreen"), breaks=c(-1.1,0.1,2.1,5.1,10.1),
     legend=F, main="QA of ASTER GDEM for the study area")
legend("bottomleft", legend = c("-1","1-2","3-5","6-10"), 
       fill = c("red","yellow","lightgreen","darkgreen"))
plot(shed, add=T)

his <- hist(numM, main="QA of ASTER GDEM for the study area", ylim=c(0,0.17),
            breaks=seq(-2,11), xlim=c(-1.6,9.6), freq=F, col="lightblue", labels=T, axes=F)
axis(1, at=his$mids,labels=(as.character(-1:11)))
axis(2)
?hist

### Error Prop non aggr
setwd("E:/thesis/workspace/MCAnalyse")
suitables1 <- stack(c("suitables1.tif", "suitables2.tif", "suitables3.tif"))
suitables2 <- stack(c("suitables11.tif", "suitables22.tif", "suitables33.tif"))
suitables3 <- stack(c("suitables111.tif", "suitables222.tif", "suitables333.tif"))
suitables <- list(suitables1, suitables2, suitables3)

for (j in 1:3){
  for (i in 1:3){
    cr <- crop(suitables[[j]][[i]], extent(ext))
    crwgs <- projectRaster(cr, crs=wgs84, method='ngb')
    crwgs[crwgs==0] <- NA
    png(sprintf("E:/thesis/reports/Thesis_report/figures/errorprop%d_%d.png", j, i),
        width = 783, height = 513, units = "px", pointsize = 16)
    plot(crwgs, main=sprintf("Percentage for scenario factor %s storage", scenarios[i]),
         zlim=c(0,50),
         xlim=ext[1:2], ylim=ext[3:4])
    plotRGB(mymap, alpha=150, add=T)
    plot(crwgs, add=T, legend=F, zlim=c(0,50))
    text(x=125.184, y=6.368, labels="background: GoogleMaps", cex=0.7)
    dev.off()
  }}

mar <- matrix(c(1,3,2,4,6,5),nrow=3)
png(sprintf("E:/thesis/reports/Thesis_report/figures/errorprop_12.png"),
    width = 2*783, height = 3*513, units = "px", pointsize = 16)
layout(mar)
j <- rbind(c(1,1),c(2,1),c(1,3),c(2,3),c(1,2),c(2,2))
for (i in 1:6){
  print(j[i])
  cr <- crop(suitables[[j[i,1]]][[j[i,2]]], extent(ext))
  crwgs <- projectRaster(cr, crs=wgs84, method='ngb')
  crwgs[crwgs==0] <- NA
  plot(crwgs, main=sprintf("Percentage for scenario factor %s storage", scenarios[j[i,2]]),
       zlim=c(0,50), legend=F, cex.main=2.0, cex.axis=2.0,
       xlim=ext[1:2], ylim=ext[3:4])
  plotRGB(mymap, alpha=150, add=T)
  plot(crwgs, add=T, legend=F, zlim=c(0,50))
  text(x=125.184, y=6.3673, labels="background: GoogleMaps", cex=1.0)
}
dev.off()

plot(crwgs, main="",
     zlim=c(0,0),
     xlim=c(125.13,125.19), ylim=ext[3:4], legend=F)
color.legend(125.135,6.38,125.185,6.384, legend=paste(seq(0,50,10),"%"), rect.col=rev(terrain.colors(6)), 
             gradient="x", cex=1)

### Error Prop aggr
setwd("E:/thesis/workspace/MCAnalyse")
suitables1 <- stack(c("suitables1Aggr.tif", "suitables2Aggr.tif", "suitables3Aggr.tif"))
suitables2 <- stack(c("suitables11Aggr.tif", "suitables22Aggr.tif", "suitables33Aggr.tif"))
suitables3 <- stack(c("suitables111Aggr.tif", "suitables222Aggr.tif", "suitables333Aggr.tif"))
suitables <- list(suitables1, suitables2, suitables3)
z<-100

for (j in 1:3){
  for (i in 1:3){
    cr <- crop(suitables[[j]][[i]], extent(ext))
    crwgs <- projectRaster(cr, crs=wgs84, method='ngb')
    crwgs[crwgs==0] <- NA
    png(sprintf("E:/thesis/reports/Thesis_report/figures/errorprop%d_%dAggr.png", j, i),
        width = 783, height = 513, units = "px", pointsize = 16)
    plot(crwgs, main=sprintf("Percentage for scenario factor %s storage", scenarios[i]),
         zlim=c(0,50),
         xlim=ext[1:2], ylim=ext[3:4])
    plotRGB(mymap, alpha=150, add=T)
    plot(crwgs, add=T, legend=F, zlim=c(0,z))
    text(x=125.184, y=6.368, labels="background: GoogleMaps", cex=0.7)
    dev.off()
  }}

mar <- matrix(c(1,3,2,4,6,5),nrow=3)
png(sprintf("E:/thesis/reports/Thesis_report/figures/errorpropAggr_12.png"),
    width = 2*783, height = 3*513, units = "px", pointsize = 16)
layout(mar)
j <- rbind(c(1,1),c(2,1),c(1,3),c(2,3),c(1,2),c(2,2))
for (i in 1:6){
  print(j[i])
  par(c(0,0,0,500))
  cr <- crop(suitables[[j[i,1]]][[j[i,2]]], extent(ext))
  crwgs <- projectRaster(cr, crs=wgs84, method='ngb')
  crwgs[crwgs==0] <- NA
  plot(crwgs, main=sprintf("Percentage for scenario factor %s storage aggregated", scenarios[j[i,2]]),
       zlim=c(0,z), cex.axis=2.0, cex.main=2,
       xlim=c(125.13,125.19), ylim=ext[3:4], legend=F)
  plotRGB(mymap, alpha=150, add=T)
  plot(crwgs, add=T, legend=F, zlim=c(0,z))
  text(x=125.184, y=6.366, labels="background: GoogleMaps", cex=1.0)
  
}
dev.off()

plot(crwgs, main="",
     zlim=c(0,0),
     xlim=c(125.13,125.19), ylim=ext[3:4], legend=F)
color.legend(125.135,6.38,125.185,6.384, legend=paste(seq(0,100,20),"%"), rect.col=rev(terrain.colors(6)), 
             gradient="x", cex=1)

### Error Prop combined
setwd("E:/thesis/workspace/MCAnalyse")
suitables3Aggr <- stack(c("suitables111Aggr.tif", "suitables222Aggr.tif", "suitables333Aggr.tif"))
suitables3 <- stack(c("suitables111.tif", "suitables222.tif", "suitables333.tif"))
suitables <- list(suitables3, suitables3Aggr)
z<-100

mar <- matrix(c(1,3,2,4,6,5),nrow=3)
png(sprintf("E:/thesis/reports/Thesis_report/figures/errorpropTotals.png"),
    width = 2*783, height = 3*513, units = "px", pointsize = 16)
layout(mar)
j <- rbind(c(1,1),c(2,1),c(1,3),c(2,3),c(1,2),c(2,2))
for (i in 1:6){
  print(j[i])
  par(c(0,0,0,500))
  cr <- crop(suitables[[j[i,1]]][[j[i,2]]], extent(ext))
  crwgs <- projectRaster(cr, crs=wgs84, method='ngb')
  crwgs[crwgs==0] <- NA
  plot(crwgs, main=sprintf("Percentage for scenario factor %s storage total", scenarios[j[i,2]]),
       zlim=c(0,z), cex.axis=2.0, cex.main=2,
       xlim=c(125.13,125.19), ylim=ext[3:4], legend=F)
  plotRGB(mymap, alpha=150, add=T)
  plot(crwgs, add=T, legend=F, zlim=c(0,z))
  if (i%%2==1){
    text(x=125.184, y=6.3673, labels="background: GoogleMaps", cex=1.0)
  }
  else{
    text(x=125.184, y=6.366, labels="background: GoogleMaps", cex=1.0)
  }
  
}
dev.off()

plot(crwgs, main="",
     zlim=c(0,0),
     xlim=c(125.13,125.19), ylim=ext[3:4], legend=F)
color.legend(125.135,6.38,125.185,6.384, legend=paste(seq(0,100,20),"%"), rect.col=rev(terrain.colors(6)), 
             gradient="x", cex=1)
