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
nms <- c("10 - 20", "20 - 50", "50 - 100", "100 - 200", "200 - 400", "400 - 750", "750 - 1,500", ">1,500", ">3.0 MW")
ext <- c(736411, 741789, 704418,707888)
mymap <- gmap(crwgs, type='satellite',rgb=T, lonlat=T, zoom=12)
months <- c("January", "February", "March", "April", "May", "June", "July", "August",
            "September", "October", "November", "December")
scenarios <- c("0.0", "0.6", "0.3")
riv <- readOGR(dsn="E:/thesis/data/rivers_small.shp", layer="rivers_small")
riv <- spTransform(riv,wgs84)
riv$lwd=riv$grid_code - 4
writeOGR(dsn="E:/thesis/data/", "rivers_smallwgs", driver="ESRI Shapefile")

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
plot(riv,lwd=riv$lwd, add=T)
plot(crwgs, col=cols, breaks=brks, add=T, legend=F)
legend("bottomleft", legend = nms[1:high], fill = cols[1:high], cex=0.8)
text(x=125.184, y=6.368, labels="background: GoogleMaps", cex=0.7)

#### Default Model
setwd("E:/thesis/workspace/results/1a")
res <- raster("output/highPotentialA.tif")
high <- 7
cr <- crop(res, extent(ext))
crwgs <- projectRaster(cr, crs=wgs84)


plot(crwgs, #main="Potential following the default model",
     col=cols, breaks=brks,
     xlim=ext[1:2], ylim=ext[3:4], legend=F)
plotRGB(mymap, alpha=150, add=T)
plot(crwgs, col=cols, breaks=brks, add=T, legend=F)
plot(riv,lwd=riv$lwd, add=T)
legend("bottomleft", legend = nms[1:high], fill = cols[1:high], cex=0.8)
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
  plot(riv,lwd=riv$lwd, add=T)
  plot(crwgs, col=cols, breaks=brks, add=T, legend=F)
  legend("bottomleft", legend = nms[1:high], fill = cols[1:high], cex=0.8)
  text(x=125.184, y=6.368, labels="background: GoogleMaps", cex=0.7)
  dev.off()
}

mar <- matrix(c(1,3,2,4,6,5),nrow=3)
png(sprintf("E:/thesis/reports/Thesis_report/figures/costMonthlyA.png"),
    width = 2*783, height = 3*513, units = "px", pointsize = 16)
layout(mar)
for (i in 1:6){
  print(i)
  cr <- crop(res[[1]][[i]], extent(ext))
  crwgs <- projectRaster(cr, crs=wgs84)
  plot(crwgs, main=sprintf("Potential for %s", months[i]),
       col=cols, breaks=brks, cex.main=2.0, cex.axis=2.0,
       xlim=ext[1:2], ylim=ext[3:4], legend=F)
  plotRGB(mymap, alpha=150, add=T)
  plot(riv,lwd=riv$lwd, add=T)
  plot(crwgs, col=cols, breaks=brks, add=T, legend=F)
  #legend("bottomleft", legend = nms[1:high], fill = cols[1:high])
  text(x=125.184, y=6.368, labels="background: GoogleMaps", cex=1)
}
dev.off()
png(sprintf("E:/thesis/reports/Thesis_report/figures/costMonthlyB.png"),
    width = 2*783, height = 3*513, units = "px", pointsize = 16)
layout(mar)
for (i in 7:12){
  print(i)
  cr <- crop(res[[1]][[i]], extent(ext))
  crwgs <- projectRaster(cr, crs=wgs84)
  plot(crwgs, main=sprintf("Potential for %s", months[i]),
       col=cols, breaks=brks, cex.main=2.0, cex.axis=2.0,
       xlim=ext[1:2], ylim=ext[3:4], legend=F)
  plotRGB(mymap, alpha=150, add=T)
  plot(riv,lwd=riv$lwd, add=T)
  plot(crwgs, col=cols, breaks=brks, add=T, legend=F)
  legend("bottomleft", legend = nms[1:high], fill = cols[1:high])
  text(x=125.184, y=6.368, labels="background: GoogleMaps", cex=1)
}
dev.off()

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
  plot(riv,lwd=riv$lwd, add=T)
  plot(crwgs, col=cols, breaks=brks, add=T, legend=F)
  legend("bottomleft", legend = nms[1:high], fill = cols[1:high], cex=0.8)
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
  plot(riv,lwd=riv$lwd, add=T)
  plot(crwgs, col=cols, breaks=brks, add=T, legend=F)
  legend("bottomleft", legend = nms[1:high], fill = cols[1:high], cex=0.8)
  text(x=125.184, y=6.368, labels="background: GoogleMaps", cex=0.7)
  dev.off()
}
rest <- stack(res[[1]][[2]],res[[1]][[12]],res[[3]][[2]],res[[3]][[12]],res[[2]][[2]],res[[2]][[12]])
monthst <- rep(c(months[2],months[12]),3)
scenariost <- c(rep(scenarios[1],2),rep(scenarios[3],2),rep(scenarios[2],2))
mar <- matrix(c(1,3,2,4,6,5),nrow=3)
png("E:/thesis/reports/Thesis_report/figures/scenariosA.png",
    width = 2*783, height = 3*513, units = "px", pointsize = 16)
layout(mar)
for (i in 1:6){
  cr <- crop(rest[[i]], extent(ext))
  crwgs <- projectRaster(cr, crs=wgs84)
  plot(crwgs, main=sprintf("Potential for %s with storage factor %s", monthst[i], scenariost[i]),     
       col=cols, breaks=brks, cex.main=2.0, cex.axis=2.0,
       xlim=ext[1:2], ylim=ext[3:4], legend=F)
  plotRGB(mymap, alpha=150, add=T)
  plot(riv,lwd=riv$lwd, add=T)
  plot(crwgs, col=cols, breaks=brks, add=T, legend=F)
  #legend("bottomleft", legend = nms[1:high], fill = cols[1:high])
  text(x=125.184, y=6.368, labels="background: GoogleMaps", cex=1)
}
dev.off()

rest <- stack(res[[1]][[6]],res[[1]][[8]],res[[3]][[6]],res[[3]][[8]],res[[2]][[6]],res[[2]][[8]])
monthst <- rep(c(months[6],months[8]),3)
png("E:/thesis/reports/Thesis_report/figures/scenariosB.png",
    width = 2*783, height = 3*513, units = "px", pointsize = 16)
layout(mar)
for (i in 1:6){
  cr <- crop(rest[[i]], extent(ext))
  crwgs <- projectRaster(cr, crs=wgs84)
  plot(crwgs, main=sprintf("Potential for %s with storage factor %s", monthst[i], scenariost[i]),     
       col=cols, breaks=brks, cex.main=2.0, cex.axis=2.0,
       xlim=ext[1:2], ylim=ext[3:4], legend=F)
  plotRGB(mymap, alpha=150, add=T)
  plot(riv,lwd=riv$lwd, add=T)
  plot(crwgs, col=cols, breaks=brks, add=T, legend=F)
  #legend("bottomleft", legend = nms[1:high], fill = cols[1:high])
  text(x=125.184, y=6.368, labels="background: GoogleMaps", cex=1)
}
dev.off()
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
legend("topleft", legend=c("1.0", "0.7","0.4"), col=c("red", "green", "blue"), pch=1, cex=0.8)
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
    plot(riv,lwd=riv$lwd, add=T)
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
  plot(riv,lwd=riv$lwd, add=T)
  plot(crwgs, add=T, legend=F, zlim=c(0,50))
  text(x=125.184, y=6.3673, labels="background: GoogleMaps", cex=1.0)
}
dev.off()

plot(crwgs, main="",
     zlim=c(0,0),
     xlim=c(125.13,125.19), ylim=ext[3:4], legend=F)
color.legend(125.135,6.38,125.185,6.384, legend=paste(seq(0,0.5,0.1)), rect.col=rev(terrain.colors(6)), 
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
  plot(riv,lwd=riv$lwd, add=T)
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
  
  
  if (i%%2==1){
    plot(riv,lwd=riv$lwd, add=T)
    plot(crwgs, add=T, legend=F, zlim=c(0,z))
    text(x=125.184, y=6.3673, labels="background: GoogleMaps", cex=1.0)
  }
  else{
    plot(crwgs, add=T, legend=F, zlim=c(0,z))
    plot(riv,lwd=riv$lwd, add=T)
    text(x=125.184, y=6.366, labels="background: GoogleMaps", cex=1.0)
  }
  
}
dev.off()

plot(crwgs, main="",
     zlim=c(0,0),
     xlim=c(125.13,125.19), ylim=ext[3:4], legend=F)
color.legend(125.135,6.38,125.185,6.384, legend=paste(seq(0,100,20),"%"), rect.col=rev(terrain.colors(6)), 
             gradient="x", cex=1)
<<<<<<< HEAD

### Scatterplot
setwd("E:/thesis/workspace/MCAnalyse")
suitables1 <- stack(c("suitables1.tif", "suitables2.tif", "suitables3.tif"))
suitables2 <- stack(c("suitables11.tif", "suitables22.tif", "suitables33.tif"))
suitables3 <- stack(c("suitables111.tif", "suitables222.tif", "suitables333.tif"))

mar <- matrix(c(1,2,3,0),nrow=2,byrow=T)
png(sprintf("E:/thesis/reports/Thesis_report/figures/scatterplot2.png"),
    width = 2*783, height = 2*513, units = "px", pointsize = 16)
layout(mar)
j <- c(50,7)
for( i in c(1,3,2)){
  plot(x=suitables1[[i]][], y=suitables2[[i]][], pch=20, col=rgb(0,0,0,0.05),
       xlab="first hundred runs", ylab="second hundred runs", 
       main=sprintf("Scatterplot of the results of the Monte Carlo analysis 
                    for the factor %s storage scenario", scenarios[i]))
  lines(x=0:70, y=0:70)
}
dev.off()

### mean and percentiles
mar <- matrix(c(1,2,3,4,5,6),nrow=3)
png(sprintf("E:/thesis/reports/Thesis_report/figures/meanetc.png"),
    width = 2*783, height = 3*513, units = "px", pointsize = 16)
layout(mar)

setwd("E:/thesis/workspace/MCAnalyse")
res <- raster("mean.tif")
crwgs <- projectRaster(res, crs=wgs84)
plot(crwgs, main="Mean potential of all the Monte Carlo runs",
     col=cols, breaks=brks,cex.axis=2.0, cex.main=2,
     xlim=ext[1:2], ylim=ext[3:4], legend=F)
plotRGB(mymap, alpha=150, add=T)
plot(riv,lwd=riv$lwd, add=T)
plot(crwgs, col=cols, breaks=brks, add=T, legend=F)
#legend("bottomleft", legend = nms[1:high], fill = cols[1:high])
text(x=125.184, y=6.3675, labels="background: GoogleMaps", cex=1)

# point <- readOGR(dsn="E:/thesis/data", layer="Inlet")
# plot(point,add=T, pch=20, cex=2)
# plot(point,add=T, cex=2)

res <- raster("mean27.tif")
crwgs <- projectRaster(res, crs=wgs84)
plot(crwgs, main="Mean potential of 27th Monte Carlo run",
     col=cols, breaks=brks,cex.axis=2.0, cex.main=2,
     xlim=ext[1:2], ylim=ext[3:4], legend=F)
plotRGB(mymap, alpha=150, add=T)
plot(riv,lwd=riv$lwd, add=T)
plot(crwgs, col=cols, breaks=brks, add=T, legend=F)
#legend("bottomleft", legend = nms[1:high], fill = cols[1:high])
text(x=125.184, y=6.3675, labels="background: GoogleMaps", cex=1)

res <- raster("Q05.tif")
crwgs <- projectRaster(res, crs=wgs84)
plot(crwgs, main="5 percentile of all the Monte Carlo runs",
     col=cols, breaks=brks,cex.axis=2.0, cex.main=2,
     xlim=ext[1:2], ylim=ext[3:4], legend=F)
plotRGB(mymap, alpha=150, add=T)
plot(riv,lwd=riv$lwd, add=T)
plot(crwgs, col=cols, breaks=brks, add=T, legend=F)
#legend("bottomleft", legend = nms[1:high], fill = cols[1:high])
text(x=125.184, y=6.3675, labels="background: GoogleMaps", cex=1)

res <- raster("mean92.tif")
crwgs <- projectRaster(res, crs=wgs84)
plot(crwgs, main="Mean potential of 92nd Monte Carlo run",
     col=cols, breaks=brks,cex.axis=2.0, cex.main=2,
     xlim=ext[1:2], ylim=ext[3:4], legend=F)
plotRGB(mymap, alpha=150, add=T)
plot(riv,lwd=riv$lwd, add=T)
plot(crwgs, col=cols, breaks=brks, add=T, legend=F)
#legend("bottomleft", legend = nms[1:high], fill = cols[1:high])
text(x=125.184, y=6.3675, labels="background: GoogleMaps", cex=1)

res <- raster("Q95.tif")
crwgs <- projectRaster(res, crs=wgs84)
plot(crwgs, main="95 percentile of all the Monte Carlo runs",
     col=cols, breaks=brks,cex.axis=2.0, cex.main=2,
     xlim=ext[1:2], ylim=ext[3:4], legend=F)
plotRGB(mymap, alpha=150, add=T)
plot(riv,lwd=riv$lwd, add=T)
plot(crwgs, col=cols, breaks=brks, add=T, legend=F)
#legend("bottomleft", legend = nms[1:high], fill = cols[1:high])
text(x=125.184, y=6.3675, labels="background: GoogleMaps", cex=1)

res <- raster("mean150.tif")
crwgs <- projectRaster(res, crs=wgs84)
plot(crwgs, main="Mean potential of 150th Monte Carlo run",
     col=cols, breaks=brks,cex.axis=2.0, cex.main=2,
     xlim=ext[1:2], ylim=ext[3:4], legend=F)
plotRGB(mymap, alpha=150, add=T)
plot(riv,lwd=riv$lwd, add=T)
plot(crwgs, col=cols, breaks=brks, add=T, legend=F)
#legend("bottomleft", legend = nms[1:high], fill = cols[1:high])
text(x=125.184, y=6.3675, labels="background: GoogleMaps", cex=1)


dev.off()


#Comparison

point <- readOGR(dsn="E:/thesis/data", layer="Inlet")
setwd("E:/thesis/workspace1b/output")
names <- lapply(1:3,FUN=function(x){sprintf("highPotential%i_%02d.tif", x, 1:12)})
res <- lapply(X=names, FUN=stack)
mean <- mean(res[[3]], na.rm=T)
ones <- reclassify(res[[3]], matrix(c(-5,0,0,0,10000000,1),nrow=2,byrow=T), right=NA)
sum <- sum(ones, na.rm=T)
selection <- reclassify(sum, matrix(c(-5,9.5,NA,9.5,15,1),nrow=2,byrow=T), right=NA)
rest <- selection * mean
restt <- reclassify(rest, matrix(c(-5,1500000,NA,1500000,10000000,1),nrow=2,byrow=T), right=NA)


mar <- matrix(c(1,2,3,4,5,6),nrow=3)

png(sprintf("E:/thesis/reports/Thesis_report/figures/comparison.png"),
    width = 2*783, height = 3*513, units = "px", pointsize = 16)
layout(mar)

#1
cr <- crop(mean, extent(ext))
crwgs <- projectRaster(cr, crs=wgs84)
plot(crwgs, main="a",
     col=cols, breaks=brks,cex.axis=2.0, cex.main=2,
     xlim=ext[1:2], ylim=ext[3:4], legend=F)
plotRGB(mymap, alpha=150, add=T)
plot(riv,lwd=riv$lwd, add=T)
plot(crwgs, col=cols, breaks=brks, add=T, legend=F)
#legend("bottomleft", legend = nms[1:high], fill = cols[1:high])
text(x=125.184, y=6.3675, labels="background: GoogleMaps", cex=1)
plot(point,add=T, pch=20, cex=4, col="red")
plot(point,add=T, cex=2)

#2
cr <- crop(rest, extent(ext))
crwgs <- projectRaster(cr, crs=wgs84)
plot(crwgs, main="b",
     col=cols, breaks=brks,cex.axis=2.0, cex.main=2,
     xlim=ext[1:2], ylim=ext[3:4], legend=F)
plotRGB(mymap, alpha=150, add=T)
plot(riv,lwd=riv$lwd, add=T)
plot(crwgs, col=cols, breaks=brks, add=T, legend=F)
#legend("bottomleft", legend = nms[1:high], fill = cols[1:high])
text(x=125.184, y=6.3675, labels="background: GoogleMaps", cex=1)
plot(point,add=T, pch=20, cex=4, col="red")
plot(point,add=T, cex=2)

#3
cr <- crop(suitables[[2]][[3]], extent(ext))
crwgs <- projectRaster(cr, crs=wgs84, method='ngb')
crwgs[crwgs==0] <- NA
plot(crwgs, main=sprintf("c", scenarios[2]),
     zlim=c(0,z), cex.axis=2.0, cex.main=2,
     xlim=c(125.13,125.19), ylim=ext[3:4], legend=F)
plotRGB(mymap, alpha=150, add=T)
plot(crwgs, add=T, legend=F, zlim=c(0,z))
plot(riv,lwd=riv$lwd, add=T)
text(x=125.184, y=6.366, labels="background: GoogleMaps", cex=1.0)
plot(point,add=T, pch=20, cex=4, col="red")
plot(point,add=T, cex=2)

#4
cr <- crop(restt, extent(ext))
crwgs <- projectRaster(cr, crs=wgs84, method="ngb")
plot(crwgs, main="d",
     #col=cols, breaks=brks,
     cex.axis=2.0, cex.main=2,
     xlim=ext[1:2], ylim=ext[3:4], legend=F)
plotRGB(mymap, alpha=150, add=T)
plot(riv,lwd=riv$lwd, add=T)
plot(crwgs, add=T, legend=F, col="red")
#legend("bottomleft", legend = nms[1:high], fill = cols[1:high])
text(x=125.184, y=6.3675, labels="background: GoogleMaps", cex=1)
plot(point,add=T, pch=20, cex=4, col="red")
plot(point,add=T, cex=2)


dev.off()



### OVerview map for results
dem <- raster("E:/thesis/data/dem/ASTGTM2_N06E125/ASTGTM2_N06E125_dem.tif")
shed <- readOGR(dsn="E:/thesis/workspace1b/step/watershed.shp", layer="watershed")
shed <- spTransform(shed, wgs84)
dem <- crop(dem,shed)
demM <- mask(dem,shed)
extshape <- polygonFromExtent(ext, sp=TRUE)
area <- crop(res,ext)
area <- projectRaster(area, crs=wgs84)
ext2 <- extent(area)
library(rgeos)
riv <- readOGR(dsn="E:/thesis/data/rivers.shp", layer="rivers")
riv2 <- gIntersection(riv, shed)

plot(demM)
lines(c(ext2[1], ext2[2]),c(ext2[3],ext2[3]),lwd=2)
lines(c(ext2[1], ext2[2]),c(ext2[4],ext2[4]),lwd=2)
lines(c(ext2[1], ext2[1]),c(ext2[3],ext2[4]),lwd=2)
lines(c(ext2[2], ext2[2]),c(ext2[3],ext2[4]),lwd=2)
plot(riv2, col="blue", add=T)

plot(demM, col=c("red","yellow","lightgreen","darkgreen"), breaks=c(-1.1,0.1,2.1,5.1,10.1),
     legend=F, main="QA of ASTER GDEM for the study area")
legend("bottomleft", legend = c("-1","1-2","3-5","6-10"), 
       fill = c("red","yellow","lightgreen","darkgreen"))
plot(shed, add=T)

### Streams
riv <- readOGR(dsn="E:/thesis/data/rivers_small.shp", layer="rivers_small")
plot(riv,col=riv$grid_code, add=T)
riv

### DEM realisations

mar <- matrix(c(1,2,3,4),nrow=2,byrow=T)
png(sprintf("E:/thesis/reports/Thesis_report/figures/realisations.png"),
    width = 2*783, height = 2*513, units = "px", pointsize = 16)
layout(mar)
res <- raster("E:/thesis/workspace/input/DEM.tif")
cr <- crop(res, extent(ext))
crwgs <- projectRaster(cr, crs=wgs84)
plot(crwgs,cex.axis=2.0,
     #col=cols, breaks=brks,
     xlim=ext[1:2], ylim=ext[3:4], legend=F)
for( i in 1:3){
  res <- raster(sprintf("E:/thesis/workspace/MCAnalyse/input/DEM/DEM00%d.tif",i))
  cr <- crop(res, extent(ext))
  crwgs <- projectRaster(cr, crs=wgs84)
  plot(crwgs, cex.axis=2.0,#main="Potential following the default model",
       #col=cols, breaks=brks,
       xlim=ext[1:2], ylim=ext[3:4], legend=F)
}
dev.off()

# rain trend
shed <- readOGR(dsn="E:/thesis/workspace1b/step/watershed.shp", layer="watershed")
P <- stack(sprintf("E:/thesis/workspace1b/input/P/P%02d.tif", 1:12))
mask <- mask(P, shed)
means <- c()
for (i in 1:12){
means[i] <- mean(mask[[i]][], na.rm=T)
}
library(ggplot2)
plot(means, xlim=c(1,12), ylim=c(0,13))
lines(means)
dat <- data.frame(precipitation=means, month=months)
dat$month <- factor(dat$month, levels = dat$month, ordered = TRUE)
ggplot(data=dat, aes(x=month, y=precipitation, group=1)) +
  geom_line()
ggplot(data=dat, aes(x=month, y=precipitation)) +
  geom_bar(stat="identity")
mean(means)*365

resamp <- resample(mask, cr)
clipped <- crop(resamp, extent(ext))
means2 <- c()
for (i in 1:12){
  means2[i] <- mean(clipped[[i]][], na.rm=T)
}
dat <- data.frame(precipitation=means2, month=months)
dat$month <- factor(dat$month, levels = dat$month, ordered = TRUE)
ggplot(data=dat, aes(x=month, y=precipitation)) +
  geom_bar(stat="identity")
mean(means2)*365
