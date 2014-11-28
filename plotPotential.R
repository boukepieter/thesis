require(plotKML)
require(spacetime)
require(raster)
require(rgeos)
require(maptools)

# Raster
PlotTimeRaster <- function(step = "month"){
  points <- rasterToPoints(highPotential[[which(noOfPoints==max(noOfPoints))]], spatial=TRUE)
  pointswgs <- spTransform(points, CRS=(wgs84))
  names(pointswgs) <- "potential"
  points <- pointswgs[order(pointswgs@data[,1], decreasing=TRUE),]
  points <- points[1:10,]
  
  starttimes <- seq(as.Date("2014/1/1"), by=step, length.out=2)
  btimes <- as.POSIXct(format(seq(starttimes[1], by=step, length.out=noSteps)), origin="1970-01-01")
  etimes <- as.POSIXct(format(seq(starttimes[2], by=step, length.out=noSteps)), origin="1970-01-01")
  potential_wgs <- projectRaster(potential, crs=wgs84) / 1000
  potential_wgs@title <- "Potential of hydropower in kW"
  RBTS <- new("RasterBrickTimeSeries", rasters=potential_wgs, variable="Potential (kW)",
              sampled = points,
              TimeSpan.begin=btimes, TimeSpan.end=etimes)
  test(RBTS, colour_scale = SAGA_pal[[1]],  
      file.name="potentialRaster.kml", folder.name="potential (kW)")
}

# Vector
PlotTimeVector <- function(step = "month") {
  starttimes <- seq(as.Date("2014/1/1"), by=step, length.out=2)
  btimes <- seq(starttimes[1], by=step, length.out=noSteps)
  etimes <- seq(starttimes[2], by=step, length.out=noSteps)
  
  wgs84 = CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')
  points <- rasterToPoints(highPotential[[1]], spatial=TRUE)
  pointswgs <- spTransform(points, CRS=(wgs84))
  names(pointswgs) <- "potential"
  pointswgs$btime <- btimes[1]
  pointswgs$etime <- etimes[1]
  
  for (i in 2:noSteps){
    if(noOfPoints[i] == 0) next
    points <- rasterToPoints(highPotential[[i]], spatial=TRUE)
    pointswgs2 <- spTransform(points, CRS=(wgs84))
    names(pointswgs2) <- "potential"
    pointswgs2$btime <- btimes[i]
    pointswgs2$etime <- etimes[i]
    pointswgs <- spRbind(pointswgs, pointswgs2)
  }
  writeOGR(pointswgs, dsn="../output", layer= "potentialVector", 
           driver="ESRI Shapefile", overwrite=TRUE)
  sp <- SpatialPoints(pointswgs@coords)
  projection(sp) <- wgs84
  st <- STIDF(sp, time = pointswgs$btime, data=pointswgs@data, endTime= pointswgs$etime)
  kml(st, shape = "http://maps.google.com/mapfiles/kml/pal2/icon18.png", 
      size = potential, colour = potential, 
      labels = paste(round(st$potential / 1000), "kW"), file.name="potentialVector.kml",
      folder.name="potential (kW)")
}

test <- function(
  obj,
  folder.name = normalizeFilename(deparse(substitute(obj, env=parent.frame()))),
  file.name = paste(folder.name, ".kml", sep=""),
  pngwidth = 680,
  pngheight = 180,
  pngpointsize = 14,
  kmz = get("kmz", envir = plotKML.opts),
  open.kml = FALSE,
  ...
){
  
  ## sampling locations:
  if(!("data" %in% slotNames(obj@sampled))){
    labs <- paste(obj@sampled@data[,1])
  } else {
    labs <- paste(1:length(obj@sampled))
  }
  ## Begin end times:
  TimeSpan.begin <- obj@TimeSpan.begin
  TimeSpan.end <- obj@TimeSpan.end
  ## copy mean times:
  obj@rasters <- setZ(obj@rasters, paste(as.POSIXct(unclass(as.POSIXct(TimeSpan.begin))+(unclass(as.POSIXct(TimeSpan.end))-unclass(as.POSIXct(TimeSpan.begin)))/2, origin="1970-01-01")))
  dtime = unclass(as.POSIXct(TimeSpan.end)) - unclass(as.POSIXct(TimeSpan.begin))
  
  ## open KML for writing:  
  kml_open(folder.name = folder.name, file.name = file.name)
  
  ## add a description for the whole folder:
  kml.out <- get("kml.out", envir=plotKML.fileIO)
  description_txt <- sprintf('<description>%s</description>', obj@rasters@title)
  parseXMLAndAdd(description_txt, parent=kml.out[["Document"]])  
  assign('kml.out', kml.out, envir=plotKML.fileIO)
  
  ## extract values at point locations:
  ov <- extract(obj@rasters, obj@sampled)
  png_names <- paste(obj@variable, "_timeseries_", 1:nrow(ov), ".png", sep="")
  html.table <- paste('<img src="', png_names, '" height="', pngheight, '" width="', pngwidth, '" align ="middle" />', sep = '')
  kml_layer.SpatialPoints(obj = obj@sampled, points_names = labs, html.table = html.table)
  
  ## plot rasters:
  kml_layer(obj = obj@rasters, dtime=dtime, ...) 
  
  ## plot the time-series data:
  for(i in 1:nrow(ov)){
    png(filename=png_names[i], width=pngwidth, height=pngheight, bg="white", pointsize=pngpointsize)
    par(mar=c(4.5,4.5,.8,.8))
    plot(as.Date(as.POSIXct(getZ(obj@rasters))), ov[i,], type="l", xlab="Date", ylab=obj@variable, col="grey", lwd=2)
    points(as.Date(as.POSIXct(getZ(obj@rasters))), ov[i,], pch="+", cex=.6)
    dev.off()
  }
  
  ## close the file:
  kml_close(file.name = file.name)
  if (kmz == TRUE){
    kml_compress(file.name = file.name)
  }
  ## open KML file in the default browser:
  if(open.kml==TRUE){
    kml_View(file.name)
  } else {
    message(paste("Object written to:", file.name))
  }
  
}