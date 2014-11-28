library(raster)
library(rgdal)
library(sp)

# files
utm51 = CRS('+proj=utm +zone=51 +ellps=WGS84 +datum=WGS84 +units=m +no_defs')
wgs84 = CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')

# projectarea
xym <- matrix(c(5.46836, 124, 7, 124, 7, 126, 5.46836, 126, 5.46836, 124), ncol=2, byrow=T)
xym2 <- xym
xym2[,1] = xym[,2]
xym2[,2] = xym[,1]
p = Polygon(xym2)
ps = Polygons(list(p),1)
sps = SpatialPolygons(list(ps))
proj4string(sps) <- wgs84
projectarea <- spTransform(sps, utm51)

# small version
xym <- matrix(c(6, 125, 6.6, 125, 6.6, 125.6, 6, 125.6, 6, 125), ncol=2, byrow=T)
xym2 <- xym
xym2[,1] = xym[,2]
xym2[,2] = xym[,1]
p = Polygon(xym2)
ps = Polygons(list(p),1)
sps = SpatialPolygons(list(ps))
proj4string(sps) <- wgs84
projectareasmall <- spTransform(sps, utm51)

# large version
xym <- matrix(c(5, 123, 8, 123, 8, 127, 5, 127, 5, 123), ncol=2, byrow=T)
xym2 <- xym
xym2[,1] = xym[,2]
xym2[,2] = xym[,1]
p = Polygon(xym2)
ps = Polygons(list(p),1)
arealarge = SpatialPolygons(list(ps))
proj4string(arealarge) <- wgs84