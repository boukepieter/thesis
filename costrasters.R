require(raster)
require(rgdal)
require(gdistance)

HeadOnRiver.large <- function(DEM, debiet, channelLength = 500, minimumDebiet = 0.1, 
                        splitSize = 500, includeSides = TRUE) {
  #TODO(text)
  reso <- res(DEM)[1]
  cells <-  round(channelLength / reso)
  head <- DEM
  head[] <- 0
  names(head) <- "Head"
  done <- 0
  
  rowIte <- ceiling(nrow(DEM) / splitSize)
  colIte <- ceiling(ncol(DEM) / splitSize)
  print(paste("splitting up in", rowIte * colIte, "pieces..."))
  for(k in 1:rowIte){
    istart <- 1 + (k - 1) * splitSize
    imin <- istart - cells
    imin <- max(1, imin)
    iend <- splitSize + (k - 1) * splitSize
    imax <- iend + cells
    imax <- min(nrow(DEM),imax)
    for(l in 1:colIte){
      jstart <- 1 + (l - 1) * splitSize
      jmin <- jstart - cells
      jmin <- max(1, jmin)
      jend <- splitSize + (l - 1) * splitSize
      jmax <- jend + cells
      jmax <- min(ncol(DEM),jmax)
      DEMpart <- raster(nrows=imax - imin + 1, ncols=jmax - jmin + 1, 
                        xmn=xmin(DEM) + (jmin - 1) * reso, 
                        xmx=xmin(DEM) + (jmax) * reso, 
                        ymn=ymax(DEM) - imax * reso, 
                        ymx=ymax(DEM) - (imin - 1) * reso, 
                        crs=projection(DEM))
      DEMpart[] <- DEM[imin:imax,jmin:jmax]
      debietPart <- raster(nrows=imax - imin + 1, ncols=jmax - jmin + 1, 
                           xmn=xmin(DEM) + (jmin - 1) * reso, 
                           xmx=xmin(DEM) + (jmax) * reso, 
                           ymn=ymax(DEM) - imax * reso, 
                           ymx=ymax(DEM) - (imin - 1) * reso, 
                           crs=projection(DEM))
      debietPart[] <- debiet[imin:imax,jmin:jmax]
      if (imin == 1 || imax == nrow(DEM) || jmin == 1 || jmax == ncol(DEM)){
        headPart <- HeadOnRiver(DEMpart, debietPart, channelLength, minimumDebiet, includeSides)
      }
      else{
        headPart <- HeadOnRiver(DEMpart, debietPart, channelLength, minimumDebiet, includeSides=FALSE)
      }
      head[istart:min(iend, imax),jstart:min(jend,jmax)] <- 
        headPart[(1+istart-imin):(nrow(headPart) + min(iend,imax) - imax),
                 (1+jstart-jmin):(ncol(headPart) + min(jend,jmax) - jmax)]
      done = done + 1
      print(paste(done, "/", colIte * rowIte, " done...", sep=""))
    }
  }
  return(head)
}

HeadOnRiver <- function(DEM, debiet, channelLength = 500, minimumDebiet = 0.1, includeSides = TRUE) {
  # function to calculate the head of cells with a neighborhood-distance (channelLength) along
  # a river. DEM and debiet should be projected rasters with cellsizes in m, with exact the same 
  # amount of cells and cellsizes.
  debiet[debiet < minimumDebiet] <- NA
  debiet[debiet >= minimumDebiet & debiet < 100000] <- 1
  nr <- nrow(DEM)
  nc <- ncol(DEM)
  reso <- res(DEM)[1]
  cells <-  round(channelLength / reso)
  head <- DEM
  head[] <- 0
  names(head) <- "Head"
  oldperc <- 0
  
  df <- data.frame(value=debiet[], nr=1:ncell(debiet))
  df <- df[!is.na(df["value"]),]
  ncal <- nrow(df)
  
  if(includeSides){
    for(k in 1:ncal){
      i <- ceiling(df[k,2] / nc)
      j <- df[k,2] %% nc
      j <- ifelse(j == 0, nc, j)
      imin <- ifelse(i - cells > 1, i - cells, 1)
      imax <- ifelse(i + cells < nr, i + cells, nr)
      jmin <- ifelse(j - cells > 1, j - cells, 1)
      jmax <- ifelse(j + cells < nc, j + cells, nc)
      yreso <- (imax - imin + 1) * reso
      xreso <- (jmax - jmin + 1) * reso
      r <- raster(nrows=imax - imin + 1, ncols=jmax - jmin + 1, 
                  xmn=0, xmx=xreso, ymn=0, ymx=yreso, crs="+proj=utm +units=m")
      r[] <- debiet[imin:imax,jmin:jmax]
      T <- transition(r, transitionFunction=mean, 8, symm=FALSE) 
      T <- geoCorrection(T)
      c1 <- c((j - jmin + 0.5) * reso,(imax - i + 0.5) * reso)
      A <- accCost(T, c1)
      head[i,j] <- DEM[i,j] - min(DEM[imin:imax,jmin:jmax][A[] < channelLength], na.rm=TRUE)
      
      perc <- floor(k / ncal * 100)
      if (perc > oldperc){
        oldperc <- perc
        print(paste(perc, "%", sep=""))
      }
    }
  }
  else {
    reslength <- (2 * cells + 1) * reso
    nro <- cells * 2 + 1
    c1 <- c((cells + 0.5) * reso,(cells + 0.5) * reso)
    for(k in 1:ncal){
      i <- ceiling(df[,2][k] / nc)
      j <- df[,2][k] %% nc
      if(i > cells && j > cells && i < nr - cells && j < nc - cells) {
        imin <- i - cells
        imax <- i + cells
        jmin <- j - cells
        jmax <- j + cells
        r <- raster(nrows=nro, ncols=nro, 
                    xmn=0, xmx=reslength, ymn=0, ymx=reslength, crs="+proj=utm +units=m")
        r[] <- debiet[imin:imax,jmin:jmax]
        T <- transition(r, transitionFunction=mean, 8, symm=FALSE) 
        T <- geoCorrection(T)
        A <- accCost(T, c1)
        head[i,j] <- DEM[i,j] - min(DEM[imin:imax,jmin:jmax][A[] < channelLength], na.rm=TRUE)
      }
      perc <- floor(k / ncal * 100)
      if (perc > oldperc){
        oldperc <- perc
        print(paste(perc, "%", sep=""))
      }
    }
  }
  return(head)
}

if(FALSE){
  channelLength = 500
  minimumDebiet = 0.1
  filledDEMsmall <- raster("step/filledDEMnew.sdat")
  cellAcc <- raster("step/cell_acc.sdat")
  debiet <- cellAcc / 1000 / 24 / 3600 * res(cellAcc)[1] ^ 2
  
  plot(filledDEMsmall)
  ext <- drawExtent()
  debietTest <- crop(debiet, ext)
  DEMTest <- crop(filledDEMsmall, ext)
  system.time(head <- HeadOnRivern(DEMTest, debietTest, channelLength=500, minimumDebiet=0.1, includeSides=FALSE))
}

