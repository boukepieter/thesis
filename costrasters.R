require(raster)
require(rgdal)
require(gdistance)  
require(doParallel)
require(foreach)
require(plyr)
require(utils)

HeadOnRiver.large <- function(DEM, debiet, channelLength = 500, minimumDebiet = 0.1, 
                              splitSize = 200, includeSides = TRUE) {
  #TODO(text)
  reso <- res(DEM)[1]
  cells <-  round(channelLength / reso)
  
  rowIte <- ceiling(nrow(DEM) / splitSize)
  colIte <- ceiling(ncol(DEM) / splitSize)
  print(paste("splitting up in", rowIte * colIte, "pieces..."))
  
  
  
  DEMparts <- NULL
  DEMparts[1:rowIte * colIte] <- list(NULL)
  debietParts <- NULL
  debietParts[1:rowIte * colIte] <- list(NULL)
  # i=row j=column
  istarts <- rep(0, rowIte * colIte) # starting cell chunk
  imins <- rep(0, rowIte * colIte) # minimum cell with buffer
  iends <- rep(0, rowIte * colIte) # ending cell of the chunk
  imaxs <- rep(0, rowIte * colIte) # maximum cell with buffer
  jstarts <- rep(0, rowIte * colIte)
  jmins <- rep(0, rowIte * colIte)
  jends <- rep(0, rowIte * colIte)
  jmaxs <- rep(0, rowIte * colIte)
  n <- 1
  
  for(k in 1:rowIte){
    istart <- 1 + (k - 1) * splitSize
    imin <- istart - cells
    imin <- max(1, imin)
    iend <- splitSize + (k - 1) * splitSize
    imax <- iend + cells
    imax <- min(nrow(DEM),imax)
    
    istarts[n:(n+colIte-1)] <- istart
    imins[n:(n+colIte-1)] <- imin
    iends[n:(n+colIte-1)] <- iend
    imaxs[n:(n+colIte-1)] <- imax
    for(l in 1:colIte){
      jstart <- 1 + (l - 1) * splitSize
      jmin <- jstart - cells
      jmin <- max(1, jmin)
      jend <- splitSize + (l - 1) * splitSize
      jmax <- jend + cells
      jmax <- min(ncol(DEM),jmax)
      
      jstarts[n] <- jstart
      jmins[n] <- jmin
      jends[n] <- jend
      jmaxs[n] <- jmax
      
      DEMpart <- raster(nrows=imax - imin + 1, ncols=jmax - jmin + 1, 
                        xmn=xmin(DEM) + (jmin - 1) * reso, 
                        xmx=xmin(DEM) + (jmax) * reso, 
                        ymn=ymax(DEM) - imax * reso, 
                        ymx=ymax(DEM) - (imin - 1) * reso, 
                        crs=projection(DEM))
      DEMpart[] <- DEM[imin:imax,jmin:jmax]
      DEMparts[[n]] <- DEMpart
      
      debietPart <- raster(nrows=imax - imin + 1, ncols=jmax - jmin + 1, 
                           xmn=xmin(DEM) + (jmin - 1) * reso, 
                           xmx=xmin(DEM) + (jmax) * reso, 
                           ymn=ymax(DEM) - imax * reso, 
                           ymx=ymax(DEM) - (imin - 1) * reso, 
                           crs=projection(DEM))
      debietPart[] <- debiet[imin:imax,jmin:jmax]
      debietParts[[n]] <- debietPart
      n <- n + 1
    }
  }
  
  
  print("clusters made, starting calculations...")
  cl <- makeCluster(6)  # Use 6 cores
  registerDoParallel(cl) # register these 3 cores with the "foreach" package
  ##foreach(i=1:3) %dopar% sqrt(i)  #run a loop in parallel
  ##aaply(1:3, sqrt, .parallel=TRUE)  #apply a function across a vector in parallel
  
  writeLines(c(""), "log.txt")  
  headParts <- foreach(i=1:(n-1), .packages=c("gdistance","raster"), .export="HeadOnRiver",
                       .errorhandling="pass") %dopar% {
                         
                         sink("log.txt", append=TRUE)
                         cat(paste("\n","Starting iteration",i,"\n"))
                         if (imins[i] == 1 || imaxs[i] == nrow(DEM) || jmins[i] == 1 || jmaxs[i] == ncol(DEM)){
                           headPart <- HeadOnRiver(DEMparts[[i]], debietParts[[i]], channelLength, minimumDebiet, includeSides)
                         }
                         else{
                           headPart <- HeadOnRiver(DEMparts[[i]], debietParts[[i]], channelLength, minimumDebiet, includeSides=FALSE)
                         }
                         cat(paste("\n", "Result", i, "\n"))
                         print(headPart)
                         sink()
                         headPart
                       }
  stopCluster(cl)
  #closeAllConnections()
  
  
  print("calculations done, sewing back together...")
  head <- DEM
  head[] <- 0
  names(head) <- "Head"
  done <- 0
  for(i in 1:(n-1)) {
    head[istarts[i]:min(iends[i], imaxs[i]),jstarts[i]:min(jends[i],jmaxs[i])] <- 
      headParts[[i]][(1+istarts[i]-imins[i]):(nrow(headParts[[i]]) + min(iends[i],imaxs[i]) - imaxs[i]),
                     (1+jstarts[i]-jmins[i]):(ncol(headParts[[i]]) + min(jends[i],jmaxs[i]) - jmaxs[i])]
    done = done + 1
    print(paste(done, "/", colIte * rowIte, " done...", sep=""))
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
  if (ncal > 0){
    if(includeSides){
      for(k in 1:ncal) {
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
        
        #       perc <- floor(k / ncal * 100)
        #       if (perc > oldperc){
        #         oldperc <- perc
        #         print(paste(perc, "%", sep=""))
        #       }
      }
    }
    else {
      reslength <- (2 * cells + 1) * reso
      nro <- cells * 2 + 1
      c1 <- c((cells + 0.5) * reso,(cells + 0.5) * reso)
      
      for(k in 1:ncal) {
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
        #       perc <- floor(k / ncal * 100)
        #       if (perc > oldperc){
        #         oldperc <- perc
        #         print(paste(perc, "%", sep=""))
        #       }
      }
    }
  }
  return(head)
}

if(FALSE){
  channelLength = 500
  minimumDebiet = 0.1
  filledDEMsmall <- raster("step/filledDEM.sdat")
  cellAcc <- stack(sprintf("step/cell_acc%02d.sdat", 1:noSteps))
  debiet <- cellAcc / 1000 / 24 / 3600 * res(cellAcc)[1] ^ 2
  debiet <- max(debiet)
  
  plot(filledDEMsmall)
  ext <- drawExtent()
  debietTest <- crop(debiet, ext)
  DEMTest <- crop(filledDEMsmall, ext)
  system.time(head <- HeadOnRiver.large(filledDEMsmall, debiet, channelLength=500, minimumDebiet=0.1, 
                                        includeSides=FALSE, splitSize=100))
  # 404
  # 110
  
  cell <- function(k) {
    i <- ceiling(dfe[k,2] / nc)
    j <- dfe[k,2] %% nc
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
    res <- DEM[i,j] - min(DEM[imin:imax,jmin:jmax][A[] < channelLength], na.rm=TRUE)
    
    return(res)
  }
  
  system.time(res <- lapply(1:ncal, cell))
  require(parallel)
  require(snow)
  beginCluster(6)
  
  system.time(res2 <- mclapply(1:ncal, cell))
  
  system.time( mclapply(1:4, function(xx){ Sys.sleep(10) }) )
  source("../workspace/thesis/mclapply.hack.R")
  
  
  df[!is.na(df["value"]),] <- res
  head[] <- df["value"]
  
}

