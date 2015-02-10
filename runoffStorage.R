require(raster)
storage.fun <- function(factor, debiet, noSteps){
  matrix.fun <- function(x){c(seq(x-1,0),ifelse(rep(x!=noSteps,noSteps-x),seq(noSteps-1,x),c()))}
  mat <- t(apply(matrix(seq(1,noSteps),ncol=1), MARGIN=1,FUN=matrix.fun))
  factors <- apply(mat, MARGIN=c(1,2),FUN=function(x){factor*(1-factor)^x})
  res <- apply(factors, MARGIN=1,FUN=function(x){sum(debiet * x)})
  return(stack(res))
}
testplot.storages <- function(res, debiet, poi){
  nul <- extract(debiet,poi)
  facts <- lapply(res,FUN=extract,y=poi)
  plot(nul[1,],col="red", type='b', xlab="step", ylab="runoff (m3/s)", xlim=c(1,nlayers(debiet)))
  cols <- c("blue", "green")
  for(i in 1:length(facts)){
    lines(facts[[i]][1,], col=cols[i], type='b')
  }
  lines(rep(0,12), col="black", type='l', lty=2)
  legend("topleft", legend=c("1.0",0.5,0.7), col=c("red",cols), pch=1)
  title(paste(sprintf("Runoff at point %d, %d for factors", 
                      as.integer(poi@coords)[1], as.integer(poi@coords)[2]), 
              do.call(paste, c("1.0",factors,sep=", "))))
}


