# check SWIR files
swiroutpath <- "D:/new_downscaling/SWIR/composites/"

fls <- list.files(swiroutpath, pattern="tc_67", full.names = T)


swirstacks <- lapply(seq(fls), function(i){
  raster(fls[i])
})

par(mfrow=c(4,4), mar=c(5,3,2,2)+2)
lapply(seq(swirstacks), function(i){
  plot(swirstacks[[i]], main=substring(basename(fls[i]), 11,17))
})
