
library(raster)
library(mapview)
library(rgdal)

# fix AOA .tif issue
main <- "D:/downscaling_after_talk/aoa/results_split/"
tifdir <- "D:/downscaling_after_talk/spatial_predictions_rf/predstacks/"
spf <- list.files(tifdir, pattern="split", full.names = T)
aoaRDSf <- list.files(main, pattern="RDS", full.names=T)
nam <- read.csv2(list.files(tifdir, pattern="names", full.names = T))


splitras <- lapply(seq(spf), function(i){
  x <- stack(spf[[i]])
  names(x) <- nam$x
  x
})


aoaRDS <- lapply(seq(aoaRDSf), function(i){
  readRDS(aoaRDSf[i])
})

aoa_fixed <- lapply(seq(aoaRDS), function(i){
  
  aoaras <- splitras[[i]]$dem
  aoaras[!is.na(aoaras)] <- aoaRDS[[i]]$AOA
  
  writeRaster(aoaras, paste0(tifdir, "split_", i, ".grd"))
  
  return(aoaras)
  
})


aoa_f_m <- do.call(merge, aoa_fixed)

mapview(aoa_f_m)

aoa_f_m <- raster::ratify(aoa_f_m)

# soil raster was not available for an area at the side of the research area, 
# this will be filtered out
no_soil_poly <- readOGR(list.files(tifdir, pattern="shp", full.names=T))

nspr <- rasterize(no_soil_poly, aoa_f_m)
aoa_f_m[nspr==1] <- NA

writeRaster(aoa_f_m, paste0(tifdir, "AOA_MYD11_L2.A2018316.1350.006.2.tif"),
            overwrite=T)
