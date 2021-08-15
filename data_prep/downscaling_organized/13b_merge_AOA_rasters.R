# checkAoA results

library(raster)
library(mapview)

aoadir <- "D:/downscaling_after_talk/aoa/results_split/"
resfR <- list.files(aoadir, pattern="RDS",full.names=T)
aoa_results <- lapply(seq(resfR), function(i){
  readRDS(resfR[[i]])
})

resfT <- list.files(aoadir, pattern="tif",full.names=T)
aoa_ras <- lapply(seq(resfT), function(i){
  raster(resfT[[i]])
})

AOA_ras <- do.call(merge, aoa_ras)
AOA_rat <- ratify(AOA_ras)
writeRaster(AOA_rat, paste0(aoadir, "AOA_MYD11_L2.A2018316.1350.006_cat.grd"),
            overwrite=T)
writeRaster(AOA_ras, paste0(aoadir, "AOA_MYD11_L2.A2018316.1350.006.grd"),
            overwrite=T)



