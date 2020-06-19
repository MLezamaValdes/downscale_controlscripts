
rm(list=ls())

library(raster)
library(rgdal)
library(parallel)

template <- raster("/scratch/tmp/llezamav/template_new.tif")
rasterOptions(tmpdir="/scratch/tmp/llezamav/tmp/")

iapath <- "/scratch/tmp/llezamav/ia/"
hspath <- "/scratch/tmp/llezamav/hs/"

iarespath <- "/scratch/tmp/llezamav/ia/ia_res/"
hsrespath <- "/scratch/tmp/llezamav/hs/hs_res/"
iahsrespath <- "/scratch/tmp/llezamav/ia_hs_res/"

rasterOptions(tmpdir="/scratch/tmp/llezamav/tmp/")

# # FOR MY LAPTOP
# template <- raster("D:/new_downscaling/clean_data/template_new.tif")
# 
# iapath <- "D:/new_downscaling/clean_data/ia/"
# hspath <- "D:/new_downscaling/clean_data/hs/"
# 
# iarespath <- "D:/new_downscaling/clean_data/ia/ia_res/"
# hsrespath <- "D:/new_downscaling/clean_data/hs/hs_res/"
# iahsrespath <- "D:/new_downscaling/clean_data/ia_hs_res/"

# 
# no_cores <- detectCores() - 3

iaf <- list.files(iapath, full.names = T)
hsf <- list.files(hspath, full.names = T)

write.csv(basename(iaf), paste0(iahsrespath, "iaf.csv"))
write.csv(basename(hsf), paste0(iahsrespath, "hsf.csv"))

no_cores <- 6
cl <- makeCluster(no_cores)

jnk = clusterEvalQ(cl, {library(raster); library(rgdal)})

clusterExport(cl, list("iaf", "hsf", "template", "iarespath", "hsrespath", "iahsrespath"))

parLapply(cl, seq(iaf), function(i){
  
  # get raster combo
  ia <- raster(iaf[i])
  hs <- raster(hsf[i])
  
  # resample
  iares <- resample(ia, template)
  hsres <- resample(hs, template)

  #write
  writeRaster(iares, paste0(iarespath, "res_", basename(iaf[i])), overwrite=T)
  writeRaster(hsres, paste0(hsrespath, "res_", basename(hsf[i])), overwrite=T)

  # stack
  st <- stack(iares, hsres)
  writeRaster(st, paste0(iahsrespath, "hs_", basename(iaf[i])),
              overwrite=T)
  
  rm(st)
  rm(hsres)
  rm(iares)
  rm(ia)
  rm(hs)
  gc()
  
  print(i)
})

stopCluster(cl)
