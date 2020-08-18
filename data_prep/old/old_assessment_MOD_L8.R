
######## ASSESSMENT OF DOWNLOADED DATA ################
j=1
modisscenepath <- paste0(modispath, time_range[[j]][1], "/")
hdfpath <- paste0(modisscenepath, "hdfs/")
MODtifHDFoutpath <- paste0(modisscenepath, "translated/")
MODLSTpath <- paste0(modisscenepath, "LST/")


modsubdirs <- list.files(modispath, pattern="....-..-..$", full.names = T)
l8subdirs <- list.files(L8datpath, pattern="....-..-..$", full.names=T)

modhdfs <- lapply(seq(modsubdirs), function(i){
  x <- list.files(paste0(modsubdirs[i], "/get_data/MODIS/"), full.names = T)
  list.files(x)
})

l8org <- lapply(seq(l8subdirs), function(i){
  x <- list.files(paste0(l8subdirs[i], "/get_data/LANDSAT/L1/"), full.names = T)
  list.files(x, pattern=".TIF$", full.names=T)
})


######### MODIS LST mosaic ############################
modlstmos <- raster(paste0(modisscenepath, areaname, "_MODIS_LST_Mosaic.tif"))

######### take a look at time difference ########
tdiff <- raster(paste0(modispath, "date/time_rasters", areaname, "_", time_range[[j]][[1]], ".tif"))

