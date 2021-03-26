library(gdalUtils)
library(tidyr)

# projection info: 
#https://gdal.org/development/rfc/rfc4_geolocate.html 

hdfdir <- "E:/new_downscaling/data_download_preprocessing/MODIS/2019-01/hdfs/"
L8scenedir <- "D:/downscaling_after_talk/data_download_preprocessing/L8/2019-01/"


timediff_comp <- read.csv2(paste0(L8scenedir, "timediff_comp.csv"))

# prepare template raster 1x1k
tmplras <- template
res(tmplras) <- c(1000, 1000)
tmplras[] <- 1

antaproj <- crs(aoianta)
wgs84 <- "+proj=longlat +datum=WGS84 +no_defs"

warp_MOD_Swath <- function(hdffilepath){
  # get LST subdataset
  hdf4_dataset <- system.file(hdffilepath, package="gdalUtils")
  sds <- get_subdatasets(hdffilepath)
  
  gdal_translate(sds[1],dstfile = paste0(hdfdir,"LST_trans_", tools::file_path_sans_ext(basename(hdffilepath)), ".tif"))
  
  trans <- raster(paste0(hdfdir,"LST_trans_", tools::file_path_sans_ext(basename(hdffilepath)), ".tif"))
  
  # warp it to wgs84 first
  gdalwarp(sds[1], dstfile = paste0(hdfdir,"LST_warp_", tools::file_path_sans_ext(basename(hdffilepath)), ".tif"),
           tps=T, verbose=TRUE,t_srs=antaproj, overwrite = T,geoloc=T,r="cubic" )
  
  # # then reproject to antaproj & make 1000x1000m pixels
  # gdalwarp(srcfile=paste0(hdfdir,"LST_warp_", tools::file_path_sans_ext(basename(hdffilepath)), ".tif"), 
  #          dstfile=paste0(hdfdir,"LST_warp_proj_", tools::file_path_sans_ext(basename(hdffilepath)), ".tif"),
  #          verbose=TRUE, s_srs=wgs84, t_srs=antaproj, overwrite = T)
  
  x <- raster(paste0(hdfdir,"LST_warp_proj_", tools::file_path_sans_ext(basename(hdffilepath)), ".tif"))
  xc <- crop(x, aoianta)
  mapview(xc)
}


gdal_translate HDF4_EOS:EOS_GRID:"MOD11A1.A2004091.h34v10.005.2007261231833.hdf":MODIS_Grid_Daily_1km_LST:LST_Day_1km LST.tif
gdaltindex -tileindex location LST-extent.shp LST.tif
ogr2ogr -segmentize 1000 LST-segmented.shp LST-extent.shp
ogr2ogr -t_srs EPSG:4326 -wrapdateline LST-extent-wgs84.shp LST-segmented.shp
ogr2ogr -clipsrc "POLYGON ((0 89.99, 179.99 89.99, 179.99 -89.99, 0 -89.99, 0 89.99))" LST-clipped.shp LST-extent-wgs84.shp
gdalwarp -overwrite -t_srs EPSG:4326 -tr 0.01 0.01 LST.tif LST-wgs84.tif
gdalwarp -overwrite -cutline LST-clipped.shp -crop_to_cutline -tr 0.01 0.01 LST-wgs84.tif LST-wgs84_clipped.tif




f <- list.files(hdfdir, pattern="hdf$", full.names = T)
#hdffilepaths <- f[which(downloadedday$modscene %in% timediff_comp$modscene)]
hdffilepaths <- f[which(grepl("707",  f))]

if(length(hdffilepaths) == nrow(timediff_comp)){
  print("all scenes in timediff_comp found")
} else {
  print("not all scenes found")
}

LST_warp_proj <- lapply(seq(length(hdffilepaths)), function(i){
  warp_MOD_Swath(hdffilepaths[i])
})


# convert values to valid range and degree C
print("converting values to valid range and degree Celsius")
dir.create(MODLSTpath)
lst_c <- lapply(seq(LST_warp_proj), function(i){
  lstc <- crop(LST_warp_proj[[i]],aoianta)
  mapview(lstc)
  # resample to template
  x <- resample(LST_warp_proj[[i]], tmplras)
  
  lstcm <- mask(lstc, aoianta)
  
  # Valid Range = 7500-65535
  lstcm[lstcm == 0 ] <- NA
  lstcm[lstcm < 7500 & lstcm > 65535] <- NA
  
  # scale factor = 0.02
  lst_1_conv <- lstcm*0.02
  
  # convert to degree C
  lstc <- lst_1_conv - 273.15
  

  
  writeRaster(x, paste0(MODLSTpath, "cels_", names(lst[[i]]), ".tif"), format="GTiff",
              overwrite=T)
  
  print(i)
  return(lstc)
})


#gdal_translate(sds[1], dst_dataset = paste0(hdfdir,"LST_", tools::file_path_sans_ext(basename(f)), ".tif"))

# # get info on geolocation out of the LST 
# gdi <- gdalinfo(sds[1])
# geoloc_fields <- which(grepl("GEOL", gdi))
# latlonloc <- gdi[geoloc_fields]
# gdal_translate(substring(latlonloc[1], 13,nchar(latlonloc[1])), dst_dataset = paste0(hdfdir,"lon_", tools::file_path_sans_ext(basename(f)), ".tif"))
# gdal_translate(substring(latlonloc[2], 13,nchar(latlonloc[1])), dst_dataset = paste0(hdfdir,"lat_", tools::file_path_sans_ext(basename(f)), ".tif"))
# lonras <- raster(list.files(hdfdir, pattern="lon", full.names=T))
# latras <- raster(list.files(hdfdir, pattern="lat", full.names=T))
# xmin <- min(lonras[])
# xmax <- max(lonras[])
# ymin <- min(latras[])
# ymax <- max(latras[])
# 
# latrasres <- resample(latras, lst)
# lonrasres <- resample(lonras, lst)
# 
# latrasres <- aggregate(latras, fact= ncell(lst)/ncell(latras))
# latrasres_lstres <- latrasres
# res(latrasres_lstres) <- c(1,1)
# latrasres_lstres[] <- latrasres[]
# 
# latrasres_lst <- aggregate(latrasres, fact= res(lst)/res(latrasres))
# 
# plot(latrasres_lst)
# 
# aggregate(latrasres, fact=)
# 
# 
# plot(latrasres_res)
# 
# res(latrasres) <- res(lst)
# plot(latrasres)
# plot(lst)
# 
# ncell(latras)*0.04
# 
# par(mfrow=c(1,3))
# plot(latras)
# plot(lonras)
# plot(lst)
# 
# plot(latrasres)
# plot(lonrasres)
# plot(lst)
# 
# xyz <- data.frame(lonrasres[], latrasres[], lst[])
# names(xyz) <- c("x", "y", "lst")
# nrow(xyz)
# lstproj <- rasterFromXYZ(xyz, res=res(lst))
# 
# 
# 

# lstheg <- raster(list.files(hdfdir, pattern="LST.tif$", full.names = T))
# mapview(lstheg)

# assign extent to LST file
#crs(lst) <- sinuproj
crs(lst) <- crs(heglst)

#extent(lst) <- extent(c(xmin, xmax, ymin, ymax))
#mapview(lst)
wgs84 <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
lst_reprojected <- projectRaster(lst, crs=hegproj)
extent(lst_reprojected) <- extent(c(xmin, xmax, ymin, ymax))

mapview(lst_reprojected)

crs(lst) <- "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
crs(lst) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs "


test <- lst
crs(test) <- hegproj  

lstc <- crop(lst[[i]],aoiwgs)
lstcm <- mask(lstc, aoiwgs)
mapview(lstcm)

# Valid Range = 7500-65535
lstcm[lstcm == 0 ] <- NA
lstcm[[i]][lstcm[[i]] < 7500 & lstcm[[1]] > 65535] <- NA

# scale factor = 0.02
lst_1_conv <- lstcm*0.02

# convert to degree C
lstc <- lst_1_conv - 273.15





trl_trf <- list.files(hdfdir, pattern = paste0("LST_", tools::file_path_sans_ext(basename(f))), full.names = T)
gdaltransform(trl_trf, paste0(hdfdir,"LST_crs_", tools::file_path_sans_ext(basename(f)), ".tif"),
              )


lst

extent(lst)
extent(lstheg)

lstc <- crop(lstheg[[i]],aoiwgs)
lstcm <- mask(lstc, aoiwgs)


