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
  
  # gdal_translate(sds[1],
  #                dstfile = paste0(hdfdir,"LST_trans_", tools::file_path_sans_ext(basename(hdffilepath)), ".tif"),
  #                overwrite=T)
  # 
  # trans <- raster(paste0(hdfdir,"LST_trans_", tools::file_path_sans_ext(basename(hdffilepath)), ".tif"))
  # 
  # warp it to wgs84 first
  # GCPs are georeferenced control points
  
  # This works, if no t_srs is supplied but it lands in front of GABON in the Gulf of Guinea without a CRS, has LONLAT extent, though
  # need to use the geolocations because it's a swath and has no coordinate reference system
  
  # gdalwarp -geoloc -r cubicspline -s_srs "epsg:4326"
  # -t_srs "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs" 
  # -tr 1 1.25 
  # TOMS-Earth-Probe_L2-TOMSEPL2_2000m1231t2303-o24142_v001-2004m0424t164259_1.vrt test.tif
  # 
  
  tif <- raster(list.files(hdfdir, pattern="HEG", full.names = T)[1])
  tif_1000 <- raster(list.files(hdfdir, pattern="HEG_res1000", full.names = T)[1])
  tif_test <- raster(list.files(hdfdir, pattern="HEG_test_707", full.names = T)[1]) 
  
  tif_test <- crop(tif_test, aoiwgs)
  tif_test[tif_test==0] <- NA
  tif_test[]

  
  # GET INFO ON PIXEL SIZE FROM GDALINFO OR WHEREVER
  
  # understand +a and +rf in HEG-Tool translated crs
  #  +proj=longlat +a=6378137 +rf=298.25722293287 +no_defs 
  # 1. what does it mean? 
  # 2. where in gdalinfo or wherever can HEGTool find that? 
  
  
  gdalwarp(sds[1], 
           dstfile = paste0(hdfdir,"LST_warp_geoloc_", tools::file_path_sans_ext(basename(hdffilepath)), ".tif"),
           tps=T, # Force use of thin plate spline transformer based on available GCPs.
           #rpc=T, # Force use of Geolocation Arrays or RPC
           verbose=TRUE,
           s_srs = wgs84,
           t_srs = wgs84,
           #t_srs=antaproj,# target spatial reference set
           overwrite = T,
           tr = c(0.062141, 0.009285),
           te = c(158.5, -78.9, 164.7, -76.0), 
           r="near")
  warp_geoloc <- raster(paste0(hdfdir,"LST_warp_geoloc_", tools::file_path_sans_ext(basename(hdffilepath)), ".tif"))
  warp_geoloc <- crop(warp_geoloc, aoiwgs)
  
  mapview(warp_geoloc)+mapview(tif_test)
  warp_geoloc
  tif_test
  
  
  aoiwgs <- spTransform(aoianta, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs "))
  ce <- extent(aoiwgs)+2
  warp_geoloc_cr <- raster::crop(warp_geoloc, ce)
  mapview(warp_geoloc_cr)
  
  
  projectedRaster <- projectRaster(warp_geoloc_cr, crs=antaproj)
  mapview(projectedRaster)
  
  
  writeRaster(warp_geoloc_cr, 
              paste0(hdfdir,"LST_warp_geoloc_", tools::file_path_sans_ext(basename(hdffilepath)), ".tif"),
              overwrite=T)
  # Geolocation data (latitude and longitude) at a coarse resolution (5 lines by
  # 5 samples) is also stored in the product
  
  
  gdalwarp(paste0(hdfdir,"LST_warp_geoloc_", tools::file_path_sans_ext(basename(hdffilepath)), ".tif"), 
           dstfile = paste0(hdfdir,"LST_warp_cropped_proj_", tools::file_path_sans_ext(basename(hdffilepath)), ".tif"),
           #tps=T, # Force use of thin plate spline transformer based on available GCPs.
           #geoloc=T, # Force use of Geolocation Arrays.
           verbose=TRUE,
           t_srs=antaproj,# target spatial reference set
           s_srs = wgs84,
           tr=c(1000,1000),
           #s_srs=wgs84,# target spatial reference set
           overwrite = T,
           #r="cubic"
           )
  
  projectedfile <- raster( paste0(hdfdir,"LST_warp_cropped_proj_", tools::file_path_sans_ext(basename(hdffilepath)), ".tif"))
  mapview(projectedfile)+mapview(aoianta)
  
  # # then reproject to antaproj & make 1000x1000m pixels
  # gdalwarp(srcfile=paste0(hdfdir,"LST_warp_", tools::file_path_sans_ext(basename(hdffilepath)), ".tif"), 
  #          dstfile=paste0(hdfdir,"LST_warp_proj_", tools::file_path_sans_ext(basename(hdffilepath)), ".tif"),
  #          verbose=TRUE, s_srs=wgs84, t_srs=antaproj, overwrite = T)
  
  x <- raster(paste0(hdfdir,"LST_warp_proj_", tools::file_path_sans_ext(basename(hdffilepath)), ".tif"))
  xc <- crop(x, aoianta)
  mapview(xc)
}


# gdal_translate HDF4_EOS:EOS_GRID:"MOD11A1.A2004091.h34v10.005.2007261231833.hdf":MODIS_Grid_Daily_1km_LST:LST_Day_1km LST.tif
# gdaltindex -tileindex location LST-extent.shp LST.tif
# ogr2ogr -segmentize 1000 LST-segmented.shp LST-extent.shp
# ogr2ogr -t_srs EPSG:4326 -wrapdateline LST-extent-wgs84.shp LST-segmented.shp
# ogr2ogr -clipsrc "POLYGON ((0 89.99, 179.99 89.99, 179.99 -89.99, 0 -89.99, 0 89.99))" LST-clipped.shp LST-extent-wgs84.shp
# gdalwarp -overwrite -t_srs EPSG:4326 -tr 0.01 0.01 LST.tif LST-wgs84.tif
# gdalwarp -overwrite -cutline LST-clipped.shp -crop_to_cutline -tr 0.01 0.01 LST-wgs84.tif LST-wgs84_clipped.tif
# 



f <- list.files(hdfdir, pattern="hdf$", full.names = T)
#hdffilepaths <- f[which(downloadedday$modscene %in% timediff_comp$modscene)]
hdffilepath <- f[which(grepl("707",  f))]

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

# get info on geolocation out of the LST
gdi <- gdalinfo(sds[11], nogcp=TRUE)
gdi2 <- gdalinfo(sds)


getfromGDALINFO <- function(what, precutoff,nparam=1){

  whatGDI = gdi[which(grepl(what, gdi))]
  #strsplit(whatGDI, split="=")[[1]][[2]]
  
  val = whatGDI %>% substr(start=precutoff, stop=nchar(whatGDI)) 
  
  if(grepl(",", val)){
      v = val %>% strsplit(split=",") 
      v = as.numeric(v[[1]])
  }
  return(v)
}


size = getfromGDALINFO(what="Size is ",precutoff = 9)
npix <- size[1]*size[2]

ext = getfromGDALINFO(what="*BOUNDINGCOORDINATE",precutoff = 9)
whatGDI = gdi[which(grepl("*BOUNDINGCOORDINATE", gdi))]

boundcord = strsplit(whatGDI, "=") %>%  lapply('[[',2) %>% as.numeric()
boundnam = unlist(strsplit(whatGDI, "=") %>%  lapply('[[',1) )
bnam = substring(boundnam, first=3, last=(nchar(boundnam)-18))

data.frame(boundcord, bnam)
# xmin, xmax, ymin, ymax
e = extent(boundcord[c(4,1,3,2)])
e/size
e*0.06
st_as_sf(e)

gdi

#0.00001° = 1.11 m
# columns and rows= 	2030 x 1354
whatGDI = gdi[which(grepl("2030", gdi))]

# ok, so size seems to be always 1354 rows x 2030 columns
# pixel 
e/size
# geographic dimensions
#2330 km x 2000 km

# extent doesn't help me, because values are on -180 and +180... 
360*111

# band 1 block
1354
738


#SPSO Parameters always 2484 and 3323


ncols=c(2030)
ncols/
spsoparameters = gdi[which(grepl("SPSOPARAMETERS", gdi))]
spsoparameters = as.numeric(strsplit(substring(spsoparameters, 18,nchar(spsoparameters)), " and ")[[1]])

size/spsoparameters

gdi[which(grepl("PIXEL", gdi))]

geoloc_fields <- which(grepl("GEOL", gdi))
latlonloc <- gdi[geoloc_fields]
gdal_translate(substring(latlonloc[1], 13,nchar(latlonloc[1])), 
               dst_dataset = paste0(hdfdir,"lon_", tools::file_path_sans_ext(basename(hdffilepath)), ".tif"))
gdal_translate(substring(latlonloc[2], 13,nchar(latlonloc[1])), 
               dst_dataset = paste0(hdfdir,"lat_", tools::file_path_sans_ext(basename(hdffilepath)), ".tif"))
lonras <- raster(list.files(hdfdir, pattern="lon", full.names=T))
latras <- raster(list.files(hdfdir, pattern="lat", full.names=T))

lon_hr <- disaggregate(lonras, fact=5)
lon_hr

latras 

xmin <- min(lonras[])
xmax <- max(lonras[])
ymin <- min(latras[])
ymax <- max(latras[])

latrasres <- resample(latras, lst)
lonrasres <- resample(lonras, lst)

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


