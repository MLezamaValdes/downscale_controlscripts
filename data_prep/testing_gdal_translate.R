library(gdalUtils)
library(tidyr)

# projection info: 
#https://gdal.org/development/rfc/rfc4_geolocate.html 


f <- list.files(indir, pattern="hdf$", full.names = T)
heglst <- raster(list.files(indir, pattern="LST.tif$", full.names = T))
mapview(heglst)
crs(heglst)
extent(heglst)
hegproj <- "+proj=longlat +a=6378137 +rf=298.25722293287 +no_defs"
sinuproj <-  "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

# get LST subdataset
sds <- get_subdatasets(f)

# get Geolocation information:
geolocation_start <- which(grepl("Geolocation", gdi))[2]

scrwn_info <- gdi[seq(geolocation_start+1,geolocation_start+4)]
scrwn_info <- extract_numeric(scrwn_info)

gdal_translate(sds[1], dst_dataset = paste0(indir,"LST_", tools::file_path_sans_ext(basename(f)), ".tif"),
               projwin_srs=sinuproj)

#gdalwarp -of GTIFF -tps -t_srs EPSG:4674 HDF4_EOS:EOS_SWATH:"MOD11_L2.A2014001.1305.006.2016179220648.hdf":MOD_Swath_LST:LST LST.tif
antaproj <- "+proj=stere +lat_0=-90 +lat_ts=-71 +lon_0=0 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs "
wgs84 <- "+proj=longlat +datum=WGS84 +no_defs"
sinuproj
gdalwarp(sds[1], dstfile = paste0(indir,"LST_warp_", tools::file_path_sans_ext(basename(f)), ".tif"),
               tps=T, verbose=TRUE, t_srs=wgs84, overwrite = T, geoloc = T)

gdalwarp(paste0(indir,"LST_warp_", tools::file_path_sans_ext(basename(f)), ".tif"), 
         dstfile=paste0(indir,"LST_warp_proj_", tools::file_path_sans_ext(basename(f)), ".tif"),
         verbose=TRUE, s_srs=wgs84,t_srs=antaproj,tr=c(1,1),overwrite = T)
# get raster
lst <- raster(list.files(indir, pattern="^LST_warp_proj", full.names = T))
lst


mapview(lst)

#gdal_translate(sds[1], dst_dataset = paste0(indir,"LST_", tools::file_path_sans_ext(basename(f)), ".tif"))

# get info on geolocation out of the LST 
gdi <- gdalinfo(sds[1])
geoloc_fields <- which(grepl("GEOL", gdi))
latlonloc <- gdi[geoloc_fields]
gdal_translate(substring(latlonloc[1], 13,nchar(latlonloc[1])), dst_dataset = paste0(indir,"lon_", tools::file_path_sans_ext(basename(f)), ".tif"))
gdal_translate(substring(latlonloc[2], 13,nchar(latlonloc[1])), dst_dataset = paste0(indir,"lat_", tools::file_path_sans_ext(basename(f)), ".tif"))
lonras <- raster(list.files(indir, pattern="lon", full.names=T))
latras <- raster(list.files(indir, pattern="lat", full.names=T))
xmin <- min(lonras[])
xmax <- max(lonras[])
ymin <- min(latras[])
ymax <- max(latras[])

latrasres <- resample(latras, lst)
lonrasres <- resample(lonras, lst)

latrasres <- aggregate(latras, fact= ncell(lst)/ncell(latras))
latrasres_lstres <- latrasres
res(latrasres_lstres) <- c(1,1)
latrasres_lstres[] <- latrasres[]

latrasres_lst <- aggregate(latrasres, fact= res(lst)/res(latrasres))

plot(latrasres_lst)

aggregate(latrasres, fact=)


plot(latrasres_res)

res(latrasres) <- res(lst)
plot(latrasres)
plot(lst)

ncell(latras)*0.04

par(mfrow=c(1,3))
plot(latras)
plot(lonras)
plot(lst)

plot(latrasres)
plot(lonrasres)
plot(lst)

xyz <- data.frame(lonrasres[], latrasres[], lst[])
names(xyz) <- c("x", "y", "lst")
nrow(xyz)
lstproj <- rasterFromXYZ(xyz, res=res(lst))




# lstheg <- raster(list.files(indir, pattern="LST.tif$", full.names = T))
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





trl_trf <- list.files(indir, pattern = paste0("LST_", tools::file_path_sans_ext(basename(f))), full.names = T)
gdaltransform(trl_trf, paste0(indir,"LST_crs_", tools::file_path_sans_ext(basename(f)), ".tif"),
              )


lst

extent(lst)
extent(lstheg)

lstc <- crop(lstheg[[i]],aoiwgs)
lstcm <- mask(lstc, aoiwgs)


