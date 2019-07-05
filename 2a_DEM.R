# 2a get DEM


library(raster)
library(mapview)
library(rgdal)


############  get REMA DEM ###############################

dempath <- "D:/DEM_8m/tiles_westcoast/"

zipf <- list.files(dempath, full.names = T, pattern=".tar.gz")
for(i in seq(zipf)){
  untar(zipf[i],  compressed = 'gzip', exdir=paste0(dempath, tools::file_path_sans_ext(tools::file_path_sans_ext(basename(zipf[i])))))
}

# get all the dem tiles
# sdirs <- list.files(dempath, pattern="m$", full.names = T)
# dems <- list.files(sdirs, pattern="dem.tif$", full.names = T)
# 
demtilepath <- paste0(dempath, "all/")
# file.copy(dems, paste0(demtilepath, basename(dems)))

dt <- list.files(demtilepath, full.names=T)
dem_tiles <- lapply(seq(dt), function(i){
  r <- raster(dt[i])
  r[r<(-300)] <- NA
  r
})

dem_tiles


########## MERGE TILES ############################

#generate command for merging all the tiles
cm <- lapply(seq(dem_tiles), function(i){
    if(i < seq(dem_tiles)[length(seq(dem_tiles))]){
    print(paste0("dem_tiles[[", i, "]], "))
  } else {
    print(paste0("dem_tiles[[", i, "]]"))
  }
})

mrg <- paste(cm[2:length(cm)], sep="", collapse="")
commd <- paste("merge(dem_tiles[[1]],", mrg, ", ext=NULL)")

mos <- merge(dem_tiles[[1]], dem_tiles[[2]], dem_tiles[[3]], dem_tiles[[4]], dem_tiles[[5]], dem_tiles[[6]], dem_tiles[[7]], dem_tiles[[8]], dem_tiles[[9]], dem_tiles[[10]], dem_tiles[[11]], dem_tiles[[12]], dem_tiles[[13]], dem_tiles[[14]], dem_tiles[[15]], dem_tiles[[16]], dem_tiles[[17]], dem_tiles[[18]] , ext=NULL)
writeRaster(mos, paste0(dempath, "dem_mosaic_8m_westcoast_corrected.tif"), format="GTiff")


# make a mosaic
mos <- raster(paste0(dempath, "dem_mosaic_8m_westcoast.tif"))
# filter out senseless values

# filter mosaic 3x3 
mos_3 <- focal(mos, w=matrix(1/9,nrow=3,ncol=3))
mos_c <- mos_3
# correct for negative NA values
mos_c[mos_c < (-300)] <- NA
writeRaster(mos_c, paste0(dempath, "dem_mosaic_8m_westcoast_filter_3x3_corr2.tif"), format="GTiff", overwrite=T)

# resample to 30m
mos_3_30 <- aggregate(mos_c, fact=(30/8))
writeRaster(mos_3_30, paste0(dempath, "dem_mosaic_30m_westcoast_filter_3x3_corr.tif"), format="GTiff", overwrite=T)


mos_c <- raster(paste0(dempath, "dem_mosaic_8m_westcoast_filter_3x3_corr2.tif"))


############  get 200m DEM to fill NA values ####################################################################

dpath<- "E:/Antarctica/DEM/demwgs200_v2.tar/wgs84_200m/wgs84_200m/hdr.adf"
x <- new("GDALReadOnlyDataset", dpath)
getDriver(x)
getDriverLongName(getDriver(x))
xx<-asSGDF_GROD(x)
dem200 <- raster(xx)
writeRaster(dem200, "E:/Antarctica/DEM/DEM_WGS200.tif", format="GTiff")

dem200 <- raster("E:/Antarctica/DEM/DEM_WGS200.tif")


############  CUT TO AOI ####################################################################

l8proj <- crs("+proj=stere +lat_0=-90 +lat_ts=-71 +lon_0=0 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m
+no_defs +ellps=WGS84 +towgs84=0,0,0")
aoipath <- "D:/run_everything/Antarctica/aoi/MDV/"
aoi <- readOGR(list.files(aoipath, pattern="adp.shp", full.names = T))
aoi <- spTransform(aoi, l8proj)

mos_caoi <- crop(mos_c, aoi)
dem200aoi <- crop(dem200, aoi)



############  fill NA values  ####################################################################

# get them to the same resolution
dem200hr <- resample(dem200aoi, mos_caoi)

# introduce dem200 in mos_c for all locations that are na in mos_c
mos_filled <- overlay(dem200hr, mos_caoi, fun = function(x, y) {
  y[is.na(y[])] <- x
  return(x)
})
writeRaster(mos_filled, "E:/Antarctica/DEM/MDV_8m_filled.tif", format="GTiff")


############  cut DEM to everything lower than XXX m in order to try TWI for the valleys ###############################

valley_DEM <- raster("E:/Antarctica/DEM/MDV_8m_filled.tif")
# path <- "E:/Antarctica/rock_outcrop/Rock_outcrop_high_res_from_landsat_8/"
# roc <- readOGR(list.files(path, pattern=".shp", full.names = T))

library(mapview)
valley_DEM[valley_DEM > 600] <- NA
mapview(valley_DEM)

writeRaster(valley_DEM, "E:/Antarctica/DEM/MDV_8m_valley_floor.tif", format="GTiff")


############################### tranlate to saga grid ##############################################################
library(gdalUtils)
library(raster)

path_saga_norm <- "C:/OSGeo4W64/apps/saga-ltr/"
sagaCmd <- paste0(path_saga_norm, "saga_cmd.exe")

saga_outpath <- "E:/Antarctica/runoff_paths/"

dem <- "E:/Antarctica/DEM/MDV_8m_filled.tif"

gdalUtils::gdalwarp(dem, paste0(saga_outpath,"MDV_DEM_8m_filled.sdat"), 
                    overwrite=TRUE,  of='SAGA')


