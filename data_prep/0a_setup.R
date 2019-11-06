library(raster)
library(mapview)
library(rgdal)
library(gdalUtils)
library(getSpatialData)
library(sf)
library(downscaleRS)
library(RStoolbox)
library(satellite)
library(RColorBrewer)
library(viridis)
library(lubridate)



time_range <- lapply(seq(year), function(j){
  lapply(seq(month), function(i){
    y <- paste(paste0(year[j],"-",month[i],"-",day), 
               paste0(year[j],"-",month[i],"-",day))
    strsplit(y, " ")
  })
})

#dir.create(file.path(aoipath), recursive = T)
dir.create(file.path(L8datpath), recursive = T)
dir.create(file.path(modispath), recursive = T)
dir.create(file.path(tdpath), recursive = T)
#dir.create(paste0(main, "timediff/"))


######## determine AOI ################
aoi <- readOGR(aoip)
antaproj <- crs("+proj=stere +lat_0=-90 +lat_ts=-71 +lon_0=0 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m
+no_defs +ellps=WGS84 +towgs84=0,0,0")
aoianta <- spTransform(aoi, antaproj)
aoiutm <- spTransform(aoi, crs("+proj=utm +zone=57 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs "))


####### get rock outcrop to assign emissivity ############
if(newarea==1){
  roc <- readOGR("D:/Antarctica/rock_outcrop/Rock_outcrop_high_res_from_landsat_8/Rock_outcrop_high_res_from_landsat_8.shp")
  roc <- crop(roc, aoianta)
  rroc <- rasterize(roc, btcmerge, progress = "text")
  rroc[!is.na(rroc)]<-0.94 # everything that's not NA = rock, eta for rock = 0.94
  rroc[is.na(rroc)]<-0.97 # what's NA is snow and ice, emissivity = 0.97
  writeRaster(rroc, paste0(main, "Rock_outcrop_ras_", areaname, ".tif"), format="GTiff", overwrite=T)
}

#### high resolution land polygon
if(newarea==1){
  cl <- readOGR(list.files(clpath, pattern="polygon.shp", full.names=T))
  land <- cl[cl$surface=="land" | cl$surface=="ice tongue",]
  writeOGR(land, paste0(clpath, "land_contours_", areaname ,".shp"), driver = "ESRI Shapefile", layer="land_contours.shp")
} else {
  land <- readOGR(paste0(clpath, "land_contours_", areaname, ".shp"))
}






######## functions ################
scriptsp <- "C:/Users/mleza/OneDrive/Documents/PhD/work_packages/auto_downscaling_30m/downscale_controlscripts/"
source(paste0(scriptsp, "read_meta_L8_PS.R"))

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

# until function incorporated in downscaleRS
source("C:/Users/mleza/OneDrive/Documents/PhD/work_packages/auto_downscaling_30m/downscaleRS/R/datestodoymod.R")
source(paste0(scriptpath, "1_DEM.R"))
source(paste0(scriptpath, "2_Landsat.R"))
source(paste0(scriptpath, "3_MODIS.R"))

