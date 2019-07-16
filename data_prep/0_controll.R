

####### OVERALL CONTROLL SCRIPT ######## 

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


######## GENERAL INFO ABOUT RUN AND AREA ######## 
# newarea can be 0 (no) or 1 (yes). If area is not new, it is assumed that 
# the static portion of the script is already available and doesn't need to be unzipped etc.
newarea <- 0
areaname <- "MDV"

# times should be 
time_range <-  list(
  #c("2019-02-19", "2019-02-19"),
  c("2019-01-19", "2019-01-19"),
  c("2018-07-19", "2018-07-19"), # to test if useful
  #c("2018-01-19", "2018-01-19"), # already processed
  c("2018-02-19", "2018-02-19"), 
  c("2017-12-19", "2017-12-19"),
  c("2017-02-19", "2017-02-19"),
  c("2017-01-19", "2017-01-19"),
  c("2016-12-19", "2016-12-19"),
  c("2016-02-19", "2016-02-19"),
  c("2016-01-19", "2016-01-19"),
  c("2015-01-19", "2015-01-19"),
  c("2014-01-19", "2014-01-19"))


###### paths #############################
scriptpath <- "C:/Users/mleza/OneDrive/Documents/PhD/work_packages/auto_downscaling_30m/downscale_controlscripts/data_prep/"
main <- "E:/Antarctica/testrun_loop/"
dempath <- "D:/DEM_8m/tiles_westcoast/"
aoipath <-  "D:/Antarctica/aoi/MDV/"
L8datpath <- paste0(main, "L8/")
modispath <- paste0(main, "MODIS/")
tdpath <-paste0(main, "timediff/")

dir.create(file.path(aoipath), recursive = T)
dir.create(file.path(L8datpath), recursive = T)
dir.create(file.path(modispath), recursive = T)
dir.create(paste0(main, "timediff/"))


######## determine AOI ################
antaproj <- crs("+proj=stere +lat_0=-90 +lat_ts=-71 +lon_0=0 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m
+no_defs +ellps=WGS84 +towgs84=0,0,0")
aoi <- readOGR(list.files(aoipath, pattern="adp.shp", full.names = T))
aoianta <- spTransform(aoi, antaproj)
aoiutm <- spTransform(aoi, crs("+proj=utm +zone=57 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs "))

####### get rock outcrop to assign emissivity ############
if(newarea==1){
  roc <- readOGR("E:/Antarctica/rock_outcrop/Rock_outcrop_high_res_from_landsat_8/Rock_outcrop_high_res_from_landsat_8.shp")
  roc <- crop(roc, aoianta)
  rroc <- rasterize(roc, btcmerge, progress = "text")
  rroc[!is.na(rroc)]<-0.94 # everything that's not NA = rock, eta for rock = 0.94
  rroc[is.na(rroc)]<-0.97 # what's NA is snow and ice, emissivity = 0.97
  writeRaster(rroc, paste0(main, "Rock_outcrop_ras_", areaname, ".tif"), format="GTiff", overwrite=T)
}



######## paths for translating to SAGA ######## 
path_saga_norm <- "C:/OSGeo4W64/apps/saga-ltr/"
sagaCmd <- paste0(path_saga_norm, "saga_cmd.exe")
saga_outpath <- paste0(main, "SAGA_run/")

######## paths for batch processing in HEG tool #########


# make an batchindir directory with the hdf files to batch convert and an batchoutdir directory with 
# the prm template file, where output files will be written to
# batchrunpath needs to be where MyHEG_batchScript.bat is located

batchrunpath <- "C:/Users/mleza/HEG/HEG_Win/bin/BatchRunning/BatchRunning/"
batchindir <- paste0(batchrunpath, "indir/") 
batchoutdir <- paste0(batchrunpath, "outdir/")


#### high resolution land polygon for the 
clpath <- paste0("E:/Antarctica/", "coastline/Coastline_high_res_polygon/") 

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


######## CALL ################
######## CALL ################
######## CALL ################



if(newarea==1){
  prepDEM()
}

## Login to USGS ERS
login_USGS("MaiteLezama")

for(j in seq(time_range)){
  getprocessLANDSAT(time_range)
  getprocessMODIS(time_range)
}


