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
library(xfun)
library(solrad)
library(oce)
library(suncalc)
library(shadow)
library(horizon)

## time range parameters (2013-2019, Jan, Feb and December, select the best day from 17 to 21)
year <- c(2019:2013)
month <- c("01","02","03","04", "09", "10","11", "12")

# getting every day
d_feb <- c(28, 28, 28, 29, 28, 28, 28)
names(d_feb) <- year

# years 2019:2013 days in all months listed above
dom <- lapply(seq(year), function(y){
  d <- c(31, d_feb[y], 31, 30, 30, 31, 30, 31)
  names(d) <- month
  d
})

time_range <- lapply(seq(year), function(i){
  lapply(seq(month), function(j){
    y <- paste(paste0(year[i],"-",month[j],"-", formatC(seq(1, dom[[i]][j]), width=2, flag=0)), 
               paste0(year[i],"-",month[j],"-",formatC(seq(1, dom[[i]][j]), width=2, flag=0)))
    strsplit(y, " ")
  })
})

#dir.create(file.path(aoipath), recursive = T)
dir.create(file.path(L8datpath), recursive = T)
dir.create(file.path(modispath), recursive = T)
dir.create(file.path(tdpath), recursive = T)
dir.create(paste0(main, "timediff/"))


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
source(paste0("C:/Users/mleza/OneDrive/Documents/PhD/work_packages/auto_downscaling_30m/downscaleRS/R/", "compbb.R"))



substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

# until function incorporated in downscaleRS
source("C:/Users/mleza/OneDrive/Documents/PhD/work_packages/auto_downscaling_30m/downscaleRS/R/datestodoymod.R")
source(paste0(scriptpath, "1_DEM.R"))
source(paste0(scriptpath, "2_Landsat.R"))
source(paste0(scriptpath, "3_MODIS_new.R"))
source(paste0(scriptpath, "4a_get_MYD_patch.R"))
#### to be updated in package

l8datlist <- function(scenes){
  
  if(L8downloadtype != "Bt"){
    tifs <- lapply(seq(scenes), function(i){
      list.files(scenes[i], pattern=".tif$",  full.names = T)
    })
    
    names <- lapply(seq(scenes), function(i){
      list.files(scenes[i], pattern=".tif$",  full.names = F)
    })
    
    meta <- lapply(seq(scenes), function(i){
      list.files(scenes[i], pattern="MTL.txt$",  full.names = T)
    })
    
  } else {
    tifs <- lapply(seq(scenes), function(i){
      list.files(scenes[i], pattern="band10", full.names=T)
    })
    names <- lapply(seq(scenes), function(i){
      list.files(scenes[i], pattern="band10", full.names=T)
    })
    meta <- lapply(seq(scenes), function(i){
      list.files(scenes[i], pattern=".xml", full.names=T)
    })
  }
  
  return(list(tifs = tifs, names = names, meta = meta))
  
}



