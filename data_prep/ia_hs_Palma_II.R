# IA & HS Palma II

rm(list=ls())
ncores <- 10

library(parallel)
library(doParallel)
library(raster)
library(sf)

# install.packages("/home/l/llezamav/R/lubridate_1.7.8.tar.gz", repos = NULL,
#                  lib="/home/l/llezamav/R/")
# install.packages("/home/l/llezamav/R/oce_1.2-0.tar.gz", repos = NULL,
#                  lib="/home/l/llezamav/R/")
# install.packages("/home/l/llezamav/R/suncalc_0.5.0.tar.gz", repos = NULL,
#                  lib="/home/l/llezamav/R/")
install.packages("/home/l/llezamav/R/solrad_1.0.0.tar.gz", repos = NULL,
                 lib="/home/l/llezamav/R/")
library(lubridate, lib.loc="/home/l/llezamav/R/")
library(suncalc, lib.loc="/home/l/llezamav/R/" )
library(oce, lib.loc="/home/l/llezamav/R/")
library(solrad, lib.loc="/home/l/llezamav/R/")

library(rgdal)
# install.packages("/home/l/llezamav/R/rgdal_1.4-8.tar.gz", repos = NULL,
#                  lib="/home/l/llezamav/R/")
# library(rgdal, lib.loc="/home/l/llezamav/R/")

rasterOptions(tmpdir="/scratch/tmp/llezamav/tmp/")
              
              
# time_range
time_range <- readRDS("/scratch/tmp/llezamav/time_range.rds")
ccpath <- "/home/l/llezamav/cc"

year <- c(2019:2013)
month <- c("01","02","03","04", "09", "10","11", "12")

# year <- c(2018:2013)
# month <- c("04", "09", "10","11", "12")

sl <- raster("/scratch/tmp/llezamav/slopeaspect/slopeMDV.tif")
as <- raster("/scratch/tmp/llezamav/slopeaspect/aspectMDV.tif")

lon <- 161.7673
lat <- -77.45706

iapath <- "/scratch/tmp/llezamav/ia/"
hspath <- "/scratch/tmp/llezamav/hs/"


########################### FUNCTION #####################################

########## make hillshading and incidence angle raster ############
make_hs_ia <- function(y,m){
  
  timediff_comp <- try(read.csv2(list.files(ccpath, pattern=substring(time_range[[y]][[m]][[1]][[1]], 1, 7), full.names=T)))
  
  if(class(timediff_comp)!="try-error"){
    
    #L8scenepath <- paste0(main, "L8/", substring(time_range[[y]][[m]][[1]][[1]], 1, 7), "/")
    #timediff_comp <- read.csv2(paste0(L8scenepath, "timediff_comp_comp.csv"))
    
    # for d in seq(nrow(timediff))
    uniqueMODscenes <- unique(timediff_comp$MODname)
    
    MD <- substring(uniqueMODscenes,11,22)
    MODDate <- as.POSIXct(MD, tz="UTC",format = "%Y%j.%H%M")
    
    #lapply(c(1:8), function(d){
    lapply(seq(uniqueMODscenes), function(d){
      # get time from MODIS (all times are UTC https://modis-images.gsfc.nasa.gov/products_filename.html)
      
      MD <- substring(uniqueMODscenes[d],11,22)
      MODDate <- as.POSIXct(MD, tz="UTC",format = "%Y%j.%H%M")
      
      doy <- substring(MD, 5,7)
      h <- hour(MODDate)
      min <- minute(MODDate)
      
      doymin <- as.numeric(doy)+((h+min/60)/24)
      
      #time should be in UTC
      sa <- sunAngle(MODDate, lon=lon, lat=lat)
      
      hs <- hillShade(slope = sl, aspect = as, angle = sa$altitude, direction = sa$azimuth)
      dateaschar <- paste0(substring(as.character(MODDate),1,10), "-", substring(as.character(MODDate),12,13),substring(as.character(MODDate),15,16))
      
      writeRaster(hs, paste0(hspath, "hs_", dateaschar, ".tif"), overwrite=T)
      
      # check whether daylight saving applies
      
      MODDateNZ <- with_tz(MODDate, tzone = "Pacific/Auckland")
      if(dst(MODDateNZ)==TRUE){
        dstnz <- 60
      } else {
        dstnz <- 0
      }
      
      ia <- Incidence(DOY = doymin, Lat=lat, Lon=lon, SLon=180, DS=dstnz, Slope = sl, Aspect = as)
      writeRaster(ia, paste0(iapath, "ia_", dateaschar, ".tif"), overwrite=T)
      
      gc()
    })
  } else {
    print("no scenes from this month")
  }
}

########################### RUN ###########################################
cl <- makeCluster(ncores)
registerDoParallel(cl)

# what is missing of 2018
lapply(c(2), function(y){
  lapply(c(1:3), function(m){
    make_hs_ia(y,m)
  })
})

# starting with all months from 2017 on  
lapply(c(3:7), function(y){
  lapply(seq(month), function(m){
    make_hs_ia(y,m)
  })
})

stopCluster(cl)
