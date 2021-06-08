

# http://gforge.se/2015/02/how-to-go-parallel-in-r-basics-tips/

# https://github.com/HannaMeyer/AntAir/blob/5638db8a39d1b7ff05999df90fde641179353bc7/AntAir/01_Preprocessing/deprecated_DownloadModisVIS.R


# IA & HS Palma II

rm(list=ls())


# install.packages("/home/l/llezamav/R/lubridate_1.7.8.tar.gz", repos = NULL,
#                  lib="/home/l/llezamav/R/")
# install.packages("/home/l/llezamav/R/oce_1.2-0.tar.gz", repos = NULL,
#                  lib="/home/l/llezamav/R/")
# install.packages("/home/l/llezamav/R/suncalc_0.5.0.tar.gz", repos = NULL,
#                  lib="/home/l/llezamav/R/")
# install.packages("/home/l/llezamav/R/solrad_1.0.0.tar.gz", repos = NULL,
#                 lib="/home/l/llezamav/R/")

library(parallel)
library(doParallel)
library(raster)
library(sf)
library(lubridate, lib.loc="/home/l/llezamav/R/")
library(suncalc, lib.loc="/home/l/llezamav/R/" )
library(oce, lib.loc="/home/l/llezamav/R/")
library(solrad, lib.loc="/home/l/llezamav/R/")
library(rgdal)


rasterOptions(tmpdir="/scratch/tmp/llezamav/tmp/")
              
              
# time_range
time_range <- readRDS("/scratch/tmp/llezamav/time_range.rds")
ccpath <- "/home/l/llezamav/cc/"

#homecc <- "D:/new_downscaling/data_download_preprocessing/cc/"

year <- c(2019:2013)
month <- c("01","02","03","04", "09", "10","11", "12")
# year <- c(2018:2013)
# month <- c("04", "09", "10","11", "12")


template <- raster("/scratch/tmp/llezamav/template_new.tif")


sl <- raster("/scratch/tmp/llezamav/slopeaspect/30m_slopeMDV.tif")
as <- raster("/scratch/tmp/llezamav/slopeaspect/30m_aspectMDV.tif")

slrad <-  raster("/scratch/tmp/llezamav/slopeaspect/30m_radians_slopeMDV.tif")
asrad <-  raster("/scratch/tmp/llezamav/slopeaspect/30m_radians_aspectMDV.tif")

lon <- 161.7673
lat <- -77.45706

iapath <- "/scratch/tmp/llezamav/ia/"
hspath <- "/scratch/tmp/llezamav/hs/"
iahsrespath <- "/scratch/tmp/llezamav/ia_hs_res/"

timediff_comp <- lapply(seq(length(year)), function(y){
  lapply(seq(month), function(m){
    try(read.csv2(list.files(ccpath, pattern=substring(time_range[[y]][[m]][[1]][[1]], 1, 7), full.names=T)))
  })
})

saveRDS(timediff_comp, paste0(ccpath, "timediff_comp_from_script.rds"))

########################### FUNCTION #####################################

moddates <- readRDS(paste0(cddir, "moddates.RDS"))
uniqueMODscenes <-unique(moddates)

########## make hillshading and incidence angle raster ############
make_hs_ia <- function(scene){
  
      # get time from MODIS (all times are UTC https://modis-images.gsfc.nasa.gov/products_filename.html)
      print(paste0("starting with ",scene))

      doy <- yday(scene)
      h <- hour(scene)
      min <- minute(scene)
      
      doymin <- as.numeric(doy)+((h+min/60)/24)
      
      #time should be in UTC
      sa <- sunAngle(MODDate, lon=lon, lat=lat)
      
      hs <- hillShade(slope = slrad, aspect = asrad, angle = sa$altitude, direction = sa$azimuth)
      dateaschar <- paste0(substring(as.character(MODDate),1,10), "-", 
                           substring(as.character(MODDate),12,13),
                           substring(as.character(MODDate),15,16))
      
      #writeRaster(hs, paste0(hspath, "hs_", dateaschar, ".tif"), overwrite=T)
      
      # check whether daylight saving applies
      
      MODDateNZ <- with_tz(MODDate, tzone = "Pacific/Auckland")
      if(dst(MODDateNZ)==TRUE){
        dstnz <- 60
      } else {
        dstnz <- 0
      }
      
      ia <- Incidence(DOY = doymin, Lat=lat, Lon=lon, SLon=180, DS=dstnz, Slope = sl, Aspect = as)
      #writeRaster(ia, paste0(iapath, "ia_", dateaschar, ".tif"), overwrite=T)
      
      iahs <- stack(ia, hs)
      
      iahsres <- resample(iahs, template)
      
      writeRaster(iahsres, paste0(iahsrespath, "ia_hs_res_", dateaschar, ".tif"), overwrite=T)
      print(paste0("wrote stack", dateaschar,"to ", paste0(iahsrespath, "ia_hs_res_", dateaschar, ".tif")))
      
      gc()
      rm(ia)
      rm(hs)
      rm(iahsres)
      print(d)
    })
  } else {
    print("no scenes from this month")
  }
}

########################### RUN ###########################################
no_cores <- 6
cl <- makeCluster(no_cores)
jnk = clusterEvalQ(cl, {library(raster);library(parallel); library(rgdal); library(solrad,lib.loc="/home/l/llezamav/R/"); 
  library(oce,lib.loc="/home/l/llezamav/R/");library(suncalc,lib.loc="/home/l/llezamav/R/"); 
  library(lubridate,lib.loc="/home/l/llezamav/R/")})
clusterExport(cl, list("ccpath", "slrad", "timediff_comp", "asrad", "iahsrespath", "lat", "lon", "sl", "as",
                       "make_hs_ia", "month", "year","template"))

y=7
#parLapply(cl, seq(length(year)), function(y){
  print(paste0("starting yearindex ", y))
  lapply(seq(month), function(m){
    print(paste0("year=",y,"month=",m))
    make_hs_ia(y,m)
  })
#})

stopCluster(cl)
