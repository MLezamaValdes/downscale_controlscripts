
# hillshading for all hours of a day

rm(list=ls())


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
ccpath <- "/home/l/llezamav/cc/"
template <- raster("/scratch/tmp/llezamav/template_new.tif")

sl <- raster("/scratch/tmp/llezamav/slopeaspect/30m_slopeMDV.tif")
as <- raster("/scratch/tmp/llezamav/slopeaspect/30m_aspectMDV.tif")

slrad <-  raster("/scratch/tmp/llezamav/slopeaspect/30m_radians_slopeMDV.tif")
asrad <-  raster("/scratch/tmp/llezamav/slopeaspect/30m_radians_aspectMDV.tif")

lon <- 161.7673
lat <- -77.45706

iahsrespath <- "/scratch/tmp/llezamav/ia_hs_res_dayex/"

# moddates <- readRDS("/scratch/tmp/llezamav/moddates.RDS")
# uniqueMODscenes <-unique(moddates)

hours <- as.character(seq(1,23))
hours <- sapply(seq(hours), function(i){
  if(nchar(hours[i])<2){
    hours[i] <- paste0("0", hours[i])
  }
  hours[i]
})

uniquescenes <- as.POSIXct(paste0("2013-11-12 ", hours, ":00:00"), tz="UTC")
  

########## make hillshading and incidence angle raster ############
make_hs_ia <- function(scene){
  
  # get time from MODIS (all times are UTC https://modis-images.gsfc.nasa.gov/products_filename.html)
  print(paste0("starting with ",scene))
  
  doy <- yday(scene)
  h <- hour(scene)
  min <- minute(scene)
  
  doymin <- as.numeric(doy)+((h+min/60)/24)
  dateaschar <- as.character(scene) %>% substring(1,16) %>% gsub(pattern="\\s", replacement="_") %>% gsub(pattern=":", replacement="_")
  
  #time should be in UTC
  sa <- sunAngle(scene, lon=lon, lat=lat)
  sa$sahs <- sa$azimuth - 180
  write.csv2(sa, paste0(iahsrespath, "sa_", dateaschar, ".csv"))
  
  hs <- hillShade(slope = slrad, aspect = asrad, angle = sa$altitude, direction = sa$sahs)
  
  
  # check whether daylight saving applies
  MODDateNZ <- with_tz(scene, tzone = "NZ")
  if(dst(MODDateNZ)==TRUE){
    dstnz <- 60
  } else {
    dstnz <- 0
  }
  
  ia <- Incidence(DOY = doymin, Lat=lat, Lon=lon, SLon=180, DS=dstnz, Slope = sl, Aspect = as)
  
  iahs <- stack(ia, hs)
  iahsres <- resample(iahs, template)
  
  writeRaster(iahsres, paste0(iahsrespath, "ia_hs_res_", dateaschar, "_az_", round(sa$azimuth), "_al_", round(sa$altitude),".tif"), overwrite=T)
  print(paste0("wrote stack", dateaschar,"to ", paste0(iahsrespath, "ia_hs_res_", dateaschar, ".tif")))
  
  gc()
  rm(ia)
  rm(hs)
  rm(iahsres)
  
}

########################### RUN ###########################################
no_cores <- 6
cl <- makeCluster(no_cores)
jnk = clusterEvalQ(cl, {library(raster);library(parallel); library(rgdal); library(solrad,lib.loc="/home/l/llezamav/R/"); 
  library(oce,lib.loc="/home/l/llezamav/R/");library(suncalc,lib.loc="/home/l/llezamav/R/"); 
  library(lubridate,lib.loc="/home/l/llezamav/R/")})
clusterExport(cl, list("ccpath","uniquescenes", "slrad", "asrad", "iahsrespath", "lat", "lon", "sl", "as",
                       "make_hs_ia", "month", "year","template"))

parLapply(cl, seq(length(uniquescenes)), function(i){
  print(paste0("====== starting with ", uniquescenes[i], "======"))
  make_hs_ia(uniquescenes[i])
})


stopCluster(cl)
