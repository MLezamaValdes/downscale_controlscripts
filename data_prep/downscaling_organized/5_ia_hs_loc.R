# ccpath <- paste0(cddir, "comp_comp_files")
# 
# template <- template
# 
# 
# sl <- raster(list.files(dempath, pattern="30m_slopeMDV.tif", full.names=T))
# as <- raster(list.files(dempath, pattern="30m_aspectMDV.tif", full.names=T))
# 
# slrad <- raster(list.files(dempath, pattern="30m_radians_slopeMDV.tif", full.names=T))
# asrad <- raster(list.files(dempath, pattern="30m_radians_slopeMDV.tif", full.names=T))
# 
# lon <- 161.7673
# lat <- -77.45706
# 
# iahsrespath <-  paste0(cddir, "ia_hs_res/")
# dir.create(iahsrespath)
# 
# moddates <- readRDS(paste0(cddir, "moddates.RDS"))
# uniqueMODscenes <-unique(moddates)

make_hs_ia <- function(scene){
  
  # get time from MODIS (all times are UTC https://modis-images.gsfc.nasa.gov/products_filename.html)
  print(paste0("starting with ",scene))
  
  doy <- yday(scene)
  h <- hour(scene)
  min <- minute(scene)
  
  doymin <- as.numeric(doy)+((h+min/60)/24)
  
  #time should be in UTC
  sa <- sunAngle(scene, lon=lon, lat=lat)
  hs <- hillShade(slope = slrad, aspect = asrad, angle = sa$altitude, direction = sa$azimuth)
  
  dateaschar <- as.character(scene) %>% substring(1,16) %>% gsub(pattern="\\s", replacement="_") %>% gsub(pattern=":", replacement="_")
  
  # check whether daylight saving applies
  MODDateNZ <- with_tz(scene, tzone = "Pacific/Auckland")
  if(dst(MODDateNZ)==TRUE){
    dstnz <- 60
  } else {
    dstnz <- 0
  }
  
  ia <- Incidence(DOY = doymin, Lat=lat, Lon=lon, SLon=180, DS=dstnz, Slope = sl, Aspect = as)
  
  iahs <- stack(ia, hs)
  iahsres <- resample(iahs, template)
  
  writeRaster(iahsres, paste0(iahsrespath, "ia_hs_res_", dateaschar, ".tif"), overwrite=T)
  print(paste0("wrote stack", dateaschar,"to ", paste0(iahsrespath, "ia_hs_res_", dateaschar, ".tif")))
  
  gc()
  rm(ia)
  rm(hs)
  rm(iahsres)
  
}



# lapply(seq(uniqueMODscenes), function(i) {
#   print(paste0("====== starting with ", uniqueMODscenes[i], "======"))
#   make_hs_ia(uniqueMODscenes[i])
# })
