
y=1
m=1

iahsrespath <- paste0(cddir, "ia_hs_res/")


auxpath <-  "D:/new_downscaling/auxiliary/"


soilMDV <- readOGR(paste0(auxpath, "update8.shp"))
crs(soilMDV) <- crs(template)
soilDV <- crop(soilMDV, extent(template))
plot(soilDV)

soilraster <- rasterize(soilDV, template, field="SOIL")
writeRaster(soilraster, paste0(auxpath, "soil_MDV_raster.tif"))

########## put allstacks + hs + ia together ############
match_sat_ia_hs <- function(y,m){
  L8scenepath <- paste0(main, "L8/", substring(time_range[[y]][[m]][[1]][[1]], 1, 7), "/")

  # get satellite stack for y and m and rename correctly
  satstack <- stack(paste0(cddir, "L_MOD_", substring(time_range[[y]][[m]][[1]][[1]], 1, 7), ".tif"))
  namdat <- read.csv2(paste0(cddir, "satnames_", substring(time_range[[y]][[m]][[1]][[1]], 1, 7), ".csv"))
  names(satstack) <- namdat$x
  
  # get matching table 
  try(timediff_comp_comp <- read.csv2(paste0(L8scenepath, "timediff_comp_comp.csv")))
  uniqueMODscenes <- unique(timediff_comp_comp$MODname)
  
  # get naming of ia and hs
  dateaschar <- lapply(seq(uniqueMODscenes), function(i){
    MD <- substring(uniqueMODscenes[i],11,22)
    MODDate <- as.POSIXct(MD, tz="UTC",format = "%Y%j.%H%M")
    paste0(substring(as.character(MODDate),1,10), "-", substring(as.character(MODDate),12,13),substring(as.character(MODDate),15,16))
  })
  
  # read resampled and stacked ia and hs
  iahs_files_month <- list.files(iahsrespath, full.names=T, pattern=substring(dateaschar[[1]], 1,7))
  iahs <- c("ia", "hs")
  
  ia_hs <- lapply(seq(iahs_files_month), function(i){
    x <- stack(iahs_files_month[[i]])
    names(x) <- paste0(iahs,substring(names(x),6,21))
    return(x)
  })
  
  ia_hs_nam <- sapply(seq(ia_hs), function(i){
    x <- substring(names(ia_hs[[i]])[1],4)
    gsub(".", "-",x,fixed=T)
  })
  
  iahsm <- stack(ia_hs)
  plot(iahsm)
  
  # get MODIS names from satstack
  lo <- seq(1,(length(names(satstack))-1),by=2)
  hi <- lo+1
  
  modsatnam <- names(satstack[[hi]])
  
  # get naming of ia and hs from MODIS scenes in stack
  dateascharstack <- lapply(seq(modsatnam), function(i){
    MD <- substring(modsatnam[i],11,22)
    MODDate <- as.POSIXct(MD, tz="UTC",format = "%Y%j.%H%M")
    paste0(substring(as.character(MODDate),1,10), "-", substring(as.character(MODDate),12,13),substring(as.character(MODDate),15,16))
  })
  
  datemod <- unlist(dateascharstack)
  
  pos <- sapply(seq(datemod), function(i){
    grep(datemod[i], ia_hs_nam)
  })
  
  # merge stacks
  satiahs_stack <- lapply(seq(modsatnam), function(i){
    stack( satstack[[lo[i]]], satstack[[hi[i]]], ia_hs[[pos[[i]]]] )
  })


  allstacks <- stack(satiahs_stack)
  
  write.csv2(names(allstacks), paste0(cddir, "names_sat_ia_hs_", substring(time_range[[y]][[m]][[1]][[1]], 1, 7), ".csv"),
             row.names = F)
  
  print("starting to write complete stack")
  writeRaster(allstacks, paste0(cddir, "L_MOD_hs_ia", substring(time_range[[y]][[m]][[1]][[1]], 1, 7), ".tif"),
              overwrite=T)
  
  
}