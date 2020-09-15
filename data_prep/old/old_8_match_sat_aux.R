
# needed: 
# aux
# ia_hs_res (already there)
# satellite stack (L_MOD) for y and m and names (satnames)
# timediff_comp_comp.csv, i.e. tdcc
# swirfiles (swir_tc_67)


iahsrespath <- paste0(cddir, "ia_hs_res/")


auxpath <-  "D:/new_downscaling/auxiliary/"

# # make soil raster
# soilMDV <- readOGR(paste0(auxpath, "update8.shp"))
# crs(soilMDV) <- crs(template)
# soilDV <- crop(soilMDV, extent(template))
# plot(soilDV)
# 
# soilraster <- rasterize(soilDV, template, field="SOIL")
# writeRaster(soilraster, paste0(auxpath, "soil_MDV_raster.tif"))

# soilraster <- raster(paste0(auxpath, "soil_MDV_raster.tif"))
# TWI <- raster(paste0(dempath, "twi_saga_07_07.tif"))
# aspect <- raster(paste0(dempath, "30m_aspect", areaname,".tif"))
# slope <- raster(paste0(dempath, "30m_slope", areaname,".tif"))
# dem <- raster(paste0(dempath, "DEM_30m_", areaname,"_clean_aoi_filled_mask_new.tif"))
# spatialblocks <- raster(paste0(dempath, "blockmask.tif"))
# landcover <- raster(paste0(main, "Rock_outcrop_ras_", areaname, ".tif"))
# landcover[landcover==0.94] <- 1 #(rock)
# landcover[landcover==0.97] <- 0 #(snow and ice) 
# 
# landcoverres <- resample(landcover, dem)
# 
# aux <- stack(dem, slope, aspect, TWI, soilraster, landcoverres, spatialblocks)
# names(aux) <- c("dem", "slope", "aspect", "TWI", "soilraster", "landcoverres", "spatialblocks")
# write.csv(names(aux), "D:/new_downscaling/auxiliary/names_aux.csv")
# writeRaster(aux, "D:/new_downscaling/auxiliary/aux_stack.tif")

y=1
m=2
ym <- substring(time_range[[y]][[m]][[1]][[1]], 1, 7)


swiroutpath <- "D:/new_downscaling/SWIR/composites/"
aux <- stack("D:/new_downscaling/auxiliary/aux_stack_xy_swir67.tif")
auxnam <- read.csv2(paste0(auxpath, "names_aux.csv"))
names(aux) <- c("dem", "slope", "aspect", "TWI", "soilraster", "landcoverres", "spatialblocks", "swir6", "swir7", "x", "y")
swirfile <- paste0(swiroutpath, "swir_tc_67", ym, ".tif")

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
  
  iahsf <- sans_ext(basename(iahs_files_month))
  ia_hs_nam <- substring(iahsf,11)
  
  iahsm <- stack(ia_hs)

  # get MODIS names from satstack
  lo <- seq(1,(length(names(satstack))-1),by=2) # lo=Landsat
  hi <- lo+1 # hi=MODIS
  
  modsatnam <- names(satstack[[hi]])
  
  # get naming of ia and hs from MODIS scenes in stack
  dateascharstack <- lapply(seq(modsatnam), function(i){
    if(grepl("small", modsatnam[i])){
      MD <- substring(modsatnam[i],22,33)
      MODDate <- as.POSIXct(MD, tz="UTC",format = "%Y%j.%H%M")
      paste0(substring(as.character(MODDate),1,10), "-", substring(as.character(MODDate),12,13),substring(as.character(MODDate),15,16))
    } else {
      MD <- substring(modsatnam[i],11,22)
      MODDate <- as.POSIXct(MD, tz="UTC",format = "%Y%j.%H%M")
      paste0(substring(as.character(MODDate),1,10), "-", substring(as.character(MODDate),12,13),substring(as.character(MODDate),15,16))
    }
  })
  
  datemod <- unlist(dateascharstack)
  
  pos <- sapply(seq(datemod), function(i){ # find positions matching 
    grep(datemod[i], ia_hs_nam)
  })
  
  # merge stacks
  satiahs_stack <- lapply(seq(modsatnam), function(i){
    if(length(pos[[i]])>0){
      print(i)
      stack(satstack[[lo[i]]], satstack[[hi[i]]], ia_hs[[pos[[i]]]])
    }
  })
  
  tf <- sapply(seq(satiahs_stack), function(i){
    length(satiahs_stack[[i]]) > 0
  })

  allstacks <- stack(satiahs_stack[tf])
  
  # get swir file
  swir <- raster(swirfile)
  tempdyn <- stack(allstacks, swir)
  
  write.csv2(names(tempdyn), paste0(cddir, "names_sat_ia_hs_swir_", substring(time_range[[y]][[m]][[1]][[1]], 1, 7), ".csv"),
             row.names = F)
  
  print("starting to write complete satellite stack")
  writeRaster(tempdyn, paste0(cddir, "L_MOD_hs_ia_swir", substring(time_range[[y]][[m]][[1]][[1]], 1, 7), ".tif"),
              overwrite=T)
  
  
  ####################### add auxiliary data #############################
  as <- stack(tempdyn, aux)
  writeRaster(as, paste0(cddir, "sat_aux_", substring(time_range[[y]][[m]][[1]][[1]], 1, 7), ".tif"),
              overwrite=T)
  
}


# lapply(seq(year), function(y){
#   lapply(seq(month), function(m){
for(y in c(1)){
  for(m in c(3:length(month))){
    match_sat_ia_hs(y,m)
  }
}
#   })
# })
