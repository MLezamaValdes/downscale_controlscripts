


##########################################################################################################
############################ PATHS & AOI #################################################################
##########################################################################################################


loc="Palma"

library(raster)
library(rgdal)
datpath <- "/scratch/tmp/llezamav/satstacks_ngb/"
aoipath <- "/scratch/tmp/llezamav/aoi/"
auxpath <- "/scratch/tmp/llezamav/satstacks/"
outpath <- "/scratch/tmp/llezamav/extraction/"
rasterOptions(tmpdir="/scratch/tmp/llezamav/tmp/")


if(loc=="Laptop"){
  datpath <- paste0(cddir, "satstacks_ngb/")
  auxpath <-  "E:/new_downscaling/auxiliary/"
}

# get aoi
aoi <- readOGR(paste0(aoipath, "Levy_MDV.shp"))
antaproj <- crs("+proj=stere +lat_0=-90 +lat_ts=-71 +lon_0=0 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m
+no_defs +ellps=WGS84 +towgs84=0,0,0")
aoianta <- spTransform(aoi, antaproj)
aoiaux <- spTransform(aoianta, crs("+proj=stere +lat_0=-90 +lat_ts=-71 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs "))

##########################################################################################################
############################ AUXILIARY DATA EXTRACTION ###################################################
##########################################################################################################

# get aux
aux <- stack(paste0(auxpath, "aux_stack_xy_final.tif"))
names(aux) <-  c("dem", "slope", "aspect", "TWI", "soilraster", "landcoverres", "spatialblocks","x", "y")

# extract aux
auxdf <- as.data.frame(extract(aux, aoiaux))
write.csv2(auxdf, paste0(outpath, "aux_df_x_y.csv"), row.names=F)
write.csv2(auxdf[1:500,], paste0(outpath, "auxdf_check.csv"), row.names=F)

auxdf_comp <- auxdf[complete.cases(auxdf),]
print(paste0("n complete auxdf cases: ", nrow(auxdf_comp)))
write.csv2(auxdf_comp, paste0(outpath, "auxdf_complete.csv"), row.names=F)

rm(auxdf_comp)
print("aux extraction done")

##########################################################################################################
############################ DYNAMIC SAT DATA EXTRACTION #################################################
##########################################################################################################


paths <- list.files(datpath, pattern="L_MOD_hs_ia", full.names=T)

# get ym
yms <- substring(basename(paths), 13,19)


# loop dynamic stack extraction for each monthly stack in paths
lapply(seq(paths), function(i){
  
  print(paste0("starting with ~~~~~~~~~~~~~~~~~ ", basename(paths[i]), " ~~~~~~~~~~~~~~~~~ "))
  tds <- raster::stack(paths[i]) # get dynamic stack
  tdnam <- read.csv(paste0(datpath, "names_sat_ia_hs_", yms[i], ".csv")) # get names for tempdyn
  names(tds) <- tdnam$x
  tempdyn <- stack(tds, aux$x, aux$y) # just to make sure everything worked fine with aux matching
  
  rasterOptions(tmpdir="/scratch/tmp/llezamav/tmp/")
  
  tmpdyndf <- as.data.frame(extract(tempdyn, aoiaux))
  write.csv2(tmpdyndf, paste0(outpath, "tempdyn_",  yms[i],"_df.csv"), row.names=F)
  
  print("tempdyn extraction done")

  ############## make checkfiles for aux and tempdyn extraction ##############
  write.csv2(tmpdyndf[1:500,], paste0(outpath, "tempdyndf_check_", yms[i], ".csv"), row.names=F)
  write.csv2(auxdf[1:500,], paste0(outpath, "auxdf_check_",  yms[i], ".csv"), row.names=F)
  
  ################### sort per scene #########################
  # scenes are in the columns for now, but we want them in the rows of course 
  
  #auxdf <- read.csv2(paste0(outpath, "aux_df_swir_x_y_", ym, ".csv"))
  
  new_package <- seq(1,(ncol(tmpdyndf)-2), by=4)
  end <- new_package+3
  
  dfslices <- lapply(seq(new_package), function(j){
    # L, MOD, ia, hs
    x <- tmpdyndf[,new_package[j]:end[j]]
    
    # aux
    x <- cbind(x, auxdf)
    
    # coordinates and ID
    x$xd <- tmpdyndf$x
    x$yd <- tmpdyndf$y
    x$id <- seq(1:nrow(x))
    
    # time info
    # month, year (time) and complete date (date)
    myd <- substring(strsplit(names(x)[2], ".", fixed = TRUE)[[1]][2], 2, 9)
    mydhm <- paste0(myd, "_", strsplit(names(x)[2], ".", fixed = TRUE)[[1]][3])
    
    mydhm_str <- as.character(as.POSIXlt(mydhm, format="%Y%j_%H%M"))
    x$modtime <- gsub(" ", "_", mydhm_str)
    x$ymo <- substring(mydhm_str, 1, 7)
    x$hmi <- substring(mydhm_str, 12, 16)
    x$Lscene <- names(x)[1]
    x$Mscene <- names(x)[2]
    
    print(paste0("x has ", ncol(x), " columns, should have 21"))
    
    names(x) <- c( "Landsat", "Modis", "ia", "hs",
                   "dem", "slope", "aspect", "TWI", "soilraster", "landcoverres",
                   "spatialblocks", "x", "y","xd", "yd",
                   "id", "modtime", "ymo", "hmi", "Lscene", "Mscene")
    
    x
  })
  
  tddf <- do.call(rbind,dfslices)
  print("tddf done: ")
  head(tddf)
  
  rm(dfslices)
  rm(tmpdyndf)
  rm(aux)
  
  ################### get complete cases ##################################
  
  tddf$Modis[tddf$Modis > 30] <- NA
  tddf$Landsat[tddf$Landsat > 30] <- NA
  
  tddf$Modis[tddf$Modis <= -100] <- NA
  tddf$Landsat[tddf$Landsat <= -100] <- NA
  
  write.csv2(tddf, paste0(outpath, "tddf_", yms[i], ".csv"), row.names=F)
  
  tddfcc <- tddf[complete.cases(tddf),]
  rm(tddf)

  write.csv2(tddfcc, paste0(outpath, "extr_complete_cases_", yms[i], ".csv"), row.names=F)
  
  write.csv2(tddfcc[1:500,], paste0(outpath, "extr_comp_check_", yms[i], ".csv"), row.names = F)

  print("tddfcc unsplit files done")
  
  print(paste0("length of all samples (tdfcc) = ", nrow(tddfcc)))
  
  
})
