


loc="Palma"

if(loc=="Palma"){
  library(raster)
  library(rgdal)
  datpath <- "/scratch/tmp/llezamav/satstacks/"
  aoipath <- "/scratch/tmp/llezamav/aoi/"
  time_range <- readRDS("/scratch/tmp/llezamav/time_range.rds")
  cddir <- "/scratch/tmp/llezamav/satstacks/"
  iahsrespath <- "/scratch/tmp/llezamav/ia_hs_res/"
  swiroutpath <- paste0(datpath, "swir/")
  
} else if(loc=="Laptop"){
  library(raster)
  library(rgdal)
  iahsrespath <- paste0(cddir, "ia_hs_res/")
  aoipath <- "D:/new_downscaling/aoi/"
  auxpath <-  "D:/new_downscaling/auxiliary/"
  datpath <- auxpath
  swiroutpath <- "D:/new_downscaling/SWIR/composites/" # TO DO!!!!!!!!!!!!!!!!!!!
  } else {
  print("something's off")
}


`%notin%` <- Negate(`%in%`)

# get aux general
aux <- stack(paste0(datpath, "aux_stack_xy_final.tif"))
names(aux) <-  c("dem", "slope", "aspect", "TWI", "soilraster", "landcoverres", "spatialblocks","x", "y")

# get aoi
aoi <- readOGR(paste0(aoipath, "Levy_MDV.shp"))
antaproj <- crs("+proj=stere +lat_0=-90 +lat_ts=-71 +lon_0=0 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m
+no_defs +ellps=WGS84 +towgs84=0,0,0")
aoianta <- spTransform(aoi, antaproj)
aoiaux <- spTransform(aoianta, crs(aux))



################## START LOOP PER MONTH ####################################

y=1
m=2
# 
# extract_train_test <- function(y,m){
  ym <- substring(time_range[[y]][[m]][[1]][[1]], 1, 7)
  
  
  # add SWIR to aux
  swirfile <- paste0(swiroutpath, "swir_tc_67", ym, ".tif")
  swir <- stack(swirfile)
  aux <- stack(aux, swir)
  names(aux) <- c("dem", "slope", "aspect", "TWI", "soilraster", "landcoverres", "spatialblocks","x", "y", "swir6", "swir7")
  
  # extract aux
  auxdf <- as.data.frame(extract(aux, aoiaux))
  write.csv2(auxdf, paste0(datpath, "aux_df_swir_x_y_swir_", ym, ".csv"), row.names=F)
  
  ################## get dyn stack for ym #############################################
  tempdyn <- raster::stack(paste0(datpath, "L_MOD_hs_ia_", ym, ".tif"))
  tdnam <- read.csv(paste0(datpath, "names_sat_ia_hs_", ym, ".csv"))
  names(tempdyn) <- tdnam$x
  
  ################## extract dyn stack  #############################################
  tempdyn <- stack(tempdyn, aux$x, aux$y)
  tmpdyndf <- as.data.frame(extract(tempdyn, aoianta))
  write.csv2(tmpdyndf, paste0(datpath, "tempdyn_new_", ym,"_df.csv"), row.names=F)
  
  # tmpdyndf <- read.csv2(paste0(datpath, "tempdyn_new_", ym,"_df.csv"), header = T)
  test <- tmpdyndf[1:100,1]==seq(1:100)
  if(all(test==TRUE, na.rm = T) | is.na(all(test))==TRUE){ # if fits, kick out column, if all is NA or doesn't fit, don't
    tmpdyndf <- tmpdyndf[,2:ncol(tmpdyndf)] # eliminate rowname column if there's one
  }
  
  # ############ make checkfiles for aux and tempdyn extraction ##############
  write.csv2(tmpdyndf[1:500,], paste0(datpath, "tempdyndf_check_", ym, ".csv"), row.names=F)
  write.csv2(auxdf[1:500,], paste0(datpath, "auxdf_check_", ym, ".csv"), row.names=F)
  
  ################### sort into useful file #########################
  #auxdf <- read.csv2(paste0(datpath, "aux_df_swir_x_y_swir_", ym, ".csv"))
  
  new_package <- seq(1,(ncol(tmpdyndf)-2), by=4)
  end <- new_package+3
  
  dfslices <- lapply(seq(new_package), function(i){
    x <- tmpdyndf[,new_package[i]:end[i]] # get all columns for one date
    xnam <- substring(names(x)[3], 4, 18) # get time info from hs/ia
    x$time <- xnam # add time as row
    x$xd <- tmpdyndf$x
    x$yd <- tmpdyndf$y
    x$id <- seq(1:nrow(x)) # as pixel ID for tempdyn
    x <- cbind(x, auxdf) # add tempdyn for one date + aux together
    names(x) <- c("Modis", "ia", "hs", "Landsat", "time", "xd", "yd", "id","dem", "slope", "aspect",
                  "TWI", "soilraster", "landcoverres", "spatialblocks","xa", "ya", "swir6", "swir7")
    x
  })
  
  tddf <- do.call(rbind,dfslices)
  
  ################### get complete cases ##################################
  
  write.csv2(tddf[1:500,], paste0(datpath, "extr_comp_check_",ym, ".csv"), row.names = F)
  #tddf <- read.csv2(paste0(datpath, "extr_comp_",ym, ".csv"), header=T)
  
  tddf$Modis[tddf$Modis > 30] <- NA
  tddf$Modis[tddf$Modis <= -100] <- NA
  tddf$Landsat[tddf$Landsat <= -100] <- NA
  tddf$Landsat[tddf$Landsat > 30] <- NA
  
  tddfcc <- tddf[complete.cases(tddf),]
  
  
  write.csv2(tddfcc[1:500,], paste0(datpath, "extr_complete_", ym, "_check.csv"), row.names=F)
  write.csv2(tddfcc, paste0(datpath, "extr_complete_cases_",ym, ".csv"), row.names=F)
  
  #tddfcc <- read.csv2(paste0(datpath, "extr_complete_cases_", ym, ".csv"))
  
  ################## take out TEST for this month ##########################
  
  testsites <- c(26, 63, 43, 12, 35, 40, 31, 79,  5,  2, 60, 11)
  
  test <- subset(tddfcc, tddfcc$spatialblocks %in% testsites)
  
  saveRDS(test, paste0(datpath, "test_ds_", ym, ".rds"))
  write.csv2(test, paste0(datpath, "test_ds_", ym, ".csv"))
  
  ts <- sample(nrow(test), 150000)
  testsubset <- test[ts,]
  
  saveRDS(testsubset, paste0(datpath, "testsubset_ds_", ym, ".rds"))
  write.csv2(testsubset, paste0(datpath, "testsubset_ds_", ym, ".csv"))
  
  ######## make TRAIN dataset #############################################
  pottrain <-  subset(tddfcc, tddfcc$spatialblocks %notin% testsites)
  
  # get 3 Mio random samples per month to choose from in random and DI picking
  splpot <- sample(rownames(pottrain), 3000000)
  pott3 <- tddfcc[splpot,]
  write.csv2(pott3, paste0(datpath, "pott3_",ym, ".csv"), row.names=F)
  
# }

# # for(y in seq(year)){
# #   for(m in seq(month)){
#     extract_train_test(y=1,m=2)
# #   }
# # }