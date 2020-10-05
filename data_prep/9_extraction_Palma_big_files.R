


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
  datpath <- "D:/new_downscaling/clean_data/satstacks/"
  swiroutpath <- "D:/new_downscaling/SWIR/composites/" # TO DO!!!!!!!!!!!!!!!!!!!
} else {
  print("something's off")
}

year <- c(2019:2013)
month <- c("01","02","03","04", "09", "10","11", "12")

`%notin%` <- Negate(`%in%`)

# get aux general
if(loc=="Laptop"){
  datpath <- auxpath
}

aux <- stack(paste0(datpath, "aux_stack_xy_final.tif"))
names(aux) <-  c("dem", "slope", "aspect", "TWI", "soilraster", "landcoverres", "spatialblocks","x", "y")

# get aoi
aoi <- readOGR(paste0(aoipath, "Levy_MDV.shp"))
antaproj <- crs("+proj=stere +lat_0=-90 +lat_ts=-71 +lon_0=0 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m
+no_defs +ellps=WGS84 +towgs84=0,0,0")
aoianta <- spTransform(aoi, antaproj)
aoiaux <- spTransform(aoianta, crs(aux))



################## START LOOP PER MONTH ####################################

extract_train_test_big_files <- function(y,m){
  
  rasterOptions(tmpdir="/scratch/tmp/llezamav/tmp/")
  
  ym <- substring(time_range[[y]][[m]][[1]][[1]], 1, 7)
  
  print(paste0("~~~~~~STARTING WITH ", ym, " NOW~~~~~~~~"))
  
  # add SWIR to aux
  swirfile <- paste0(swiroutpath, "swir_tc_67", ym, ".tif")
  
  if(loc=="Laptop"){
    datpath <- "D:/new_downscaling/clean_data/satstacks/"
  }
  
  pat <- paste0("L_MOD_hs_ia_", ym)

  allf <- list.files(datpath, pattern = pat, full.names=T)
  
  all_splits <- allf[grepl(allf, pattern="_..tif$")] # files with _1, _2 ending
  not_split <- allf[!grepl(allf, pattern="_..tif$")] # original unsplit file
  
  tdnam <- read.csv(paste0(datpath, "names_sat_ia_hs_", ym, ".csv")) # names for tempdyn
  
  # generate split ranges
  rs <- stack(not_split)
  maxpackages <- 15
  new_package_split <- seq(1,(nlayers(rs)-2), by=(maxpackages*4))
  end_split <- new_package_split+(maxpackages*4-1)
  end_split[length(end_split)] <- nlayers(rs)
  
  new_package_split
  end_split

  
  if(file.exists(paste0(swiroutpath, "swir_tc_67", ym, ".tif"))){ # if there is a SWIR file
    swir <- stack(swirfile)
    aux <- stack(aux, swir)
    names(aux) <- c("dem", "slope", "aspect", "TWI", "soilraster", "landcoverres", "spatialblocks","x", "y", "swir6", "swir7")
    
    # extract aux
    auxdf <- as.data.frame(extract(aux, aoiaux))
    write.csv2(auxdf, paste0(datpath, "aux_df_swir_x_y_swir_", ym, ".csv"), row.names=F)
    write.csv2(auxdf[1:500,], paste0(datpath, "extraction_result/auxdf_check_", ym, ".csv"), row.names=F)
    
    print("aux extraction done")
    
    ################## get dyn stack for ym #############################################
    if(loc=="Laptop"){
      datpath <- "D:/new_downscaling/clean_data/satstacks/"
    }
    
    for(i in seq(all_splits)){ # for all splits
      print(paste0("starting with tempdyn split ", i, " now"))
      tempdyn <- stack(all_splits[i])
      names(tempdyn) <- tdnam$x[new_package_split[i]:end_split[i]]
      
      
      ################## extract dyn stack  #############################################
      tempdyn <- stack(tempdyn, aux$x, aux$y)
      
      rasterOptions(tmpdir="/scratch/tmp/llezamav/tmp/")
      
      # for(i in seq(nlayers(tempdyn))){
      #   print(i)
      #   print(summary(tempdyn[[i]]))
      # }
      
      tmpdyndf <- as.data.frame(extract(tempdyn, aoiaux))
      write.csv2(tmpdyndf, paste0(datpath, "extraction_result/tempdyn_", ym,"_", i, "_df.csv"), row.names=F)
      
      print("tempdyn extraction done")
      
      # tmpdyndf <- read.csv2(paste0(datpath, "tempdyn_new_", ym,"_df.csv"), header = T)
      test <- tmpdyndf[1:100,1]==seq(1:100)
      if(all(test==TRUE, na.rm = T) | is.na(all(test))==TRUE){ # if fits, kick out column, if all is NA or doesn't fit, don't
        tmpdyndf <- tmpdyndf[,2:ncol(tmpdyndf)] # eliminate rowname column if there's one
      }
      
      # ############ make checkfiles for aux and tempdyn extraction ##############
      write.csv2(tmpdyndf[1:500,], paste0(datpath, "extraction_result/tempdyndf_check_", ym, "_", i, ".csv"), row.names=F)
      
      ################### sort into useful file #########################
      #auxdf <- read.csv2(paste0(datpath, "aux_df_swir_x_y_swir_", ym, ".csv"))
      
      new_package <- seq(1,(ncol(tmpdyndf)-2), by=4)
      end <- new_package+3
      
      dfslices <- lapply(seq(new_package), function(i){
        # L, MOD, ia, hs
        x <- tmpdyndf[,new_package[i]:end[i]] 
        
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
        
        names(x) <- c( "Landsat", "Modis", "ia", "hs",
                       "dem", "slope", "aspect", "TWI", "soilraster", "landcoverres", 
                       "spatialblocks", "x", "y", "swir6", "swir7",
                       "id", "modtime", "ymo", "hmi", "Lscene", "Mscene")
        x
      })
      
      tddf <- do.call(rbind,dfslices)
      
      print("tddf done")
      
      ################### get complete cases ##################################
      
      
      tddf$Modis[tddf$Modis > 30] <- NA
      tddf$Modis[tddf$Modis <= -100] <- NA
      tddf$Landsat[tddf$Landsat <= -100] <- NA
      tddf$Landsat[tddf$Landsat > 30] <- NA
      
      tddfcc <- tddf[complete.cases(tddf),]
      
      write.csv2(tddfcc[1:500,], paste0(datpath, "extraction_result/extr_comp_check_",ym, "_", i, ".csv"), row.names = F)
      #tddf <- read.csv2(paste0(datpath, "extr_comp_",ym, ".csv"), header=T)
      
      write.csv2(tddfcc, paste0(datpath, "extraction_result/extr_complete_cases_",ym, "_", i, ".csv"), row.names=F)
      
      #tddfcc <- read.csv2(paste0(datpath, "extr_complete_cases_", ym, ".csv"))
      
      print(paste0("tddfcc", i, "done"))
      
      print(paste0("length of all samples (tdfcc ", i, ") = ", nrow(tddfcc)))
      
    }
    
    # list all tdfcc files for this month and put them together to form one dataframe (rbind)
    pat_tddfcc <- paste0("extr_complete_cases_",ym, "_")
    tdfiles <- list.files(datpath, pattern=pat_tddfcc, full.names = T)
    
    tddfcc_list <- lapply(seq(tdfiles), function(i){
      read.csv2(tdfiles)
    })
    
    tddfcc <- do.call(rbind,tddfcc_list)
    
    
    if(length(tddfcc)>0){
      ################## take out TEST for this month ##########################
      
      # take only samples that are in test blocks
      testsites <- c(26, 63, 43, 12, 35, 40, 31, 79,  5,  2, 60, 11)
      
      test <- subset(tddfcc, tddfcc$spatialblocks %in% testsites)
      
      saveRDS(test, paste0(datpath, "extraction_result/test_ds_", ym, ".rds"))
      write.csv2(test, paste0(datpath, "extraction_result/test_ds_", ym, ".csv"))
      
      # make a max 150000 samples big test subset
      if(nrow(test)<150000){
        ntest <- nrow(test)
      } else {
        ntest <- 150000
      }
      
      ts <- sample(nrow(test), ntest)
      testsubset <- test[ts,]
      
      saveRDS(testsubset, paste0(datpath, "extraction_result/testsubset_ds_", ym, ".rds"))
      write.csv2(testsubset, paste0(datpath, "extraction_result/testsubset_ds_", ym, ".csv"))
      
      print("test done")
      
      
      ######## make TRAIN dataset #############################################
      pottrain <-  subset(tddfcc, tddfcc$spatialblocks %notin% testsites)
      
      # get 3 Mio random samples per month to choose from in random and DI picking
      if(nrow(pottrain)<3000000){
        ntrain <- nrow(pottrain)
      } else {
        ntrain <- 3000000
      }
      
      splpot <- sample(rownames(pottrain), ntrain)
      pott3 <- tddfcc[splpot,]
      write.csv2(pott3, paste0(datpath, "extraction_result/pott3_",ym, ".csv"), row.names=F)
      
      print("train done")
      
      # clean up environment
      rm(pott3)
      rm(tempdyn)
      rm(aux)
      rm(dfslices)
      rm(tddfcc)
      rm(tddf)
      rm(swirfile)
      
    } else {
      com <- "not enough complete cases for this month"
      write.csv2(com, paste0(datpath, "extraction_result/few_samples_",ym, ".csv"))
    }
  } else {
    print("no SWIR file available for this month")
  }
  
  
}


(fs_tab <- read.csv2(list.files(datpath, pattern="fs_tab", full.names = T)))

for(y in seq(year)){
  for(m in seq(month)){
    
    print(c(y,m))
    ym <- substring(time_range[[y]][[m]][[1]][[1]], 1, 7)
    print(ym)
    pat <- paste0("L_MOD_hs_ia_", ym)
    allf <- list.files(datpath, pattern = pat, full.names=T)
    septest <- as.numeric(fs_tab$sep[fs_tab$year==year[y] & fs_tab$month==as.numeric(month)[m]])
    test_1_2 <- any(grepl(allf, pattern="_..tif$"))
    if(septest!=0){
      if(test_1_2==TRUE){ # if there are files with _1, _2 ending
          print(paste0(ym, "big files extraction"))
          extract_train_test_big_files(y,m)
      } 
    } else {
        print(paste0(ym, "already done"))
    }
    
  }
}


file.remove(list.files("/scratch/tmp/llezamav/tmp/", full.names=T))
