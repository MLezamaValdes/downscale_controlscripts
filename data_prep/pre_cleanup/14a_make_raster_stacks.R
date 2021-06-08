


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

extract_train_test <- function(y,m){
  
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
  
  if(file.exists(allf[1])){
    tdnam <- read.csv(paste0(datpath, "names_sat_ia_hs_", ym, ".csv")) # names for tempdyn
    
    ################## if there's a swir file: extract aux ####################################
    print(paste0("SWIR file = ", paste0(swiroutpath, "swir_tc_67", ym, ".tif")))
    
    if(file.exists(paste0(swiroutpath, "swir_tc_67", ym, ".tif"))){ # if there is a SWIR file
      print(paste0("SWIR file available for", ym))
      swir <- stack(swirfile)
      aux <- stack(aux, swir)
      names(aux) <- c("dem", "slope", "aspect", "TWI", "soilraster", "landcoverres", "spatialblocks",
                      "x", "y", "swir6", "swir7")
      
      
      # extract aux
      auxdf <- as.data.frame(extract(aux, aoiaux))
      write.csv2(auxdf, paste0(datpath, "aux_df_swir_x_y_swir_", ym, ".csv"), row.names=F)
      write.csv2(auxdf[1:500,], paste0(datpath, "extraction_result_new/auxdf_check_", ym, ".csv"), row.names=F)
      
      auxdf_comp <- auxdf[complete.cases(auxdf),]
      print(paste0("n complete auxdf cases: ", nrow(auxdf_comp)))
      write.csv2(auxdf_comp, paste0(datpath, "extraction_result_new/auxdf_complete_", ym, ".csv"), row.names=F)
      
      rm(auxdf_comp)
      print("aux extraction done")
      
      if(loc=="Laptop"){
        datpath <- "D:/new_downscaling/clean_data/satstacks/"
      }
      
      ################## if files are split ####################################
      
      
      if(length(all_splits)>0){ # if files are split
        # generate split ranges
        rs <- stack(not_split)
        maxpackages <- 15
        new_package_split <- seq(1,(nlayers(rs)-2), by=(maxpackages*4))
        end_split <- new_package_split+(maxpackages*4-1)
        end_split[length(end_split)] <- nlayers(rs)
        
        new_package_split
        end_split
        
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
          write.csv2(tmpdyndf, paste0(datpath, "extraction_result_new/tempdyn_", ym,"_", i, "_df.csv"), row.names=F)
          
          print("tempdyn extraction done")
          
          # tmpdyndf <- read.csv2(paste0(datpath, "tempdyn_new_", ym,"_df.csv"), header = T)
          # test <- tmpdyndf[1:100,1]==seq(1:100)
          # if(all(test==TRUE, na.rm = T) | is.na(all(test))==TRUE){ # if fits, kick out column, if all is NA or doesn't fit, don't
          #   tmpdyndf <- tmpdyndf[,2:ncol(tmpdyndf)] # eliminate rowname column if there's one
          # }
          
          # ############ make checkfiles for aux and tempdyn extraction ##############
          write.csv2(tmpdyndf[1:500,], paste0(datpath, "extraction_result_new/tempdyndf_check_", ym, "_", i, ".csv"), row.names=F)
          
          ################### sort into useful file #########################
          #auxdf <- read.csv2(paste0(datpath, "aux_df_swir_x_y_swir_", ym, ".csv"), nrow=600000)
          
          new_package <- seq(1,(ncol(tmpdyndf)-2), by=4) # excluding x and y here already
          end <- new_package+3
          
          print(paste0("ncol tmpdyndf = ", ncol(tmpdyndf),
                       "start new package at: ", new_package,
                       "end package at: ", end))
          
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
            
            print(paste0("x has ", ncol(x), "columns, should have 23"))
            
            names(x) <- c( "Landsat", "Modis", "ia", "hs",
                           "dem", "slope", "aspect", "TWI", "soilraster", "landcoverres",
                           "spatialblocks", "x", "y", "swir6", "swir7","xd", "yd",
                           "id", "modtime", "ymo", "hmi", "Lscene", "Mscene")
            
            x
          })
          
          tddf <- do.call(rbind,dfslices)
          head(tddf)
          rm(dfslices)
          rm(tmpdyndf)
          rm(aux)
          
          
          print("tddf done ")
          
          #summary(tddf)
          
          
          ################### get complete cases ##################################
          
          
          tddf$Modis[tddf$Modis > 30] <- NA
          tddf$Landsat[tddf$Landsat > 30] <- NA
          tddf$Modis[tddf$Modis <= -100] <- NA
          tddf$Landsat[tddf$Landsat <= -100] <- NA
          
          write.csv2(tddf, paste0(datpath, "extraction_result_new/tddf_",ym, "_", i, ".csv"), row.names = F)
          
          tddfcc <- tddf[complete.cases(tddf),]
          rm(tddf)
          
          write.csv2(tddfcc[1:500,], paste0(datpath, "extraction_result_new/extr_comp_check_",ym, "_", i, ".csv"), row.names = F)
          #tddf <- read.csv2(paste0(datpath, "extr_comp_",ym, ".csv"), header=T)
          
          write.csv2(tddfcc, paste0(datpath, "extraction_result_new/extr_complete_cases_",ym, "_", i, ".csv"), row.names=F)
          
          #tddfcc <- read.csv2(paste0(datpath, "extr_complete_cases_", ym, ".csv"))
          
          print(paste0("tddfcc split files, ", i, " / ", length(seq(all_splits)), "done"))
          
          print(paste0("length of all samples (tdfcc ", i, ") = ", nrow(tddfcc)))
          
          rm(tddfcc)
          
          
        }
        
        ##########################  routine per split done ############################
        
        ###################### list all tdfcc files for this month and ##################
        ###############put them together to form one dataframe (rbind) ##################
        print("reading tddfcc now")
        pat_tddfcc <- paste0("extr_complete_cases_",ym, "_")
        
        tdfiles <- list.files(paste0(datpath, "extraction_result_new"), pattern=pat_tddfcc, full.names = T)
        print(tdfiles)
        
        tddfcc_list <- lapply(seq(tdfiles), function(i){
          print(i)
          read.csv2(tdfiles[i])
        })
        
        tddfcc <- do.call(rbind,tddfcc_list)
        head(tddfcc)
        write.csv2(tddfcc, paste0(datpath, "extraction_result_new/extr_complete_cases_",ym, ".csv"), row.names=F)
        
        
      } else {
        
        ############ if files are not split: standard procedure 9 for regular files ######################
        tempdyn <- raster::stack(not_split)
        names(tempdyn) <- tdnam$x
        
        ################## extract dyn stack  #############################################
        tempdyn <- stack(tempdyn, aux$x, aux$y)
        
        rasterOptions(tmpdir="/scratch/tmp/llezamav/tmp/")
        
        # for(i in seq(nlayers(tempdyn))){
        #   print(i)
        #   print(summary(tempdyn[[i]]))
        # }
        
        tmpdyndf <- as.data.frame(extract(tempdyn, aoiaux))
        write.csv2(tmpdyndf, paste0(datpath, "extraction_result_new/tempdyn_", ym,"_df.csv"), row.names=F)
        
        print("tempdyn extraction done")
        
        # tmpdyndf <- read.csv2(paste0(datpath, "tempdyn_", ym,"_1_df.csv"), nrow=600000)
        # test <- tmpdyndf[1:100,1]==seq(1:100)
        # if(all(test==TRUE, na.rm = T) | is.na(all(test))==TRUE){ # if fits, kick out column, if all is NA or doesn't fit, don't
        #   tmpdyndf <- tmpdyndf[,2:ncol(tmpdyndf)] # eliminate rowname column if there's one
        # }
        
        # ############ make checkfiles for aux and tempdyn extraction ##############
        write.csv2(tmpdyndf[1:500,], paste0(datpath, "extraction_result_new/tempdyndf_check_", ym, ".csv"), row.names=F)
        write.csv2(auxdf[1:500,], paste0(datpath, "extraction_result_new/auxdf_check_", ym, ".csv"), row.names=F)
        
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
          
          print(paste0("x has ", ncol(x), " columns, should have 23"))
          
          names(x) <- c( "Landsat", "Modis", "ia", "hs",
                         "dem", "slope", "aspect", "TWI", "soilraster", "landcoverres",
                         "spatialblocks", "x", "y", "swir6", "swir7","xd", "yd",
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
        
        write.csv2(tddf, paste0(datpath, "extraction_result_new/tddf_",ym, ".csv"), row.names=F)
        
        tddfcc <- tddf[complete.cases(tddf),]
        rm(tddf)
        #tddfcc <- tddf[complete.cases(tddf$Landsat, tddf$Modis, tddf$swir6),]
        write.csv2(tddfcc, paste0(datpath, "extraction_result_new/extr_complete_cases_",ym, ".csv"), row.names=F)
        
        write.csv2(tddfcc[1:500,], paste0(datpath, "extraction_result_new/extr_comp_check_",ym, ".csv"), row.names = F)
        #tddf <- read.csv2(paste0(datpath, "extr_comp_",ym, ".csv"), header=T)
        #tddfcc <- read.csv2(paste0(datpath, "extr_complete_cases_", ym, ".csv"))
        
        print("tddfcc unsplit files done")
        
        print(paste0("length of all samples (tdfcc) = ", nrow(tddfcc)))
      }
      
      ################## join routine for split and unsplit files #############################################
      tddfcc <- read.csv2(paste0(datpath, "extraction_result_new/extr_complete_cases_",ym, ".csv"))
      if(nrow(tddfcc)>0){
        ################## take out TEST for this month ##########################
        
        # take only samples that are in test blocks
        testsites <- c(26, 63, 43, 12, 35, 40, 31, 79,  5,  2, 60, 11)
        
        test <- subset(tddfcc, tddfcc$spatialblocks %in% testsites)
        
        saveRDS(test, paste0(datpath, "extraction_result_new/test_ds_", ym, ".rds"))
        write.csv2(test, paste0(datpath, "extraction_result_new/test_ds_", ym, ".csv"))
        
        # make a max 150000 samples big test subset
        if(nrow(test)<150000){
          ntest <- nrow(test)
        } else {
          ntest <- 150000
        }
        
        ts <- sample(nrow(test), ntest)
        testsubset <- test[ts,]
        
        saveRDS(testsubset, paste0(datpath, "extraction_result_new/testsubset_ds_", ym, ".rds"))
        write.csv2(testsubset, paste0(datpath, "extraction_result_new/testsubset_ds_", ym, ".csv"))
        
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
        write.csv2(pott3, paste0(datpath, "extraction_result_new/pott3_",ym, ".csv"), row.names=F)
        
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
        write.csv2(com, paste0(datpath, "extraction_result_new/few_samples_",ym, ".csv"))
      }
    } else {
      print("no SWIR file available for this month")
    }
  } else {
    print("no satellite data for this month available")
  }
}

# for(y in seq(year)){
#   for(m in seq(month)){
for(y in c(6)){
  for(m in c(8)){
    print(c(y,m))
    ym <- substring(time_range[[y]][[m]][[1]][[1]], 1, 7)
    print(ym)
    extract_train_test(y,m)
    gc()
  }
}

for(y in c(7)){
  for(m in seq(month)){
    print(c(y,m))
    ym <- substring(time_range[[y]][[m]][[1]][[1]], 1, 7)
    print(ym)
    extract_train_test(y,m)
    gc()
  }
}

file.remove(list.files("/scratch/tmp/llezamav/tmp/", full.names=T))
