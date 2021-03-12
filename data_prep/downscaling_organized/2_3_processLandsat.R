#### 2_processLandsat

#################### MAKE SCENE IDENTIFIER TEXTFILE ##################################
processLandsat <- function(time_range, new_download=FALSE){
  
  ymid <- substring(time_range[[y]][[m]][[1]][[1]], 1, 7)
  L8scenepath <- paste0(main, "L8/", substring(time_range[[y]][[m]][[1]][[1]], 1, 7), "/")
  print(L8scenepath)
  
  ####### GET DATA ONLINE ##############################################################################################
  
  print("STARTING LANDSAT PROCESSING")
  
  if(new_download==TRUE){
    
    ###################################### get all scenes from directory #############################################
    
    downloadpath <- paste0(L8datpath, "espa_order_downloads/")
    list.files(downloadpath)
    
    ###################################### PROCESS LANDSAT #############################################
    
    sc <- list.files(paste0(L8scenepath, "get_data/LANDSAT/BT/"), full.names=T, 
                     pattern="Bt$")
    
    # unzip BT
    for(i in seq(sc)){
      btfilename <- list.files(paste0(sc[i], "/"), full.names=T, pattern="tar.gz")
      btfile <- list.files(paste0(sc[i], "/"), full.names=F, pattern="tar.gz")
      btf <- paste0(sc[i], "/", tools::file_path_sans_ext(tools::file_path_sans_ext(btfile)))
      #btcontent <- untar(btfilename,list=T)
      #b10pos <- grepl("band10",btcontent)
      dir.create(paste0(btf, "/"))
      untar(btfilename,exdir=paste0(btf, "/"))
      print(i)
    }
    
  } else {
    sc <- list.files(paste0(file_location, "/", ymid, "/get_data/LANDSAT/BT/"), full.names=T, 
                     pattern="Bt$")
  }
  
  
  if(length(sc)!=0){
    
    
    
    # get Metadata
    f <- list.files(paste0(list.files(sc[1], pattern="Bt$", full.names = T), "/"), full.names=T)
    #L8querymatched <- readRDS(list.files(paste0(file_location, "/", ymid), pattern="L8querymatched", full.names = T))
    
    L8querymatched <- readRDS(paste0(L8scenepath, "L8querymatched.rds"))
    
    ################## organize help files #################################################################
    
    
    sdirs <- list.files(sc, full.names = T, pattern="Bt$")
    
    
    L8info <- lapply(seq(L8querymatched), function(i){
      if(length(L8querymatched[[i]]) > 15){ # if only one level of list
        lapply(seq(nrow(L8querymatched[[i]])), function(j){

          date <- L8querymatched[[i]]$date_acquisition
          
          t <- L8querymatched[[i]]$start_time
          time <- substring(t, 10, 14)
          lcc <- L8querymatched[[i]]$cloudcov_land
          
          eid <- strsplit(L8querymatched[[i]]$summary, ",")[[1]][1]
          fname <- substring(eid, 5,nchar(eid))
          
          return(list(date=date, time=time, fname=fname, lcc=lcc))
        })
      } else {
        lapply(seq(L8querymatched[[i]]), function(x){
          lapply(seq(nrow(L8querymatched[[i]][[x]])), function(j){

            date <- L8querymatched[[i]][[x]]$date_acquisition[j]
            
            t <- L8querymatched[[i]][[x]]$start_time[j]
            time <- substring(t, 10, 14)
            lcc <- L8querymatched[[i]][[x]]$cloudcov_land[j]
            
            eid <- strsplit(L8querymatched[[i]][[x]]$summary[j], ",")[[1]][1]
            fname <- substring(eid, 5,nchar(eid))
            
            return(list(date=date, time=time, fname=fname, lcc=lcc))
          })
        })
      }
    })
    
    # rmatch <- function(x, name) {
    #   pos <- match(name, names(x))
    #   if (!is.na(pos)) return(x[[pos]])
    #   for (el in x) {
    #     if (class(el) == "list") {
    #       out <- Recall(el, name)
    #       if (!is.null(out)) return(out)
    #     }
    #   }
    # }
    # 
    # allnamsL8info <- sapply(seq(L8info), function(i){
    #   rmatch(L8info[[i]], "fname")
    # })
    
    # make df with Path, Row, Date and Time (GMT)
    df <- data.frame(matrix(unlist(L8info), ncol = 4, byrow=T))
    names(df) <- c("date", "time", "fname", "lcc")
    df$scenenumber <- as.numeric(rownames(df))
    nums <- seq(1:nrow(df))
    
    
    l8datetime <- df
    l8datetime$fname <- as.character(l8datetime$fname)
    write.csv(l8datetime, paste0(L8datpath, "L8_date_", areaname, time_range[[y]][[m]][[1]][1], ".csv"))
    
    print("relevant files selected and date and time written out")
    
    
    timediff_df <- read.csv2(paste0(L8scenepath, "timediff_df.csv"))
    
    dfm <- merge(timediff_df, df[,c("fname","date", "time", "lcc")], by.x="L8scene", by.y="fname", all.x=TRUE)
    write.csv(dfm, paste0(L8datpath, "timediff_df_extended_lcc", areaname, time_range[[y]][[m]][[1]][1], ".csv"))
    

    # # write Landsat time into timediff_df
    # timediff_df$L8time <- NA
    # timediff_df$L8date <- NA
    # df$fname <- as.character(df$fname)
    # for(i in seq(nrow(timediff_df))){
    #   ind <- which(grepl(timediff_df$L8scene[i],df$fname))
    #   print(c(timediff_df$L8name[i], df$fname[ind]))
    #   if(length(ind)>0){
    #     timediff_df$L8time[i] <- as.character(df[ind,"time"])
    #     timediff_df$L8date[i] <- as.character(df[ind,"date"])
    #   }
    # }
    
    write.csv2(timediff_df, paste0(L8scenepath, "timediff_df.csv"))
    timediff_df <- read.csv2(paste0(L8scenepath, "timediff_df.csv"))
    
    
    
    # actualize MODIS query
    timediff_comp <- timediff_df[complete.cases(timediff_df),]
    
    # if matches were found
    if(nrow(timediff_comp)!=0){
      # eliminate leading space
      timediff_comp$MODname <- timediff_comp$modscene
      
      n_occur <- data.frame(table(timediff_comp$MODname))
      
      timediff_comp$Mdupl <- NA
      timediff_comp$Mdupl[timediff_comp$MODname %in% n_occur$Var1[n_occur$Freq > 1]] <- 1
      
      
      # get which MODIS scenes are double 
      timediff_comp$Msel <- 0
      modmatchind <- sapply(seq(nrow(n_occur)), function(i){
        match(as.character(n_occur$Var1[i]), timediff_comp$MODname)
      })
      timediff_comp$Msel[modmatchind] <- 1
      
      # row in timediff_msel corresponds to list index in msel
      modquery <- readRDS(paste0(L8scenepath, "MODquerymatched_msel.rds"))
      timediff_msel <- timediff_comp[timediff_comp$Msel==1,]
      
      MODquery_scenenames <- readRDS(paste0(L8scenepath, "MODquery_scenenames.rds"))
      
      lapply( MODquery_scenenames , match  , timediff_msel$modscene )
      
      modscene_in_query <- lapply(seq(MODquery_scenenames), function(i){
        lapply(seq(MODquery_scenenames[[i]]), function(j){
          unlist(sapply(seq(timediff_msel$modscene), function(z){
            which(grepl(timediff_msel$modscene[z], MODquery_scenenames[[i]][[j]]))
            
                }))
        })
      })
      
      
      table(unlist(modscene_in_query))
      
      mselnew <- lapply(seq(modquery), function(i){
        lapply(seq(modquery[[i]]), function(j){
          
          q <- modquery[[i]][[j]][modscene_in_query[[i]][[j]],]
          if(!is.null(q)){
            modquery[[i]][[j]]
          } else{
            NULL
          }
          
        })
      })
      
      write.csv2(timediff_msel, paste0(L8scenepath, "timediff_msel.csv"))
      write.csv2(timediff_comp, paste0(L8scenepath, "timediff_comp.csv"))
      
      msel <- mselnew
      saveRDS(msel, paste0(L8scenepath, "MODquerymatched_msel.rds"))
      
      ################## process thermal bands #################################################################
      
      datloc <- l8datlist(sdirs, bandpattern=c("band10", "band11"))
      
      
      s <- lapply(seq(datloc$tifs[[1]]), function(i){
        
        # cloud mask
        x <- raster(datloc$bqa[[i]])
        c <- x
        cs <- is.element(values(c),cloud)
        c[] <- cs
        
        if(length(datloc$tifs)>1){
          sat <- stack(datloc$tifs[[1]][[i]], datloc$tifs[[2]][[i]]) # only made for 2 bands
          sat[c==1] <- NA # clean out clouds
          names(sat) <- c(basename(datloc$tifs[[1]][[i]]), basename(datloc$tifs[[2]][[i]]))
          
        } else {
          sat <- raster(datloc$tifs[[i]])
          sat[c==1] <- NA # clean out clouds
          names(sat) <- c(basename(datloc$tifs[[1]][[i]]))
          
        }
        return(sat)
      })
      

      
      ################## CUT TO AOI ##################################################################################
      
      # cut out to research area
      s.aoi <- lapply(seq(s), function(i){
        crop(s[[i]], extent(aoianta))
      })
      
      print("cut to research area")
      
      
      btc <- lapply(seq(s), function(i){
        (s.aoi[[i]]*0.1)-273.15
      })
      
      #writeRaster(btc[[2]][[1]], paste0(L8datpath, "BTC_", names(btc[[2]][[1]]), ".tif"))

      
      ################## CALCULATE LST ####################################################################

      
      # get rock outcrop raster with Emissivity values
      eta_res <- raster(paste0("E:/new_downscaling/data_download_preprocessing/Rock_outcrop_ras_", areaname, "_res.tif"))
      
      # eta_res <- resample(eta, template)
      # writeRaster(eta_res, paste0("E:/new_downscaling/data_download_preprocessing/Rock_outcrop_ras_", areaname, "_res.tif"))
      BTC <- btc
      
      BTC_res <- lapply(seq(BTC), function(i){
        resample(BTC[[i]], template)
      })
      #writeRaster(BTC_res[[2]][[1]], paste0(L8datpath, "BTC_res_", names(BTC_res[[2]][[1]])))
      

      # Calculate the Land Surface Temperature
      LST <- lapply(seq(BTC), function(i){
        x <- BTC_res[[i]]/(1+(0.0010895*BTC_res[[i]]/0.01438)*log(eta_res))

      })
      print("LST calculated")
      
      
      #writeRaster(LST[[2]][[1]], paste0(L8datpath, "LST_", names(BTC_res[[2]][[1]]), ".tif"))
      
      
      # bring to same extent and write LST 
      dir.create(paste0(L8scenepath, "LST/"))
    
      
      
      # # mask LST by AOI & write LST out
      lst_aoi <- lapply(seq(LST), function(i){
        x <- mask(LST[[i]], aoianta)
        names(x) <- names(BTC[[i]])
        x
        for(j in nlayers(x)){
          writeRaster(x[[j]], paste0(L8scenepath, "LST/", names(BTC[[i]][[j]])),
                      format="GTiff", overwrite=T)
        }
      })
      

      
      print("LST calculated, LANDSAT routine for this timestep done")
      return(list(msel=msel, LST=LST, l8datetime
                  =l8datetime))
    } else {txt <- "no available data for time range"
    print(txt)
    return("nothing")} 
    
    
    
  } else {
    "no data here"
  }
}
