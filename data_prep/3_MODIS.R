#' Get and process MODIS LST Swath files
#' 
#' @description 
#' First, MODIS tries to read the qualitycheck.csv file from the getprocessLandsat() function. If it doesn't only contain 
#' the information, that "no data suitable" or "no available data for time range", the day of the month that was being downloaded
#' for is being extracted. 
#' For those days, "MODIS_MOD11_L2_V6" and "MODIS_MYD11_L2_V6" products are queried, and check the queries' tile's overlap with the aoi.
#' Only tiles that have at least 10% area intersection with the aoi will be downloaded and hdfs put into one folder together.
#' The MODIS Swath file is batch tranlated via the HEG tool and collected in one folder.
#' The LST files are being read into R and values converted to valid range (7500-65535) and a scale factor = 0.02 applied. 
#' Data was also converted from K to °C by subtracting 273.15. Tiles are projected  to EPSG 3031 WGS 84 / Antarctic Polar Stereographic.
#' Find a common extent for all files and resample to a clean 1x1km resolution. All MODIS files are being mosaiced. 
#' Find the  max bounding box, bring them all to this resolution and then stack the files and crop to aoi extent. 
#' Next, 4 date rasters are being created, which inform on the amount of available datapoints for each pixel, the minimum and maximum 
#' time and time range covered (time_rasters_MDV_time_range.tif)
#' Then, the goodness of fit between the acquisition time of Landsat 8 and MODIS is calculated by retrieving the L8_date_MDV_timerange.csv
#' file. The L8 time is converted to minute of day and the time difference is being calculated. 
#' @param time_range
#' @return -
#' @author Maite Lezama Valdes
#' @examples
#' year <- c(2019:2013)
#' month <- c("01","02", "12")
#' day <- c(17:22)
#' time_range <- lapply(seq(year), function(j){
#'   lapply(seq(month), function(i){
#'       y <- paste(paste0(year[j],"-",month[i],"-",day), 
#'       paste0(year[j],"-",month[i],"-",day))
#'       strsplit(y, " ")
#'       })
#' })
#' for(y in seq(year)){
#' for(m in seq(month)){
#'   getprocessMODIS(time_range)
#'   }
#' }

getprocessMODIS <- function(time_range){
  
  ####### LOAD PACKAGES, SET PATHS ###############################################################################
  
  # match MODIS downlaod time to available L8 data
  L8scenepath <- paste0(main, "L8/", substring(time_range[[y]][[m]][[1]][[1]], 1, 7), "/")
  try(qualL8 <- read.csv(list.files(L8scenepath, pattern="quality", full.names=T)),
      silent=T)

  if(exists("qualL8")){

  if(!qualL8[1,] == "no data suitable" && !qualL8[1,]=="no available data for time range"){
    downloadedday <- read.csv(list.files(L8scenepath, pattern="downloaded", full.names=T))
    
    cs <- strsplit(as.character(downloadedday$summary), ",", fixed = TRUE)
    ad <- lapply(cs, `[[`, 2)
    daynum <- as.numeric(lapply(strsplit(as.character(lapply(strsplit(as.character(ad), ":"),`[[`, 2)), "-"), `[[`, 1))
    
    print("STARTING MODIS DOWNLOAD AND PREP")
    
    modisscenepath <- paste0(modispath, substring(time_range[[y]][[m]][[1]][[1]], 1, 7), "/")
    hdfpath <- paste0(modisscenepath, "hdfs/")
    MODtifHDFoutpath <- paste0(modisscenepath, "translated/")
    MODLSTpath <- paste0(modisscenepath, "LST/")
    
    
    ## set archive directory
    set_archive(modisscenepath)
    
    print("basic settings done")
    
    
    ####### GET DATA ONLINE ##############################################################################################
    
    ## set aoi and time range for the query
    set_aoi(aoiutm)
    
    ## get available products and select one
    product1 <- "MODIS_MOD11_L2_V6"
    product2 <- "MODIS_MYD11_L2_V6"
    # product_names <- getMODIS_names(username="MaiteLezama", password = "Eos300dmmmmlv")
    # product1 <- grep("MOD11_L2_V6", product_names, value = T)
    # product2 <- grep("MYD11_L2_V6", product_names, value = T)
    
    ## query for records for your AOI, time range and product
    L8dayc <- NA
    for(i in seq(length(daynum))){
      L8dayc[i] <- which(as.numeric(substring(time_range[[y]][[m]], 12,13))==daynum[i])
    }
    
    L8day <- unique(L8dayc)
    
    query <- lapply(seq(length(L8day)), function(z){ # for all days
          query1 <- getMODIS_query(time_range = time_range[[y]][[m]][[L8day[z]]], name = product1,
                             username="MaiteLezama", password = "Eos300dmmmmlv",
                             aoi=get_aoi())
          query2 <- getMODIS_query(time_range = time_range[[y]][[m]][[L8day[z]]], name = product2,
                             username="MaiteLezama", password = "Eos300dmmmmlv",
                             aoi=get_aoi())
          return(list(query1, query2))
    })

    
    seq(query)
    wgsproj <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
    footprints <- lapply(seq(query), function(x){
                    lapply(seq(query[[x]]), function(y){
                      tiles <- lapply(seq(nrow(query[[x]][[y]])), function(i){
                        a <- query[[x]][[y]]$spatialFootprint[[i]]
                        a1 <- strsplit(substring(a, 11,(nchar(a)-2)), split = ",")
                        ap <- strsplit(a1[[1]], split=" +")
                        
                        abc <- unlist(lapply(seq(ap), function(i){
                          ap[[i]][ap[[i]] != ""] 
                        }))
                        df <- data.frame(matrix(as.numeric(abc), ncol=2, byrow=T))
                        xy <- df[,c(1:2)]
                        fp <- SpatialPointsDataFrame(coords = xy, data = df,
                                                     proj4string = CRS(wgsproj))
                        fpp <- Polygon(fp)
                        Ps1 = SpatialPolygons(list(Polygons(list(fpp), ID = "a")), 
                                              proj4string=CRS(wgsproj))
                      })
      return(tiles)
    })
    })
    
    ## preview a record
    #getMODIS_preview(query1[9,])
    
    
    # select those that overlap with aoi
    # are those also in aoi
    aoiwgs <- spTransform(aoi, wgsproj)
    
    aoiint <- lapply(seq(footprints), function(i){
      lapply(seq(footprints[[i]]), function(j){
        lapply(seq(footprints[[i]][[j]]), function(k){
          print(c(i,j,k))
        try(intersect(aoiwgs, footprints[[i]][[j]][[k]]))
      })
      })
    })
    
    aoiarea <- lapply(seq(footprints), function(i){
      lapply(seq(footprints[[i]]), function(j){
        lapply(seq(footprints[[i]][[j]]), function(k){
        if(!is.null(aoiint[[i]][[j]][[k]]) & class(aoiint[[i]][[j]][[k]])!="try-error"){
          sum(area(aoiint[[i]][[j]][[k]]))
        } else {
          0
        }
      })
      })
    })
    
    areaaoi <- area(aoiwgs)
    
    aoiprop <- lapply(seq(footprints), function(i){
      lapply(seq(footprints[[i]]), function(j){
        lapply(seq(footprints[[i]][[j]]), function(k){
        if(aoiarea[[i]][[j]][[k]]!=0){
          aoiarea[[i]][[j]][[k]]/areaaoi
        } else {
          0
        }
      })
    })
    })
    
    # get only tiles that have at least 10% of their area over aoi
    
    sel_aoi <- lapply(seq(query), function(x){ #days
      lapply(seq(query[[x]]), function(y){ # MYD and MOD
      z <- unlist(aoiprop[[x]][[y]])
      z <- which(z>0.1)
    }) 
    })
    
    ## download records
        # create hdfpath
    dir.create(file.path(modisscenepath, "hdfs/"))
    
    for(i in seq(query)){ # amount of days that were ok in Landsat data
      for(j in seq(sel_aoi[[i]])){ # 2, MOD and MYD
        if(length(sel_aoi[[i]][[j]])!=0){
          files <- getMODIS_data(query[[i]][[j]][sel_aoi[[i]][[j]],])
          # put all hdfs into one folder (hdfpath)
          nl <- paste0(hdfpath, basename(files))
          file.copy(from=files, to=nl, 
                    overwrite = TRUE)
        }

      }
    }
    
    print("MODIS data downloaded and in place")
    
    ######### BATCH TRANSLATING SWATH TO GEOTIFF WITH HEG TOOL ####################################################
    
    # get template prm file
    tpl <- read.delim(list.files(batchoutdir, pattern="unix.prm", full.names = T),
                      sep="=", col.names = c("nam", "val"), header = F, stringsAsFactors = F)
    
    # run HEG tool to tranlate swaths to geotiff 
    tplpath <- list.files(batchoutdir, pattern="unix.prm", full.names = T)
    
    # list all files in hdf dir
    filescomp <- list.files(hdfpath, full.names=T)
    
    # clean out batchoutdir if something there from previous run
    a <- character(0)
    if(!identical(a, list.files(batchoutdir, pattern=".tif"))){
      file.remove(list.files(batchoutdir, pattern=".tif", full.names = T))
    }
    
    # run HEG tool
    runheg(files=filescomp, batchindir, batchoutdir, tplpath, layer = "LST|")
    
    # transport results to other filespath
    dir.create(file.path(MODtifHDFoutpath)) # create MODtifHDFoutpath
    MODtiffiles <- list.files(batchoutdir, pattern=".tif$", full.names=T)
    MODtifHEG <- paste0(MODtifHDFoutpath, basename(MODtiffiles))
    file.copy(from=MODtiffiles, to=MODtifHEG, 
              overwrite = TRUE)
    
    print("batch translating hdf to tif done")
    
    ######## GO ON PROCESSING LST IN R ########################################################################################
    
    # get tif files
    lst <- lapply(seq(filescomp), function(i){
      raster(list.files(MODtifHDFoutpath, pattern=".tif$", full.names=T)[i])
    }) 
    
    # convert values to valid range and degree C
    print("converting values to valid range and degree Celsius")
    dir.create(MODLSTpath)
    lst_c <- lapply(seq(lst), function(i){
      # Valid Range = 7500-65535
      lst[[i]][lst[[i]] == 0 ] <- NA
      #lst[[i]][lst[[i]] < 7500 & lst[[1]] > 65535] <- NA
      
      # scale factor = 0.02
      lst_1_conv <- lst[[i]]*0.02
      
      # convert to degree C
      lstc <- lst_1_conv - 273.15 
      
      writeRaster(lstc, paste0(MODLSTpath, "cels_", names(lst[[i]]), ".tif"), format="GTiff", 
                  overwrite=T)
      
      print(i)
      return(lstc)
    })
    
    # save projected and resampled rasters
    dir.create(file.path(MODLSTpath)) # create MODtifHDFoutpath
    
    
    # project rasters
    print("projecting rasters - will take quite a while")
    lapply(seq(lst_c), function(i){
      print(i)
      x <- projectRaster(lst_c[[i]], crs = antaproj)
      writeRaster(x, paste0(MODLSTpath, "proj_", names(lst[[i]]), ".tif"), format="GTiff", 
                  overwrite=T)
      x
    })
    
  
    rm(lst_c)
    gc()
      
    projfiles <- list.files(MODLSTpath, pattern="proj", full.names = T)
    lst_cp <- lapply(seq(projfiles), function(i){
      raster(projfiles[i])
    })
    tmplras <- lst_cp[[1]]
    
    if(length(lst_cp)>1){
      # get extent of files
      MODext <- lapply(seq(lst_cp), function(i){
        p <- as(extent(lst_cp[[i]]), 'SpatialPolygons')
        crs(p) <- antaproj
        p
      })
      me <- do.call(bind, MODext)
      
      # make a template to force 1x1km pixels
      extent(tmplras) <- extent(me)
    }

    res(tmplras) <- c(1000, 1000)
    tmplras[] <- 1
    
  
    # resample all rasters to 1km x 1km resolution  
    lst_res <- lapply(seq(lst_cp), function(i){
      print(i)
      x <- resample(lst_cp[[i]], tmplras)
      writeRaster(x, paste0(MODLSTpath, names(lst[[i]]), ".tif"), format="GTiff", 
                  overwrite=T)
      x
    })
    
    print("rasters resampled to 1x1km resolution")
    

    # for(i in seq(lst_res)){
    #   print(i)
    #   writeRaster(lst_res[[i]], paste0(MODLSTpath, names(lst_res[[i]]), ".tif"), format="GTiff", 
    #               overwrite=T)
    # }
    # 
    # # read converted to ?C, projected and resampled tifs back in
    # lst_cp <- lapply(seq(grep(list.files(path=paste0(modisscenepath, "LST_2018_01_19/"), full.names=T), pattern='res', inv=T, value=T)), function(i){
    #   f <- grep(list.files(path=paste0(modisscenepath, "LST_2018_01_19/"), full.names=T), pattern='res', inv=T, value=T)
    #   raster(f[i])
    # })
    # 
    
    # # read converted to degree ?C, projected and resampled tifs back in
    # fls <- list.files(MODLSTpath, full.names=T)
    # lst_resm <- lapply(seq(fls), function(i){
    #   raster(fls[i])
    # })
    # lst_res <- lst_resm
    
    print("values, resolution and projection adapted")
    
    ############# PATCH IMAGES ####################################################################################
    
    if(length(lst_res) > 1){
            # write mosaic command 
      mrg <- character()
      for(i in seq(lst_res)){
        mrg[i] <- paste0("lst_res[[", i, "]]")
      }
      #mrg[length(mrg)] <- paste0("lst_res[[", length(mrg), "]]")
      mrg <- paste(mrg, sep="", collapse=",") 
      cm <- paste("raster::mosaic(", mrg, ", tolerance=0.9, 
                  fun=mean, overwrite=T, overlap=T, ext=NULL)")
      
      mosaic <- eval(parse(text=cm))
      mosaic[mosaic < -90] <- NA # correct for too low values
      
      writeRaster(mosaic, paste0(modisscenepath, areaname, "_MODIS_LST_Mosaic.tif"), format="GTiff", 
                  overwrite=T, bylayer=T)
      #mosaic <- raster(paste0(modisscenepath, areaname, "_MODIS_LST_Mosaic.tif"))
      
      # visualize mosaic
      #mapview(mosaic, col.regions = viridis(500), legend = TRUE)
      
      print("MODIS images patched")
    }

    
    ############ MAKE A RASTER WHICH GIVES INFO ON INPUT RASTER ##########################################################
    
    #mosaic <- raster(paste0(modisscenepath, "MDV_MODIS_LST_Mosaic.tif"))
    
    # find max bounding box
    newextent <- compbb(lst_res)
    
    # bring them all to new extent
    lst_ex <- lapply(seq(lst_res), function(i){
      x <- crop(lst_res[[i]], newextent)
      names(x) <- names(lst_res[[i]])
      x
    })
    
    lst_ex <- stack(lst_ex)
    
    # put in correct names if any got missing during processing
    testnam <- which(grepl( "layer",names(lst_ex)))
    for(i in seq(testnam)){
      names(lst_ex[[testnam[i]]]) <- names(lst[[testnam[i]]])
    }
    
    # # put in original names if files have been read in or lost original filename
    # only to be used when rasters are being read in after writing, which in final
    # routine shouldn't be the case
    
    # while(any(!grepl("11_L2", names(lst_ex)))){
    # 
    #   nums <- as.numeric(gsub("[^0-9]", "", names(lst_ex))) # take just the numeric parts of the name
    #   orgnam <- lapply(seq(lst), function(i){
    #     x<-names(lst[[nums[i]]])
    #     print(i)
    #     x
    #   })
    # 
    #   # check if everything went well
    #   i <- 19
    #   orgnam[[i]]
    #   names(lst[[nums[i]]])
    # 
    #   # put in original names
    #   for(i in seq(nlayers(lst_ex))){
    #     names(lst_ex[[i]]) <- orgnam[[i]]
    #   }
    # 
    #   # test
    #   res <- names(lst_ex)
    #   c <- sapply(seq(lst), function(i){
    #     names(lst[[nums[i]]])
    #   })
    # 
    #   res == c
    # }
    
    # crop everything to aoi - perhaps here? 
    lst_s <- crop(lst_ex, aoianta)
    
    # write
    for(i in seq(nlayers(lst_s))){
      print(i)
      writeRaster(lst_s[[i]], paste0(MODLSTpath, "small_", names(lst_res[[i]]), ".tif"), format="GTiff", 
                  overwrite=T)
    }
    
    print("indidvidual MODIS images stacked and cut to aoi")
    
    ################ MAKE DATE RASTERS #################################
    
    #lst_s <- stack(list.files(MODLSTpath, pattern="small", full.names=T))
    
    fnams <- sapply(seq(lst), function(i){
      names(lst[[i]])
    })
    
    utcdates <- lapply(seq(fnams), function(i) {
      fnam <- fnams[i]
      # get UTC date from fnam
      su <- strsplit(fnam, "A")
      su <- su[[1]][length(su[[1]])]
      org <- paste0(substring(su, 1,4), "-01-01")
      utcday <- as.Date((as.numeric(substring(su, 5,7))-1), origin=org)
      
      if(grepl("v", su)){
        utctime <- NULL
        utcdate <- utcday
      } else {
        utctime <- paste0(substring(su, 9, 10), ":", substring(su, 11, 12))
        utcdate <- strptime(paste0(utcday,utctime), format='%Y-%m-%d %H:%M', tz="UTC")
      }
      print(i)
      return(utcdate)
    })
    
    # test
    v <- NULL
    for(i in seq(utcdates)){
      v[i] <- as.character(utcdates[[i]])
    }
    data.frame(fnams=fnams, utc = v)
    # 
    # rm(lst_c)
    # rm(lst)
    # rm(lst_ex)
    # rm(lst_cp)
    # rm(lst_res)
    
    MODdate <- datestodoymod(utcdates, fnams, lst_s)
    
    # make a raster with min of time range and max of time range 
    # and one with amount of rasters going into the pixel
    
    emptlay <- lapply(seq(nlayers(MODdate$minutesofdayras)), function(i) {
      any(!is.na(MODdate$minutesofdayras[[i]])[]==1)
    })
    
    sell <- unlist(emptlay)
    b <- seq(1:length(sell))
    sel <- b[sell]
    
    subtimeras <- subset(MODdate$minutesofdayras, sel)
    
    # to get amount of available values
    nonna <- raster(apply(as.array(subtimeras), 1:2, function(x) length(na.omit(x))))
    nv <- nonna[]
    nonnares <- subtimeras[[1]]
    nonnares[] <- nv
    
    # min and max to get time range
    mi <- min(subtimeras, na.rm = T)
    ma <- max(subtimeras, na.rm=T)
    diff <- ma-mi
    
    dir.create(paste0(modisscenepath, "date/"))
    
    writeRaster(nonna, paste0(modisscenepath, "date/amount_available_data.tif"), format="GTiff", overwrite=T)
    writeRaster(mi, paste0(modisscenepath, "date/min_time.tif"), format="GTiff", overwrite=T)
    writeRaster(ma, paste0(modisscenepath, "date/max_time.tif"), format="GTiff", overwrite=T)
    writeRaster(diff, paste0(modisscenepath, "date/time_range.tif"), format="GTiff", overwrite=T)
    
    
    # find max bounding box
    tl <- list(nonnares, mi, ma, diff)
    
    newextent <- compbb(tl)
    
    # bring them all to new extent
    tl_ex <- lapply(seq(tl), function(i){
      crop(tl[[i]], newextent)
    })
    
    
    tstack <- stack(nonnares, mi, ma, diff)
    names(tstack) <- c("sum_av", "min_t", "max_t", "t_range")
    writeRaster(tstack, paste0(modisscenepath, "date/time_rasters", areaname, time_range[[y]][[m]][[1]][1], ".tif"), 
                format="GTiff", overwrite=T)
    
    print("time rasters done")
    
    ############# GOODNESS OF FIT OF ACQUISITION TIME (L8 / MODIS) ##############################
    
    timeex <- data.frame(extract(tstack, extent(tstack)))
    

    L8time <- read.csv(paste0(L8datpath, "L8_date_", areaname, time_range[[y]][[m]][[1]][1], ".csv"))
    L8date <- lapply(seq(nrow(L8time)), function(i){
      strptime(paste(L8time$date[i], L8time$time[i]), format='%Y-%m-%d %H:%M:%S', tz="UTC")
    })
    
    # convert L8 time to minute of day
    
              #L8dates <- datestodoymod(L8date, fnams, lst_s)
              doy <- sapply(seq(L8date), function(i){
                  strftime(L8date[[i]], format = "%j")
              })
              
              minutes <- sapply(seq(L8date), function(i){
                minute(L8date[[i]])
              })
              
              hours <- sapply(seq(L8date), function(i){
                hour(L8date[[i]])
              })
              
              L8LST <- lapply(seq(list.files(paste0(L8scenepath, "bt/"), pattern="LST")), function(i){
                raster(list.files(paste0(L8scenepath, "bt/"), pattern="LST", full.names = T)[i])
              })
                
              
              dateras <- lapply(seq(L8LST), function(i){
                d <- L8LST[[i]]
                d[!is.na(d[])] <- as.numeric(doy[[i]])
                mod <- L8LST[[i]]
                mod[!is.na(mod)] <- (hours[[i]]*60)+minutes[[i]]
                y <- stack(d, mod)
                print(i)
                return(y)
              })
              
              drs <- stack(dateras)
              dayras <- subset(drs, c(seq(1,nlayers(drs), by=2)))
              timeras <- subset(drs, c(seq(2,nlayers(drs), by=2)))
              
              minutesofday <- (hours*60)+minutes
    
    
    timeex$L8time <- rep(mean(L8dates$minutesofday), nrow(timeex))
    
    timeex$fit <- 99
    for(i in seq(nrow(timeex))){
      if(any(is.na(timeex[i,]))==F){ # are there NA values interfering
        if(timeex$L8time[i] < timeex$max_t[i] & timeex$L8time[i] > timeex$min_t[i]){
          timeex$fit[i] <- 1 # one where L8 time lies within MODIS timeframe 
        } else {
          timeex$fit[i] <- 0
        }
      }
      print(i)
    }
    timeex$fit[timeex$fit==99] <- NA
    
    
    d <- table(timeex$fit)
    d[2]/d[1]
    d[2]/nrow(timeex) #Anteil Pixel mit ?berlappender Zeitspanne
    
    # write this one out 
    
    timeex$dev <- 99
    for(i in seq(nrow(timeex))){
      if(any(is.na(timeex[i,]))==F){ # are there NA values interfering
        if(timeex$L8time[i] > timeex$max_t[i]){
          timeex$dev[i] <- timeex$L8time[i] - timeex$max_t[i] # one where L8 time lies within MODIS timeframe 
        } else if (timeex$L8time[i] < timeex$min_t[i]) {
          timeex$dev[i] <- timeex$max_t[i] - timeex$L8time[i] 
        } else if (timeex$fit[i]==1){
          timeex$dev[i] <- 0
        }
      }
      print(i)
    }
    timeex$dev[timeex$dev==99] <- NA
    
    table(timeex$dev)
    
    # write out as well 
    
    
    timediff <- tstack[[1]]
    
    timediff[] <- timeex$dev
    
    writeRaster(timediff, 
                paste0(modisscenepath, "date/time_rasters", areaname, "_", time_range[[y]][[m]][[1]][1], ".tif"),
                format="GTiff", overwrite=T)
    
    print("timedifference to L8 written")
    
  } else {
    print("no data downloaded and processed, because no suitable L8 data found")
  }
  

  gc()
  } else {
    print("no data downloaded an processed, because no suitable L8 data found")
  }
}




