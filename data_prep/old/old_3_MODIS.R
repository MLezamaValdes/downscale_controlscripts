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
  
  L8scenepath <- paste0(main, "L8/", substring(time_range[[y]][[m]][[1]][[1]], 1, 7), "/")
  msel <- readRDS(paste0(L8scenepath, "MODquerymatched_msel.rds"))
  msel$msel <- readRDS(paste0(L8scenepath, "MODquerymatched_msel.rds"))

  if(any(msel!="nothing")){
    
  
  if(length(msel$msel)!=0){
    
  # if there is something useful in L8 data
  #if(exists("qualL8")){
    
  if(!qualL8[1,] == "no data suitable" && !qualL8[1,]=="no available data for time range"){
  downloadedday <- read.csv(list.files(L8scenepath, pattern="downloaded", full.names=T))

  # cs <- strsplit(as.character(downloadedday$summary), ",", fixed = TRUE)
  # ad <- lapply(cs, `[[`, 2)
  # daynum <- as.numeric(lapply(strsplit(as.character(lapply(strsplit(as.character(ad), ":"),`[[`, 2)), "-"), `[[`, 1))

  L8datetime <- as.POSIXlt(downloadedday$datetime, format="%Y:%j:%H:%M:%S")
    # 
    # # +/- 2h around L8datetime
    # maxtimerange <- L8datetime+hours(1)
    # mintimerange <- L8datetime-hours(1)
    # # take all days that are in the range of 2h around L8date
    # daynum <- unique(c(day(maxtimerange), day(mintimerange)))
    # 
    
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
    # product1 <- "MODIS_MOD11_L2_V6"
    # product2 <- "MODIS_MYD11_L2_V6"
    # product_names <- getMODIS_names(username="MaiteLezama", password = "Eos300dmmmmlv")
    # product1 <- grep("MOD11_L2_V6", product_names, value = T)
    # product2 <- grep("MYD11_L2_V6", product_names, value = T)
    
    # ## query for records for your AOI, time range and product
    # L8dayc <- NA
    # for(i in seq(length(daynum))){
    #   L8dayc[i] <- which(as.numeric(substring(time_range[[y]][[m]], 12,13))==daynum[i])
    # }
    # 
    # L8day <- unique(L8dayc)
    # 
    # query <- lapply(seq(length(L8day)), function(z){ # for all days
    #       query1 <- getMODIS_query(time_range = time_range[[y]][[m]][[L8day[z]]], name = product1,
    #                          username="MaiteLezama", password = "Eos300dmmmmlv",
    #                          aoi=get_aoi())
    #       query2 <- getMODIS_query(time_range = time_range[[y]][[m]][[L8day[z]]], name = product2,
    #                          username="MaiteLezama", password = "Eos300dmmmmlv",
    #                          aoi=get_aoi())
    #       return(list(query1, query2))
    # })
    # 
    # 
    aoiwgs <- spTransform(aoi, wgsproj)
    
    query <- msel$msel
    wgsproj <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
    if(any(sapply(query, is.list))){
         footprints <- lapply(seq(query), function(x){
          a <- query[[x]]$spatialFootprint
          a1 <- strsplit(substring(a, 11,(nchar(a)-2)), split = ",")
          ap <- strsplit(a1[[1]], split=" +")
          
          abc <- unlist(lapply(seq(ap), function(i){
            ap[[i]][ap[[i]] != ""]
          }))
          df <- data.frame(matrix(as.numeric(abc), ncol=2, byrow=T))
          #df <- df[1:4,1:2]
          # change position 3 and 4
          #df <- df[c(1,2,4,3,5),]
          xy <- df[,c(1:2)]
          names(xy) <- c("X", "Y")
          fp <- SpatialPointsDataFrame(coords = xy, data = df,
                                       proj4string = CRS(wgsproj))
          fpp <- Polygon(fp)
          Ps1 = SpatialPolygons(list(Polygons(list(fpp), ID = "a")),
                                proj4string=CRS(wgsproj))
          if(!gIsValid(Ps1)){
            df <- df[c(1,2,4,3,5),]
            xy <- df[,c(1:2)]
            names(xy) <- c("X", "Y")
            fp <- SpatialPointsDataFrame(coords = xy, data = df,
                                         proj4string = CRS(wgsproj))
            fpp <- Polygon(fp)
            Ps1 = SpatialPolygons(list(Polygons(list(fpp), ID = "a")),
                                  proj4string=CRS(wgsproj))
          }
          Ps1
        })
    
    } else {    footprints <- lapply(seq(query), function(x){
      lapply(seq(query[[x]]), function(j){
        tiles <- lapply(seq(nrow(query[[x]][[j]])), function(i){
          a <- query[[x]][[j]][i,]$spatialFootprint
          a1 <- strsplit(substring(a, 11,(nchar(a)-2)), split = ",")
          ap <- strsplit(a1[[1]], split=" +")
          
          abc <- unlist(lapply(seq(ap), function(i){
            ap[[i]][ap[[i]] != ""]
          }))
          df <- data.frame(matrix(as.numeric(abc), ncol=2, byrow=T))
          #df <- df[1:4,1:2]
          # change position 3 and 4
          #df <- df[c(1,2,4,3,5),]
          xy <- df[,c(1:2)]
          names(xy) <- c("X", "Y")
          fp <- SpatialPointsDataFrame(coords = xy, data = df,
                                       proj4string = CRS(wgsproj))
          fpp <- Polygon(fp)
          Ps1 = SpatialPolygons(list(Polygons(list(fpp), ID = "a")),
                                proj4string=CRS(wgsproj))
          if(!gIsValid(Ps1)){
            df <- df[c(1,2,4,3,5),]
            xy <- df[,c(1:2)]
            names(xy) <- c("X", "Y")
            fp <- SpatialPointsDataFrame(coords = xy, data = df,
                                         proj4string = CRS(wgsproj))
            fpp <- Polygon(fp)
            Ps1 = SpatialPolygons(list(Polygons(list(fpp), ID = "a")),
                                  proj4string=CRS(wgsproj))
          }
          Ps1
        })
        return(tiles)
      })
    })
    }
      
 

    ## preview a record
    #getMODIS_preview(query1[9,])


    # select those that overlap with aoi
    # are those also in aoi


    try(aoiint <- lapply(seq(footprints), function(i){
      lapply(seq(footprints[[i]]), function(j){
        lapply(seq(footprints[[i]][[j]]), function(k){
        x <- intersect(aoiwgs, footprints[[i]][[j]][[k]])
        print(c(i,j,k))
        x
        # if(!is.null(x)){
        #   return(x)
        # } else  {return("FALSE")}
        })
      })
    }), silent = T)

    # get those scenes if there is an intersection with MODIS
    if(length(unlist(aoiint))!=0){

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

      # get only tiles that cover at least 10% of aoi area

      sel_aoi <- lapply(seq(footprints), function(i){
        lapply(seq(footprints[[i]]), function(j){
          lapply(seq(footprints[[i]][[j]]), function(k){
                z <- unlist(aoiprop[[i]][[j]][[k]])
                z > 0.1
          })
        })
      })



    #if(length(unlist(sel_aoi))!=0){
    if(any(msel!="nothing")){
      
    ## download records
        # create hdfpath
    dir.create(file.path(modisscenepath, "hdfs/"))
    
    # get names from selected MODIS query per day
    MODselfnams <- lapply(seq(query), function(i){
        lapply(seq(query[[i]]), function(j){
            x <- query[[i]][[j]][unlist(sel_aoi[[i]][[j]]),]
            if(nrow(x)==0){
              return(paste0("not_selected", i,j))
            } else if(nrow(x)>1){
              return(paste(x$displayId, collapse="AND"))
            } else if(length(nrow(x))==1) {
              return(x$displayId)
            }
        })
      })
    
    # get exact time of MODIS scene capturing for those tiles, that overlap with aoi enough
    datetimeMODISquery <- lapply(seq(query), function(i){
      lapply(seq(query[[i]]), function(j){
        # if(nrow(query[[i]][[j]][unlist(sel_aoi[[i]][[j]]),])!=0){
        #   xd <- query[[i]][[j]][unlist(sel_aoi[[i]][[j]]),]
          if(nrow(query[[i]][[j]])!=0){
          xd <- query[[i]][[j]]
          xd <- xd$displayId
          y <- strsplit(xd, "A")[[1]][2]
          y1 <- paste0(substring(y, 1, 4), ":", substring(y, 5, 7), ":", 
                       substring(y, 9, 10), ":", substring(y, 11, 12))
          y2 <- as.POSIXlt(y1, format="%Y:%j:%H:%M")
        }
      })
    })
    
    seldatetimeMODIS <- do.call("c", datetimeMODISquery)
    
    MODuseful <- sapply(seq(seldatetimeMODIS), function(i){
      !is.null(seldatetimeMODIS[[i]])
    })
    
    
    ### select time matches and download data ##############################################################

    if(length(datetimeMODISquery)!=0){
      
      
      # compare time of MODIS aoi overlapping scenes with L8 viable scene's (aoi, clouds, land, matching L8 scenes available) 
      # capturing time
      timediff <- lapply(seq(length(L8datetime)), function(z){
        lapply(seq(length(seldatetimeMODIS)), function(yz){
          if(!is.null(seldatetimeMODIS[[yz]])){
              as.numeric(abs(difftime(L8datetime[z], seldatetimeMODIS[[yz]], units="hours")))
          }
        })
      })
      

      # take only MODIS scenes, that are 2h close to L8 time
      if(any(unlist(timediff)<=2)){
        mat <- data.frame(matrix(data=unlist(timediff), ncol=length(L8datetime), byrow=T))
        names(mat) <- paste0("L8_", seq(length(L8datetime)))
        rownames(mat) <- unlist(MODselfnams)
        
        # for each column, i.e. each L8 scene: which MOD scene is <2h away
        nModmatchL8 <- sapply(seq(ncol(mat)), function(c){
          which(mat[,c]<2)
        })
        
        # for each row, i.e. each MODIS scene: which L8 scene is <2h away
        modscenematching <- sapply(seq(nrow(mat)), function(c){
          which(mat[c,]<2)
        })
      
        
        L8scenematched <- sapply(seq(length(L8datetime)), function(xm){
          length(nModmatchL8[[xm]])>0
        })
        
        # write out which L8 file is being matched by MODIS
        downloadedday$matched <- L8scenematched
        downloadedday$MODscenematch <- unlist(as.character(nModmatchL8))
        downloadedday$modnam <- NA
        
        umsn <- unlist(MODselfnams)
        for(i in seq(nrow(downloadedday))){
          if(length(downloadedday$MODscenematch[i])==1){
            downloadedday$modnam[i] <- paste(umsn[as.numeric(downloadedday$MODscenematch[i])],
                                             collapse = ";")
          }
          if(grepl("c",downloadedday$MODscenematch[i]) | grepl(":",downloadedday$MODscenematch[i])){ # wenn c(1,8) z.B. oder c(1:3)
                mnams <- paste(umsn[eval(parse(text=downloadedday$MODscenematch[i]))],
                collapse = ";")
                downloadedday$modnam[i] <- mnams
          }
          if(grepl("integ",downloadedday$MODscenematch[i])){ # wenn integer(0), i.e. no match
            downloadedday$modnam[i] <- "no match"
          }          
        }
        
        write.csv2(downloadedday, paste0(L8scenepath, "downloaded_days_MODmatch.csv"))
      
        Modscenematching <- sapply(seq(nModmatchL8), function(xm){
          if(length(nModmatchL8[[xm]])>0){
            MODselfnams[nModmatchL8[[xm]]]
          }
        })
        
        if(any(!is.na(downloadedday$modnam))){
        # get MODIS files selected by overlap aoi, etc & time
          
        
        modquerynew <- lapply(seq(datetimeMODISquery), function(i){
          lapply(seq(datetimeMODISquery[[i]]), function(j){
            if(!is.null(datetimeMODISquery[[i]][[j]])){
              query[[i]][[j]][unlist(sel_aoi[[i]][[j]]),]
            }
          })
        })
        
        # get MODIS files from USGS
        files <- lapply(seq(modquerynew), function(i){
          lapply(seq(modquerynew[[i]]), function(j){
            if(!is.null(modquerynew[[i]][[j]])){
              try(getMODIS_data(modquerynew[[i]][[j]]), silent=T)
            }
          })
        })
        
        files <- lapply(seq(query), function(i){
          lapply(seq(query[[i]]), function(j){
            if(!is.null(query[[i]][[j]])){
              try(getMODIS_data(query[[i]][[j]]), silent=T)
            }
          })
        })

        # put all hdfs into one folder (hdfpath)
        nl <- paste0(hdfpath, basename(unlist(files)))
        file.copy(from=unlist(files), to=nl, 
                  overwrite = TRUE)
        
        print("MODIS data downloaded and in place")
        
        if(length(list.files(hdfpath))!=0){
          
          
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
          runheg(files=filescomp, indir=batchindir, outdir=batchoutdir, tplpath=tplpath, layer = "LST|")
          
          # transport results to other filespath
          dir.create(file.path(MODtifHDFoutpath)) # create MODtifHDFoutpath
          MODtiffiles <- list.files(batchoutdir, pattern=".tif$", full.names=T)
          MODtifHEG <- paste0(MODtifHDFoutpath, basename(MODtiffiles))
          file.copy(from=MODtiffiles, to=MODtifHEG, 
                    overwrite = TRUE)
          
          print("batch translating hdf to tif done")
          
          ######## GO ON PROCESSING LST IN R ########################################################################################
          
          # get tif files
          lst <- lapply(seq(list.files(MODtifHDFoutpath, pattern=".tif$")), function(i){
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
          projfiles <- projfiles[  sapply(seq(projfiles), function(i){
            !grepl("small", projfiles[i])
          })]
          
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
          testnam <- which(grepl("layer",names(lst_ex)))
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
          
          namdate <- data.frame(fnams=fnams, utc = v)
          # 
          # rm(lst_c)
          # rm(lst)
          # rm(lst_ex)
          # rm(lst_cp)
          # rm(lst_res)
          
          # MODdate contains dayofyear, minutesofday as data and as rasters as well 
          MODdate <- datestodoymod(utcdates, fnams, lst_s)
          
          # make a raster with min of time range and max of time range 
          # and one with amount of rasters going into the pixel
          
          # emptlay <- lapply(seq(nlayers(MODdate$minutesofdayras)), function(i) {
          #   any(!is.na(MODdate$minutesofdayras[[i]])[]==1)
          # })
          # 
          # sell <- unlist(emptlay)
          # b <- seq(1:length(sell))
          # sel <- b[sell]
          # 
          # subtimeras <- subset(MODdate$minutesofdayras, sel)
          # 
          # # to get amount of available values
          # nonna <- raster(apply(as.array(subtimeras), 1:2, function(x) length(na.omit(x))))
          # nv <- nonna[]
          # nonnares <- subtimeras[[1]]
          # nonnares[] <- nv
          # 
          # # min and max to get time range
          # mi <- min(subtimeras, na.rm = T)
          # ma <- max(subtimeras, na.rm=T)
          # diff <- ma-mi
          # 
          # dir.create(paste0(modisscenepath, "date/"))
          # 
          # writeRaster(nonna, paste0(modisscenepath, "date/amount_available_data.tif"), format="GTiff", overwrite=T)
          # writeRaster(mi, paste0(modisscenepath, "date/min_time.tif"), format="GTiff", overwrite=T)
          # writeRaster(ma, paste0(modisscenepath, "date/max_time.tif"), format="GTiff", overwrite=T)
          # writeRaster(diff, paste0(modisscenepath, "date/time_range.tif"), format="GTiff", overwrite=T)
          # 
          
          # find max bounding box
          # tl <- list(nonnares, mi, ma, diff)
          
          # newextent <- compbb(tl)
          
          # # bring them all to new extent
          # tl_ex <- lapply(seq(tl), function(i){
          #   crop(tl[[i]], newextent)
          # })
          # 
          # time_rastersMDV2019-01-17.tif:
          # which pixels are not na, minimum minute of day, maximum minute of day, difference between minimum and maximum
          # tstack <- stack(nonnares, mi, ma, diff)
          # names(tstack) <- c("sum_av", "min_t", "max_t", "t_range")
          # writeRaster(tstack, paste0(modisscenepath, "date/time_rasters", areaname, time_range[[y]][[m]][[1]][1], ".tif"), 
          #             format="GTiff", overwrite=T)
          
          print("MODIS time done")
          
          ############# GOODNESS OF FIT OF ACQUISITION TIME (L8 / MODIS) ##############################
          
          # timeex <- data.frame(extract(tstack, extent(tstack)))
          
          
          L8time <- read.csv(paste0(L8datpath, "L8_date_", areaname, time_range[[y]][[m]][[1]][1], ".csv"))
          L8date <- lapply(seq(nrow(L8time)), function(i){
            strptime(paste(L8time$date[i], L8time$time[i]), format='%Y-%m-%d %H:%M:%S', tz="UTC")
          })
          
          # convert L8 time to minute of day
          fnamsL8 <- list.files(paste0(L8scenepath, "bt/"), pattern="BTC")
          
          
          #L8dates <- datestodoymod(L8date, fnamsL8, lst_s)
          doy <- sapply(seq(L8date), function(i){
            strftime(L8date[[i]], format = "%j")
          })
          
          minutes <- sapply(seq(L8date), function(i){
            minute(L8date[[i]])
          })
          
          hours <- sapply(seq(L8date), function(i){
            hour(L8date[[i]])
          })
          
          
          # L8LST <- lapply(seq(list.files(paste0(L8scenepath, "bt/"), pattern="BTC")), function(i){
          #   raster(list.files(paste0(L8scenepath, "bt/"), pattern="BTC", full.names = T)[i])
          # })
          
          # # dateras / drs [[1]] = day of year = dayras, [[2]]=minute of day=timeras
          # dateras <- lapply(seq(L8LST), function(i){
          #   d <- L8LST[[i]]
          #   d[!is.na(d[])] <- as.numeric(doy[[i]])
          #   mod <- L8LST[[i]]
          #   mod[!is.na(mod)] <- (hours[[i]]*60)+minutes[[i]]
          #   y <- stack(d, mod)
          #   print(i)
          #   return(y)
          # })
          # 
          # drs <- stack(dateras)
          # dayras <- subset(drs, c(seq(1,nlayers(drs), by=2)))
          # timeras <- subset(drs, c(seq(2,nlayers(drs), by=2)))
          # 
          
          
          # make L8 date df 
          minutesofday <- (hours*60)+minutes
          L8datedf <- L8time
          
          L8datedf$fnam <- fnamsL8
          L8datedf$doy <- doy
          L8datedf$min <- minutes
          L8datedf$hrs <- hours
          L8datedf$minday <- minutesofday
          
          # make MODIS date df 
          moddate <- character()
          for(i in seq(utcdates)){
            moddate[i] <- as.character( utcdates[[i]][1])
          }
          
          modL8datedf <- data.frame(doy=MODdate$dayofyear, minday=MODdate$minutesofday, date=moddate, fnam=fnams)
          
          dir.create(paste0(main, "timediff/", substring(time_range[[y]][[m]][[1]][[1]], 1, 7), "/"))
          
          write.csv2(L8datedf, paste0(main, "timediff/", substring(time_range[[y]][[m]][[1]][[1]], 1, 7), "/", "dates_L8.csv"))
          write.csv2(modL8datedf, paste0(main, "timediff/", substring(time_range[[y]][[m]][[1]][[1]], 1, 7), "/", "dates_MODIS.csv"))
          
          # timeex$L8time <- rep(mean(minutesofday), nrow(timeex))
          # minutesofday/60
          # timeex$fit <- 99
          # for(i in seq(nrow(timeex))){
          #   if(any(is.na(timeex[i,]))==F){ # are there NA values interfering
          #     if(timeex$L8time[i] < timeex$max_t[i] & timeex$L8time[i] > timeex$min_t[i]){
          #       timeex$fit[i] <- 1 # one where L8 time lies within MODIS timeframe 
          #     } else {
          #       timeex$fit[i] <- 0
          #     }
          #   }
          # }
          # timeex$fit[timeex$fit==99] <- NA
          # 
          # 
          # d <- table(timeex$fit)
          # d[2]/d[1]
          # d[2]/nrow(timeex) #Anteil Pixel mit ?berlappender Zeitspanne
          # 
          # # write this one out 
          # 
          # timeex$dev <- 99
          # for(i in seq(nrow(timeex))){
          #   if(any(is.na(timeex[i,]))==F){ # are there NA values interfering
          #     if(timeex$L8time[i] > timeex$max_t[i]){
          #       timeex$dev[i] <- timeex$L8time[i] - timeex$max_t[i] # one where L8 time lies within MODIS timeframe 
          #     } else if (timeex$L8time[i] < timeex$min_t[i]) {
          #       timeex$dev[i] <- timeex$max_t[i] - timeex$L8time[i] 
          #     } else if (timeex$fit[i]==1){
          #       timeex$dev[i] <- 0
          #     }
          #   }
          # }
          # timeex$dev[timeex$dev==99] <- NA
          # 
          # table(timeex$dev)
          # 
          # # write out as well 
          # 
          # 
          # timediff <- tstack[[1]]
          # 
          # timediff[] <- timeex$dev
          # 
          # writeRaster(timediff, 
          #             paste0(modisscenepath, "date/time_rasters", areaname, "_", time_range[[y]][[m]][[1]][1], ".tif"),
          #             format="GTiff", overwrite=T)
          
          print("timedifference to L8 written")
          
          # daydiff <- abs(as.numeric(MODdate$dayofyear)-as.numeric(doy))
          # minutesdiff <- abs(MODdate$minutesofday-minutesofday)
          # 
          # (datediff <- paste("MODIS day of year:", MODdate$dayofyear,
          #             "; Landsat 8 day of year", doy , 
          #             "; MODIS minutes of day:", MODdate$minutesofday, 
          #             "; Landsat 8 minutesofday", minutesofday,
          #             "MINUTESDIFF:", minutesdiff, 
          #             "DAYDIFF:", daydiff, "Landsat scenes",
          #             downloadedday$summary, downloadedday$lcc,
          #             "MODIS scenes", fnams, namdate))
          # 
          # write.csv2(datediff, paste0(modisscenepath, "date/datediff.csv"))
        
    
            
          
    }
    
        } } else {print("no temporally matching scenes")
          
          file.rename(L8scenepath, paste0(substring(L8scenepath, 1, (nchar(L8scenepath)-nchar(basename(L8scenepath))-1)), 
                                          paste0("no_tmatch_",basename(L8scenepath))))
          }# for if there are MODIS scenes within <2h of L8
    } 
    
    gc()
    } else {print("temporally matching MODIS scenes not useful")
      file.rename(L8scenepath, paste0(substring(L8scenepath, 1, (nchar(L8scenepath)-nchar(basename(L8scenepath))-1)), 
                                      paste0("no_tmatch_",basename(L8scenepath))))
      }
    } else {print("no intersection between MODIS and AOI")
    file.rename(L8scenepath, paste0(substring(L8scenepath, 1, (nchar(L8scenepath)-nchar(basename(L8scenepath))-1)), 
                                    paste0("no_tmatch_",basename(L8scenepath))))
    }# for if there are MODIS scenes within <2h of L8
  } else {print("no L8 data available here")
                file.rename(L8scenepath, paste0(substring(L8scenepath, 1, (nchar(L8scenepath)-nchar(basename(L8scenepath))-1)), 
                                                paste0("no_tmatch_",basename(L8scenepath))))
      } 
  
  # if there are any files in L8 
  } else {print("no L8 data")
    file.rename(L8scenepath, paste0(substring(L8scenepath, 1, (nchar(L8scenepath)-nchar(basename(L8scenepath))-1)), 
                                    paste0("no_L8_data_",basename(L8scenepath))))
    }
  }} # function




