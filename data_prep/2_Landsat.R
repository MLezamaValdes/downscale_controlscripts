
#' Get and process LANDSAT files
#' 
#' @description Function gets a query for Landsat 8 C1 data online for the aoi and time-range. 
#' The query is subselected for high quality images with little cloud cover by taking the footprints from the query, making polygons
#' out of them and calculating the overlap of the tiles of interest with the area of interest. Only tiles with an aoi-coverage of 
#' >= 30% will be taken. In case that there is no data in the query, write a csv file to be picked up by MODIS procedure later
#' and stop this function - also, if cloud cover is too great, do the same. Generate and write a quality check df, which sums up
#' info on cloud cover, quality of images, etc. Select tiles from query. If any is selected, find out which are the tiles, that
#' overlap enough and only have 20% cloud cover. Write a summary file "downloaded_days.csv" to L8scenepath 
#' (paste0(main, "L8/", substring(time_range[[y]][[m]][[1]][[1]], 1, 7), "/")). Download the selected tiles from USGS. If there are
#' are any files to process, summarize and read all available scenes into R with their metadata, get acquisition times summarized.
#' calculate, how much of the area of the downloaded scenes actually lies in AOI and on land. Write selected files after Cloud cover, 
#' aoi overlap and land overlap to (paste0(L8datpath, "L8_date_", areaname, time_range[[y]][[m]][[1]][1], ".csv")). 
#' Change 0 values to NA. 
#' Cut to AOI.
#' Make atmospheric correction (stimating digital number pixel value of dark objects in visible wavelength with estimateHaze(), with
#' hazebands 3 and 4. Radiometric calibration and correction of Landsat data using radCor, method="sdos" and estimated haze values 
#' from above. Write to paste0(L8scenepath, "ac/").
#' Calculate Brightness Temperature: Get additive (AL) and multiplicative (ML) radiative rescaling constants from Band 10, calculate
#' TOA spectral radiance: TOA = (ML * B10_dn) + AL 
#' To convert TOA spectral radiance into BT, K1 and K2 = Band-specific thermal conversion constants are retrieved from the metadata.
#' L = TOA.
#' BTK <- (K2 /(log((K1 / TOA) +1)))
#' BTC <- (BTK-273.15)
#' All BTC below -90°C is being set to NA. Files are being written to paste0(L8scenepath, "bt/").
#' Get rock outcrop raster with Emissivity values (eta) and calculate LST: BTC[[i]]/(1+(0.0010895*BTC[[i]]/0.01438)*log(eta)). 
#' Mask LST by AOI and bring them all to new extent so that they can be stacked. 
#' Merge LST files for this time frame.
#' @param time_range
#' @return list of LST stack of all tiles that were processed to LST and l8datetime (selection based on cloud cover, aoi overlap
#' and land overlap)
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
#'   getprocessLANDSAT(time_range)
#'   }
#' }



####### LANDSAT 8 ##############################################################################################

getprocessLANDSAT <- function(time_range){
  
  L8scenepath <- paste0(main, "L8/", substring(time_range[[y]][[m]][[1]][[1]], 1, 7), "/")
  print(L8scenepath)
  
  ####### GET DATA ONLINE ##############################################################################################
  
  print("STARTING LANDSAT DOWNLOAD AND PREP")
  
  ## set aoi and time range for the query
  l8proj <- crs("+proj=utm +zone=57 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs ")
  aoiutm <- spTransform(aoi, l8proj)
  set_aoi(aoiutm)
  
  ## set archive directory
  set_archive(L8scenepath)
  
  ## get available products and select one
  #product_names <- getLandsat_names(username="MaiteLezama", password = "Eos300dmmmmlv")
  product <- "LANDSAT_8_C1"
  
  ## query for records for your AOI, time range and product
  nodat <- list(0)
  
  query <- lapply(seq(day), function(d){
    try(getLandsat_query(time_range = time_range[[y]][[m]][[d]], name = product,
                         aoi=get_aoi()), silent=T)
  })
  
  te <- sapply(seq(query), function(x){
    class(query[[x]])
  })
  
  if(any(te!="try-error")){
    
    

    ######### subselect query directly for high quality images with little cloud cover #########
    
    #find out which overlaps mostly with aoi, so that this tile can be checked thoroughly
    wgsproj <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
    footprints <- lapply(seq(query), function(x){
      tiles <- lapply(seq(nrow(query[[x]])), function(i){
        a <- query[[x]]$spatialFootprint[[i]] # get footprint from query
        a1 <- strsplit(substring(a, 11,(nchar(a)-2)), split = ",")
        ap <- strsplit(a1[[1]], split=" +")
        
        abc <- unlist(lapply(seq(ap), function(i){
          ap[[i]][ap[[i]] != ""] 
        }))
        df <- data.frame(matrix(as.numeric(abc), ncol=2, byrow=T))
        xy <- df[,c(1:2)]
        fp <- SpatialPointsDataFrame(coords = xy, data = df, #make spatial points data frame from footprint
            proj4string = wgsproj)
        fpp <- Polygon(fp)
        Ps1 = SpatialPolygons(list(Polygons(list(fpp), ID = "a")), 
                              proj4string=wgsproj)
      })
      return(tiles)
    })
    
    # are those also in aoi
    aoiwgs <- spTransform(aoi, wgsproj)
    
    # find out intersection between aoi and footprints
    aoiint <- lapply(seq(footprints), function(i){
      lapply(seq(footprints[[i]]), function(j){
              intersect(aoiwgs, footprints[[i]][[j]])
      })
    })
    
    # area of overlap
    aoiarea <- lapply(seq(footprints), function(i){
      lapply(seq(footprints[[i]]), function(j){
        if(!is.null(aoiint[[i]][[j]])){
          sum(area(aoiint[[i]][[j]]))
        } else {
          0
        }
      })
    })
    
    areaaoi <- area(aoiwgs)

    # which proportion of aoi is covered by tile? 
    aoiprop <- lapply(seq(footprints), function(i){
      lapply(seq(footprints[[i]]), function(j){
        if(aoiarea[[i]][[j]]!=0){
          aoiarea[[i]][[j]]/areaaoi
        } else {
          0
        }
      })
    })
    
    # get only tiles that have at least 30% of their area over aoi
    sel_aoi <- lapply(seq(query), function(x){
      z <- unlist(aoiprop[[x]])
      z <- which(z>0.3)
    }) 
    
    # if there is no data in the query, write a csv file to be picked up by MODIS procedure later
    # and stop this function
    # also, if cloud cover is too great, do the same
    qualitycheck <- lapply(seq(query), function(x){
      nodat <- 0
      lowqual <- 0
      cc <- 0
      selaoi <- 0
      if(class(query[[x]])=="NULL"){ # if no data was found at all for this day
        nodat <- 1
        print("no data for this time frame available")
        txt <- "nodat"
        write.csv(txt, paste0(L8scenepath, "nodat_day_", day[[x]], ".csv"))
      } 
      if(any(grepl("T1",query[[x]]$browseUrl)==F)){ # if any of the files is not best quality
        #https://www.usgs.gov/centers/eros/science/usgs-eros-archive-landsat-archives-landsat-8-oli-operational-land-imager-and?qt-science_center_objects=0#qt-science_center_objects
        lowqual <- 1
        print("no good quality products available")
        txt <- "lowqual"
        write.csv(txt, paste0(L8scenepath, "lowqual_day_", day[[x]], ".csv"))
      } 
      if(!length(sel_aoi[[x]])==0){
        landclouds_aoi <- mean(query[[x]]$LandCloudCover[sel_aoi[[x]]])
        mrg <- character()
        for(i in seq(length(sel_aoi[[x]]))){
          mrg[i] <- paste0("sel_aoi[[x]][", i, "]")
        }
        mrg <- paste(mrg, sep="", collapse=",")
        cm <- paste("as.numeric(paste0(", mrg, "))")
        selaoi <- eval(parse(text=cm))

        if(landclouds_aoi>65){
          cc <- 1
          print("too much cloud cover for this date")
          txt <- "cc"
          write.csv(txt, paste0(L8scenepath, "cc_day_", day[[x]], ".csv"))
        }
      } else {landclouds_aoi <- NA}

      return(list(nodat = nodat, lowqual = lowqual, cc=cc, 
                  cloudmean_aoitiles=landclouds_aoi, selaoi=selaoi))
    })
    
    qualitycheckdf <- data.frame(matrix(unlist(qualitycheck), nrow=length(qualitycheck), byrow=T))
    names(qualitycheckdf) <- names(qualitycheck[[1]])
    qualitycheckdf$day <- day
    
    #query[5,]$levels_available
    
    ## preview a record
    #getLandsat_preview(query[[3]][3,])
    
    qualitycheckdf$select <- unlist(lapply(seq(nrow(qualitycheckdf)), function(i){
      if(all(qualitycheckdf[i,1:3]==c(0,1,0))==TRUE & qualitycheckdf$selaoi[i] !=0){
        x="yes"
      } else if(all(qualitycheckdf[i,1:3]==c(0,0,0))==TRUE & qualitycheckdf$selaoi[i] !=0){ # also take scenes, where cc is ok that are low quality
        x = "yes"
      } else {
        x="no"
      }
      x
    }))
    
    write.csv(qualitycheckdf, paste0(L8scenepath, "qualitycheck.csv"), row.names = F)
    
    ## select suitable days 
    selectquery <- lapply(seq(nrow(qualitycheckdf)), function(i){
      ret <- 0
      if(qualitycheckdf$select[i] == "yes"){
        ret <- 1
      }
      ret
    })
    
    #whichtile <- aoiprop[unlist(selectquery==1)]
    
    
    if(any(unlist(selectquery)==1)){
      # subsq <- which(unlist(selectquery)==1)
      # # which from those selected to be suitable is lowest in mean cloud cover?
      # if(subsq>1){
      #   subsqopt <- which(qualitycheckdf$cloudmean == min(qualitycheckdf[subsq,]$cloudmean))
      # } else {
      #   subsqopt <- subsq
      # }
      # 
      
      # WHICH of the tiles from those days overlaps > 30% over aoi?
      bigarea <- sapply(seq(aoiprop), function(i){
        a <- unlist(aoiprop[[i]])[(unlist(aoiprop[[i]]) > 0.30) == T]
        sapply(seq(a), function(j){
          which(unlist(aoiprop[[i]]) == a[j])
        })
        })
      
      subsqopt <- which((selectquery)==1)
      
      # list of infos concerning the selected tiles selquery[[day]][[1]][[1=query, 2=summary, 3=day and tile # in original query]]
      selquery <- lapply(seq(selectquery), function(i){ # for all days
        if(selectquery[[i]]==1){
          if(length(bigarea[[i]])!=0){
            s <- lapply(seq(length(bigarea[[i]])), function(j){ # for all tiles of the day
                print(c(i,j))
                q <- query[[ i ]] [(bigarea[[i]][[j]]),] # nimm die Kacheln mit ausreichend Überlappung mit AOI
                d <- q$summary
                ij <- c(i,j)
                return(list(q,d,ij))
            })
          } else {s <- "no good scene here"}
        } else {s <- "no good scene here"}
        return(s)
      })
      
      # get the selected tiles together
      sum_selquery <- lapply(seq(selquery), function(i){ # für alle Tage
        lapply(seq(length(selquery[[i]])), function(j){ # für alle Kacheln
              if(!selquery[[i]][[j]][1]=="no good scene here"){
                print(c(i,j))
                selquery[[i]][[j]][[2]]
              }
            })
          })
    
      
      # get amount of land cloud cover for the selected tiles
      cc <- lapply(seq(selquery), function(i){ # für alle Tage
        lapply(seq(length(selquery[[i]])), function(j){
          if(!selquery[[i]][[j]][1]=="no good scene here"){
          selquery[[i]][[j]][[1]]$LandCloudCover}
        })
      })
      
      # construct the new query out of tiles, which have only 20% cc
      querynew <- lapply(seq(selquery), function(i){ # für alle Tage
        lapply(seq(length(selquery[[i]])), function(j){
          if(!selquery[[i]][[j]][1]=="no good scene here" && cc[[i]][[j]]<20){
            selquery[[i]][[j]][[1]]
          }
        })
      })
      
      # write info file on downloaded tiles
      downloadsumdf <- data.frame(summary= unlist(sum_selquery), lcc = unlist(cc))
      seldf <- downloadsumdf[downloadsumdf$lcc < 20,]
      write.csv(seldf, paste0(L8scenepath, "downloaded_days.csv"), row.names = F)
      
      # get files from USGS
      files <- lapply(seq(querynew), function(i){
        lapply(seq(querynew[[i]]), function(j){
          if(!is.null(querynew[[i]][[j]])){
            try(getLandsat_data(records=querynew[[i]][[j]], level="l1", espa_order=NULL), silent=T)
          }
        })
      })
      
      # get file type of files to check if there is anything in it
      fte <- sapply(seq(files), function(x){
        lapply(seq(querynew[[x]]), function(j){
        class(files[[x]][[j]])
      })
      })
      
      if(any(unlist(fte)!="try-error" & unlist(fte)!="NULL")){ # if there is any scene to process
      
      #### PREP DOWNLOADED L8 IMAGES  #######################################################
      
      L8dirs <- paste0(L8datpath, substring(time_range[[y]][[m]][[1]][[1]], 1, 7), "/get_data/LANDSAT/L1/")
      sdirs <- list.files(L8dirs, full.names = T)
      
      ##### LOAD ALL DOWNLOADED L8 SCENES  #####################################################################
      # summarize all available scenes
      datloc <- l8datlist(sdirs)
      
      metaData <- lapply(seq(datloc$meta), function(i){
        readMeta(datloc$meta[[i]], raw=T)
      })
      
      lsat8o <- lapply(seq(datloc$meta), function(i){
        stackMeta(datloc$meta[[i]], category = "image", allResolutions = F)
      })
      
      dir.create(paste0(L8scenepath, "bt/"))
      
      lapply(seq(datloc$tifs), function(i){
        bqa <- raster(grep('BQA', datloc$tifs[[i]], value=TRUE))
        bqa[bqa >= 3 & bqa <= 2720] <- 9999
        bqa[bqa >= 3744 & bqa <= 3756] <- 9999
        bqa[bqa!=9999] <- NA
        # if(crs(bqa)== antaproj){
        #   bqa <- projectRaster(bqa, crs=antaproj, method="bilinear")
        # }
        writeRaster(bqa, paste0(L8scenepath, "bt/", basename(grep('BQA', datloc$tifs[[i]], value=TRUE))),
                    overwrite=T)
      })
      
      print("data location, metaData and stack done")
      
      ###### CHECK WHICH SCENES GO TOGETHER #########################################################################
      
      #  Landsat data acquisition times are expressed in Greenwich Mean Time (GMT) standard.
      pathrow <- lapply(seq(metaData), function(i){
        path <- metaData[[i]]$PRODUCT_METADATA[rownames(metaData[[i]]$PRODUCT_METADATA) == "WRS_PATH",]
        row <- metaData[[i]]$PRODUCT_METADATA[rownames(metaData[[i]]$PRODUCT_METADATA) == "WRS_ROW",]
        date <- metaData[[i]]$PRODUCT_METADATA[rownames(metaData[[i]]$PRODUCT_METADATA) == "DATE_ACQUIRED",]
        time <- metaData[[i]]$PRODUCT_METADATA[rownames(metaData[[i]]$PRODUCT_METADATA) == "SCENE_CENTER_TIME",]
        return(list(path=path, row=row, date=date, time=time))
      })
      
      # make df with Path, Row, Date and Time (GMT)
      df <- data.frame(matrix(unlist(pathrow), ncol = 4, byrow=T))
      names(df) <- names(pathrow[[1]])
      df$scenenumber <- as.numeric(rownames(df))
      df
  
      nums <- seq(1:nrow(df))
      
      # ordered by time: stacks
      s <- lapply(seq(nums), function(i){
        stackMeta(datloc$meta[[nums[i]]], quantity = 'all')
      })
      
      ############# CHECK WHICH FILES ARE OVER LAND ######################################################
      # check, how much of the area of the downloaded scenes that might be selected actually lies on land
      
      # do all tiles overlap with land? 
      t <- lapply(seq(s), function(i){
        intersect(land, s[[i]])
      })
      
      # how much area overlaps with land?
      tarea <- lapply(seq(t), function(i){
        if(!is.null(t[[i]])){
          sum(area(t[[i]]))
        }
      })
      
      # which are over 10000000000 area
      ba <- lapply(seq(t), function(i){
        tarea[[i]]>=10000000000
      })
      selnum <- which(ba==T)
      
      # take those with over 10000000000 area
      sel <- s[unlist(ba)]
      
      # write date and time for later use with MODIS 
      l8datetime <- df[selnum,]
      write.csv(l8datetime, paste0(L8datpath, "L8_date_", areaname, time_range[[y]][[m]][[1]][1], ".csv"))
      
      print("relevant files selected and date and time written out")
      
      
      
      
      ############# ELIMINATE 0 VALUES ########################################################################## 
      s <- sel # use subselected after cloud cover, aoi overlap and land overlap
      for(i in seq(s)){
        for(j in seq(nlayers(s[[i]]))){
          s[[i]][[j]][s[[i]][[j]]==0] <- NA
        }
      }
      print("0 values replaced by NA")
      
      ################## CUT TO AOI ##################################################################################
      
      # cut out to research area
      s.aoi <- lapply(seq(s), function(i){
        crop(s[[i]], extent(aoianta))
      })
      
      print("cut to research area")
      
      ################## ATMOSPHERIC CORRECTION ####################################################################
      
      dir.create(paste0(L8scenepath, "ac/"))
      lsat8_sdos <- lapply(seq(s.aoi), function(i){
        lsat8 <- s.aoi[[i]]
        names(lsat8) <- names(lsat8o[[selnum[i]]])
        
        #estimate digital number pixel value of dark objects in visible wavelength
        hazeDN    <- estimateHaze(lsat8, hazeBands = c("B3_dn", "B4_dn"),
                                  darkProp = 0.01)
        
        # radiometric calibration and correction of Landsat data 
        lsat8_sdos <- radCor(lsat8, 
                             metaData = readMeta(datloc$meta[[i]], raw=F),
                             hazeValues = hazeDN,
                             hazeBands = c("B3_dn", "B4_dn"),
                             method = "sdos")
        
        mD <- readMeta(datloc$meta[[i]], raw=T)
        nam <- mD$METADATA_FILE_INFO["LANDSAT_PRODUCT_ID",]
        dir.create(paste0(L8scenepath, "ac/", nam, "/"))
        for(j in seq(nlayers(lsat8_sdos))){
          writeRaster(lsat8_sdos[[j]], paste0(L8scenepath, "ac/", nam, "/", nam, "_", names(lsat8_sdos)[j], ".tif"), 
                      format="GTiff", overwrite=T)
        }
        lsat8_sdos
      })
      
      
      # # get files in, ordered
      # fac <- list.files(paste0(L8datpath, "ac/"), full.names = T, pattern=".tif$")
      # lo <- seq(1,length(metaData[unlist(bigarea)][unlist(aoibigarea)])*10, by=10) # *10 because channel 1:11 without channel 8 (pancromatic, in 15m resolution)
      # hi <- lo+9
      # lsat8_sdos <- lapply(seq(sel_f), function(i){
      #   stack(fac[lo[i]:hi[i]])
      # })
      # 
      

      # # get extent of relevant files to crop DEM
      # L8exts <- lapply(seq(s), function(i){
      #   p <- as(extent(lsat8_sdos[[i]]), 'SpatialPolygons')
      #   crs(p) <- antaproj
      #   p
      # })
      # m <- do.call(bind, L8exts)
      # 
      # # SpatialPolygons to SpatialPolygonsDataFrame
      # mid <- sapply(slot(m, "polygons"), function(x) slot(x, "ID"))
      # m.df <- data.frame( ID=1:length(m), row.names = mid) 
      # mn <- SpatialPolygonsDataFrame(m, m.df)
      # 
      # writeOGR(mn, dsn=paste0(L8datpath, "L8_ext.shp"), layer="L8_ext",
      #          driver="ESRI Shapefile")
      
      print("AC done")
      #############  Calculation of TOA (Top of Atmospheric) spectral radiance and #################### 
      #################  brightness temperature ##################################################
      
      #TO DO: check why values are off!
      dir.create(paste0(L8scenepath, "bt/"))
      
      BTC <- lapply(seq(selnum), function(i){
        
        # TOA (L) = ML * Qcal + AL
        # ML = Band-specific multiplicative rescaling factor from the metadata (RADIANCE_MULT_BAND_x, where x is the band number).
        # Qcal = corresponds to band 10.
        # AL = Band-specific additive rescaling factor from the metadata (RADIANCE_ADD_BAND_x, where x is the band number).
        
        mD <- readMeta(datloc$meta[[selnum[i]]], raw=T)
        nam <- mD$METADATA_FILE_INFO["LANDSAT_PRODUCT_ID",]
        
        ML <- mD$RADIOMETRIC_RESCALING["RADIANCE_MULT_BAND_10",]
        AL <- mD$RADIOMETRIC_RESCALING["RADIANCE_ADD_BAND_10",]
        TOA = (ML * lsat8o[[selnum[i]]]$B10_dn) + AL # this is band 10
        
        
        # TOA to Brightness Temperature conversion
        # BT = (K2 / (ln (K1 / L) + 1)) ??? 273.15
        
        # K1 = Band-specific thermal conversion constant from the metadata (K1_CONSTANT_BAND_x, where x is the thermal band number).
        # K2 = Band-specific thermal conversion constant from the metadata (K2_CONSTANT_BAND_x, where x is the thermal band number).
        # L = TOA
        # Therefore, to obtain the results in Celsius, the radiant temperature is adjusted 
        # by adding the absolute zero (approx. -273.15?C).
        
        K1 <- mD$TIRS_THERMAL_CONSTANTS["K1_CONSTANT_BAND_10",]
        K2 <- mD$TIRS_THERMAL_CONSTANTS["K2_CONSTANT_BAND_10",]
        BTK <- (K2 /(log((K1 / TOA) +1)))
        BTC <- (BTK-273.15)
        
        BTC[BTC<=(-90)] <- NA
        
        writeRaster(BTC, paste0(L8scenepath, "bt/", nam, "_BTC", ".tif"), 
                    format="GTiff", overwrite=T)
        BTC
      })
      
      print("BTC calculated")
      
      ################## CALCULATE LST ####################################################################
      # # get BTC files
      # f <- list.files(paste0(L8scenepath, "bt/"), full.names = T, pattern=".tif$")
      # BTC <- lapply(seq(f), function(i){
      #   raster(f[i])
      # })
      # 
      
      # get quality band
      
      
      # get rock outcrop raster with Emissivity values
      eta <- raster(paste0(main, "Rock_outcrop_ras_", areaname, ".tif"))
      
      # Calculate the Land Surface Temperature
      LST <- lapply(seq(BTC), function(i){
        x <- (BTC[[i]]/(1+(0.0010895*BTC[[i]]/0.01438)*log(eta))) 
        # write LST raster
        # writeRaster(x,paste0(L8scenepath, "bt/LST_", i,".tif"), 
        #             format="GTiff", overwrite=T)
      })
      print("LST calculated")
      
      if(length(LST)>1){
            # bring them all to the same extent
            
            # LST <- lapply(seq(list.files(paste0(L8scenepath, "bt/"), pattern="LST")), function(i){
            #   raster(list.files(paste0(L8scenepath, "bt/"), pattern="LST", full.names = T)[i])
            # })
      
            
            # # mask LST by AOI (NECESSARY??? SHOULD BE DONE ALREADY!   )
            lst_aoi <- lapply(seq(LST), function(i){
              x <- mask(LST[[i]], aoianta)
              names(x) <- names(LST[[i]])
              x
            })
            
            #newextent <- extent(compbbmax(lst_aoi))
            #rnew <- lst_aoi[[1]]+lst_aoi[[2]]+lst_aoi[[3]]+lst_aoi[[4]]
            newextent <- compbbmax(lst_aoi)
            p <- as(newextent, 'SpatialPolygons')  
            crs(p) <- crs(lst_aoi[[1]])
            
            # bring them all to new extent
            lst_aoi_ex <- lapply(seq(LST), function(i){
              x <- extend(LST[[i]], p)
              names(x) <- names(LST[[i]])
              x
            })
            
            lst_ex <- stack(lst_aoi_ex)
          
            for(j in seq(nlayers(lst_ex))){
              writeRaster(lst_ex[[j]], paste0(L8scenepath, "ac/", names(BTC)[j], ".tif"), 
                          format="GTiff", overwrite=T)
            }
            
            # MERGE LST
            print("starting to merge LST")
            
            mrg <- character()
            for(i in seq(LST)){
              mrg[i] <- paste0("LST[[", i, "]]")
            }
            
            mrg <- paste(mrg, sep="", collapse=",")
            cm <- paste("raster::mosaic(", mrg, ", tolerance=0.9, fun=mean, overwrite=T, overlap=T, ext=NULL)")
            
            # merging 
            LSTmerge <- eval(parse(text=cm))
            writeRaster(LSTmerge, paste0(L8scenepath, "bt/LST_merged", areaname,".tif"), 
                        format="GTiff", overwrite=T)
            
            
      }
      
      print("LST calculated, LANDSAT routine for this timestep done")
      return(list(LST=LST, l8datetime=l8datetime))
      
    } else {
      txtf <- "no data suitable"
      print(txtf)
      write.csv(txtf, paste0(L8scenepath, "qualitycheck.csv"), row.names = F)
    }
  } else {
    txt <- "no available data for time range"
    print(txt)
    write.csv(txt, paste0(L8scenepath, "qualitycheck.csv"), row.names = F)
    
  }
  }
}

