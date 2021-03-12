
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

selectLANDSAT <- function(time_range){
  
  ymid <- substring(time_range[[y]][[m]][[1]][[1]], 1, 7)
  L8scenepath <- paste0(main, "L8/", substring(time_range[[y]][[m]][[1]][[1]], 1, 7), "/")
  print(L8scenepath)
  
  ####### GET DATA ONLINE ##############################################################################################
  
  print("STARTING LANDSAT SELECTION")
  
  ## set aoi and time range for the query
  l8proj <- crs("+proj=utm +zone=57 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs ")
  aoiutm <- spTransform(aoi, l8proj)
  set_aoi(aoiutm)
  
  ## set archive directory
  set_archive(L8scenepath)
  
  ## get available products and select one
  #product_names <- getLandsat_products(username="MaiteLezama", password = "Eos300dmmmmlv")
  product <- "LANDSAT_8_C1"
  
  ## query records for AOI, time range and product
  nodat <- list(0)
  
  day <- seq(length(time_range[[y]][[m]]))  
 
  ############## check for internet connection


  checkMyInternet()
  
  query <- lapply(seq(day), function(d){
    try(getLandsat_records(time_range = time_range[[y]][[m]][[d]], name = product,aoi=get_aoi()), silent=T)
  })
  
  te <- sapply(seq(query), function(x){
    class(query[[x]])
  })
  
  if(any(te!="try-error") & !is.null(unlist(query))){
    
    # select only bt 
    query <- lapply(seq(query), function(d){
      query[[d]][query[[d]]$level=="bt",]
    })
    
    ######### check, whether there are MODIS files 2h around this time #########
    
    ######### subselect query directly for high quality images with little cloud cover #########
    
    #find out which overlaps mostly with aoi, so that this tile can be checked thoroughly
    wgsproj <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
    footprints <- lapply(seq(day), function(x){
        query[[x]]$footprint[which(query[[x]]$level=="bt")]
      })
    
    # are those also in aoi?
    # find out intersection between aoi and footprints
    aoiwgs <- spTransform(aoi, wgsproj)
    aoiwgssf <- st_as_sf(aoiwgs)
    
    aoiint <- lapply(seq(footprints), function(i){
        lapply(seq(footprints[[i]]), function(k){
              st_intersection(footprints[[i]][k], aoiwgssf)
        })
    })
    
    # area of overlap
    aoiarea <- lapply(seq(footprints), function(i){
        lapply(seq(footprints[[i]]), function(k){
          
        if(!is.null(aoiint[[i]][k])){
          st_area(aoiint[[i]][k][[1]])
        } else {
          0
        }
        })
    })
    
    areaaoi <-st_area(st_as_sf(aoiutm))

    # which proportion of aoi is covered by tile? 
    aoiprop <- lapply(seq(footprints), function(i){
        lapply(seq(footprints[[i]]), function(k){
        
          #print(c(i,k))
          
        if(length(as.numeric(aoiarea[[i]][[k]]))!=0){
          as.numeric(aoiarea[[i]][[k]]/areaaoi)
        } else {
          0
        }
      })
    })
    
    
    # which proportion of aoi is covered by tile? 
    arealimit <- areaaoi/3

    # get only tiles that have at least 30% of their area over aoi
    sel_aoi <- lapply(seq(query), function(x){
      z <- as.numeric(unlist(aoiprop[[x]]))
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
      
      if(all(class(query[[x]])=="NULL")){ # if no data was found at all for this day
        nodat <- 1
        #print("no data for this time frame available")
        #txt <- "nodat"
        #write.csv(txt, paste0(L8scenepath, "nodat_day_", day[[x]], ".csv"))
        
      } else if (length(sel_aoi[[x]])>0) { # IF SOME FILES WERE SELECTED BECAUSE OF OVERLAP
        sclen <- nrow(query[[x]])
        
        #https://www.usgs.gov/centers/eros/science/usgs-eros-archive-landsat-archives-landsat-8-oli-operational-land-imager-and?qt-science_center_objects=0#qt-science_center_objects

        
        queryx <- query[[x]][sel_aoi[[x]],]
        queryx
        scene <- queryx$record_id
        time <- substring(queryx$start_time, 1, 17)
        
        t1pos <- rep(0, nrow(queryx))
        t2pos  <- rep(0, nrow(queryx))
        
        t1pos[which(grepl("T1",queryx$preview_url))] <- 1
        t2pos[which(grepl("T2",queryx$preview_url))] <- 1
        
        #print("no good quality products available")
        
        landclouds_aoi <- query[[x]]$cloudcov_land[sel_aoi[[x]]]
        
        clouds_ok <- rep(0, length(landclouds_aoi))
        clouds_ok[landclouds_aoi<20] <- 1
        
        return(data.frame(scene=scene, time=time, tier1 = t1pos, tier2 = t2pos, 
                          landclouds_aoi=landclouds_aoi, clouds_ok=clouds_ok))
      }
      print(x) 
      # all na means not enough overlap in the scenes with aoi
      return(data.frame(scene=NA, time=NA, tier1 = NA, tier2 = NA, 
                        landclouds_aoi=NA, clouds_ok=NA))
    })
    
    qualitycheckdf<-do.call("rbind", qualitycheck)
    
    qualitycheckdf <- qualitycheckdf[complete.cases(qualitycheckdf),]
    
    qualitycheckdf$date <- as.POSIXct(qualitycheckdf$time, format="%Y:%j:%H:%M:%S", tz="GMT")

    
    write.csv2(qualitycheckdf, paste0(L8scenepath, "qualitycheck.csv"), row.names = F)
    downloadScenes <- qualitycheckdf[qualitycheckdf$clouds_ok==1,]
    
    
    downloadScenes$day <- day(downloadScenes$date)
    downloadScenes$month <- month(downloadScenes$date)
    downloadScenes$hour <- hour(downloadScenes$date)
    downloadScenes$min <- minute(downloadScenes$date)
    
    write.csv2(downloadScenes, paste0(L8scenepath, "downloadScenes.csv"), row.names = F)
    
    querynew <- lapply(seq(query), function(i){
          pos <- which(query[[i]]$record_id %in% downloadScenes$scene)
          if(length(pos)>0){
             query[[i]][pos,]
          }
        })    
      
    
    saveRDS(querynew, paste0(L8scenepath, "querynew.RDS"))
      
    #querynew <- readRDS(paste0(L8scenepath, "querynew.RDS"))
    ############### START WITH MODIS QUERYING ############################
     
      
      if(nrow(downloadScenes)!=0){
        # do everything except for the ifs above

      
      ##### select only those L8 tiles, where MODIS matches
    
      # get L8 times from selected in querynew
        maxtimerange <- downloadScenes$date+minutes(20)
        mintimerange <- downloadScenes$date-minutes(20)
        # +/- 20 min around L8datetime
        
        l8days <- unique(c(day(maxtimerange), day(mintimerange)))
      
      


      # take all days that are in the range of 40min around L8date
      print("CHECK MODIS availability 40min around L8 dates")
      
      modisscenepath <- paste0(modispath, substring(time_range[[y]][[m]][[1]][[1]], 1, 7), "/")
      hdfpath <- paste0(modisscenepath, "hdfs/")
      MODtifHDFoutpath <- paste0(modisscenepath, "translated/")
      MODLSTpath <- paste0(modisscenepath, "LST/")
      
      
      ## set archive directory
      set_archive(modisscenepath)
    
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
      for(i in seq(length(l8days))){
        L8dayc[i] <- which(as.numeric(substring(time_range[[y]][[m]], 12,13))==l8days[i])
      }
      
      checkMyInternet()
      
      modquery <- lapply(seq(length(L8dayc)), function(z){ # for all days
        query1 <- getMODIS_records(time_range = time_range[[y]][[m]][[L8dayc[z]]], name = product1,
                                 username="MaiteLezama", password = "Eos300dmmmmlv",
                                 aoi=get_aoi())
        query2 <- getMODIS_records(time_range = time_range[[y]][[m]][[L8dayc[z]]], name = product2,
                                 username="MaiteLezama", password = "Eos300dmmmmlv",
                                 aoi=get_aoi())
        return(list(query1, query2))
      })
      
      
      saveRDS(modquery, paste0(L8scenepath, "modquery.RDS"))
      
      # extract times from MODIS query
      # get exact time of scene capturing
          
      datetimeMODISquery <-lapply(seq(modquery), function(i){
        lapply(seq(modquery[[i]]), function(j){
          lapply(seq(modquery[[i]][[j]]$record_id), function(x){
            
          y <- strsplit(modquery[[i]][[j]]$record_id[x], "A")[[1]][2]
          y1 <- paste0(substring(y, 1, 4), ":", substring(y, 5, 7), ":", 
                       substring(y, 9, 10), ":", substring(y, 11, 12))
          y2 <- as.POSIXlt(y1, format="%Y:%j:%H:%M", tz="UTC")
          
        })
      })
      })
      
      MODquery_scenenames <-lapply(seq(modquery), function(i){
        lapply(seq(modquery[[i]]), function(j){
 
            modquery[[i]][[j]]$record_id

        })
      })
      
      saveRDS(MODquery_scenenames, paste0(L8scenepath, "MODquery_scenenames.rds"))
      
      #lubridate::with_tz(datetimeMODISquery[[1]][[1]][[1]],tzone="Pacific/Auckland")

      # compare time of MODIS scene with L8 capturing time
      
      # get L8 times from selected in querynew
      L8time <- lapply(seq(querynew), function(i){
        lapply(nrow(querynew[[i]]), function(j){
          if(length(querynew[[i]][j,]$start_time)!=0){
            x <- querynew[[i]][j,]$start_time
            x <- as.POSIXlt(x, format="%Y:%j:%H:%M:%S", tz="UTC")
          } else {x=NULL}
          x
        })
      })
      

 
      
      timediff <- lapply(seq(length(downloadScenes$date)), function(z){
        if(length(downloadScenes$date)>0){
            lapply(seq(length(datetimeMODISquery)), function(yz){
              lapply(seq(length(datetimeMODISquery[[yz]])), function(yz1){
                lapply(seq(length(datetimeMODISquery[[yz]][[yz1]])), function(yz2){
                  
                  if(length(datetimeMODISquery[[yz]][[yz1]]) > 0){
                    if(length(datetimeMODISquery[[yz]][[yz1]][[yz2]])!=0){
                      posinfo <- c(z, yz, yz1, yz2)

                      x <- as.numeric(abs(difftime(downloadScenes$date[z], datetimeMODISquery[[yz]][[yz1]][[yz2]], units="hours")))
                      mins <- minute(downloadScenes$date[z])
                      if(nchar(mins)==1){
                        mins <- paste0(0, mins)
                      } 
                      modscene <- modquery[[yz]][[yz1]][yz2,]$record_id
                      moddate <- modquery[[yz]][[yz1]][yz2,]$start_time
                      l8date <- as.character(downloadScenes$date[z])
                      
                      l8t <- as.numeric(paste0(hour(downloadScenes$date[z]), mins))
                      l8scene <- downloadScenes$scene[z]
                      
                      return(c(x, posinfo, l8t, l8scene, modscene, moddate, l8date))
                      
                    }
                  }
                })
              })
            })
        }
        
      })
      
    
      # make df 
      # < half an hour <0.5h
      timediff_df <- data.frame(matrix(unlist(timediff), ncol = 10, byrow=T))
      names(timediff_df) <- c("timediff","L_scene", "M_L_days", "MODMYD", "M_scene", "L8hour", "L8scene", "modscene", "moddate", "l8date")
      
      for(i in seq(5)){
        timediff_df[,i] <- as.numeric(timediff_df[,i])
      }
      
        

      timediff_df <- timediff_df[timediff_df$timediff<0.6,]
      #timediff_df <- timediff_df[timediff_df$timediff<2.0,]
      timediff_df

      write.csv2(timediff_df, paste0(L8scenepath, "timediff_df.csv"))
      #timediff_df <- read.csv2(paste0(L8scenepath, "timediff_df.csv"))
      
      if(nrow(timediff_df)>0){
        
        
      modquery <- readRDS( paste0(L8scenepath, "modquery.RDS"))
        
      MODmatcheddf <- unique(timediff_df[,c("M_L_days", "MODMYD", "M_scene")])
      uniquedays <- unique(MODmatcheddf$M_L_days)
      
      MODquerymatched <- lapply(seq(length(uniquedays)), function(i){ # for all days that were selected
        lapply(seq(length(unique(MODmatcheddf$MODMYD[MODmatcheddf$M_L_days==uniquedays[i]]))), function(j){ # for all MOD and MYD that are there per day i
          #lapply(seq(unique(MODmatcheddf$M_scene[MODmatcheddf$M_L_days==i & MODmatcheddf$MODMYD==j])), function(k){ # for all scenes that are there per day i, MOD/MYD j
          #print(i,j,k)
          snums <- MODmatcheddf$M_scene[MODmatcheddf$M_L_days==uniquedays[i] & MODmatcheddf$MODMYD==j]
          modquery[[unique(MODmatcheddf$M_L_days)[i]]][[j]][snums,]
          #})
        })
      })
      
        
        msel <- MODquerymatched
        saveRDS(msel, paste0(L8scenepath, "MODquerymatched_msel.rds"))
        #msel <- readRDS(paste0(L8scenepath, "MODquerymatched_msel.rds"))
        
        # any combination <0.6h?
        if(length(msel)!=0){
          
          ## SELECT ONLY THOSE L8 scenes that are being matched by MODIS
          
          uniqueL8days <- unique(day(timediff_df$l8date))
          
          
          # get matched query for landsat
          L8querymatched <- lapply(seq(querynew), function(i){
            if(!is.null(querynew[[i]])){
              lapply(seq(nrow(querynew[[i]])), function(j){
                id <- querynew[[i]][j,]$record_id
                if(id %in% timediff_df$L8scene){
                  querynew[[i]][j,]
                } else {
                  NULL
                }
              })
              
            }
            
          })
          
          #do.call(c, unlist(L8querymatched, recursive=T))
          saveRDS(L8querymatched, paste0(L8scenepath, "L8querymatched.rds"))
          
          
          
          ri <- lapply(seq(L8querymatched), function(i){
            lapply(seq(L8querymatched[[i]]), function(j){
              if(!is.null(L8querymatched[[i]][[j]])){
                L8querymatched[[i]][[j]]$record_id
              }
            })
          })
          
          ri <- unlist(ri)
          
          write.table(ri, paste0(L8datpath, "ri_txt/bt_ri_", ymid, ".txt"), 
                      quote=F,row.names = F, col.names = F)
        
      } else {
        print("timediff greater than 0.6 in all cases")
        write.table("timediff greater than 0.6 in all cases", paste0(L8datpath, "ri_txt/big_tmdff_", ymid, ".txt"), 
                    quote=F,row.names = F, col.names = F)
      }
      
      } else {
        print("all time differences > 0.6h")
      }
      
      } else {
        print("no good Landsat scenes (cloud and overlap)")
      }
    
      } else {
    print("no scene in bt query")
      }
  
  } # end function
      
      
 