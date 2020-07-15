
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
  
  login_earthdata(username="Mlezama", )
  
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
  
  ## query records for AOI, time range and product
  nodat <- list(0)
  
  day <- seq(length(time_range[[y]][[m]]))  
 
  
  query <- lapply(seq(day), function(d){
    print(d)
    try(getLandsat_records(time_range = time_range[[y]][[m]][[d]], name = product,
                         aoi=get_aoi()), silent=T)
  })
  
  te <- sapply(seq(query), function(x){
    class(query[[x]])
  })
  
  if(any(te!="try-error") & !is.null(unlist(query))){
    ######### check, whether there are MODIS files 2h around this time #########
    
    ######### subselect query directly for high quality images with little cloud cover #########
    
    #find out which overlaps mostly with aoi, so that this tile can be checked thoroughly
    wgsproj <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
    footprints <- lapply(seq(query), function(x){
        # a <- query[[x]]$footprint[[i]] # get footprint from query
        # a1 <- strsplit(substring(a, 11,(nchar(a)-2)), split = ",")
        # ap <- strsplit(a1[[1]], split=" +")
        # 
        # abc <- unlist(lapply(seq(ap), function(i){
        #   ap[[i]][ap[[i]] != ""] 
        # }))
        # df <- data.frame(matrix(as.numeric(abc), ncol=2, byrow=T))
        # xy <- df[,c(1:2)]
        # fp <- SpatialPointsDataFrame(coords = xy, data = df, #make spatial points data frame from footprint
        #     proj4string = wgsproj)
        # fpp <- Polygon(fp)
        # Ps1 = SpatialPolygons(list(Polygons(list(fpp), ID = "a")), 
        #                       proj4string=wgsproj)
        query[[x]]$footprint[which(query[[x]]$level=="bt")]
      })
    
    

    # are those also in aoi?
    # find out intersection between aoi and footprints
    aoiwgs <- spTransform(aoi, wgsproj)
    aoiwgssf <- st_as_sf(aoiwgs)
    
    aoiint <- lapply(seq(footprints), function(i){
      #lapply(seq(footprints[[i]]), function(j){
        lapply(seq(footprints[[i]]), function(k){
              #intersect(aoiwgs, footprints[[i]][[j]])
              st_intersection(footprints[[i]][k], aoiwgssf)
        })
      #})
    })
    
    # area of overlap
    aoiarea <- lapply(seq(footprints), function(i){
      #lapply(seq(footprints[[i]]), function(j){
        lapply(seq(footprints[[i]]), function(k){
          
        if(!is.null(aoiint[[i]][k])){
          #sum(area(aoiint[[i]][[j]]))
          st_area(aoiint[[i]][k][[1]])
        } else {
          0
        }
        })
      #})
    })
    
    areaaoi <- st_area(aoiwgssf)

    # which proportion of aoi is covered by tile? 
    aoiprop <- lapply(seq(footprints), function(i){
      #lapply(seq(footprints[[i]]), function(j){
        lapply(seq(footprints[[i]]), function(k){
        
          print(c(i,k))
          
        if(length(as.numeric(aoiarea[[i]][[k]]))!=0){
          as.numeric(aoiarea[[i]][[k]]/areaaoi)
        } else {
          0
        }
      #})
      })
    })
    
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
      if(class(query[[x]])=="NULL"){ # if no data was found at all for this day
        nodat <- 1
        print("no data for this time frame available")
        txt <- "nodat"
        write.csv(txt, paste0(L8scenepath, "nodat_day_", day[[x]], ".csv"))
      } 
      if(any(grepl("T1",query[[x]]$preview_url)==F)){ # if any of the files is not best quality
        #https://www.usgs.gov/centers/eros/science/usgs-eros-archive-landsat-archives-landsat-8-oli-operational-land-imager-and?qt-science_center_objects=0#qt-science_center_objects
        lowqual <- 1
        print("no good quality products available")
        txt <- "lowqual"
        write.csv(txt, paste0(L8scenepath, "lowqual_day_", day[[x]], ".csv"))
      } 
      if(!length(sel_aoi[[x]])==0){
        landclouds_aoi <- mean(query[[x]]$cloudcov_land[sel_aoi[[x]]])
        mrg <- character()
        for(i in seq(length(sel_aoi[[x]]))){
          mrg[i] <- paste0("sel_aoi[[x]][", i, "]")
        }
        mrg <- paste(mrg, sep="", collapse=",")
        cm <- paste("as.numeric(paste0(", mrg, "))")
        selaoi <- eval(parse(text=cm))

        if(landclouds_aoi>20){
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
    
    # taking only days, where there is any data and the mean cloud cover is below 20% 

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
                bigarea_and_bt <- which(seq(which(query[[i]]$level=="bt")) == (bigarea[[i]][[j]]))
                q <- query[[i]][query[[i]]$level=="bt",][bigarea_and_bt,] # nimm die Kacheln mit ausreichend Überlappung mit AOI
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
                paste(selquery[[i]][[j]][[2]], ",", selquery[[i]][[j]][[1]]$start_time)
              }
            })
          })
    
      
      # get amount of land cloud cover for the selected tiles
      cc <- lapply(seq(selquery), function(i){ # für alle Tage
        if(!selquery[[i]][[j]][1]=="no good scene here"){
        lapply(seq(length(selquery[[i]])), function(j){
          if(!selquery[[i]][[j]][1]=="no good scene here"){
          selquery[[i]][[j]][[1]]$cloudcov_land}
        })
        }
      })
      
      # construct the new query out of tiles, which have only 20% cc
      querynew <- lapply(seq(selquery), function(i){ # für alle Tage
        print(i)
        if(length(cc[[i]])>0){
        lapply(seq(length(selquery[[i]])), function(j){
          if(!selquery[[i]][[j]][1]=="no good scene here" && cc[[i]][[j]]<20){
            selquery[[i]][[j]][[1]]
            }
          })
        }

      })
    
      
      # write info file on downloaded tiles
      downloadsumdf <- paste(unlist(sum_selquery), ",", lcc = unlist(cc))
      downloadsumdf <- data.frame(matrix(data=unlist(strsplit(downloadsumdf, ",")), ncol=6, byrow=T))
      names(downloadsumdf) <- c("fnam", "date", "path", "row", "datetime", "lcc")
      downloadsumdf$lcc <-  as.numeric(levels(downloadsumdf$lcc))[downloadsumdf$lcc]

      seldf <- downloadsumdf[downloadsumdf$lcc < 20,]
      write.csv(seldf, paste0(L8scenepath, "downloaded_days.csv"), row.names = F)
      
      if(nrow(seldf)!=0){
        # do everything except for the ifs above

      
      ##### select only those L8 tiles, where MODIS matches
    
      # get L8 times from selected in querynew
      L8time <- lapply(seq(querynew), function(i){
        lapply(seq(querynew[[i]]), function(j){
          if(length(querynew[[i]][[j]]$start_time)!=0){
            x <- querynew[[i]][[j]]$start_time
            x <- as.POSIXlt(x, format="%Y:%j:%H:%M:%S", tz="UTC")
          } else {x=NULL}
          x
        })
      })
      

      # +/- 20 min around L8datetime
      
      l8days <- sapply(seq(L8time), function(i){
        lapply(seq(querynew[[i]]), function(j){
        if(length(L8time[[i]][[j]])!=0){
          #maxtimerange <- L8time[[i]][[j]]+hours(1)
          #mintimerange <- L8time[[i]][[j]]-hours(1)
          maxtimerange <- L8time[[i]][[j]]+minutes(20)
          mintimerange <- L8time[[i]][[j]]-minutes(20)
          daynum <- unique(c(day(maxtimerange), day(mintimerange)))
        }
      })
      })
      
      l8days <- unique(unlist(l8days))

      # take all days that are in the range of 2h around L8date
      print("CHECK MODIS availability 2h around L8 dates")
      
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
    
      modquery <- lapply(seq(length(L8dayc)), function(z){ # for all days
        query1 <- getMODIS_records(time_range = time_range[[y]][[m]][[L8dayc[z]]], name = product1,
                                 username="MaiteLezama", password = "Eos300dmmmmlv",
                                 aoi=get_aoi())
        query2 <- getMODIS_records(time_range = time_range[[y]][[m]][[L8dayc[z]]], name = product2,
                                 username="MaiteLezama", password = "Eos300dmmmmlv",
                                 aoi=get_aoi())
        return(list(query1, query2))
      })
      
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

      # compare time of MODIS scene with L8 capturing time
      timediff <- lapply(seq(length(L8time)), function(z){
        if(length(L8time[[z]])>0){
        lapply(seq(length(L8time[[z]])), function(zi){
          lapply(seq(length(datetimeMODISquery)), function(yz){
            lapply(seq(length(datetimeMODISquery[[yz]])), function(yz1){
              lapply(seq(length(datetimeMODISquery[[yz]][[yz1]])), function(yz2){
                
                if(length(datetimeMODISquery[[yz]][[yz1]]) > 1){
                  
                  if(length(L8time[[z]][[zi]])!=0 & length(datetimeMODISquery[[yz]][[yz1]][[yz2]])!=0){
                      print(c(z,zi,yz,yz1,yz2))
                      posinfo <- c(z, zi, yz, yz1, yz2)
                      x <- as.numeric(abs(difftime(L8time[[z]][[zi]], datetimeMODISquery[[yz]][[yz1]][[yz2]], units="hours")))
                      
                      m <- minute(L8time[[z]][[zi]])
                      if(nchar(m)==1){
                        m <- paste0(0, m)
                      } 
                      
                      l8t <- as.numeric(paste0(hour(L8time[[z]][[zi]]), m))
                      c(x, posinfo, l8t)
                  }
                }
          })
          })
        })
        })
        }
      })
    
      # make df 
      # < half an hour <0.5h
      timediff_df <- data.frame(matrix(unlist(timediff), ncol = 7, byrow=T))
      names(timediff_df) <- c("timediff", "L_days", "L_scene", "M_L_days", "MODMYD", "M_scene", "L8hour")
      timediff_df <- timediff_df[timediff_df$timediff<0.6,]

      # get names of scenes
      for(i in seq(nrow(timediff_df))){
          # MODIS
          modsum <- modquery[[timediff_df$M_L_days[i]]] [[timediff_df$MODMYD[i] ]][timediff_df$M_scene[i],]$summary
          print(modsum)
          timediff_df$MODname[i] <- strsplit(strsplit(modsum, ",")[[1]][[1]], ":")[[1]][[2]]
          
          # L8
          l8sum <- selquery[[timediff_df$L_days[i]]][[timediff_df$L_scene[i]]][[2]]
          print(l8sum)
          if(!is.null(l8sum)){
            l8sum1 <- strsplit(strsplit(l8sum, ",")[[1]][[1]], ":")[[1]][[2]]
            timediff_df$L8name[i] <- substring(l8sum1, 1, nchar(l8sum1)-15)
          }

      }
      
      timediff_df$L8name <- substring(timediff_df$L8name, 2, nchar(timediff_df$L8name)) # eliminate leading space

      write.csv2(timediff_df, paste0(L8scenepath, "timediff_df.csv"))
      #timediff_df <- read.csv2(paste0(L8scenepath, "timediff_df.csv"))
      
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
      
      # any combination <2h?
      if(length(msel)!=0){
        
      ## SELECT ONLY THOSE L8 scenes that are being matched by MODIS
      l8matcheddf <- unique(timediff_df[,c("L_days","L_scene")])
      
      l8matchedcheck <- timediff_df$L8name[ rownames(timediff_df)  %in% rownames(l8matcheddf) ]
      
      l8matcheddf$l8nam <- l8matchedcheck
      
      uniqueL8days <- unique(l8matcheddf$L_days)
      L8querymatched <- lapply(seq(uniqueL8days), function(i){ # for all days that were selected
        #lapply(seq(nrow(l8matcheddf[l8matcheddf$L_days==unique(l8matcheddf$L_days)[i],])), function(j){ # for all scenes that are there per day
          snums <- l8matcheddf$L_scene[l8matcheddf$L_days==uniqueL8days[i]] # for all scenes there are per day
          if(length(snums) == 1){
            x <- querynew[[uniqueL8days[i]]][[snums]]
            print(x$summary)
          } else {
            x <- querynew[[uniqueL8days[i]]][snums]
            for(i in seq(x)){
              print(x[[i]]$summary)
            }
          }
          x
        #})
      })
      
      #do.call(c, unlist(L8querymatched, recursive=T))
      saveRDS(L8querymatched, paste0(L8scenepath, "L8querymatched.rds"))
      
      L8querymatched <- readRDS(paste0(L8scenepath, "L8querymatched.rds"))
      ## preview records
      #source(paste0(scriptpath, "fun_EE_preview.R"))
      
      dir.create(paste0(L8scenepath,"previews/"))
      
      # for(i in seq(L8querymatched)){
      #     if(length(L8querymatched[[i]]) > 15){
      #       for(j in seq(nrow(L8querymatched[[i]]))){
      #         map <- getLandsat_preview(L8querymatched[[i]][j,])
      #         mapshot(map, file=paste0(L8scenepath, "previews/query",i,"tile",rownames(L8querymatched[[i]])[j],".png"))
      #         print(c(i,j))
      #       } 
      #       } else {
      #       for(x in seq(L8querymatched[[i]])){
      #         for(k in seq(nrow(L8querymatched[[i]][[x]]))){
      #         map <- getLandsat_preview(L8querymatched[[i]][[x]][k,])
      #         mapshot(map, file=paste0(L8scenepath, "previews/query",i,"tile",rownames(L8querymatched[[i]][[x]])[k],".png"))
      #         print(c(i,x,k))
      #       }
      #     }
      #   }
      #   }
      
      # reset archive directory and product names to get LANDSAT 
      ## set archive directory
      set_archive(L8scenepath)
      
      ## get available products and select one
      #product_names <- getLandsat_names(username="MaiteLezama", password = "Eos300dmmmmlv")
      #product <- "LANDSAT_8_C1"
        
      
      # get L8 files from USGS (AWS)
      filesbt <- lapply(seq(L8querymatched), function(i){
         if(length(L8querymatched[[i]]) > 15){
            getLandsat_data(records=L8querymatched[[i]])
          print(i)
        } else {
          for(x in seq(L8querymatched[[i]])){
        #     if(grepl("bt", L8querymatched[[i]][[x]]$levels_available)){
              get_data(records=L8querymatched[[i]][[x]], level="bt", espa_order=NULL,source="auto")
              print(i,x)
            # }
           }
         }
      })
      
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
      
      # get Metadata
      f <- list.files(paste0(list.files(sc[1], pattern="Bt$", full.names = T), "/"), full.names=T)

      # # get L8 files from USGS (AWS)
      # files <- lapply(seq(L8querymatched), function(i){
      #   #lapply(seq(querynew[[i]]), function(j){
      #   if(!is.null(L8querymatched[[i]])){
      #     try(getLandsat_data(records=L8querymatched[[i]], level="l1", espa_order=NULL,source="auto"), silent=T)
      #   } 
      #   #})
      # })
      # 
      
      # Tier 2 (T2) Tier 2 scenes adhere to the same radiometric standard as Tier 1 scenes, but do not meet the Tier 1 
      # geometry specification due to less accurate orbital information (specific to older Landsat sensors), significant 
      # cloud cover, insufficient ground control, or other factors.

      
      # # get file type of files to check if there is anything in it
      # fte <- sapply(seq(L8querymatched), function(x){
      #   #lapply(seq(L8querymatched[[x]]), function(j){
      #   class(files[[x]])
      # #})
      # })
      
      # # if only T2 available, use espa order & make a mark in file
      # if(any(unlist(fte)=="try-error")){
      #   
      #   results <- lapply(seq(L8querymatched), function(i){
      #     #lapply(seq(querynew[[i]]), function(j){
      #     if(!is.null(L8querymatched[[i]])){
      #       try(getLandsat_data(records=L8querymatched[[i]],  level="bt",source="auto"), silent=T)
      #     } 
      #     #})
      #   })
      # }
      
      # TO DO: IF THERE IS ANY SCENE TO PROCESS 
      #if(any(unlist(fte)!="try-error" & unlist(fte)!="NULL")){ # if there is any scene to process
      
      #### PREP DOWNLOADED L8 IMAGES  #######################################################
      
      # if Bt was downloaded or L1 images
      if(L8downloadtype != "Bt"){
          L8dirs <- paste0(L8datpath, substring(time_range[[y]][[m]][[1]][[1]], 1, 7), "/get_data/LANDSAT/L1/")
          sdirs <- list.files(L8dirs, full.names = T)
          datloc <- l8datlist(sdirs)
          # summarize all available scenes
          
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
          ###### CHECK WHICH SCENES GO TOGETHER #########################################################################
          
          #  Landsat data acquisition times are expressed in Greenwich Mean Time (GMT) standard.
          pathrow <- lapply(seq(metaData), function(i){
            path <- metaData[[i]]$PRODUCT_METADATA[rownames(metaData[[i]]$PRODUCT_METADATA) == "WRS_PATH",]
            row <- metaData[[i]]$PRODUCT_METADATA[rownames(metaData[[i]]$PRODUCT_METADATA) == "WRS_ROW",]
            date <- metaData[[i]]$PRODUCT_METADATA[rownames(metaData[[i]]$PRODUCT_METADATA) == "DATE_ACQUIRED",]
            time <- metaData[[i]]$PRODUCT_METADATA[rownames(metaData[[i]]$PRODUCT_METADATA) == "SCENE_CENTER_TIME",]
            fn <- metaData[[i]]$PRODUCT_METADATA[rownames(metaData[[i]]$PRODUCT_METADATA) == "FILE_NAME_BAND_1",]
            fname <- substring(fn, 1, nchar(fn)-22)
            sceneID <- metaData[[i]]$METADATA_FILE_INFO[rownames(metaData[[i]]$METADATA_FILE_INFO) == "LANDSAT_SCENE_ID",]
            return(list(path=path, row=row, date=date, time=time, fname=fname, sceneID=sceneID))
          })
          
          print("data location, metaData and stack done")
          
      } else { # if it's BT images
        
        sdirs <- list.files(sc, full.names = T, pattern="Bt$")
        datloc <- l8datlist(sdirs)
        
        pathrow <- lapply(seq(L8querymatched), function(i){
          if(length(L8querymatched[[i]]) > 15){ # if only one level of list
            lapply(seq(nrow(L8querymatched[[i]])), function(j){
              path <- L8querymatched[[i]][j,]$WRSPath
              row <- L8querymatched[[i]][j,]$WRSRow
              date <- L8querymatched[[i]][j,]$acquisitionDate
              
              t <- L8querymatched[[i]][j,]$StartTime
              time <- substring(t, 10, 14)
              lcc <- L8querymatched[[i]][j,]$LandCloudCover
              
              eid <- strsplit(L8querymatched[[i]][j,]$summary, ",")[[1]][1]
              fname <- substring(eid, 12,nchar(eid))
              
              return(list(path=path, row=row, date=date, time=time, fname=fname, lcc=lcc))
            })
          } else {
              lapply(seq(L8querymatched[[i]]), function(x){
                lapply(seq(nrow(L8querymatched[[i]][[x]])), function(j){
                  
                path <- L8querymatched[[i]][[x]][j,]$WRSPath
                row <- L8querymatched[[i]][[x]][j,]$WRSRow
                date <- L8querymatched[[i]][[x]][j,]$acquisitionDate
                
                t <- L8querymatched[[i]][[x]][j,]$StartTime
                time <- substring(t, 10, 14)
                lcc <- L8querymatched[[i]][[x]][j,]$LandCloudCover
                
                eid <- strsplit(L8querymatched[[i]][[x]][j,]$summary, ",")[[1]][1]
                fname <- substring(eid, 12,nchar(eid))
                
                return(list(path=path, row=row, date=date, time=time, fname=fname, lcc=lcc))
              })
            })
          }
        })
      }
      
      ##### LOAD ALL DOWNLOADED L8 SCENES  #####################################################################

      
      # make df with Path, Row, Date and Time (GMT)
      df <- data.frame(matrix(unlist(pathrow), ncol = 6, byrow=T))
      names(df) <- c("path", "row", "date", "time", "fname", "lcc")
      df$scenenumber <- as.numeric(rownames(df))
      nums <- seq(1:nrow(df))
      
      # # ordered by time: stacks
      # s <- lapply(seq(nums), function(i){
      #   stackMeta(datloc$meta[[nums[i]]], quantity = 'all')
      # })
      

      
      cloud_shadow <- c(328, 392, 840, 904, 1350)
      cld <- c(352, 368, 416, 432, 480, 864, 880, 928, 944, 992)
      mc_cloud <- c(386, 388, 392, 400, 416, 432, 898, 900, 904, 928, 944)
      hc_cloud <- c(480, 992)
      hc_cirrus <- c(834, 836, 840, 848, 864, 880, 898, 900, 904, 912, 928, 944, 992)
      lc_cirrus <- c(322, 324, 328, 336, 352, 368, 386, 388, 392, 400, 416, 432, 480)
      lc_cloud <- c(322, 324, 328, 336, 352, 368, 834, 836, 840, 848, 864, 880)
      
      cloud <- c(cloud_shadow,cld,mc_cloud,hc_cloud,hc_cirrus)
      
      
      
      s <- lapply(seq(datloc$tifs), function(i){
        
        
        # cloud mask
        x <- raster(datloc$bqa[[i]])
        c <- x
        cs <- is.element(values(c),cloud)
        c[] <- cs
        
        
        # clean out clouds
        sat <- raster(datloc$tifs[[i]])
        
        s <- stack(sat, c)
        # clean out clouds
        s[[1]][s[[2]]==1] <- NA
        s
        
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
      l8datetime$fname <- as.character(l8datetime$fname)
      write.csv(l8datetime, paste0(L8datpath, "L8_date_", areaname, time_range[[y]][[m]][[1]][1], ".csv"))
      
      print("relevant files selected and date and time written out")
      
      # write Landsat time into timediff_df
      timediff_df$L8time <- NA
      timediff_df$L8date <- NA
      df$fname <- as.character(df$fname)
      for(i in seq(nrow(timediff_df))){
        ind <- which(grepl(timediff_df$L8name[i],df$fname))
        print(c(timediff_df$L8name[i], df$fname[ind]))
        if(length(ind)>0){
          timediff_df$L8time[i] <- as.character(df[ind,"time"])
          timediff_df$L8date[i] <- as.character(df[ind,"date"])
        }
      }
      
      write.csv2(timediff_df, paste0(L8scenepath, "timediff_df.csv"))
      timediff_df <- read.csv2(paste0(L8scenepath, "timediff_df.csv"))
      # actualize MODIS query
      timediff_comp <- timediff_df[complete.cases(timediff_df),]
      
      # if matches were found
      if(nrow(timediff_comp)!=0){
        # eliminate leading space
        timediff_comp$MODname <- substring(timediff_comp$MODname,2,nchar(as.character(timediff_comp$MODname)))
        
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
        timediff_msel <- timediff_comp[timediff_comp$Msel==1,]
        mselnew <- lapply(seq(nrow(timediff_msel)), function(i){
          modquery[[timediff_msel$M_L_days[i]]][[timediff_msel$MODMYD[i]]][timediff_msel$M_scene[i],]
        })      
        mselsummary <- sapply(seq(mselnew), function(i){
          mselnew[[i]]$summary
        })
        
        mselsummary <- substring(mselsummary, 12,55)
        
        a <- n_occur$mselsummary[n_occur$Freq==2]
        for(i in seq(a)){
          timediff_comp$sel[min(which(timediff_comp$MODname == a[i]))] <- 1
        }
        
        
        write.csv2(timediff_msel, paste0(L8scenepath, "timediff_msel.csv"))
        write.csv2(timediff_comp, paste0(L8scenepath, "timediff_comp.csv"))
        
        msel <- mselnew
        saveRDS(msel, paste0(L8scenepath, "MODquerymatched_msel.rds"))
        
        # ############# ELIMINATE 0 VALUES ########################################################################## 
        # s <- sel # use subselected after cloud cover, aoi overlap and land overlap
        # for(i in seq(s)){
        #   for(j in seq(nlayers(s[[i]]))){
        #     s[[i]][[j]][s[[i]][[j]]==0] <- NA
        #   }
        # }
        # print("0 values replaced by NA")
        
        ################## CUT TO AOI ##################################################################################
        
        # cut out to research area
        s.aoi <- lapply(seq(s), function(i){
          crop(s[[i]], extent(aoianta))
        })
        
        print("cut to research area")
        
        
        btc <- lapply(seq(s), function(i){
          (s.aoi[[i]]*0.1)-273.15
        })
        
        
        #############  Calculation of TOA (Top of Atmospheric) spectral radiance and #################### 
        #################  brightness temperature ##################################################
        
        # dir.create(paste0(L8scenepath, "bt/"))
        # 
        # BTC <- lapply(seq(selnum), function(i){
        #   
        #   # TOA (L) = ML * Qcal + AL
        #   # ML = Band-specific multiplicative rescaling factor from the metadata (RADIANCE_MULT_BAND_x, where x is the band number).
        #   # Qcal = corresponds to band 10.
        #   # AL = Band-specific additive rescaling factor from the metadata (RADIANCE_ADD_BAND_x, where x is the band number).
        #   
        #   mD <- readMeta(datloc$meta[[selnum[i]]], raw=T)
        #   nam <- mD$METADATA_FILE_INFO["LANDSAT_PRODUCT_ID",]
        #   
        #   ML <- mD$RADIOMETRIC_RESCALING["RADIANCE_MULT_BAND_10",]
        #   AL <- mD$RADIOMETRIC_RESCALING["RADIANCE_ADD_BAND_10",]
        #   TOA = (ML * lsat8o[[selnum[i]]]$B10_dn) + AL # this is band 10
        #   
        #   
        #   # TOA to Brightness Temperature conversion
        #   # BT = (K2 / (ln (K1 / L) + 1)) ??? 273.15
        #   
        #   # K1 = Band-specific thermal conversion constant from the metadata (K1_CONSTANT_BAND_x, where x is the thermal band number).
        #   # K2 = Band-specific thermal conversion constant from the metadata (K2_CONSTANT_BAND_x, where x is the thermal band number).
        #   # L = TOA
        #   # Therefore, to obtain the results in Celsius, the radiant temperature is adjusted 
        #   # by adding the absolute zero (approx. -273.15?C).
        #   
        #   K1 <- mD$TIRS_THERMAL_CONSTANTS["K1_CONSTANT_BAND_10",]
        #   K2 <- mD$TIRS_THERMAL_CONSTANTS["K2_CONSTANT_BAND_10",]
        #   BTK <- (K2 /(log((K1 / TOA) +1)))
        #   BTC <- (BTK-273.15)
        #   
        #   BTC[BTC<=(-90)] <- NA
        #   
        #   writeRaster(BTC, paste0(L8scenepath, "bt/", nam, "_BTC", ".tif"), 
        #               format="GTiff", overwrite=T)
        #   BTC
        # })
        
        print("BTC calculated")
        
        ################## CALCULATE LST ####################################################################
        # # # get BTC files
        #mr <- list.files(paste0(L8scenepath, "bt/"), pattern="BTC", full.names=T)
        #f <- mr[grep('tif$', mr)]
        #BTC <- lapply(seq(f), function(i){
        #  raster(f[i])
        #})
        
        # get rock outcrop raster with Emissivity values
        eta <- raster(paste0(main, "Rock_outcrop_ras_", areaname, ".tif"))
        
        BTC <- btc
        # Calculate the Land Surface Temperature
        LST <- lapply(seq(BTC), function(i){
          x <- (BTC[[i]]/(1+(0.0010895*BTC[[i]]/0.01438)*log(eta))) 
          # write LST raster
          # writeRaster(x,paste0(L8scenepath, "bt/LST_", i,".tif"), 
          #             format="GTiff", overwrite=T)
        })
        print("LST calculated")
        
        # bring to same extent and write LST 
        dir.create(paste0(L8scenepath, "LST/"))
        
        # bring them all to the same extent
        
        # LST <- lapply(seq(list.files(paste0(L8scenepath, "bt/"), pattern="LST")), function(i){
        #   raster(list.files(paste0(L8scenepath, "bt/"), pattern="LST", full.names = T)[i])
        # })
        
        
        # # mask LST by AOI
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
        lst_aoi_ex <- lapply(seq(lst_aoi), function(i){
          x <- extend(lst_aoi[[i]], p)
          names(x) <- names(LST[[i]])
          x
        })
        
        lst_ex <- stack(lst_aoi_ex)
        
        #dir.create(paste0(L8scenepath, "mos/"))
        for(j in seq(nlayers(lst_ex))){
          writeRaster(lst_ex[[j]], paste0(L8scenepath, "LST/", names(BTC[[j]]), ".tif"), 
                      format="GTiff", overwrite=T)
        }
        
        # # MERGE LST
        # print("starting to merge LST")
        # 
        # mrg <- character()
        # for(i in seq(LST)){
        #   mrg[i] <- paste0("LST[[", i, "]]")
        # }
        # 
        # mrg <- paste(mrg, sep="", collapse=",")
        # cm <- paste("raster::mosaic(", mrg, ", tolerance=0.9, fun=mean, overwrite=T, overlap=T, ext=NULL)")
        # 
        # # merging 
        # LSTmerge <- eval(parse(text=cm))
        # writeRaster(LSTmerge, paste0(L8scenepath, "bt/LST_merged", areaname,".tif"), 
        #             format="GTiff", overwrite=T)
        # 
        
        
        print("LST calculated, LANDSAT routine for this timestep done")
        return(list(msel=msel, LST=LST, l8datetime=l8datetime))
      } else {txt <- "no available data for time range"
      print(txt)
      write.csv(txt, paste0(L8scenepath, "qualitycheck.csv"), row.names = F)
      return("nothing")} 
      
    #} else {
    #   txtf <- "no data suitable"
    #   print(txtf)
    #   write.csv(txtf, paste0(L8scenepath, "qualitycheck.csv"), row.names = F)
    #   return("nothing")
    # }
  } else {print("no time match in MODIS found")
    #file.rename(L8scenepath, paste0(substring(L8scenepath, 1, (nchar(L8scenepath)-nchar(basename(L8scenepath))-1)), 
    #                                paste0("no_tmatch_",basename(L8scenepath))))
    file.remove(paste0(L8scenepath, "qualitycheck.csv"))
    return("nothing")
    }# for if there are MODIS scenes within <2h of L8
    
      } else {txt <- "no available data for time range"
    print(txt)
    write.csv(txt, paste0(L8scenepath, "qualitycheck.csv"), row.names = F)
    return("nothing")} 
      
     } else {txt <- "no available data for time range"
    print(txt)
    write.csv(txt, paste0(L8scenepath, "qualitycheck.csv"), row.names = F)
    return("nothing")}
      
    } else {txt <- "no available data for time range"
    print(txt)
    write.csv(txt, paste0(L8scenepath, "qualitycheck.csv"), row.names = F)
    return("nothing")}
  }
