

####### LANDSAT 8 ##############################################################################################

getprocessLANDSAT <- function(time_range){
  
  ####### GET DATA ONLINE ##############################################################################################
  
  print("STARTING LANDSAT DOWNLOAD AND PREP")
  
  ## Login to USGS ERS
  login_USGS("MaiteLezama")
  
  ## set aoi and time range for the query
  aoiutm <- spTransform(aoi, crs("+proj=utm +zone=57 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs "))
  set_aoi(aoiutm)
  
  ## set archive directory
  set_archive(paste0(L8datpath, time_range[[j]][[1]], "/"))
  
  ## get available products and select one
  product_names <- getLandsat_names(username="MaiteLezama", password = "Eos300dmmmmlv")
  product <- "LANDSAT_8_C1"
  
  ## query for records for your AOI, time range and product
  query <- getLandsat_query(time_range = time_range[[j]], name = product,
                            aoi=get_aoi())
  
  query[5,]$levels_available
  
  ## preview a record
  #getLandsat_preview(query[3,])
  
  ## download records
  files <- getLandsat_data(query, level="l1", espa_order=NULL)
  
  
  #### PREP DOWNLOADED L8 IMAGES  #######################################################
  
  L8dirs <- paste0(L8datpath, time_range[[j]][[1]], "/get_data/LANDSAT/L1/")
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
  
  cc <- lapply(seq(metaData), function(i){
      a <- metaData[[i]]$IMAGE_ATTRIBUTES[rownames(metaData[[i]]$IMAGE_ATTRIBUTES) == "CLOUD_COVER_LAND",]
      b <- metaData[[i]]$IMAGE_ATTRIBUTES[rownames(metaData[[i]]$IMAGE_ATTRIBUTES) == "CLOUD_COVER",]
      return(c(a,b))
  })
  
  print(cc)
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
  
  df <- data.frame(matrix(unlist(pathrow), ncol = 4, byrow=T))
  names(df) <- names(pathrow[[1]])
  df$scenenumber <- as.numeric(rownames(df))
  df
  
  
  nums <- seq(1:nrow(df))
  
  # ordered by time: stacks
  s <- lapply(seq(nums), function(i){
    stackMeta(datloc$meta[[nums[i]]], quantity = 'all')
  })
  
  
  
  ############# CHECK WHICH FILES ARE ACTUALLY RELEVANT ######################################################
  
  # do all tiles overlap with land? 
  
  t <- lapply(seq(s), function(i){
    intersect(land, s[[i]])
  })
  
  # subselect scenes, whose amount of pixels is bigger than threshold in 
  # calculate area sums
  tarea <- lapply(seq(t), function(i){
    if(!is.null(t[[i]])){
      sum(area(t[[i]]))
    }
  })
  
  bigarea <- lapply(seq(t), function(i){
    tarea[[i]]>=10000000000
  })
  
  selnum <- which(bigarea==T)
  
  sel <- s[unlist(bigarea)]
  
  # # are those also in aoi
  # aoiint <- lapply(seq(sel), function(i){
  #   intersect(aoi, sel[[i]])
  # })
  # 
  # aoiarea <- lapply(seq(aoiint), function(i){
  #   if(!is.null(aoiint[[i]])){
  #     sum(area(aoiint[[i]]))
  #   }
  # })
  # 
  # aoibigarea <- lapply(seq(aoiarea), function(i){
  #   aoiarea[[i]]>=10000000000
  # })
  # 
  # sel_f <- sel[unlist(aoibigarea)]
  # 
  # ss1 <- which(unlist(bigarea)==T)
  # ss_f <- ss1[which(aoibigarea==T)]
  
  # write date and time for later use with MODIS 
  l8datetime <- df[selnum,]
  write.csv(l8datetime, paste0(L8datpath, "L8_date_", areaname, time_range[[j]][[1]], ".csv"))
  
  print("relevant files selected and date and time written out")
  ############# ELIMINATE 0 VALUES ########################################################################## 
  s <- sel
  for(i in seq(s)){
    for(j in seq(nlayers(s[[1]]))){
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
  
  
  dir.create(paste0(L8datpath, "ac/"))
  lsat8_sdos <- lapply(seq(s.aoi), function(i){
    lsat8 <- s.aoi[[i]]
    names(lsat8) <- names(lsat8o[[selnum[i]]])
    
    #estimate digital number pixel value of dark objects in visible wavelength
    hazeDN    <- estimateHaze(lsat8, hazeBands = c("B3_dn", "B4_dn"),
                              darkProp = 0.01)
    
    # radiometric calibration and correction of Landsat data 
    lsat8_sdos <- radCor(lsat8, 
                         metaData = readMeta(datloc$meta[[which(bigarea==T)[[i]]]], raw=F),
                         hazeValues = hazeDN,
                         hazeBands = c("B3_dn", "B4_dn"),
                         method = "sdos")
    
    mD <- readMeta(datloc$meta[[i]], raw=T)
    nam <- mD$METADATA_FILE_INFO["LANDSAT_PRODUCT_ID",]
    
    for(j in seq(nlayers(lsat8_sdos))){
      writeRaster(lsat8_sdos[[j]], paste0(L8datpath, "ac/", nam, "_", names(lsat8_sdos)[j], ".tif"), 
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
  dir.create(paste0(L8datpath, "bt/"))
  
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
    
    writeRaster(BTC, paste0(L8datpath, "bt/", nam, "_BTC", ".tif"), 
                format="GTiff", overwrite=T)
    BTC
  })
  
  print("BTC calculated")
  
  ################## CALCULATE LST ####################################################################
  # # get BTC files
  # f <- list.files(paste0(L8datpath, "bt/"), full.names = T, pattern=".tif$")
  # BTC <- lapply(seq(f), function(i){
  #   raster(f[i])
  # })
  # 
  
  # generate command for merging all the tiles
  mrg <- character()
  for(i in seq(BTC)){
    mrg[i] <- paste0("BTC[[", i, "]]")
  }
  
  mrg <- paste(mrg, sep="", collapse=",")
  cm <- paste("raster::mosaic(", mrg, ", tolerance=0.9, fun=mean, overwrite=T, overlap=T, ext=NULL)")
  
  # merging 
  btcmerge <- eval(parse(text=cm))
  writeRaster(btcmerge, paste0(L8datpath, "bt/BTC_merged", areaname,".tif"), format="GTiff")
  
  # get rock outcrop raster with Emissivity values
  eta <- raster(paste0(main, "Rock_outcrop_ras.tif"))
  
  # Calculate the Land Surface Temperature
  LST <- lapply(seq(BTC), function(i){
    x <- (BTC[[i]]/(1+(0.0010895*BTC[[i]]/0.01438)*log(eta))) 
    # write LST raster
    writeRaster(x,paste0(L8datpath, "bt/LST_", i,".tif"), format="GTiff")
  })
  
  print("LST calculated, LANDSAT routine for this timestep done")
}

