# 1 DEM



prepDEM <- function(x){
  
  demtilepath <- paste0(dempath, "all/")
  
  if(newarea==1){
    # unzip tiles
    zipf <- list.files(dempath, full.names = T, pattern=".tar.gz")
    for(i in seq(zipf)){
      untar(zipf[i],  compressed = 'gzip', exdir=paste0(dempath, tools::file_path_sans_ext(tools::file_path_sans_ext(basename(zipf[i])))))
    }
    
    # put all dem tiles into one location
    sdirs <- list.files(dempath, pattern="m$", full.names = T)
    dems <- list.files(sdirs, pattern="dem.tif$", full.names = T)
    file.copy(dems, paste0(demtilepath, basename(dems)))
  }
  
  ############  get REMA DEM ###############################
  
  # get all dem tiles
  patterns=c("17_34_8m", "17_35_8m","18_35_8m", "18_34_8m", "19_34_8m", "16_35_8m", "17_36_8m", "19_35_8m")
  dt <-  unlist(lapply(seq(patterns), function(i){
    list.files(demtilepath, full.names=T)[grep(patterns[i],list.files(demtilepath, full.names=T))]
  }))
  
  dem_tiles <- lapply(seq(dt), function(i){
    r <- raster(dt[i])
    r[r<(-100)] <- NA
    r
  })
  
  print("all files in")
  ########## MERGE TILES ############################
  
  # generate command for merging all the tiles
  mrg <- character()
  for(i in seq(dem_tiles)){
    mrg[i] <- paste0("dem_tiles[[", i, "]]")
  }
  
  mrg <- paste(mrg, sep="", collapse=",")
  cm <- paste("raster::mosaic(", mrg, ", tolerance=0.9, fun=mean, overwrite=T, overlap=T, ext=NULL)")
  
  mos <- eval(parse(text=cm))
  
  writeRaster(mos, paste0(dempath, "DEM_8m_", areaname,".tif"), format="GTiff", overwrite=T)
  
  # mos <- raster(paste0(dempath, "DEM_8m_", areaname,".tif"))
  print("mosaic done")
  
  ########## CLEAN UP DEM ############################
  # correct for negative NA values
  mos[mos < (-50)] <- NA
  
  # filter mosaic 3x3 
  mos_c <- focal(mos, w=matrix(1/9,nrow=3,ncol=3))

  writeRaster(mos_c, paste0(dempath, "DEM_8m_", areaname,"_clean.tif"), format="GTiff", overwrite=T)
  
  ########## cut to aoi ############################
  mos_cr <- crop(mos_c, aoianta)  
  mos_caoi <- crop(mos_cr, aoianta)
  
  writeRaster(mos_caoi, paste0(dempath, "DEM_8m_", areaname,"_clean_aoi.tif"))
  
  # # resample to 15m
  # mos_3_30 <- aggregate(mos_c, fact=(15/8))
  # writeRaster(mos_3_30, paste0(dempath, "DEM_8m_", areaname,"_clean_15m.tif"), format="GTiff", overwrite=T)
  # 
  #mos_c <- raster(paste0(dempath, "DEM_8m_", areaname,"_clean_30m.tif"))
  print("DEM cleaned up")
  

  
  ############  get 200m DEM to fill NA values ####################################################################
  
  # dpath<- "E:/Antarctica/DEM/demwgs200_v2.tar/wgs84_200m/wgs84_200m/hdr.adf"
  # x <- new("GDALReadOnlyDataset", dpath)
  # getDriver(x)
  # getDriverLongName(getDriver(x))
  # xx<-asSGDF_GROD(x)
  # dem200 <- raster(xx)
  # writeRaster(dem200, "D:/new_downscaling/tiles_westcoast/DEM_WGS200.tif", format="GTiff")
  
  dem200 <- raster("D:/Antarctica/DEM/DEM_WGS200.tif")
  dem200aoi <- crop(dem200, aoianta)
  
  ############  fill NA values  ####################################################################
  
  # get them to the same resolution
  dem200hr <- resample(dem200aoi, mos_caoi)
  
  # introduce dem200 in mos_c for all locations that are na in mos_c
  mos_filled <- overlay(dem200hr, mos_caoi, fun = function(x, y) {
    y[is.na(y[])] <- x
    return(x)
  })
  writeRaster(mos_filled, paste0(dempath, "DEM_8m_", areaname,"_clean_aoi_filled.tif"), format="GTiff")
  
  mos_filled_aoi <- mask(mos_filled, aoianta)
  writeRaster(mos_filled_aoi, paste0(dempath, "DEM_8m_", areaname,"_clean_aoi_filled_mask.tif"), format="GTiff")
  
  print("fill NA values with 200m DEM")
  
  ############################## filter DEM ########################################
  
  # GO ON HERE !!!!
  mos_filled <- raster(paste0(dempath, "DEM_8m_", areaname,"_clean_aoi_filled_mask.tif"))
  dem_30m <- resample(mos_filled, template)
  d3_f <- focal(dem_30m, w=matrix(1/25,nrow=5,ncol=5))
  
  ############################### calculate slope and aspect ##############################################################
  
  slas <- terrain(d3_f, opt=c("slope", "aspect"), unit="degrees", neighbors=8)
  
  for(i in seq(2)){
    writeRaster(slas[[i]], paste0(dempath, "30m_", names(slas[[i]]), areaname,".tif"), format="GTiff", overwrite=T)
  }
  
  slasrad <- terrain(d3_f, opt=c("slope", "aspect"), unit="radians", neighbors=8)
  for(i in seq(2)){
    writeRaster(slasrad[[i]], paste0(dempath, "30m_radians_", names(slas[[i]]), areaname,".tif"), format="GTiff", overwrite=T)
  }
  print("slope and aspect done")

  
  ############################### tranlate filled DEM and slope to SAGA grid ##############################################################
  
  # fdemp <- paste0(dempath, "DEM_8m_", areaname,"_clean_filled_15.tif")
  # plot(raster(fdemp))
  # gdalUtils::gdalwarp(fdemp, paste0(saga_outpath, areaname,"_DEM_15m_filled.sdat"), 
  #                     overwrite=TRUE,  of='SAGA')
  # 
  # fslopep <- paste0(dempath, "slope", areaname,".tif")
  # gdalUtils::gdalwarp(fslopep, paste0(saga_outpath, areaname,"slope.sdat"), 
  #                     overwrite=TRUE,  of='SAGA')
  # 
  # print("tifs translated to SAGA")
  
  
  ################ MAKE A BLOCKMASK #######################################
  r <- raster(paste0(dempath, "DEM_8m_", areaname,"_clean_filled_15.tif"))
  rt <- r
  res(rt) <- c(20000, 20000)
  rt[] <- seq(1:ncell(rt))
  
  blockmask <- resample(rt, r, method="ngb")
  writeRaster(blockmask, paste0(dempath, "blockmask.tif"), format="GTiff", overwrite=T)
  
  print("Blockmask created")
  
}
