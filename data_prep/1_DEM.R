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
  dt <- list.files(demtilepath, full.names=T)[1:3]
  dem_tiles <- lapply(seq(dt), function(i){
    r <- raster(dt[i])
    r[r<(-300)] <- NA
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
  
  # generate command for merging all the tiles
  mos <- eval(parse(text=cm))
  writeRaster(mos, paste0(dempath, "DEM_8m_", areaname,".tif"), format="GTiff")
  
  # mos <- raster(paste0(dempath, "DEM_8m_", areaname,".tif"))
  print("mosaic done")
  
  ########## CLEAN UP DEM ############################
  
  # filter mosaic 3x3 
  mos_c <- focal(mos, w=matrix(1/9,nrow=3,ncol=3))
  mos_c <- mos_3
  
  # correct for negative NA values
  mos_c[mos_c < (-300)] <- NA
  writeRaster(mos_c, paste0(dempath, "DEM_8m_", areaname,"_clean.tif"), format="GTiff", overwrite=T)
  
  # resample to 30m
  mos_3_30 <- aggregate(mos_c, fact=(30/8))
  writeRaster(mos_3_30, paste0(dempath, "DEM_8m_", areaname,"_clean_30m.tif"), format="GTiff", overwrite=T)
  
  #mos_c <- raster(paste0(dempath, "DEM_8m_", areaname,"_clean_30m.tif"))
  print("DEM cleaned up")
  
  ############  get 200m DEM to fill NA values ####################################################################
  
  # dpath<- "E:/Antarctica/DEM/demwgs200_v2.tar/wgs84_200m/wgs84_200m/hdr.adf"
  # x <- new("GDALReadOnlyDataset", dpath)
  # getDriver(x)
  # getDriverLongName(getDriver(x))
  # xx<-asSGDF_GROD(x)
  # dem200 <- raster(xx)
  # writeRaster(dem200, "E:/Antarctica/DEM/DEM_WGS200.tif", format="GTiff")
  
  dem200 <- raster("E:/Antarctica/DEM/DEM_WGS200.tif")
  
  
  ############  CUT TO AOI ####################################################################
  
  mos_caoi <- crop(mos_c, aoianta)
  dem200aoi <- crop(dem200, aoianta)
  
  ############  fill NA values  ####################################################################
  
  # get them to the same resolution
  dem200hr <- resample(dem200aoi, mos_caoi)
  
  # introduce dem200 in mos_c for all locations that are na in mos_c
  mos_filled <- overlay(dem200hr, mos_caoi, fun = function(x, y) {
    y[is.na(y[])] <- x
    return(x)
  })
  writeRaster(mos_filled, paste0(dempath, "DEM_8m_", areaname,"_clean_filled.tif"), format="GTiff")
  
  
  print("fill NA values with 200m DEM")
  
  
  
  ############################### calculate slope and aspect ##############################################################
  
  slas <- terrain(mos_filled, opt=c("slope", "aspect"))
  
  for(i in seq(2)){
    writeRaster(slas[[i]], paste0(dempath, names(slas[[i]]), areaname,".tif"), format="GTiff")
  }
  print("slope and aspect done")
  
  
  ############################### tranlate filled DEM and slope to SAGA grid ##############################################################
  
  fdemp <- paste0(dempath, "DEM_8m_", areaname,"_clean_filled.tif")
  gdalUtils::gdalwarp(fdemp, paste0(saga_outpath, areaname,"_DEM_8m_filled.sdat"), 
                      overwrite=TRUE,  of='SAGA')
  
  fslopep <- paste0(dempath, "slope", areaname,".tif")
  gdalUtils::gdalwarp(fslopep, paste0(saga_outpath, areaname,"slope.sdat"), 
                      overwrite=TRUE,  of='SAGA')
  
  print("tifs translated to SAGA")
  
  
  ################ MAKE A BLOCKMASK #######################################
  r <- raster(paste0(dempath, "DEM_8m_", areaname,"_clean_filled.tif"))
  rt <- r
  res(rt) <- c(20000, 20000)
  rt[] <- seq(1:ncell(rt))
  
  blockmask <- resample(rt, r, method="ngb")
  writeRaster(blockmask, paste0(dempath, "blockmask.tif"), format="GTiff")
  
  print("Blockmask created")
  
}
