# 1 DEM


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
  r[r<(-100)] <- NA # eliminate those that aboslutely need to go
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
mos_caoi <- raster(paste0(dempath, "DEM_8m_", areaname,"_clean_aoi.tif"))

# NA in 8m DEM
m <- mos_caoi
m[is.na(m)] <- -100
m[m > -100] <- NA

writeRaster(m, paste0(dempath, "DEM_8m_", areaname,"_NA.tif"), format="GTiff")


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

nashape <- readOGR(paste0(dempath, "dem_8m_NA.shp"))

# resample first to 8m and cover and mask, then resample to 30m to evade sharp cuts
mos_caoi_na <- mask(mos_caoi, nashape, inverse=TRUE)
dem200_8 <- resample(dem200aoi, mos_caoi)

mos_filled_8 <- cover(mos_caoi_na, dem200_8)
mos_filled_30 <- resample(mos_filled_8, template)

mos_filled_30_aoi <- mask(mos_filled_30, aoianta)

writeRaster(mos_filled_30_aoi, paste0(dempath, "DEM_30m_", areaname,"_clean_aoi_filled_mask_new_II.tif"), format="GTiff",
            overwrite=T)
#writeRaster(mos_filled_30_aoi_filter5, paste0(dempath, "DEM_8m_", areaname,"_clean_aoi_filled_mask_filter5_new.tif"), format="GTiff",
#            overwrite=T)

print("fill NA values with 200m DEM")

mos_filled_30_aoi <- raster(paste0(dempath, "DEM_30m_", areaname,"_clean_aoi_filled_mask_new.tif"))


############################### calculate slope and aspect ##############################################################

slas <- terrain(mos_filled_30_aoi, opt=c("slope", "aspect"), unit="degrees", neighbors=8)
slasrad <- terrain(mos_filled_30_aoi, opt=c("slope", "aspect"), unit="radians", neighbors=8)

for(i in seq(2)){
  writeRaster(slas[[i]], paste0(dempath, "30m_", names(slas[[i]]), areaname,".tif"), format="GTiff", overwrite=T)
}

for(i in seq(2)){
  writeRaster(slasrad[[i]], paste0(dempath, "30m_radians_", names(slas[[i]]), areaname,".tif"), format="GTiff", overwrite=T)
}
print("slope and aspect done")



################ MAKE A BLOCKMASK #######################################
r <- raster(paste0(dempath, "DEM_8m_", areaname,"_clean_aoi_filled_mask_new.tif"))
rt <- r
res(rt) <- c(20000, 20000)
rt[] <- seq(1:ncell(rt))

blockmask <- resample(rt, r, method="ngb")
writeRaster(blockmask, paste0(dempath, "blockmask.tif"), format="GTiff", overwrite=T)

print("Blockmask created")


# mask blockmask to aoi

bm <- raster(paste0(dempath, "blockmask.tif"))
bmaoi <- mask(bm, aoianta)
writeRaster(bmaoi, paste0(dempath, "blockmask_aoi.tif"), format="GTiff", overwrite=T)

