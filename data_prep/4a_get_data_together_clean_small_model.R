library(raster)
library(mapview)
library(rgdal)

l8p1 <- "D:/new_downscaling/data_download_preprocessing/L8/2019-01/"
mp1 <- "D:/new_downscaling/data_download_preprocessing/MODIS/2019-01/"
l8p2 <- "D:/new_downscaling/data_download_preprocessing/L8/2018-02/"
mp2 <- "D:/new_downscaling/data_download_preprocessing/MODIS/2018-02/"
main <- "D:/new_downscaling/data_download_preprocessing/"
areaname <- "MDV"

# get aoi
aoipath <-  "D:/new_downscaling/aoi/MDV/"
aoip <- list.files(aoipath, pattern="adp.shp", full.names = T)
aoi <- readOGR(aoip)
antaproj <- crs("+proj=stere +lat_0=-90 +lat_ts=-71 +lon_0=0 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m
+no_defs +ellps=WGS84 +towgs84=0,0,0")
aoianta <- spTransform(aoi, antaproj)

################## GET & STACK SATELLITE DATA ###########################################

# 2019_01_01
l8r1 <- list.files(paste0(l8p1, "LST/"), full.names=T)
L8r1s <- raster(l8r1[2])

mr1 <- list.files(paste0(mp1, "LST/"), pattern="small", full.names=T)[1]
#mr1 <- mr1[grep('tif$', mr1)]
mras1 <- raster(mr1)
if(any(mras1[]<(-100))){
  mras1[mras1<(-100)] <- NA
}

# 2018_02_11
l8r2 <- list.files(paste0(l8p2, "LST/"), full.names=T)
L8r2s <- raster(l8r2)

mr2 <- list.files(paste0(mp2, "LST/"), pattern="small", full.names=T)[5]
mras2 <- raster(mr2)

ml <- list(mras1, mras2)
ll <- list(L8r1s, L8r2s)

# stack according to satellite source
  # MODIS
  # get extent of files
  mtmplras <- ml[[1]]
  mext <- lapply(seq(ml), function(i){
    p <- as(extent(ml[[i]]), 'SpatialPolygons')
    crs(p) <- antaproj
    p
  })
  me <- do.call(bind, mext)
  
  # make a template to force 1x1km pixels
  extent(mtmplras) <- extent(me)
  dir.create(paste0(main, "MODIS_model/"))
  # resample all MODIS rasters
  mlres <- lapply(seq(ml), function(i){
    print(i)
    x <- resample(ml[[i]], mtmplras)
    writeRaster(x, paste0(main, "MODIS_model/", names(ml[[i]]), ".tif"), format="GTiff", 
                overwrite=T)
    x
  })
  
  # LANDSAT
  ltmplras <- ll[[1]]
  lext <- lapply(seq(ll), function(i){
    p <- as(extent(ll[[i]]), 'SpatialPolygons')
    crs(p) <- antaproj
    p
  })
  le <- do.call(bind, lext)
  
  # make a template to force 1x1km pixels
  extent(ltmplras) <- extent(le)
  dir.create(paste0(main, "LANDSAT_model/"))
  # resample all MODIS rasters
  llres <- lapply(seq(ll), function(i){
    print(i)
    x <- resample(ll[[i]], ltmplras)
    writeRaster(x, paste0(main, "LANDSAT_model/", names(ll[[i]]), ".tif"), format="GTiff", 
                overwrite=T)
    x
  })

m <- stack(list.files(paste0(main, "MODIS_model/"), full.names = T))
plot(m)
l <- stack(list.files(paste0(main, "LANDSAT_model/"), full.names = T))
plot(l)

# resample MODIS to L8 resolution
mres <- resample(m, l[[1]])

satstack <- stack(l, mres)

################## GET AUXILIARY DATA ###########################################

# # prep auxiliary variables: 
# # stack MODIS and L8, DEM, TWI, (aspect), hillshade, spatial blocks for CV
# dempath <- "D:/new_downscaling/tiles_westcoast/"
# dem <- raster(paste0(dempath, "DEM_8m_", areaname,"_clean_filled_15.tif"))
# blockmask <- raster(paste0(dempath, "blockmask.tif"))
# slope <- raster(paste0(dempath, "slopeMDV.tif"))
# hillsh <- raster(paste0(dempath, "hillshading_8m.tif"))
# hillshade <- crop(hillsh, dem)
# twipath <- "D:/Antarctica/runoff_paths/"
# twi <- raster(paste0(twipath, "TWI_Rslope.tif"))
# twires <- resample(twi, dem)
# s <- stack(dem, slope, twires, hillshade, blockmask)
# saveRDS(s, paste0(main, "auxiliary_stack_org.rds"))
# writeRaster(s, paste0(main, "auxiliary_stack_org.tif"))

# # resample s to fit with satstack
# sres <- resample(s, satstack[[1]])
# saveRDS(sres, paste0(main, "auxiliary_stack_30m.rds"))
# writeRaster(sres, paste0(main, "auxiliary_stack_30m.tif"))


sres <- readRDS(paste0(main, "auxiliary_stack_30m.rds"))


s <- stack(satstack, sres)
saveRDS(s, paste0(main, "comp_stack.rds"))
################# EXTRACT #####################################################
ex <- extract(s, aoianta)
saveRDS(ex, paste0("C:/Users/mleza/OneDrive/Documents/PhD/work_packages/auto_downscaling_30m/", "ex_fake_model.rds"))

################# GROUP L8 and MODIS together #################################
exc <- ex[[1]][complete.cases(ex[[1]]),]
excd <- data.frame(exc)

names(excd)
set1 <- excd[,c(1,4,5:9)] # 2019_01_01
set2 <- excd[,c(2,3,5:9)] # 2018_02_11

names(mras[[1]]) #15:15
names(L8ras)[2] #13:49

names(set1) <- c("L8", "MODIS", "DEM", "slope","TWI" ,"hillsh", "blockmask")
names(set2) <- c("L8", "MODIS", "DEM", "slope","TWI" ,"hillsh", "blockmask")

set1$datetimeL <- rep(201901011349, nrow(set1))
set1$datetimeM <- rep(201901011515, nrow(set1))

set2$datetimeL <- rep(201802112044, nrow(set2))
set2$datetimeM <- rep(201902112010, nrow(set2))


exdf <- rbind(set1,set2)
saveRDS(exdf, paste0(paste0("C:/Users/mleza/OneDrive/Documents/PhD/work_packages/auto_downscaling_30m/", "ex_fake_model_final.rds")))


par(mfrow=c(1,2))
boxplot(exdf$L8, main="L8 LST", ylim=c(-80, 24))
boxplot(exdf$MODIS, main="MODIS LST", ylim=c(-80, 24))