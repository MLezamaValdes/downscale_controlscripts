

#### TAKE A LOOK AT DOWNLOADED L8 IMAGES  #######################################################

library(raster)
library(rgdal)
library(gdalUtils)
library(downscaleRS)
library(RStoolbox)
library(mapview)
library(satellite)

datpath <- "D:/run_everything/Antarctica/"
l8out <- paste0(datpath, "L8_data/")
lcpath <- paste0(datpath, "LC_training/")

main <- "C:/Users/mleza/OneDrive/Documents/PhD/work_packages/auto_downscaling_30m/data/L8_data/get_data/LANDSAT/L1/"
sdirs <- list.files(main, full.names = T)

scriptpath <- "C:/Users/mleza/OneDrive/Documents/PhD/work_packages/auto_downscaling_30m/downscale_controlscripts/"
source(paste0(scriptpath, "read_meta_L8_PS.R"))

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}


#### GET AND PROJECT AREA OF INTEREST ####################################################################
l8proj <- crs("+proj=stere +lat_0=-90 +lat_ts=-71 +lon_0=0 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m
+no_defs +ellps=WGS84 +towgs84=0,0,0")
aoipath <- "D:/run_everything/Antarctica/aoi/MDV/"
aoi <- readOGR(list.files(aoipath, pattern="adp.shp", full.names = T))
aoi <- spTransform(aoi, l8proj)


##### LOAD ALL DOWNLOADED L8 SCENES  #####################################################################
# summarize all available scenes
datloc <- l8datlist(sdirs)

metaData <- lapply(seq(datloc$meta), function(i){
  readMeta(datloc$meta[[i]], raw=T)
})

lsat8o <- lapply(seq(datloc$meta), function(i){
  stackMeta(datloc$meta[[i]], category = "image", allResolutions = F)
})


# ## Import SR rasters
# list.files(sdirs,pattern=".tif")
# rl <- lapply(paste0(path, files), raster)


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

# # find those with consequtive dates, that are not too far from each other
# df <- df[order(as.Date(df$date, format="%Y-%m-%d")),]
# df$date <- as.Date(df$date, format="%Y-%m-%d")
# 
# daydiff <- sapply(seq(nrow(df)-1), function(i){
#     df$date[i] - df$date[i+1]
#   })
# dd <- c(0, daydiff)
# df$daydiff <- dd
# df$cumdays <- cumsum(abs(df$daydiff))
# 
# # take all scenes that are not more than 10 days apart
# nums <- df$scenenumber[df$cumdays < 10]

nums <- seq(1:nrow(df))

# ordered by time: stacks
s <- lapply(seq(nums), function(i){
  stackMeta(datloc$meta[[nums[i]]], quantity = 'all')
})


############# CHECK WHICH FILES ARE ACTUALLY RELEVANT ######################################################

clpath <- "D:/run_everything/Antarctica/coastline/Coastline_high_res_polygon/"
# cl <- readOGR(list.files(clpath, pattern=".shp", full.names=T))
# land <- cl[cl$surface=="land" | cl$surface=="ice tongue",]
# writeOGR(land, paste0(clpath, "land_contours.shp"), driver = "ESRI Shapefile", layer="land_contours.shp")

# do all tiles overlap with land? 
land <- readOGR(paste0(clpath, "land_contours.shp"))

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

sel <- s[unlist(bigarea)]

aoiint <- lapply(seq(sel), function(i){
  intersect(aoi, sel[[i]])
})


aoiarea <- lapply(seq(aoiint), function(i){
  if(!is.null(aoiint[[i]])){
    sum(area(aoiint[[i]]))
  }
})

aoibigarea <- lapply(seq(aoiarea), function(i){
  aoiarea[[i]]>=10000000000
})

sel_f <- sel[unlist(aoibigarea)]

ss1 <- which(unlist(bigarea)==T)
ss_f <- ss1[which(aoibigarea==T)]

############# ELIMINATE 0 VALUES ########################################################################## 
s <- sel_f
for(i in seq(s)){
  for(j in seq(nlayers(s[[1]]))){
    s[[i]][[j]][s[[i]][[j]]==0] <- NA
  }
}

################## ATMOSPHERIC CORRECTION ####################################################################

lsat8_sdos <- lapply(seq(s), function(i){
  lsat8 <- s[[i]]
  names(lsat8) <- names(lsat8o[unlist(bigarea)][unlist(aoibigarea)][[i]])
  
  #estimate digital number pixel value of dark objects in visible wavelength
  hazeDN    <- estimateHaze(lsat8, hazeBands = c("B3_dn", "B4_dn"),
                            darkProp = 0.01)
  
  # radiometric calibration and correction of Landsat data 
  lsat8_sdos <- radCor(lsat8, 
                       metaData = readMeta(datloc$meta[[1]], raw=F),
                       hazeValues = hazeDN,
                       hazeBands = c("B3_dn", "B4_dn"),
                       method = "sdos")
  
  mD <- readMeta(datloc$meta[[i]], raw=T)
  nam <- mD$METADATA_FILE_INFO["LANDSAT_PRODUCT_ID",]
  
  for(j in seq(nlayers(lsat8_sdos))){
    writeRaster(lsat8_sdos[[j]], paste0(l8out, "ac/", nam, "_", names(lsat8_sdos)[j], ".tif"), 
                format="GTiff", overwrite=T)
  }
  lsat8_sdos
})



# get files in, ordered
fac <- list.files(paste0(l8out, "ac/"), full.names = T, pattern=".tif$")
lo <- seq(1,length(metaData[unlist(bigarea)][unlist(aoibigarea)])*10, by=10) # *10 because channel 1:11 without channel 8 (pancromatic, in 15m resolution)
hi <- lo+9
lsat8_sdos <- lapply(seq(sel_f), function(i){
  stack(fac[lo[i]:hi[i]])
})

#############  Calculation of TOA (Top of Atmospheric) spectral radiance and #################### 
#################  brightness temperature ##################################################

#TO DO: check why values are off!

BTC <- lapply(seq(ss_f), function(i){
  
  # TOA (L) = ML * Qcal + AL
  # ML = Band-specific multiplicative rescaling factor from the metadata (RADIANCE_MULT_BAND_x, where x is the band number).
  # Qcal = corresponds to band 10.
  # AL = Band-specific additive rescaling factor from the metadata (RADIANCE_ADD_BAND_x, where x is the band number).
  
  mD <- readMeta(datloc$meta[[ss_f[i]]], raw=T)
  nam <- mD$METADATA_FILE_INFO["LANDSAT_PRODUCT_ID",]
  
  ML <- mD$RADIOMETRIC_RESCALING["RADIANCE_MULT_BAND_10",]
  AL <- mD$RADIOMETRIC_RESCALING["RADIANCE_ADD_BAND_10",]
  TOA = (ML * lsat8o[[ss_f[i]]]$B10_dn) + AL # this is band 10
  
  
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
  
  writeRaster(BTC, paste0(l8out, "bt/", nam, "_BTC", ".tif"), 
              format="GTiff", overwrite=T)
  BTC
})


################## MAKE TERRAIN CORRECTION FOR LANDCOVER PREDICTION #############################################

# get DEM
mos <- raster(list.files("E:/Antarctica/DEM/", full.names=T))

L8exts <- lapply(seq(BTC), function(i){
  p <- as(extent(BTC[[i]]), 'SpatialPolygons')
  crs(p) <- l8proj
  p
})

m <- do.call(bind, L8exts) 

mos_s <- crop(mos, extent(m))
mos_s[mos_s < (-300)] <- NA

# write small DEM for runoff path calculation
writeRaster(mos_s, paste0(l8out, "DEM_MDV_8m_not_corr.tif"), format="GTiff", overwrite=T)


# resample DEM to the tile, make slope, aspect and hillshade rasters
# calculate topographic correction for the tile 

slope <- terrain(mos_s, opt='slope')
aspect <- terrain(mos_s, opt='aspect')
writeRaster(slope, paste0(l8out, "slope_8m_s.tif"), format="GTiff", overwrite=T)
writeRaster(aspect, paste0(l8out, "aspect_8m_s.tif"), format="GTiff", overwrite=T)


slope <- raster(paste0(l8out, "slope_8m_s.tif"))
aspect <- raster(paste0(l8out, "aspect_8m_s.tif"))
sazel <- lapply(seq(ss_f), function(i){
  mD <- readMeta(datloc$meta[[ss_f[i]]], raw=T)
  sun_azimuth <- as.numeric(mD$IMAGE_ATTRIBUTES["SUN_AZIMUTH",])
  sun_elev <- as.numeric(mD$IMAGE_ATTRIBUTES["SUN_ELEVATION",])
  list(sun_azimuth, sun_elev)
})

# take mean of sun elevation and azimuth to be found in Landsat files that are being used in scene
saz <- mean(unlist(lapply(sazel, `[[`, 1)))
sev <- mean(unlist(lapply(sazel, `[[`, 2)))

hils <- hillShade(slope, aspect, angle=sev, direction=saz)
writeRaster(hils, paste0(l8out, "hillshading_8m_s.tif"), format="GTiff")

#hils <- raster(paste0(l8out, "hillshading_8m.tif"))

# run topocorr
ac <- lsat8_sdos
for(i in seq(ac)){
  # does ac L8 fall into land area? 
  ls <- crop(land, extent(ac[[i]]))
  testac <- aggregate(ac[[i]][[1]], fact=40)
  te <- unlist(extract(testac, ls))
  if(any(!is.na(te)) & any(!is.nan(te))){
      hs <- crop(hils, extent(ac[[i]][[1]]))
      hils_30 <- resample(hs, ac[[i]][[1]])
      x <- calcTopoCorr(ac[[i]], hillsh = hils_30)
      writeRaster(x, paste0(l8out, "tc/tc_", i, ".tif"), format="GTiff")
  }
}

# # compare ac and tc
# plotRGB(ac[[4]], r=7, g=6, b=5, stretch="hist")
# plotRGB(x, r=7, g=6, b=5, stretch="hist")

tc <- lapply(seq(2), function(i){
    stack(list.files(paste0(l8out, "tc/"), full.names=T, pattern="tif$")[i])
}) 

# merge topographically corrected tiles


################## MAKE LANDCOVER PREDICTION ####################################################################
# # VIS; NIR; SWIR for Landcover training
# lctrdat <- lapply(seq(metaData), function(i){
#   stack(ac[[i]][[4:9]])
# })

# TO DO: TEST WITH AC ONLY to see if we can skip tc

lctrdat <- tc
mos <- merge(lctrdat[[1]], lctrdat[[2]])
writeRaster(mos, paste0(l8out, "L8_ac_tc_mosaic.tif"), format="GTiff")

mos <- raster(paste0(l8out, "L8_ac_tc_mosaic.tif"))
hls <- raster(paste0(l8out, "hillshading_8m_s.tif"))

lctrdat <- mos

hlsres <- resample(hls, mos)

# take a look at tiles 
# mapview(lctrdat[[1]][[1]])+mapview(lctrdat[[2]][[1]])

# load training shapes from QGis
lcts <- readOGR(list.files(lcpath, pattern=".shp", full.names = T))
lcts$id <- seq(1:nrow(lcts@data))

# subset without wet soil
lcts <- lcts[lcts$type!="wet soil",]

names(hlsres) <- "hills"
datstack <- stack(lctrdat, hlsres)
lctrdat <- datstack

# # to run on another machine
# writeRaster(lctrdat, paste0(l8out, "extr/lctrdat.tif"), format="GTiff")
# writeOGR(lcts, paste0(l8out, "extr/lcts.shp"), layer="lcts.shp", driver="ESRI Shapefile")

lctraindf <- lapply(seq(lctrdat), function(i){
  extract(lctrdat, lcts, df=T)
})

lcts$orgID <- lcts$id

is <- lapply(seq(lctrdat), function(i){
  intersect(lcts, lctrdat)
})

# run from here

# eliminate NULL listeneinträge
trainex <- lapply(seq(lctraindf), function(i){
  lctraindf[[i]][vapply(lctraindf[[i]], Negate(is.null), NA)]
})

sapply(seq(trainex), function(i){
  seq(trainex[[i]])
})


trainexdf <- lapply(seq(trainex), function(i){
  lapply(seq(trainex[[i]]), function(j){
    df <- data.frame(trainex[[i]][[j]])
    df$type <- rep(as.character(is[[i]]$type[j]), nrow(trainex[[i]][[j]]))
    df$sh_ID <- rep(is[[i]]$orgID[j], nrow(trainex[[i]][[j]]))
    names(df) <- substrRight(names(df), 6)
    df
  })
})


# add info concerning sample shape number, type of sample shape content and scene 
trainex <- lapply(seq(lctraindf), function(i){
  if()
})
df <- data.frame(lctraindf[[4]][[3]])
df$sshape <- rep("1", nrow(df))
df$type <- rep(as.character(lcts@data$type[1]), nrow(df))
df$scene <- rep("4", nrow(df))

lctraindf[[4]][[2]]$sshape <- rep("1", nrow(lctraindf[[4]][[2]]))
lctraindf[[4]][[1]]$type <- as.character(lcts@data$type[1])




which(is.null(lctraindf[[4]][[is.null]][[1,]]))

library(plyr)
df <- ldply(lctraindf, data.frame)


lctraindfc <- do.call(rbind, lctraindf)
rbind()

write.csv(lctraindf, paste0(lcpath, "traindf.csv"))


################## CUT TO AOI ##################################################################################

# cut out to research area
lsat8.aoi <- crop(lsat8, aoi)
lsat8.aoi <- mask(lsat8.aoi, aoi)


################## CALCULATE LST ####################################################################


# TO DO: Make function!
# Emissivity
eta <- lc
eta[eta==2] <- 0.94 
eta[eta==1] <- 0.97

# Calculate the Land Surface Temperature
LST <- (BTC/(1+(0.0010895*BTC/0.01438)*log(eta))) 

# ?? = 0.004 * Pv + 0.986
# use 0.9668 (Yu, Guo, Wu) as correction value for bare soil
emis <- 0.004*lsat8.PV+0.9668

# Calculate the Land Surface Temperature
# LST = (BT / (1 + (0.00115 * BT / 1.4388) * Ln(??)))
LST <- (BTC/(1+(0.00115*BTC/1.4388)*log(emis))) 


# write LST raster
writeRaster(LST, paste0(datapath, "LST.tif"), format="GTiff")

