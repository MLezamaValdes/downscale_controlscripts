# take a look at downloaded L8 images

library(raster)
library(rgdal)
library(gdalUtils)
library(downscaleRS)
library(mapview)

datapath <-"E:/L8_MDV/get_data/LANDSAT/L1/"
scriptpath <- "C:/Users/mleza/Documents/PhD/work_packages/auto_downscaling_30m/downscale_controlscripts/"
scenedir <- list.files(datapath)
scenes <- paste0(datapath, scenedir, "/")
source(paste0(scriptpath, "read_meta_L8_PS.R"))

# get and project area of interest
l8proj <- crs("+proj=stere +lat_0=-90 +lat_ts=-71 +lon_0=0 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m
+no_defs +ellps=WGS84 +towgs84=0,0,0")
aoipath <- "C:/Users/mleza/Documents/PhD/work_packages/auto_downscaling_30m/data/aoi_MDV/"
aoi <- readOGR(list.files(aoipath, pattern=".shp", full.names = T))
aoi <- spTransform(aoi, l8proj)

# function for locating tifs and metadata as well as generating filenames 
# for all downloaded scenes

# summarize all available scenes
datloc <- l8datlist(scenes)

metaData <- lapply(seq(datloc$meta), function(i){
  readMeta(datloc$meta[[i]], raw=T)
})
lsat8 <- lapply(seq(datloc$meta), function(i){
  stackMeta(datloc$meta[[i]], quantity = 'all')
})
  

# CHECK WHICH SCENES GO TOGETHER
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

# PATCH SCENES TOGETHER

# find those with consequtive dates, that are not too far from each other
df <- df[order(as.Date(df$date, format="%Y-%m-%d")),]
df$date <- as.Date(df$date, format="%Y-%m-%d")

daydiff <- sapply(seq(nrow(df)-1), function(i){
    df$date[i] - df$date[i+1]
  })
dd <- c(0, daydiff)
df$daydiff <- dd
df$cumdays <- cumsum(abs(df$daydiff))

# take all scenes that are not more than 10 days apart
nums <- df$scenenumber[df$cumdays < 10]


# ordered by time: stacks
scene1 <- lapply(seq(nums), function(i){
  stackMeta(datloc$meta[[nums[i]]], quantity = 'all')
})

# good
s1 <- scene1[c(2,3,5,6,7,8)]
sc <- s1

for(i in seq(2)){
  for(j in seq(nlayers(s1[[1]]))){
    s1[[i]][[j]][s1[[i]][[j]]==0] <- NA
  }
}

# run again tomorrow!
mosaic <- merge(s1[[1]], s1[[2]], s1[[3]], s1[[4]], s1[[5]], s1[[6]],
      tolerance=0.05, filename="testmosaic.tif", overwrite=T, overlap=T, ext=NULL)

writeRaster(mosaic, "E:/L8_MDV/mosaic/testmosaic.tif", format="GTiff", 
            overwrite=T, bylayer=T)

mapview(mosaic[[1]])

scene <- mosaic(s1[[1]], s1[[2]], s1[[3]], s1[[4]], s1[[5]], s1[[6]], 
                fun=max)

# TO DO!
ac <- function(lsat8, aoi, metaData){
  # cut out to research area
  lsat8.aoi <- crop(lsat8, aoi)
  lsat8.aoi <- mask(lsat8.aoi, aoi)
  
  # Calculation of TOA (Top of Atmospheric) spectral radiance.
  # TOA (L) = ML * Qcal + AL
  # ML = Band-specific multiplicative rescaling factor from the metadata (RADIANCE_MULT_BAND_x, where x is the band number).
  # Qcal = corresponds to band 10.
  # AL = Band-specific additive rescaling factor from the metadata (RADIANCE_ADD_BAND_x, where x is the band number).
  
  ML <- metaData$RADIOMETRIC_RESCALING["RADIANCE_MULT_BAND_10",]
  AL <- metaData$RADIOMETRIC_RESCALING["RADIANCE_ADD_BAND_10",]
  TOA = (ML * lsat8.aoi$B10_dn) + AL
  
  
  # TOA to Brightness Temperature conversion
  # BT = (K2 / (ln (K1 / L) + 1)) ??? 273.15
  
  # K1 = Band-specific thermal conversion constant from the metadata (K1_CONSTANT_BAND_x, where x is the thermal band number).
  # K2 = Band-specific thermal conversion constant from the metadata (K2_CONSTANT_BAND_x, where x is the thermal band number).
  # L = TOA
  # Therefore, to obtain the results in Celsius, the radiant temperature is adjusted 
  # by adding the absolute zero (approx. -273.15?C).
  
  K1 <- metaData$TIRS_THERMAL_CONSTANTS["K1_CONSTANT_BAND_10",]
  K2 <- metaData$TIRS_THERMAL_CONSTANTS["K2_CONSTANT_BAND_10",]
  BTK <- (K2 /(log(K1 / TOA)))+1
  BTC <- (BTK-273.15)
  return(BTC)
}



# CALCULATE THE LANDCOVER PREDICTION
lc <- 
  
# TO DO!
makeL8LST <- function(BTC, lc, datapath){

  # Calculate Emissivity
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
  
}




# WRITE ONE CHANNEL TO SEE HOW THEY FIT IN QGIS
# blue <- lapply(seq(tifs), function(i){
#   raster(tifs[[i]][4])
# })
# 
# bluepath <- "E:/L8_MDV/blue/"
# 
# for(i in seq(blue)){
#   writeRaster(blue[[i]], paste0(bluepath, names[[i]][4]), format="GTiff", overwrite=T)
# }


# ATMOSPHERIC CORRECTION
# ATMOSPHERIC CORRECTION
# estimate digital number pixel value of dark objects in visible wavelength
hazeDN    <- estimateHaze(lsat8, hazeBands = c("B3_dn", "B4_dn"),
                          darkProp = 0.01)

# radiometric calibration and correction of Landsat data 
lsat8_sdos <- radCor(lsat8, metaData = metaData,
                     hazeValues = hazeDN,
                     hazeBands = c("B3_dn", "B4_dn"),
                     method = "sdos")


lsat8.input <- lsat8_sdos[[2:7]]

# IMAGE PATCHING


# check for those of the same row, those are from the same day...