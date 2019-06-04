

#### TAKE A LOOK AT DOWNLOADED L8 IMAGES  #######################################################

library(raster)
library(rgdal)
library(gdalUtils)
library(downscaleRS)
library(RStoolbox)
library(mapview)

datpath <- "C:/Users/mleza/OneDrive/Documents/PhD/work_packages/auto_downscaling_30m/data/L8_data/"
lcpath <- "C:/Users/mleza/OneDrive/Documents/PhD/work_packages/auto_downscaling_30m/data/LC_training/"
main <- paste0(datpath, "get_data/LANDSAT/L1/")
sdirs <- list.files(main, full.names = T)

scriptpath <- "C:/Users/mleza/OneDrive/Documents/PhD/work_packages/auto_downscaling_30m/downscale_controlscripts/"
source(paste0(scriptpath, "read_meta_L8_PS.R"))

#### GET AND PROJECT AREA OF INTEREST ####################################################################
l8proj <- crs("+proj=stere +lat_0=-90 +lat_ts=-71 +lon_0=0 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m
+no_defs +ellps=WGS84 +towgs84=0,0,0")
aoipath <- "C:/Users/mleza/OneDrive/Documents/PhD/work_packages/auto_downscaling_30m/data/aoi_MDV/"
aoi <- readOGR(list.files(aoipath, pattern="new.shp", full.names = T))
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
s1 <- lapply(seq(nums), function(i){
  stackMeta(datloc$meta[[nums[i]]], quantity = 'all')
})

############# ELIMINATE 0 VALUES ########################################################################## 

for(i in seq(s1)){
  for(j in seq(nlayers(s1[[1]]))){
    s1[[i]][[j]][s1[[i]][[j]]==0] <- NA
  }
}


#############  Calculation of TOA (Top of Atmospheric) spectral radiance and ##################### 
#################  brightness temperature (°C) ########################################

#TO DO: Make Function, make loop!

BTC <- lapply(seq(s1), function(i){
  
  # TOA (L) = ML * Qcal + AL
  # ML = Band-specific multiplicative rescaling factor from the metadata (RADIANCE_MULT_BAND_x, where x is the band number).
  # Qcal = corresponds to band 10.
  # AL = Band-specific additive rescaling factor from the metadata (RADIANCE_ADD_BAND_x, where x is the band number).
  
  mD <- readMeta(datloc$meta[[i]], raw=T)
  nam <- mD$METADATA_FILE_INFO["LANDSAT_PRODUCT_ID",]
  
  ML <- mD$RADIOMETRIC_RESCALING["RADIANCE_MULT_BAND_10",]
  AL <- mD$RADIOMETRIC_RESCALING["RADIANCE_ADD_BAND_10",]
  #TOA = (ML * lsat8$B10_dn) + AL
  TOA = (ML * lsat8_sdos$B10_bt) + AL
  
  
  # TOA to Brightness Temperature conversion
  # BT = (K2 / (ln (K1 / L) + 1)) ??? 273.15
  
  # K1 = Band-specific thermal conversion constant from the metadata (K1_CONSTANT_BAND_x, where x is the thermal band number).
  # K2 = Band-specific thermal conversion constant from the metadata (K2_CONSTANT_BAND_x, where x is the thermal band number).
  # L = TOA
  # Therefore, to obtain the results in Celsius, the radiant temperature is adjusted 
  # by adding the absolute zero (approx. -273.15?C).
  
  K1 <- mD$TIRS_THERMAL_CONSTANTS["K1_CONSTANT_BAND_10",]
  K2 <- mD$TIRS_THERMAL_CONSTANTS["K2_CONSTANT_BAND_10",]
  BTK <- (K2 /(log(K1 / TOA)))+1
  BTC <- (BTK-273.15)
  
  writeRaster(BTC, paste0(datpath, "ac/", nam, "_BTC", ".tif"), 
              format="GTiff", overwrite=T)
  
})



################## ATMOSPHERIC CORRECTION ####################################################################

lapply(c(2:8), function(i){
  lsat8 <- s1[[i]]
  names(lsat8) <- names(lsat8o[[i]])
  
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
      writeRaster(lsat8_sdos[[j]], paste0(datpath, "ac/", nam, "_", names(lsat8_sdos)[j], ".tif"), 
              format="GTiff", overwrite=T)
  }
})

# get files in, ordered
fac <- list.files(paste0(datpath, "ac/"), full.names = T)
lo <- seq(1,length(metaData)*10, by=10) # *10 because channel 1:11 without channel 8 (pancromatic, in 15m resolution)
hi <- lo+9
ac <- lapply(seq(metaData), function(i){
  stack(fac[lo[i]:hi[i]])
})

################## MAKE TERRAIN CORRECTION FOR LANDCOVER PREDICTION #############################################



################## MAKE LANDCOVER PREDICTION ####################################################################
# VIS; NIR; SWIR for Landcover training
lctrdat <- lapply(seq(metaData), function(i){
  stack(ac[[i]][[4:9]])
})

# take a look at tiles 
# mapview(lctrdat[[1]][[1]])+mapview(lctrdat[[2]][[1]])+mapview(lctrdat[[3]][[1]])+mapview(lctrdat[[4]][[1]])+
# mapview(lctrdat[[5]][[1]])+mapview(lctrdat[[6]][[1]])+mapview(lctrdat[[7]][[1]])+mapview(lctrdat[[8]][[1]])

# select those that are good for classifying
names(lctrdat[[4]])
names(lctrdat[[5]])

# load training shapes from QGis
lcts <- readOGR(list.files(lcpath, pattern=".shp", full.names = T))

lcts$id <- seq(1:nrow(lcts@data))
unique(lcts$type)




lctraindf <- lapply(c(4,5), function(i){
  x <- extract(lctrdat[[i]], lcts)
  print(i)
  x
})

lctraindf[[c(4:5)]]
str(lctraindfn)

# eliminate NULL listeneinträge
trainex <- lapply(seq(lctraindf), function(i){
  lctraindf[[i]][vapply(lctraindf[[i]], Negate(is.null), NA)]
})

sapply(seq(lctraindfn), function(i){
  seq(lctraindfn[[i]])
})

str(trainex[[8]])


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

