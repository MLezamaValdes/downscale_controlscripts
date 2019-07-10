
####### LOAD PACKAGES, SET PATHS ###############################################################################

library(getSpatialData)
library(downscaleRS)
library(sf)
library(raster)
library(rgdal)
library(mapview)
library(gdalUtils)
library(RColorBrewer)
library(viridis)


# filepath <- "D:/MODIS_MDV/HDF/"
aoipath <- "E:/Antarctica/aoi/MDV/"
hdfpath <- "D:/MODIS_MDV/2018_01_19/hdfs/"

## set archive directory
set_archive("D:/MODIS_MDV/2018_01_19/")

# # for retrieving automatically downloaded images
# dirs1 <- list.files(filepath, pattern="MOD11_")
# dirs2 <- list.files(filepath, pattern="MYD11_")
# files1 <- list.files(paste0(filepath, dirs1), pattern=".hdf$", full.names = T)
# files2 <- list.files(paste0(filepath, dirs2), pattern=".hdf$", full.names = T)

# make an indir directory with the hdf files to batch convert and an outdir directory with 
# the prm template file, where output files will be written to
# batchrunpath needs to be where MyHEG_batchScript.bat is located
batchrunpath <- "C:/Users/mleza/HEG/HEG_Win/bin/BatchRunning/BatchRunning/"
indir <- paste0(batchrunpath, "indir/") 
outdir <- paste0(batchrunpath, "outdir/")
datloc <- "C:/Users/mleza/OneDrive/Documents/PhD/work_packages/auto_downscaling_30m/data/"

# projections
antaproj <- "+proj=stere +lat_0=-90 +lat_ts=-71 +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"


####### GET DATA ONLINE ##############################################################################################

## set aoi and time range for the query
aoi <- readOGR(list.files(aoipath, pattern="adp.shp", full.names = T))
aoiutm <- spTransform(aoi, crs("+proj=utm +zone=57 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs "))
aoianta <- spTransform(aoi, antaproj)
set_aoi(aoiutm)

time_range <-  c("2018-01-19", "2018-01-19")

## get available products and select one
product1 <- "MODIS_MOD11_L2_V6"
product2 <- "MODIS_MYD11_L2_V6"
# product_names <- getMODIS_names(username="MaiteLezama", password = "Eos300dmmmmlv")
# product1 <- grep("MOD11_L2_V6", product_names, value = T)
# product2 <- grep("MYD11_L2_V6", product_names, value = T)

## query for records for your AOI, time range and product
query1 <- getMODIS_query(time_range = time_range, name = product1,
                        username="MaiteLezama", password = "Eos300dmmmmlv",
                        aoi=get_aoi())
query2 <- getMODIS_query(time_range = time_range, name = product2,
                         username="MaiteLezama", password = "Eos300dmmmmlv",
                         aoi=get_aoi())

## preview a record
#getMODIS_preview(query1[9,])

## download records
files1 <- getMODIS_data(query1)
files2 <- getMODIS_data(query2)


# put all hdfs into one folder (hdfpath)
nl1 <- paste0(hdfpath, basename(files1))
file.copy(from=files1, to=nl1, 
          overwrite = TRUE)
nl2 <- paste0(hdfpath, basename(files2))
file.copy(from=files2, to=nl2, 
          overwrite = TRUE)

######### BATCH TRANSLATING SWATH TO GEOTIFF WITH HEG TOOL ####################################################

# get template prm file
tpl <- read.delim(list.files(outdir, pattern="unix.prm", full.names = T),
                       sep="=", col.names = c("nam", "val"), header = F, stringsAsFactors = F)

# run HEG tool to tranlate swaths to geotiff 
tplpath <- list.files(outdir, pattern="unix.prm", full.names = T)

# list all files in hdf dir
filescomp <- list.files(hdfpath, full.names=T)

# run HEG tool
runheg(files=filescomp, indir, outdir, tplpath, layer = "View_time|")

######## GO ON PROCESSING LST IN R ########################################################################################

# get tif files
lst <- lapply(seq(filescomp), function(i){
  raster(list.files(outdir, pattern=".tif$", full.names=T)[i])
}) 

# convert values to valid range and °C
lst_c <- lapply(seq(lst), function(i){
  # Valid Range = 7500-65535
  lst[[i]][lst[[i]] == 0 ] <- NA
  #lst[[i]][lst[[i]] < 7500 & lst[[1]] > 65535] <- NA
  
  # scale factor = 0.02
  lst_1_conv <- lst[[i]]*0.02
  
  # convert to °C
  lstc <- lst_1_conv - 273.15 
  print(i)
  return(lstc)
})

# project rasters
lst_cp <- lapply(seq(lst_c), function(i){
  projectRaster(lst_c[[i]], crs = antaproj)
})


# get extent of files
MODext <- lapply(seq(lst_cp), function(i){
  p <- as(extent(lst_cp[[i]]), 'SpatialPolygons')
  crs(p) <- antaproj
  p
})
m <- do.call(bind, MODext)


# make a template to force 1x1km pixels
tmplras <- lst_cp[[1]]
tmplras
extent(tmplras) <- extent(m)
res(tmplras) <- c(1000, 1000)
tmplras[] <- 1

# resample all rasters to 1km x 1km resolution  
lst_res <- lapply(seq(lst_cp), function(i){
  print(i)
  resample(lst_cp[[i]], tmplras)
})

# save projected and resampled rasters
for(i in seq(lst_res)){
  print(i)
  writeRaster(lst_res[[i]], paste0(datloc, "LST_2018_01_19/", names(lst_res[[i]]), i, ".tif"), format="GTiff", 
              overwrite=T)
}

# # read converted to °C, projected and resampled tifs back in
# lst_cp <- lapply(seq(grep(list.files(path=paste0(datloc, "LST_2018_01_19/"), full.names=T), pattern='res', inv=T, value=T)), function(i){
#   f <- grep(list.files(path=paste0(datloc, "LST_2018_01_19/"), full.names=T), pattern='res', inv=T, value=T)
#   raster(f[i])
# })
# 





# read converted to °C, projected and resampled tifs back in
fls <- list.files(path=paste0(datloc, "LST_2018_01_19/"), full.names=T, pattern='new')
lst_res <- lapply(seq(fls), function(i){
  raster(fls[i])
})



############# PATCH IMAGES ####################################################################################

# write mosaic command 
mrg <- character()
for(i in seq(lst_res)){
  mrg[i] <- paste0("lst_res[[", i, "]]")
}
#mrg[length(mrg)] <- paste0("lst_res[[", length(mrg), "]]")
mrg <- paste(mrg, sep="", collapse=",") 
cm <- paste("raster::mosaic(", mrg, ", tolerance=0.9, filename=paste0(datloc, \"testmosaic_MODIS.tif\"), fun=mean, overwrite=T, overlap=T, ext=NULL)")

mosaic <- eval(parse(text=cm))
mosaic[mosaic < -90] <- NA # correct for too low values

writeRaster(mosaic, paste0(datloc, "MDV_MODIS_LST_Mosaic.tif"), format="GTiff", 
            overwrite=T, bylayer=T)

# visualize mosaic
mapview(mosaic, col.regions = viridis(500), legend = TRUE)

############ MAKE A RASTER WHICH GIVES INFO ON INPUT RASTER ##########################################################

mosaic <- raster(paste0(datloc, "MDV_MODIS_LST_Mosaic.tif"))

# find max bounding box
newextent <- compbb(lst_res)

# bring them all to new extent
lst_ex <- lapply(seq(lst_res), function(i){
  crop(lst_res[[i]], newextent)
})

lst_ex <- stack(lst_ex)

# put in original names
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

nums <- as.numeric(gsub("[^0-9]", "", names(lst_ex))) # take just the numeric parts of the name
orgnam <- lapply(seq(lst), function(i){
  x<-names(lst[[nums[i]]])
  print(i)
  x
})

# check if everything went well
i <- 19
orgnam[[i]]
names(lst[[nums[i]]])

# put in original names
for(i in seq(nlayers(lst_ex))){
  names(lst_ex[[i]]) <- orgnam[[i]]
}

# test
res <- names(lst_ex)
c <- sapply(seq(lst), function(i){
  names(lst[[nums[i]]])
})

res == c

lst_s <- lst_ex

## crop everything to aoi - perhaps here? 
plot(lst_s[[1]])
plot(aoianta, add=T)

lst_s <- crop(lst_s, aoianta)


################ MAKE DATE RASTERS #################################

fnams <- names(lst_s)

utcdates <- lapply(seq(fnams), function(i) {
  fnam <- fnams[i]
  # get UTC date from fnam
  su <- strsplit(fnam, "A")
  su <- su[[1]][length(su[[1]])]
  org <- paste0(substring(su, 1,4), "-01-01")
  utcday <- as.Date((as.numeric(substring(su, 5,7))-1), origin=org)
  
  if(grepl("v", su)){
    utctime <- NULL
    utcdate <- utcday
  } else {
    utctime <- paste0(substring(su, 9, 10), ":", substring(su, 11, 12))
    utcdate <- strptime(paste0(utcday,utctime), format='%Y-%m-%d %H:%M', tz="UTC")
  }
})

# test
v <- NULL
for(i in seq(utcdates)){
  v[i] <- as.character(utcdates[[i]])
}
data.frame(fnams=fnams, utc = v)

# 
library(lubridate)


# function to take minutes, hours and days out of date format
dates <- utcdates

datestodoymod <- function(dates){
  
  doy <- sapply(seq(dates), function(i){
    fnam <- fnams[i]
    # get UTC date from fnam
    su <- strsplit(fnam, "A")
    su <- su[[1]][length(su[[1]])]
    doy <- substring(su, 5,7)
  })
  
  minutes <- sapply(seq(dates), function(i){
    minute(dates[[i]])
  })
  
  hours <- sapply(seq(dates), function(i){
    hour(dates[[i]])
  })
  
  
  dateras <- lapply(seq(dates), function(i){
    d <- lst_s[[i]]
    d[!is.na(d)] <- doy[[i]]
    mod <- lst_s[[i]]
    mod[!is.na(mod)] <- (hours[[i]]*60)+minutes[[i]]
    stack(d, mod)
  })
  
  drs <- stack(dateras)
  dayras <- subset(drs, c(seq(1,nlayers(drs), by=2)))
  timeras <- subset(drs, c(seq(2,nlayers(drs), by=2)))
  
  minutesofday <- (hours*60)+minutes
  
  return(list(dayofyear = doy, minutesofday = minutesofday, 
              dayofyearras = dayras, minutesofdayras = timeras))
}


MODdate <- datestodoymod(dates)

# make a raster with min of time range and max of time range 
# and one with amount of rasters going into the pixel



emptlay <- lapply(seq(nlayers(MODdate$minutesofdayras)), function(i) {
  any(!is.na(MODdate$minutesofdayras[[i]])[]==1)
})

sell <- unlist(emptlay)
b <- seq(1:length(sell))
sel <- b[sell]

subtimeras <- subset(MODdate$minutesofdayras, sel)
plot(subtimeras)

# to get amount of available values
nonna <- raster(apply(as.array(subtimeras), 1:2, function(x) length(na.omit(x))))
nv <- nonna[]
nonnares <- subtimeras[[1]]
nonnares[] <- nv
plot(nonnares)
nonnares

# min and max to get time range
mi <- min(subtimeras, na.rm = T)
ma <- max(subtimeras, na.rm=T)
diff <- ma-mi

writeRaster(nonna, "E:/Antarctica/MODIS/date/amount_available_data.tif", format="GTiff", overwrite=T)
writeRaster(mi, "E:/Antarctica/MODIS/date/min_time.tif", format="GTiff", overwrite=T)
writeRaster(ma, "E:/Antarctica/MODIS/date/max_time.tif", format="GTiff", overwrite=T)
writeRaster(diff, "E:/Antarctica/MODIS/date/time_range.tif", format="GTiff", overwrite=T)


# find max bounding box
tl <- list(nonnares, mi, ma, diff)

newextent <- compbb(tl)

# bring them all to new extent
tl_ex <- lapply(seq(tl), function(i){
  crop(tl[[i]], newextent)
})


tstack <- stack(nonnares, mi, ma, diff)
names(tstack) <- c("sum_av", "min_t", "max_t", "t_range")
writeRaster(tstack, "E:/Antarctica/MODIS/date/time_rasters.tif", format="GTiff", overwrite=T)


############# GOODNESS OF FIT OF ACQUISITION TIME (L8 / MODIS) ##############################

timeex <- data.frame(extract(tstack, extent(tstack)))

L84 <- "2018-01-19 20:38:44"
L85 <- "2018-01-19 20:39:08"

l8time <- c(L84, L85)

L8date <- lapply(seq(l8time), function(i){
    strptime(l8time[i], format='%Y-%m-%d %H:%M:%S', tz="UTC")
})

# convert L8 time to minute of day
L8dates <- datestodoymod(L8date)


timeex$L8time <- rep(mean(L8dates$minutesofday), nrow(timeex))

timeex$fit <- 99
for(i in seq(nrow(timeex))){
  if(any(is.na(timeex[i,]))==F){ # are there NA values interfering
      if(timeex$L8time[i] < timeex$max_t[i] & timeex$L8time[i] > timeex$min_t[i]){
          timeex$fit[i] <- 1 # one where L8 time lies within MODIS timeframe 
      } else {
            timeex$fit[i] <- 0
          }
  }
  print(i)
}
timeex$fit[timeex$fit==99] <- NA


d <- table(timeex$fit)
d[2]/d[1]
d[2]/nrow(timeex) #Anteil Pixel mit �berlappender Zeitspanne

timeex$dev <- 99
for(i in seq(nrow(timeex))){
  if(any(is.na(timeex[i,]))==F){ # are there NA values interfering
    if(timeex$L8time[i] > timeex$max_t[i]){
      timeex$dev[i] <- timeex$L8time[i] - timeex$max_t[i] # one where L8 time lies within MODIS timeframe 
    } else if (timeex$L8time[i] < timeex$min_t[i]) {
      timeex$dev[i] <- timeex$max_t[i] - timeex$L8time[i] 
    } else if (timeex$fit[i]==1){
      timeex$dev[i] <- 0
    }
  }
  print(i)
}
timeex$dev[timeex$dev==99] <- NA


table(timeex$dev)
timediff <- tstack[[1]]

timediff[] <- timeex$dev
writeRaster(timediff, "E:/Antarctica/MODIS/date/time_diff_L8_MOD.tif", format="GTiff", overwrite=T)

hist(timediff[])

