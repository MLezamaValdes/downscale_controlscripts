
####### LOAD PACKAGES, SET PATHS ###################

library(getSpatialData)
library(downscaleRS)
library(sf)
library(raster)
library(rgdal)
library(mapview)
library(gdalUtils)

# filepath <- "D:/MODIS_MDV/HDF/"

hdfpath <- "D:/MODIS_MDV/2018_01_19/hdfs/"

####### GET DATA ONLINE ###################

# # projections
# l8proj <- crs("+proj=stere +lat_0=-90 +lat_ts=-71 +lon_0=0 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m
# +no_defs +ellps=WGS84 +towgs84=0,0,0")
# latlon = CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0')
# wgs84 <- CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs ')
# sinus <- CRS('+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs')
# lambert <- CRS('+proj=laea +lat_0=45.5 +lon_0=-114.125 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs')
# isinus <- CRS('+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs')



## set aoi and time range for the query
aoipath <- "C:/Users/mleza/OneDrive/Documents/PhD/work_packages/auto_downscaling_30m/data/aoi_MDV/"
aoi <- readOGR(list.files(aoipath, pattern="aoi_MDV_new.shp", full.names = T))

# aoil8 <- spTransform(aoi, l8proj)
# aoisinus <- spTransform(aoi, isinus)
aoiutm <- spTransform(aoi, crs("+proj=utm +zone=57 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs "))
# aoilamb <- spTransform(aoi, lambert)
# aoilatlon <- spTransform(aoi, latlon)
# aoiwgs <- spTransform(aoi, wgs84)

set_aoi(aoiutm)
time_range <-  c("2018-01-19", "2018-01-19")

## Login to USGS ERS
## Not run:
#login_USGS("username")

## set archive directory
set_archive("D:/MODIS_MDV/2018_01_19/")

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

# new test on Laptop
#filepath <- "C:/Users/mleza/OneDrive/Documents/PhD/work_packages/auto_downscaling_30m/data/MODIS_test/"
#files <- list.files(filepath, pattern="MOD11_", full.names = T)

# # for automatically downloaded images
# dirs1 <- list.files(filepath, pattern="MOD11_")
# dirs2 <- list.files(filepath, pattern="MYD11_")
# files <- list.files(paste0(filepath, dirs), pattern=".hdf$", full.names = T)


# put all hdfs into one folder
nl1 <- paste0(hdfpath, basename(files1))
file.copy(from=files1, to=nl1, 
          overwrite = TRUE)
nl2 <- paste0(hdfpath, basename(files2))
file.copy(from=files2, to=nl2, 
          overwrite = TRUE)


######### BATCH TRANLATING SWATH TO GEOTIFF WITH HEG TOOL ######################
# make an indir directory with the hdf files to batch convert and an outdir directory with the prm template file, 
# where output files will be written to


# batchrunpath needs to be where MyHEG_batchScript.bat is located
batchrunpath <- "C:/Users/mleza/HEG/HEG_Win/bin/BatchRunning/BatchRunning/"
indir <- paste0(batchrunpath, "indir/") 
outdir <- paste0(batchrunpath, "outdir/")

# get template prm file
tpl <- read.delim(list.files(outdir, pattern="unix.prm", full.names = T),
                       sep="=", col.names = c("nam", "val"), header = F, stringsAsFactors = F)

# run HEG tool to tranlate swaths to geotiff 
tplpath <- list.files(outdir, pattern="unix.prm", full.names = T)

# list all files in hdf dir
filescomp <- list.files(hdfpath, full.names=T)

# run HEG tool
runheg(files=filescomp, indir, outdir, tplpath)

lst <- lapply(seq(filescomp), function(i){
  raster(list.files(outdir, pattern=".tif$", full.names=T)[i])
}) 


# convert values to 
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


# view all of them 
mvc <- character()
for(i in seq(lst_c)){
  mvc[i] <- paste0("mapview(lst_c[[", i, "]])+")
}
mvc[length(mvc)] <- paste0("mapview(lst_c[[", length(mvc), "]])")

c = parse(text = mvc) 
eval(c) 

# patch them
datloc <- "C:/Users/mleza/OneDrive/Documents/PhD/work_packages/auto_downscaling_30m/data/"
m <- do.call(rastermerge, lst_c)
mosaic <- raster::merge(lst_c[[1]], lst_c[[2]],
                tolerance=0.1, filename=paste0(datloc, "testmosaic_MODIS.tif"), overwrite=T, overlap=T, ext=NULL)

writeRaster(mosaic, "E:/L8_MDV/mosaic/testmosaic.tif", format="GTiff", 
            overwrite=T, bylayer=T)

mapview(mosaic[[1]])