## Load packages
library(getSpatialData)
library(sf)
library(raster)
library(rgdal)
library(mapview)
library(gdalUtils)

filepath <- "D:/MODIS_MDV/HDF/"
hdfpath <- "D:/MODIS_MDV/hdfs/"

# try with Flo's suggestion
#remotes::install_github("environmentalinformatics-marburg/reset")
library(reset)

## set aoi and time range for the query

# projections
l8proj <- crs("+proj=stere +lat_0=-90 +lat_ts=-71 +lon_0=0 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m
+no_defs +ellps=WGS84 +towgs84=0,0,0")
latlon = CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0')
wgs84 <- CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs ')
sinus <- CRS('+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs')
lambert <- CRS('+proj=laea +lat_0=45.5 +lon_0=-114.125 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs')
isinus <- CRS('+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs')



# get and project area of interest
aoipath <- "C:/Users/mleza/OneDrive/Documents/PhD/work_packages/auto_downscaling_30m/data/aoi_MDV/"
aoi <- readOGR(list.files(aoipath, pattern=".shp", full.names = T))

aoil8 <- spTransform(aoi, l8proj)
aoisinus <- spTransform(aoi, isinus)
aoiutm <- spTransform(aoi, crs("+proj=utm +zone=57 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs "))
aoilamb <- spTransform(aoi, lambert)
aoilatlon <- spTransform(aoi, latlon)
aoiwgs <- spTransform(aoi, wgs84)

set_aoi(aoiutm)
time_range <-  c("2018-01-01", "2018-01-30")

## Login to USGS ERS
## Not run:
#login_USGS("username")

## set archive directory
set_archive("D:/MODIS_MDV/")

## get available products and select one
#product_names <- getMODIS_names(username="MaiteLezama", password = "Eos300dmmmmlv")
product <- grep("MOD11_L2_V6", product_names, value = T)

## query for records for your AOI, time range and product
query <- getMODIS_query(time_range = time_range, name = product,
                        username="MaiteLezama", password = "Eos300dmmmmlv",
                        aoi=get_aoi())

## preview a record
getMODIS_preview(query[9,])

## download records 1 and 2

names(query)

files <- getMODIS_data(query[c(1,2,8,9),])


# new test on Laptop
#filepath <- "C:/Users/mleza/OneDrive/Documents/PhD/work_packages/auto_downscaling_30m/data/MODIS_test/"
files <- list.files(filepath, pattern="MOD11_", full.names = T)

# for automatically downloaded images
dirs <- list.files(filepath, pattern="MOD11_")
files <- list.files(paste0(filepath, dirs), pattern=".hdf$", full.names = T)


# put all hdfs into one folder
nl <- paste0(hdfpath, basename(files))
file.copy(from=files, to=nl, 
          overwrite = TRUE)

######### BATCH RUNNING WITH HEG TOOL ######################
# make an indir directory with the hdf files to batch convert and an outdir directory with the prm template file, 
# where output files will be written to

# read prm file
batchrunpath <- "C:/Users/mleza/HEG/HEG_Win/bin/BatchRunning/BatchRunning/"
indir <- paste0(batchrunpath, "indir/") 
outdir <- paste0(batchrunpath, "outdir/")

# get template prm file
tpl <- read.delim(list.files(outdir, pattern="unix.prm", full.names = T),
                       sep="=", col.names = c("nam", "val"), header = F, stringsAsFactors = F)


# make a function out of it

runheg(files, indir, outdir, tpl)



lst <- lapply(seq(files), function(i){
  raster(list.files(outdir, pattern=".tif", full.names=T)[i])
}) 
  

