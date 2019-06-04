

# get L8 data for 
library(getSpatialData)
library(sf)
library(rgdal)
library(raster)

datpath <- "C:/Users/mleza/OneDrive/Documents/PhD/work_packages/auto_downscaling_30m/data/L8_data/"

####### GET DATA ONLINE ##############################################################################################

## Login to USGS ERS
login_USGS("MaiteLezama")

## set aoi and time range for the query
aoipath <- "C:/Users/mleza/OneDrive/Documents/PhD/work_packages/auto_downscaling_30m/data/aoi_MDV/"
aoi <- readOGR(list.files(aoipath, pattern="aoi_MDV_new.shp", full.names = T))
aoiutm <- spTransform(aoi, crs("+proj=utm +zone=57 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs "))
set_aoi(aoiutm)

## set archive directory
set_archive(datpath)

time_range <-  c("2018-01-19", "2018-01-19")

## get available products and select one
product_names <- getLandsat_names(username="MaiteLezama", password = "Eos300dmmmmlv")
product <- "LANDSAT_8_C1" # output without thermal channels
product <- "LANDSAT_MSS_C1"

## query for records for your AOI, time range and product
query <- getLandsat_query(time_range = time_range, name = product,
                         aoi=get_aoi())

## preview a record
#getLandsat_preview(query[4,])

## download records
files <- getLandsat_data(query, level="l1",
                         espa_order=NULL)

####### UNZIP DATA IF LEVEL = "SR" ##############################################################################################

main <- paste0(datpath, "get_data/LANDSAT/SR/")
sdirs <- list.files(main, full.names = T)

# unzip 
for(i in seq(sdirs)){
  untar(list.files(sdirs[i], full.names = T, pattern=".tar.gz"),  compressed = 'gzip', exdir = sdirs[i])
}
