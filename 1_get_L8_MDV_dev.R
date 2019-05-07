

# get L8 data for 
library(getSpatialData)
library(sf)
library(rgdal)
library(raster)

un <- "MaiteLezama"
pw <- "Eos300dmmmmlv"

aoipath <- "C:/Users/mleza/OneDrive/Documents/PhD/work_packages/auto_downscaling_30m/data/aoi_MDV/"
aoi <- readOGR(list.files(aoipath, pattern=".shp", full.names = T))


aoiutm <- spTransform(aoi, crs("+proj=utm +zone=57 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs "))

set_aoi(aoiutm)

time_range <-  c("2018-01-19", "2018-05-19")



## Login to USGS ERS
## Not run: 
#login_USGS("MaiteLezama")

## set archive directory
set_archive("D:/L8_MDV/new/")

## get available products and select one
product_names <- getLandsat_names(username=un, password=pw)
services_avail()
view_aoi()

## query for records for your AOI, time range and product
query <- getLandsat_query(time_range = time_range, 
                          aoi = get_aoi(),
                          username="MaiteLezama", password="Eos300dmmmmlv",
                          name="LANDSAT_8_C1")
                          #product_names[4]
                          #username=un, password=pw

querysmall <- query[query$SceneCloudCover < 40.0,]
querysmall <- querysmall[querysmall$WRSPath <= 62 & querysmall$WRSPath >= 47 &
                     querysmall$WRSRow <= 117 & querysmall$WRSRow >= 114,]

data.frame(querysmall$acquisitionDate, querysmall$WRSPath, querysmall$WRSRow)

# ## preview a record
# x=1
# 
# getLandsat_preview(querysmall[x,])
# x <- x+1
# excludeim <- c(1)
# querysmall <- querysmall[!excludeim]

## download with level "l1" (will direct to AWS automaticaly)

for(i in seq(nrow(querysmall))){
  getLandsat_data(records = querysmall[i,], level = "l2", source = "auto", 
                         username=un, password=pw)
}




