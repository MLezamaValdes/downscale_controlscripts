## Load packages
library(getSpatialData)
library(sf)
library(raster)
library(rgdal)
library(gdalUtils)

## set aoi and time range for the query

# projections
l8proj <- crs("+proj=stere +lat_0=-90 +lat_ts=-71 +lon_0=0 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m+no_defs +ellps=WGS84 +towgs84=0,0,0")
latlon = CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0')
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

# 
# set_aoi(aoiutm)
# time_range <-  c("2018-01-01", "2018-01-30")
# 
# ## Login to USGS ERS
# ## Not run: 
# #login_USGS("username")
# 
# ## set archive directory
# set_archive("D:/MODIS_MDV/")
# 
# ## get available products and select one
# product_names <- getMODIS_names(username="MaiteLezama", password = "Eos300dmmmmlv")
# product <- grep("MOD11_L2_V6", product_names, value = T)
# 
# ## query for records for your AOI, time range and product
# query <- getMODIS_query(time_range = time_range, name = product,
#                         username="MaiteLezama", password = "Eos300dmmmmlv",
#                         aoi=get_aoi())
# 
# ## preview a record
# getMODIS_preview(query[9,])
# 
# ## download records 1 and 2
# files <- getMODIS_data(query[c(1,2,8,9),])

# look for detailed info 
gi <- gdalinfo(files[1])
sds <- get_subdatasets(files[1])


gdalinfo(sds[1])

gdalwarp -of GTIFF -tps -t_srs EPSG:4326 HDF4_EOS:EOS_SWATH:"MOD04_L2.A2003001.0005.051.2010313005421.hdf":mod04:Image_Optical_Depth_Land_And_Ocean 2003.tif

gdalwarp(sds[1], dstfile = paste0(filepath, "test_warp.tif"), t_srs = "EPSG:4326", tps = T, overwrite = T, of="GTiff")
test <- raster(paste0(filepath, "test_warp.tif"))
mapview(test)

filepath <- "D:/MODIS_MDV/get_data/MODIS/"
gdal_translate(sds[1], dst_dataset = paste0(filepath, "LST.tif"))

# Load and plot the new .tif
LST <- raster(paste0(filepath, "LST.tif"))
crs(LST) <- latlon
mapview(LST)

# get HEG-converted LST
path <- "D:/MODIS_MDV/get_data/MODIS/MOD11_L2.A2017213.0035.006.2017214084534/"
LST_conv <- raster(list.files(path, pattern="LST.tif$", full.names = T))

lstlatlon <- projectRaster(LST, crs=latlon)

plot(lstlatlon)
plot(aoilatlon, add=T)

crs(aoilatlon)
plot(aoilatlon, add=T)
