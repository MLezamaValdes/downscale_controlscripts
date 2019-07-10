

# get L8 data for 
library(getSpatialData)
library(sf)
library(rgdal)
library(raster)
library(getSpatialData)
library(downscaleRS)
library(sf)
library(raster)
library(rgdal)
library(mapview)
library(gdalUtils)
library(RColorBrewer)
library(viridis)

datpath <- "C:/Users/mleza/OneDrive/Documents/PhD/work_packages/auto_downscaling_30m/data/LocST_Adelaide/"

####### GET DATA ONLINE ##############################################################################################


## set aoi and time range for the query
aoipath <- datpath
aoi <- readOGR(list.files(aoipath, pattern=".shp", full.names=T))

# filepath <- "D:/MODIS_MDV/HDF/"
#aoipath <- "C:/Users/mleza/OneDrive/Documents/PhD/work_packages/auto_downscaling_30m/data/aoi_MDV/"
hdfpath <- paste0(datpath, "hdfs/")

## set archive directory
set_archive(datpath)

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

#datloc <- "C:/Users/mleza/OneDrive/Documents/PhD/work_packages/auto_downscaling_30m/data/"

# projections
#antaproj <- "+proj=utm +zone=32 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"


####### GET DATA ONLINE ##############################################################################################

## set aoi and time range for the query
#aoi <- readOGR(list.files(aoipath, pattern="aoi_MDV_new.shp", full.names = T))
aoiutm <- spTransform(aoi, crs("+proj=utm +zone=54 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))
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
getMODIS_preview(query1[3,])

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

################ PROCESS VIEWTIME IN R ########################################################################

# eliminate .met files
file.remove(list.files(outdir, pattern="tif.met$", full.names=T))

# make new folder in datpath
vtpath <- paste0(datpath, "vtr/")

# get converted tif viewtime rasters
vtr <- lapply(seq(filescomp), function(i){
  x <- raster(list.files(vtpath, full.names=T)[i])
  x[x>240] <- NA
  x
}) 

hist(vtr[[1]])

# run locmodisviewtime 
#aoigerm <- spTransform(aoi, crs("+proj=utm +zone=33 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))
aoiad <- spTransform(aoi, crs("+proj=utm +zone=54 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))
writeOGR(aoiad, dsn="C:/Users/mleza/OneDrive/Documents/PhD/work_packages/auto_downscaling_30m/data/aoi_example.shp",
         layer="aoi_example.shp", driver="ESRI Shapefile")
vt <- lapply(seq(vtr), function(i){
  #x <- locmodisviewtime(ra=extent(aoiad), fnam = basename(filescomp)[i], ltz = "Australia/ACT",
   #                     newproj= "+proj=utm +zone=54 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs", viewtime = vtr[[i]])
  
  LocST_UTC(utc=utcdate, LocST=vtr[[1]], lon=NULL) # date from swath
  
  lmvt(fnam=basename(filescomp)[i], viewtime=vtr[[i]], ra=extent(aoiad), ltz="Australia/ACT", 
       aoiproj="+proj=utm +zone=54 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
})



saveRDS(vt, file=paste0(datloc, "viewtime1_2.rds"))

vt <- readRDS(paste0(datloc, "viewtime1_2.rds"))
vt