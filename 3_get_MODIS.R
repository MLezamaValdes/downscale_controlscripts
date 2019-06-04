
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
aoipath <- "C:/Users/mleza/OneDrive/Documents/PhD/work_packages/auto_downscaling_30m/data/aoi_MDV/"
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
aoi <- readOGR(list.files(aoipath, pattern="aoi_MDV_new.shp", full.names = T))
aoiutm <- spTransform(aoi, crs("+proj=utm +zone=57 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs "))
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

# get converted tif files
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


# # view all of them - does not really make sense
# mvc <- character()
# for(i in seq(lst_c)){
#   mvc[i] <- paste0("mapview(lst_c[[", i, "]])+")
# }
# mvc[length(mvc)] <- paste0("mapview(lst_c[[", length(mvc), "]])")
# 
# c = parse(text = mvc) 
# eval(c) 

# project rasters
lst_cp <- lapply(seq(lst_c), function(i){
  projectRaster(lst_c[[i]], crs = antaproj)
})

# # look at properties
# lapply(seq(lst_cp), function(i){
#   res(lst_cp[[i]])
# })

# make a template to force 1x1km pixels
tmplras <- lst_cp[[1]]
res(tmplras) <- c(1000, 1000)
tmplras[] <- 1

# resample all rasters to 1km x 1km resolution  
lst_res <- lapply(seq(lst_cp), function(i){
  resample(lst_cp[[i]], tmplras)
})

# save projected and resampled rasters
for(i in seq(lst_res)){
  writeRaster(lst_res[[i]], paste0(datloc, "LST_2018_01_19/lst_proj_res", i, ".tif"), format="GTiff", 
              overwrite=T)
}

# # read them back in
# lst_cp <- lapply(seq(grep(list.files(path=paste0(datloc, "LST_2018_01_19/"), full.names=T), pattern='res', inv=T, value=T)), function(i){
#   f <- grep(list.files(path=paste0(datloc, "LST_2018_01_19/"), full.names=T), pattern='res', inv=T, value=T)
#   raster(f[i])
# })

################ PROCESS VIEWTIME IN R ########################################################################

# eliminate .met files
#file.remove(list.files(outdir, pattern="tif.met$", full.names=T))

# get converted tif viewtime rasters
vtr <- lapply(seq(filescomp), function(i){
  x <- raster(list.files(outdir, pattern="View", full.names=T)[i])
  x[x>240] <- NA
  x
}) 

hist(vtr[[1]])

# run locmodisviewtime 
aoianta <- spTransform(aoi, crs(antaproj))
vt <- lapply(seq(vtr), function(i){
  x <- locmodisviewtime(ra=extent(aoianta), fnam = basename(filescomp)[i], ltz = "Pacific/Auckland",
                   newproj= antaproj, viewtime = vtr[[i]])
  if(typeof(x)=="list"){
    print(x$fnam_UTC[1]- x$utc_frm_LocST[1])
  }
  return(x)
})

saveRDS(vt, file=paste0(datloc, "viewtime1_2.rds"))

vt <- readRDS(paste0(datloc, "viewtime1_2.rds"))
vt

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

# find max bounding box
newextent <- compbb(lst_res)

# bring them all to new extent
lst_ex <- lapply(seq(lst_res), function(i){
  crop(lst_res[[i]], newextent)
})

lst_s <- stack(lst_ex)

# find out which rasters with valid values are under each pixel
lst_val <- lst_s
for(i in seq(nlayers(lst_val))){
  lst_val[[i]][!is.na(lst_val[[i]])] <- 1
  print(i)
}

# make cell numbers for extraction
cn <- c(1:length(lst_s[[1]][]))

rasloc <- extract(lst_val, cn)

# write / read raster locations table 
write.csv(rasloc, file=paste0(datloc, "MODIS_swath_locations.csv"))
rasloc <- read.csv(file=paste0(datloc, "MODIS_swath_locations.csv"))

# make raster with swath sum values
rassum <- rowSums(rasloc, na.rm = T)
rlocr <- lst_s[[1]]
rlocr[] <- rassum

writeRaster(rlocr, paste0(datloc, "raslocsum.tif"), format="GTiff", 
            overwrite=T, bylayer=T)

# get MODIS dates

# make batchfunction, with option to process viewtime raster and LST_error
# get viewtime rasters
# run locmodisviewtime function to extract the viewtimes
