rm(list=ls())

#######    OVERALL CONTROLL SCRIPT    ############################################################################################ 
###########################################################################################################################


########    GENERAL INFO ABOUT AREA AND TIMESTEP & RUN    ###################################################################### 

# newarea can be 0 (no) or 1 (yes). If area is not new, it is assumed that 
# the static portion of the script is already available and doesn't need to be unzipped etc.
newarea <- 0
areaname <- "MDV"


library(rgeos)

#######################    SET PATHS    ###################################################################################### 

scriptpath <- "C:/Users/mleza/OneDrive/Documents/PhD/work_packages/auto_downscaling_30m/downscale_controlscripts/data_prep/"
# E = IRONWOLF
main <- "D:/new_downscaling/data_download_preprocessing/"
dempath <- "D:/new_downscaling/tiles_westcoast/"
aoipath <-  "D:/new_downscaling/aoi/MDV/"
L8datpath <- paste0(main, "L8/")
modispath <- paste0(main, "MODIS/")
tdpath <-paste0(main, "timediff/")

######## set path to AOI
aoip <- list.files(aoipath, pattern="adp.shp", full.names = T)

#### set path to high resolution land polygon
clpath <- "D:/new_downscaling/coastline/Coastline_high_res_polygon/" 

######## set paths for translating to SAGA
path_saga_norm <- "C:/OSGeo4W64/apps/saga-ltr/"
sagaCmd <- paste0(path_saga_norm, "saga_cmd.exe")
saga_outpath <- paste0(main, "SAGA_run/")

######## set paths for batch processing in HEG tool
# make an batchindir directory with the hdf files to batch convert and an batchoutdir directory with 
# the prm template file, where output files will be written to
# batchrunpath needs to be where MyHEG_batchScript.bat is located
batchrunpath <- "C:/Users/mleza/HEG/HEG_Win/bin/BatchRunning/BatchRunning/"
batchindir <- paste0(batchrunpath, "indir/") 
batchoutdir <- paste0(batchrunpath, "outdir/")



########    CALL SETUP    #####################################################################################################

source(paste0(scriptpath, "0a_setup.R"))

########    CALL DOWNLOAD AND PREPROCESSING    #################################################################################

if(newarea==1){
  prepDEM()
}

## Login to USGS ERS
login_USGS("MaiteLezama")


      # y=1
      # m=5
      # time_range[[y]][[m]]
      # getprocessLANDSAT(time_range)
      # getprocessMODIS(time_range)
      # 
y=1
m=2


# from 2019-11 on 
# should be like this when it all works:
for(y in seq(year)){
  for(m in seq(month)){
      getprocessLANDSAT(time_range)
      getprocessMODIS(time_range)
  }
}


# 
# # for(y in c(2)){
# #   for(m in seq(month)){
#       msel <- getprocessLANDSAT(time_range)
#       getprocessMODIS(time_range)
# #   }
# # }



