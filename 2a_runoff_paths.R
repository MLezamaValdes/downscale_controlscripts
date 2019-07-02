
library(gdalUtils)
library(raster)

path_saga_norm <- "C:/OSGeo4W64/apps/saga-ltr/"
#path_saga_norm <- "C:/Users/mleza/Downloads/saga-7.2.0_x64/saga-7.2.0_x64/"
saga_outpath <- "E:/Antarctica/runoff_paths/"
sagaCmd <- paste0(path_saga_norm, "saga_cmd.exe")

dempath <- paste0(saga_outpath, "DEM_MDV_8m_not_corr.tif")

gdalUtils::gdalwarp(dempath, paste0(saga_outpath,"dem_MDV_s_8m.sdat"), 
                    overwrite=TRUE,  of='SAGA')

# NOT WORKING - WHY?
# cmd_pp <-paste0(sagaCmd, " saga_cmd ta_preprocessor 4 ", 
#                 "-ELEV ", saga_outpath, "dem.sgrd",
#                 " -FILLED ", saga_outpath, "dem_filled.sgrd",
#                 " -FDIR ", 	saga_outpath, "flow_directions.sgrd",
#                 " -WSHED ", saga_outpath, "wathershed_basins.sgrd")
# system(cmd_pp)
# 
# 
# 
# 
# cmd_tah <-paste0(sagaCmd, " saga_cmd ta_hydrology 0",
#                 "-ELEVATION ", saga_outpath, "dem_filled.sgrd",
#                 " -FLOW ", saga_outpath, "flow_accumulation.sgrd",
#                 " -METHOD ", 0,
#                 " -FLOW_LENGTH ", saga_outpath, "flow_length.sgrd")
# system(cmd_tah)



# convert back and load
gdalUtils::gdalwarp(paste0(saga_outpath,"Watershed_Basins.sdat"),
                    paste0(saga_outpath,"Watershed_Basins.tif") , 
                    overwrite=TRUE) 

watershed <- raster(paste0(saga_outpath, "Watershed_Basins.tif"))


gdalUtils::gdalwarp(paste0(saga_outpath,"Watershed_Basins.sdat"),
                    paste0(saga_outpath,"Watershed_Basins.tif") , 
                    overwrite=TRUE) 

watershed <- raster(paste0(saga_outpath, "Watershed_Basins.tif"))

plot(watershed)


# convert valley depth
gdalUtils::gdalwarp(paste0(saga_outpath,"Valley Depth.sdat"),
                    paste0(saga_outpath,"Valley_Depth.tif") , 
                    overwrite=TRUE) 

valdep <- raster(paste0(saga_outpath, "Valley_Depth.tif"))





# make distance to runoff paths as a  raster
require(rgdal)
library(raster)
cn <- readOGR(paste0(saga_outpath, "Channel Network_2.shp"))

gdalUtils::gdalwarp(paste0(saga_outpath,"small_DEM.sdat"),
                    paste0(saga_outpath,"small_DEM.tif") , 
                    overwrite=TRUE) 

r <- raster(paste0(saga_outpath, "small_DEM.tif"))
require(rgeos)
gUnion(cn, cn) # to get one feature from multiple lines and not a huge matrix
dd = gDistance(cn, as(r,"SpatialPoints"), byid=TRUE)
r[] = apply(dd,1,min)
plot(r)

