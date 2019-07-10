

# calc slope for TWI in Saga and tranlate back


library(gdalUtils)
library(raster)

path_saga_norm <- "C:/OSGeo4W64/apps/saga-ltr/"
sagaCmd <- paste0(path_saga_norm, "saga_cmd.exe")

saga_outpath <- "E:/Antarctica/runoff_paths/"


dem <- raster("E:/Antarctica/DEM/MDV_8m_filled.tif")
slope <- slopeAspect(dem, filename='E:/Antarctica/DEM/slope.tif', out='slope')
writeRaster(slope, "E:/Antarctica/runoff_paths/slope_R.tif")

p <- "E:/Antarctica/runoff_paths/slope_R.tif"
gdalUtils::gdalwarp(p, paste0(saga_outpath,"slope_R.sdat"), 
                    overwrite=TRUE,  of='SAGA')

gdalUtils::gdalwarp(paste0(saga_outpath,"TWI_Rslope_completeGrid.sdat"),
                    paste0(saga_outpath,"TWI_Rslope.tif") , 
                    overwrite=TRUE) 


