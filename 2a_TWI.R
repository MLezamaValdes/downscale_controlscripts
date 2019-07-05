

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




# # make a useful indicator for wet soil from TWI
# twi <- raster(paste0(saga_outpath,"TWI_Rslope.tif"))
# plot(twi)
# 
# # make subset for testing
# e <- drawExtent()
# twis <- crop(twi, e)
# 
# plot(twis)
# 
# #twimid <- twi[twi<15 & twi>12]
# 
# twi_f5 <- focal(twis, w=matrix(1/49, nc=7, nr=7))
# plot(twi_f5)
# 
# twir <- terrain(twi_f5, opt="roughness")
# plot(twir)
# q <- quantile(twir[], na.rm=T, probs = c(0.75,0.8,0.99))
# twir[twir<q[3]] <- 0
# plot(twir)
# 
# q <- quantile(twi[], na.rm=T, probs=0.99)
# twi[twi<q] <- 0
# plot(twi)
