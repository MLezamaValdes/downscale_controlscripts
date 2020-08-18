################### making test location raster #########################

auxpath <-  "D:/new_downscaling/auxiliary/"
aoipath <- "D:/new_downscaling/aoi/"
  
aux <- stack(paste0(auxpath, "aux_stack_xy_swir67.tif"))
aoi_actually <- readOGR(paste0(aoipath, "Levy_MDV_actually.shp"))

testraster <- aux$spatialblocks
testraster <- mask(testraster, aoi_actually)

#barplot(table(testraster[]))

usb <- unique(testraster)
# set.seed(20)
# testsites <- sample(usb, length(usb)*0.2)
testsites <- c(26, 63, 43, 12, 35, 40, 31, 79,  5,  2, 60, 11)

# GO ON HERE ################
sbtest <- aux$spatialblocks[] %in% testsites
test01 <- ifelse(sbtest, 1,0)
testraster[] <- test01
plot(testraster)
plot(aoi_actually, add=T)

writeRaster(testraster, "D:/new_downscaling/modelling/testsite_raster.tif")
