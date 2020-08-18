
y=1
m=1

ym <- substring(time_range[[y]][[m]][[1]][[1]], 1, 7)
tempdyn <- stack(paste0(cddir, "new_L_MOD_hs_ia", ym, ".tif"))

# hier noch automatisch suchen
tdnam <- read.csv("D:/new_downscaling/clean_data/names_sat_ia_hs_2019-01.csv")
names(tempdyn) <- tdnam$x
aux <- stack("D:/new_downscaling/auxiliary/aux_stack.tif")
names(aux) <- c("dem", "slope", "aspect", "TWI", "soilraster", "landcoverres", "spatialblocks")
extractionpath <- paste0(maindir, "extraction/")

# this should happen separately! TO DO!!!
swiroutpath <- paste0(cddir, "SWIR/")
swir <- raster(paste0(swiroutpath, "swir_tc_", ym, ".tif"))

aux <- stack(aux, swir)

# ADD TIMEDIFFERENCE IN DFSLICES AS A column


# test 

# e <- extent(c(383828.5, 389660.8, -1294332, -1288986))
# tempdyn <- crop(tempdyn, e)
# aux <- crop(aux,e)


#### extract

auxdf <- as.data.frame(extract(aux, aoianta))
write.csv2(auxdf, paste0(extractionpath, "aux_df.csv"))
tmpdyndf <- as.data.frame(extract(tempdyn, aoianta))
write.csv2(tmpdyndf, paste0(extractionpath, "tempdyn", ym,
                            "_df.csv"))

new_package <- seq(1,ncol(tmpdyndf), by=4)
end <- new_package+3

dfslices <- lapply(seq(new_package), function(i){
  x <- tmpdyndf[,new_package[i]:end[i]]
  xnam <- substring(names(x)[3], 4, 18)
  x$time <- xnam
  x$id <- seq(1:nrow(x))
  x <- cbind(x, auxdf)
  names(x) <- c("Landsat", "Modis", "ia", "hs", "time", "id","dem", "slope", "aspect", 
                "TWI", "soilraster", "landcoverres", "spatialblocks", "swir")
  x
})

tddf <- do.call(rbind,dfslices)
tddfcc <- tddf[complete.cases(tddf),]


write.csv2(tddfcc, paste0(extractionpath, "extr_comp_",ym, ".csv"), row.names = F)
