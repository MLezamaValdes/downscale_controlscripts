
y=1
m=1

tempdyn <- stack(paste0(cddir, "L_MOD_hs_ia", substring(time_range[[y]][[m]][[1]][[1]], 1, 7), ".tif"))

# hier noch automatisch suchen
tdnam <- read.csv("D:/new_downscaling/clean_data/names_sat_ia_hs_2019-01.csv")
names(tempdyn) <- tdnam$x
aux <- stack("D:/new_downscaling/auxiliary/aux_stack.tif")
names(aux) <- c("dem", "slope", "aspect", "TWI", "soilraster", "landcoverres", "spatialblocks")
extractionpath <- paste0(maindir, "extraction/")


# ADD TIMEDIFFERENCE IN DFSLICES AS A column


# test 

# e <- extent(c(383828.5, 389660.8, -1294332, -1288986))
# tempdyn <- crop(tempdyn, e)
# aux <- crop(aux,e)


#### extract

auxdf <- as.data.frame(extract(aux, e))

tmpdyndf <- as.data.frame(extract(tempdyn, e))
new_package <- seq(1,ncol(tmpdyndf), by=4)
end <- new_package+3

dfslices <- lapply(seq(new_package), function(i){
  x <- tmpdyndf[,new_package[i]:end[i]]
  xnam <- substring(names(x)[3], 4, 18)
  x$time <- xnam
  x$id <- seq(1:nrow(x))
  x <- cbind(x, auxdf)
  names(x) <- c("Landsat", "Modis", "ia", "hs", "time", "id","dem", "slope", "aspect", "TWI", "soilraster", "landcoverres", "spatialblocks")
  x
})

tddf <- do.call(rbind,dfslices)


tddfcc <- tddf[complete.cases(tddf),]


write.csv2(tddfcc, paste0(extractionpath, "extr_", substring(time_range[[y]][[m]][[1]][[1]], 1, 7), ".csv"), row.names = F)
