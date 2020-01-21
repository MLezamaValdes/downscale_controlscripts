L8scenepath <- paste0(main, "L8/", substring(time_range[[y]][[m]][[1]][[1]], 1, 7), "/")
modisscenepath <- paste0(modispath, substring(time_range[[y]][[m]][[1]][[1]], 1, 7), "/")
L8LSTpath <- paste0(L8scenepath, "LST/")
MODLSTpath <- paste0(modisscenepath, "LST/")

timediff_msel <- read.csv2(list.files(L8scenepath, pattern="msel.csv", full.names=T))
timediff_comp <- read.csv2(list.files(L8scenepath, pattern="timediff_comp", full.names=T))


l8r <- list.files(L8LSTpath, full.names=T)
L8ras <- stack(l8r)

mr <- list.files(MODLSTpath, pattern="small", full.names=T)
mr <- mr[grep('tif$', mr)]
mras <- stack(mr)

############## PLOT #######################
#spplot(mras)
#spplot(L8ras)

# which place do those images occupy in the stacks L8ras and mras?
L8stacknams <- substring(names(L8ras), 1, 25)

L8pos <- sapply(seq(nlayers(L8ras)), function(i){
  which(L8stacknams[i] == as.character(timediff_comp$L8name))
})

timediff_comp$L8stackpos <- NA
for(i in seq(L8pos)){
  timediff_comp$L8stackpos[c(L8pos[[i]])] <- i
}

MODmatch <- timediff_comp$MODname[!is.na(timediff_comp$L8stackpos)]

timediff_comp$mrstackpos <- NA
for(i in seq(MODmatch)){
  timediff_comp$mrstackpos[which(timediff_comp$MODname== MODmatch[i])] <- i
}

timediff_comp$dupl <- NULL
timediff_comp$Mdupl <- NULL

matchdf <- timediff_comp[complete.cases(timediff_comp),]
write.csv2(matchdf, paste0(L8scenepath, "matchdf_final.csv"))

# par(mfrow=c(1,2))
# for(i in seq(nrow(matchdf))){
#     plot(L8ras[[matchdf[i,"L8stackpos"]]], main=paste(matchdf[i,"L8name"], "/", i), 
#        sub=matchdf[i,"L8time"], cex.main=0.7)
#   plot(mras[[matchdf[i,"mrstackpos"]]], main=matchdf[i,"MODname"], cex.main=0.7)
# }

# make a comparsion image


# resample to 30m 
modl8res <- lapply(seq(nrow(matchdf)), function(i){
   x <- resample(mras[[matchdf[i,"mrstackpos"]]], L8ras[[matchdf[i,"L8stackpos"]]])
   x[x<100] <- NA
})

# stack respective scenes
L8MODstacks <- lapply(seq(nrow(matchdf)), function(i){
  stack(L8ras[[matchdf[i,"L8stackpos"]]], modl8res[[i]])
})

allstacks <- stack(L8MODstacks)

# add terrain data to stack

# stack MODIS and L8, DEM, TWI, (aspect), hillshade, spatial blocks for CV
dempath <- "D:/new_downscaling/tiles_westcoast/"
dem <- raster(paste0(dempath, "DEM_8m_", areaname,"_clean_filled_15.tif"))
blockmask <- raster(paste0(dempath, "blockmask.tif"))
slope <- raster(paste0(dempath, "slopeMDV.tif"))
hillsh <- raster(paste0(dempath, "hillshading_8m.tif"))
hillshade <- crop(hillsh, dem)
twipath <- "D:/Antarctica/runoff_paths/"
lc <- raster(paste0(main, "Rock_outcrop_ras_", areaname, ".tif"))
lc[lc==0.94] <- 1 #(rock)
lc[lc==0.97] <- 0 #(snow and ice) 
lc <- resample(lc, dem)
twi <- raster(paste0(twipath, "TWI_Rslope.tif"))
twires <- resample(twi, dem)

s <- stack(dem, slope, twires, hillshade, lc, blockmask)


# resample s to fit with satstack
sres <- resample(s, L8MODstacks[[1]])
sresmask <- mask(sres, aoianta)
# save terrain data stack
writeRaster(s, paste0(main, paste0(main, "_auxiliary_data_8m.tif")))
writeRaster(sresmask, paste0(main, paste0(main, "_auxiliary_data_30m.tif")))

sres <- stack(paste0(main, paste0(main, "_auxiliary_data_30m.tif")))


stack.complete <- stack(allstacks,sres)
writeRaster(stack.complete, overwrite=T,paste0(L8scenepath, areaname, "_", substring(time_range[[y]][[m]][[1]][[1]], 1, 7),"_complete.tif"))

stack.complete <- stack(paste0(L8scenepath, areaname, "_", substring(time_range[[y]][[m]][[1]][[1]], 1, 7),"_complete.tif"))

# extract data
names(stack.complete) <- c(names(allstacks),"DEM", "slope","TWI","hillsh","lc","blockmask")

# extract
exdat <- extract(stack.complete, aoianta)
saveRDS(exdat, paste0(L8scenepath, "exdat.rds"))

# select what's auxiliary and what satellite data
sat <- exdat[[1]][,c(1:(ncol(exdat[[1]])-6))]
aux <- exdat[[1]][,c((ncol(exdat[[1]])-5):ncol(exdat[[1]]))]

x <- seq(ncol(sat))
ls <- x[lapply(x, "%%", 2) != 0]
mod <- x[lapply(x, "%%", 2) == 0]

# bring dataset into correct form
satscenestacksex <- lapply(seq(ncol(sat)/2), function(i){

  # select which go together
  pair <- sat[,c(ls[i],mod[i])]
  
  # take MODIS date
  id <- rep(substring(colnames(pair)[2], 22, 33),length(pair[,1]))
  pairid <- cbind(id, pair, aux)

})

mrg <- character()
for(i in seq(length(satscenestacksex))){
  mrg[i] <- paste0("satscenestacksex[[", i, "]]")
}
mrg <- paste(mrg, sep="", collapse=",")
cm <- paste("rbind(", mrg, ")")
satord <- eval(parse(text=cm))

# write csv dataset for respective month
write.saveRDS(satord, paste0(L8scenepath, areaname, "_", substring(time_range[[y]][[m]][[1]][[1]], 1, 7),"_extraction_ordered.rds"))
csv2(satord, paste0(L8scenepath, areaname, "_", substring(time_range[[y]][[m]][[1]][[1]], 1, 7),"_extraction_ordered.csv"))
