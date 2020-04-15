

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# DOWNLOADED SCENES ARE GATHERED AND MATCHED ACCORDING TO THE TIMEDIFF FILE
#
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


#y=1
#m=1

#++++++++++++++ stack all satellite scenes +++++++++++++++++++++++++++++++++++++
as <- list.files(cddir, pattern="L_MOD_", full.names = T)
stacklist <- lapply(seq(as), function(i){
  stack(as[[i]])
})  
allstacks <- stack(stacklist)
writeRaster(allstacks, paste0(cddir, "L_MOD_all.tif"))
  
##+++++++++++++ add terrain data to stack ++++++++++++++++++++++++++++++++++++++

#stack MODIS and L8, DEM, TWI, (aspect), hillshade, spatial blocks for CV

dempath <- "D:/new_downscaling/tiles_westcoast/"
dem <- raster(paste0(dempath, "DEM_8m_", areaname,"_clean_aoi_filled_mask.tif"))

############################### Sky view factor ##############################################################


############################### incidence angle ##############################################################


twipath <- "D:/Antarctica/runoff_paths/"

hillsh <- raster(paste0(dempath, "hillshading_8m.tif"))
hillshade <- crop(hillsh, dem)

lc <- raster(paste0(main, "Rock_outcrop_ras_", areaname, ".tif"))
lcc <- crop(lc, aoianta)
lccm <- mask(lcc,aoianta)

lccm[lccm==0.94] <- 1 #(rock)
lccm[lccm==0.97] <- 0 #(snow and ice)
#lccm <- resample(lccm, dem)

writeRaster(hillshade, paste0(main, "hillshade.tif"))
writeRaster(lc, paste0(main, "lc.tif"))
twi <- raster(paste0(twipath, "TWI_Rslope.tif"))
twires <- resample(twi, dem)
writeRaster(twires, paste0(main, "TWI.tif"))
blockmask <- raster(paste0(dempath, "blockmask.tif"))
slope <- raster(paste0(dempath, "slopeMDV.tif"))
hillshade <- raster(paste0(main, "hillshade.tif"))
lc <- raster(paste0(main, "lc.tif"))
twi <- raster(paste0(main, "TWI.tif"))
s <- stack(dem, slope, twires, hillshade, lc, blockmask)
writeRaster(s, paste0(cddir, "aux_stack_8m.tif"))


s <- stack(paste0(cddir, "aux_stack_8m.tif"))

# resample s to fit with satstack
sres <- resample(s, allstackedsatellitescenes[[1]])
sresmask <- mask(sres, aoianta)

# save terrain data stack
writeRaster(s, paste0(main, "auxiliary_data_8m.tif"))
writeRaster(sresmask, paste0(main, "auxiliary_data_30m.tif"))

sres <- stack(paste0(main, "_auxiliary_data_30m.tif"))




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
saveRDS(satord, paste0(L8scenepath, areaname, "_", substring(time_range[[y]][[m]][[1]][[1]], 1, 7),"_extraction_ordered.rds"))
write.csv2(satord, paste0(L8scenepath, areaname, "_", substring(time_range[[y]][[m]][[1]][[1]], 1, 7),"_extraction_ordered.csv"))

head(satord)
satord.comp <- satord[complete.cases(satord),]
# write csv dataset for respective month
saveRDS(satord.comp, paste0(L8scenepath, areaname, "_", substring(time_range[[y]][[m]][[1]][[1]], 1, 7),"_extraction_ordered.rds"))
write.csv2(satord.comp, paste0(L8scenepath, areaname, "_", substring(time_range[[y]][[m]][[1]][[1]], 1, 7),"_extraction_ordered.csv"))
