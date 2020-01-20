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
par(mfrow=c(1,2))
for(i in seq(nrow(matchdf))){
    plot(L8ras[[matchdf[i,"L8stackpos"]]], main=paste(matchdf[i,"L8name"], "/", i), 
       sub=matchdf[i,"L8time"], cex.main=0.7)
  plot(mras[[matchdf[i,"mrstackpos"]]], main=matchdf[i,"MODname"], cex.main=0.7)
}

# make a comparsion image


# resample to 30m 
modl8res <- lapply(seq(nrow(matchdf)), function(i){
   resample(mras[[matchdf[i,"mrstackpos"]]], L8ras[[matchdf[i,"L8stackpos"]]])
})




satstack <- stack(L8ras[[2]], modl8resval)


# stack respective scenes
# extract data
# merge datasets for all matched scenes

