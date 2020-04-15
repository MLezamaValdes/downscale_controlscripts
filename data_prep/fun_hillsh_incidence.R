
################################### ADD SCENES TOGETHER FOR EXTRACTION #######################
aux <- stack(paste0("D:/new_downscaling/clean_data/aux_stack_8m.tif"))
names(aux) <- c("dem", "slope", "twires", "hillshade", "lc", "blockmask")


for(i in seq(aux)){
  plot(aux[[i]], main=names(aux[[i]]))
}

# Package 'solrad'

# make a function for hillshading and generate a stack that will be sorted together with the rest of the scenes

# also: make solar incidence angle

install.packages("GeoLight")
library(GeoLight)
solar(Sys.time())

# tm: a vector of POSIXct times (get from filenames / ???)
sun <- solar(tm)
zenith(sun, lon, lat)


make_L8_MOD_stack <- function(y, m, timethres){
  
  L8scenepath <- paste0(main, "L8/", substring(time_range[[y]][[m]][[1]][[1]], 1, 7), "/")
  modisscenepath <- paste0(modispath, substring(time_range[[y]][[m]][[1]][[1]], 1, 7), "/")
  L8LSTpath <- paste0(L8scenepath, "LST/")
  MODLSTpath <- paste0(modisscenepath, "LST/")
  
  if(length(list.files(L8scenepath, pattern="msel.csv", full.names=T))!=0){
    try(timediff_msel <- read.csv2(list.files(L8scenepath, pattern="msel.csv", full.names=T)))
    try(timediff_comp <- read.csv2(list.files(L8scenepath, pattern="timediff_comp", full.names=T)))
    
    print("read timediff csvs")
    
    if(exists("timediff_msel") & exists("timediff_comp")){
      
      l8r <- list.files(L8LSTpath, full.names=T)
      mr <- list.files(MODLSTpath, pattern="small", full.names=T)
      mr <- mr[grep('tif$', mr)]
      
      
      ########## get stacks of all downloaded LST data in ############
      
      L8ras <- stack(l8r)
      mras <- stack(mr)
      
      print("read stacks")
      
      
      ########## get timediff csvs ############
      tdpath <- paste0(main, "timediff/", substring(time_range[[y]][[m]][[1]][[1]], 1, 7), "/")
      tdf <- list.files(tdpath, full.names = T)
      l8dates <- read.csv2(tdf[1])
      mdates <- read.csv2(tdf[2])
      timediff <- timediff_comp
      
      ########## take only closely matching ############
      tdthres <- timediff[timediff$timediff < timethres,]
      modnames <- substring(as.character(tdthres$MODname), 1, nchar(as.character(tdthres$MODname))-4)
      l8names <- as.character(tdthres$L8name)
      
      
      ########## which in the raster stacks are the closely matching scenes? ############
      mlayer <- sapply(seq(modnames), function(i){
        which(grepl(modnames[i], substring(names(mras), 12,nchar(names(mras)))))
      })
      mlayer <- unlist(mlayer)
      
      llayer <- sapply(seq(l8names), function(i){
        which(grepl(l8names[i], names(L8ras)))
      })
      
      llayer <- unlist(llayer)
      
      # stack
      mstack <- mras[[mlayer]]
      lstack <- L8ras[[llayer]]
      
      print("selected matching scenes")
      
      
      ########## resample MODIS to L8 resolution ############
      print("starting resampling of MODIS")
      
      mstackres <- resample(mstack, template)
      
      print("starting resampling of MODIS")
      
      lstackres <- resample(lstack, template)
      
      #LSTstack <- stack(mstackres, lstack)
      
      ########## put L8 and corresponding MODIS together ############
      
      # L8
      L8stacknams <- substring(names(L8ras), 1, 25)
      
      L8pos <- sapply(seq(nrow(timediff_comp)), function(i){
        which(as.character(timediff_comp$L8name[i]) == L8stacknams)
      })
      
      # MODIS 
      mstacknams <- substring(names(mstack), 12, nchar(names(mstackres)))
      tdiff_dfmnams <- substring(as.character(timediff_comp$MODname), 1, (nchar(as.character(timediff_comp$MODname))-4))
      mpos <- sapply(seq(nrow(timediff_comp)), function(i){
        which(tdiff_dfmnams[i] == substring(mstacknams, 1,40))
      })
      
      L8pos <- as.numeric(unlist(as.character(L8pos)))
      mpos <- as.numeric(unlist(as.character(mpos)))
      
      #### MATCHING #############################
      
      s <- lapply(seq(nrow(timediff_comp)), function(i){
        if(!is.na(L8pos[i]) & !is.na(mpos[i])){ # if there is a raster matching this position
          valuecheckL8M <- c(L8ras[[L8pos[i]]]@data@min, L8ras[[L8pos[i]]]@data@max,
                             mstackres[[mpos[i]]]@data@min, mstackres[[mpos[i]]]@data@max)
          if(all(is.finite(valuecheckL8M))){ # if the raster is not empty
            x <- stack(lstackres[[L8pos[i]]], mstackres[[mpos[i]]])
          }
        }
      })
      
      snotnull <- sapply(seq(s), function(i){
        !is.null(s[[i]])
      })
      
      allstacks <- stack(s[snotnull])
      
      print("matched scenes")
      
      ########## make a hillshading raster ############
      
      # get time from L8
      l8dates$time
      
      print("starting to write stack")
      
      writeRaster(allstacks, paste0(cddir, "L_MOD_", substring(time_range[[y]][[m]][[1]][[1]], 1, 7), ".tif"))
      
    } else {
      print("No scenes for this time step")
    }
    
  } else {
    print("no scenes available for stacking")
  }
  
}