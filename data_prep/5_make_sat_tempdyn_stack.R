


make_L8_MOD_stack <- function(y, m, timethres){
  
  L8scenepath <- paste0(main, "L8/", substring(time_range[[y]][[m]][[1]][[1]], 1, 7), "/")
  modisscenepath <- paste0(modispath, substring(time_range[[y]][[m]][[1]][[1]], 1, 7), "/")
  L8LSTpath <- paste0(L8scenepath, "LST/")
  MODLSTpath <- paste0(modisscenepath, "LST/")
  
  if(length(list.files(L8scenepath, pattern="msel.csv", full.names=T))!=0){
    #try(timediff_msel <- read.csv2(list.files(L8scenepath, pattern="msel.csv", full.names=T)))
    try(timediff_comp <- read.csv2(list.files(L8scenepath, pattern="timediff_comp.csv", full.names=T)))
    
    print("read timediff csvs")
    
    if(exists("timediff_comp")){
      
      # make file lists of all downloaded LST scenes for L and M
      
      l8r <- grep(list.files(L8LSTpath, full.names=T), pattern='bt_band10', inv=T, value=T) # get those that are cloud_rm.tif
      
      mr <- list.files(MODLSTpath, pattern="small", full.names=T)
      
      if(length(mr)!=0){
        mr <- mr[grep('tif$', mr)]
        
        
        ########## get stacks of all downloaded LST data in ############
        
        L8ras <- stack(l8r)
        
        mrY <- mr[grep('MYD11', mr)]
        mrO <- mr[grep('MOD11', mr)]
        
        if(length(mrY)!=0){
          mrYs <- stack(mrY)
          mrYres <- resample(mrYs, template)
        }
        
        if(length(mrO)!=0){
          mrOs <- stack(mrO)
          mrOres <- resample(mrOs, template)
        }
        
        if(exists("mrOres") & exists("mrYres")){
          mras <- stack(mrYres, mrOres)
        } else if(exists("mrOres")){
          mras <- mrOres
        } else {
          mras <- mrYres
        }
        
        
        names(mras) <- substring(basename(mr), 12, 51)
        names(L8ras) <- substring(basename(l8r), 1,40)
        
        print("read stacks")
        
        
        ########## get timediff csvs ############
        tdpath <- paste0(main, "timediff/", substring(time_range[[y]][[m]][[1]][[1]], 1, 7), "/")
        #tdf <- list.files(tdpath, full.names = T)
        #l8dates <- read.csv2(tdf[1])
        #mdates <- read.csv2(tdf[2])
        timediff <- timediff_comp
        
        ########## take only closely matching ############
        tdthres <- timediff[timediff$timediff < timethres,]
        
        ########## which in the raster stacks are the closely matching scenes? ############
        
        
        
        # which of timediff_comp$MODname is actually in names(mras)? only those scenes available, mark in timediff_df
        MODavailable <- sapply(seq(nrow(tdthres)), function(i){
          print(i)
          pos <- which(grepl(substring(as.character(tdthres$MODname[i]), 1, 40), names(mras)))
          r1 <- any(grepl(substring(as.character(tdthres$MODname[i]), 1, 40), names(mras))) 
          if(r1){
            r2 <- any(!is.na(minValue(mras[[pos]])| maxValue(mras[[pos]]))) # values not NA NA - empty raster in this area 
            res <- r1&r2
          }
          r1
        })
        
        
        # system.time({range(mras[[pos]][])})
        # system.time({min(mras[[pos]][])})
        # system.time({minValue(mras[[pos]])})
        
        
        L8available <- sapply(seq(nrow(tdthres)), function(i){
          any(grepl(substring(tdthres$L8name[i], 1, 40), names(L8ras)))
        })
        
        tdthres$Mcomp <- MODavailable
        tdthres$Lcomp <- L8available
        
        
        #tdthres[tdthres$Mcomp==FALSE,]
        
        tdthres <- tdthres[tdthres$Lcomp == T & tdthres$Mcomp ==T,]
        
        modnames <- substring(as.character(tdthres$MODname), 1, nchar(as.character(tdthres$MODname))-4)
        l8names <- as.character(tdthres$L8name)
        
        write.csv2(tdthres, paste0(L8scenepath, "timediff_comp_comp.csv"))
        
        
        if(length(tdthres$MODname) > 0){
          # mlayer and llayer stem from tdthres
          mlayer <- sapply(seq(modnames), function(i){
            which(grepl(modnames[i], names(mras)))[1]
          })
          mlayer <- unlist(mlayer)
          
          llayer <- sapply(seq(l8names), function(i){
            which(grepl(l8names[i], names(L8ras)))[1]
          })
          llayer <- unlist(llayer)
          
          # stack
          
          mstack <- mras[[mlayer]]
          lstack <- L8ras[[llayer]]
          
          print("selected matching scenes")
          
          ########## resample MODIS to L8 resolution ############
          print("starting resampling of MODIS")
          
          mstackres <- resample(mstack, template)
          
          print("starting resampling of landsat")
          
          lstackres <- resample(lstack, template)
          
          
          ########## put L8 and corresponding MODIS together ############
          
          
          # TO DO: PUT THEM TOGETHER RIGHT AWAY
          
          print("stacking lstack and mstack")
          s <- lapply(seq(nlayers(lstackres)), function(i){
            print(i)
            stack(lstackres[[i]], mstackres[[i]])
          })
          
          print("making satstack")
          satstack <- stack(s)
          
          print("matched scenes")
          
          write.csv2(names(satstack), paste0(cddir, "satstacks/satnames_", substring(time_range[[y]][[m]][[1]][[1]], 1, 7), ".csv"),
                     row.names = F)
          
          print("starting to write sat stack")
          writeRaster(satstack, paste0(cddir, "satstacks/L_MOD", substring(time_range[[y]][[m]][[1]][[1]], 1, 7), ".tif"), overwrite=T)
        } else {
          print("MODIS scenes empty")
        }
        
        
      } else {
        print("No MODIS scenes available")
      }
      
    } else {
      print("No scenes for this time step")
    }
    
  } else {
    print("no scenes available for stacking")
  }
}



########################### RUN #####################################
y=2

#for(y in c(5:7)){
  #for(m in seq(month)){
  for(m in c(5:8)){
    print(c(y,m))
    rasterOptions(tmpdir="D:/new_downscaling/run/")
    make_L8_MOD_stack(y,m,timethres)
    gc()
    file.remove(list.files("D:/new_downscaling/run/", full.names = T))
  }
#}

