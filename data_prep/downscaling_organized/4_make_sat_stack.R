


make_L8_MOD_stack <- function(y, m, timethres){
  
  L8scenepath <- paste0(main, "L8/", substring(time_range[[y]][[m]][[1]][[1]], 1, 7), "/")
  modisscenepath <- paste0(modispath, substring(time_range[[y]][[m]][[1]][[1]], 1, 7), "/")
  L8LSTpath <- paste0(L8scenepath, "LST/")
  MODLSTpath <- paste0(modisscenepath, "LST/")
  
  if(length(list.files(L8scenepath, pattern="timediff_df.csv", full.names=T))!=0){
    #try(timediff_msel <- read.csv2(list.files(L8scenepath, pattern="msel.csv", full.names=T)))
    try(timediff_df <- read.csv2(list.files(L8scenepath, pattern="timediff_df.csv", full.names=T)))
    
    print("read timediff csvs")
    
    if(exists("timediff_df")){
      
      # make file lists of all downloaded LST scenes for L and M
      
      l8r <- list.files(L8LSTpath, full.names=T, pattern='bt_band10')
      mr <- list.files(MODLSTpath, pattern="proj_c_warp", full.names=T)
      

      unique(timediff_df$L8scene)
      unique(basename(l8r))
      
      # check in timedf_df for matching scenes
    
      ############ MATCH ACUTALLY AVAILABLE SCENES FROM FILEPATH WITH TIMEDIFF-DF ######################
      mradapted <- substring(basename(mr), 17, 42)
      Mpostimediff <- which(timediff_df$modscene %in% gsub(pattern="\\.",replacement= "_", x=mradapted))
      
      Lpostimediff <- which(timediff_df$L8scene %in% substring(basename(l8r), 1, 40))
      posl8r <- which(substring(basename(l8r), 1, 40) %in% timediff_df$L8scene)
      
      timediff_MandL <- intersect(Lpostimediff,Mpostimediff)
      timediff_df_act <- timediff_df[timediff_MandL,]
      
      l8r <- l8r[posl8r]
      posmr <- which(gsub(pattern="\\.",replacement= "_", x=mradapted) %in% timediff_df_act$modscene)
      mr <- mr[posmr]

      write.csv2(timediff_df_act, paste0(L8scenepath, "timediff_df_stacks.csv"))


      if(length(mr)!=0){
        mr <- mr[grep('tif$', mr)]
        
        
        ########## get stacks of all downloaded LST data in ############
        
        L8ras <- stack(l8r)
        mras <- stack(mr)
        
        # do I need the length to match already foror easier assembly later on? 
        names(mras) <- substring(basename(mr), 17, 42)
        names(L8ras) <- substring(basename(l8r), 1,40)
        
        print("read stacks")

        timediff <- timediff_df_act
        
        ########## take only closely matching ############
        tdthres <- timediff[timediff$timediff < timethres,]
        
        ########## which in the raster stacks are the closely matching scenes? ############
        
        
        
        # # which of timediff_df$MODname is actually in names(mras)? only those scenes available, mark in timediff_df
        # MODavailable <- sapply(seq(nrow(tdthres)), function(i){
        #   print(i)
        #   pos <- which(grepl(substring(as.character(tdthres$MODname[i]), 1, 40), names(mras)))
        #   r1 <- any(grepl(substring(as.character(tdthres$MODname[i]), 1, 40), names(mras))) 
        #   if(r1){
        #     r2 <- any(!is.na(minValue(mras[[pos]])| maxValue(mras[[pos]]))) # values not NA NA - empty raster in this area 
        #     res <- r1&r2
        #   }
        #   r1
        # })
        # 
        # 
        # # system.time({range(mras[[pos]][])})
        # # system.time({min(mras[[pos]][])})
        # # system.time({minValue(mras[[pos]])})
        # 
        # 
        # L8available <- sapply(seq(nrow(tdthres)), function(i){
        #   any(grepl(substring(tdthres$L8name[i], 1, 40), names(L8ras)))
        # })
        
        tdthres$Mcomp <- TRUE
        tdthres$Lcomp <- TRUE
        
        
        #tdthres[tdthres$Mcomp==FALSE,]
        
        tdthres <- tdthres[tdthres$Lcomp == T & tdthres$Mcomp ==T,]
        
        modnames <- tdthres$modscene
        l8names <- as.character(tdthres$L8scene)
        
        write.csv2(tdthres, paste0(L8scenepath, "timediff_comp_comp.csv"))
        
        #### NOW THE MATCHING AND STACKING IS HAPPENING ####################################
        
        
        # to do: change sublines to points etc
        
        
        
        
        posmr <- which(gsub(pattern="\\.",replacement= "_", x=mradapted) %in% timediff_df_act$modscene)
        
        if(length(tdthres$modscene) > 0){
          # mlayer and llayer stem from tdthres
          mlayer <- sapply(seq(modnames), function(i){
            which(grepl(modnames[i], gsub(pattern="\\.",replacement= "_",names(mras))))[1]
          })

          llayer <- sapply(seq(l8names), function(i){
            which(grepl(l8names[i], names(L8ras)))[1]
          })

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




