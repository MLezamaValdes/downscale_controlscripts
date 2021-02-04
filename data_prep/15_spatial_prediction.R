rm(list=ls())

library(ggplot2)
library(viridis)
library(gridExtra)
library(grid)
library(raster)

modelpath <-"C:/Users/mleza/OneDrive/Documents/PhD/work_packages/auto_downscaling_30m/final_150000/"
figurepath <- "C:/Users/mleza/OneDrive/Documents/PhD/work_packages/auto_downscaling_30m/paper/paper_draft/figures/"
rasterpath <- "D:/new_downscaling/clean_data/satstacks/"
predpath <- "C:/Users/mleza/OneDrive/Documents/PhD/work_packages/auto_downscaling_30m/raster_predictions/"

swirpath <- "D:/new_downscaling/SWIR/composites/" 

method <- "rf"
print(method)

date <- c("2019-10")

n <- 150000
predoutpath <- paste0(predpath, date, "/")
dir.create(predoutpath)

final_model <- get(load(paste0(modelpath,"final_model_",method,"_", n, ".RData")))
print(paste0("model name = ", "final_model_",method,"_", n, ".RData"))

#### prepare predictor raster stack ################################################

# which predictors are to be used? 
predictornames_final_model <- final_model$finalModel$xNames

# get auxiliary stack
aux <- stack("D:/new_downscaling/auxiliary/aux_stack_xy_final.tif")
names(aux) <-  c("dem", "slope", "aspect", "TWI", "soilraster", "landcoverres", 
                 "spatialblocks","x", "y")

# subset aux by predictornames

aux <- aux[[predictornames_final_model]]


# get raster data, slice them and predict all data per month
lapply(seq(date), function(i){
  
    # get MOD, L, hs, ia stacks
    raspos <- which(grepl(date[i] ,list.files(rasterpath, pattern="L_MOD_hs_ia")))
    ras <- stack(list.files(rasterpath, pattern="L_MOD_hs_ia", full.names = T)[raspos][1])
    
    # get corresponding names
    rasnampos <- which(grepl(date[i] ,list.files(rasterpath, pattern="names_sat_ia_hs")))
    rasnam <- read.csv(list.files(rasterpath, pattern="names_sat_ia_hs", full.names = T)[rasnampos])
    
    length(rasnam$x) == nlayers(ras)
    names(ras) <- rasnam$x
    
    lo <- seq(1,(length(names(ras))-1),by=4)
    hi <- lo+3
    lo <- lo +1
    
    # # get SWIR stacks
    # all_swir <- list.files(swirpath, pattern="_tc_", full.names = T)
    # swir <- stack(all_swir[which(grepl(date[i], list.files(swirpath, pattern="_tc_")))])
    # names(swir) <- c("swir6","swir7")
    # 
    # # sliced raster stack list for prediction
    # rasslice <- lapply(seq(hi), function(x){
    #   stack(ras[[lo[x]:hi[x]]], aux, swir)
    # })
    
    # sliced raster stack list for prediction
    rasslice <- lapply(seq(length(hi)), function(x){
      stack(ras[[lo[x]:hi[x]]], aux)
    })
    
    # sliced raster stack list with landsat for comparison of prediction and original values
    lo <- seq(1,(length(names(ras))-1),by=4)
    hi <- lo+3
    rasslice_landsat <- lapply(seq(length(hi)), function(x){
      stack(ras[[lo[x]:hi[x]]], aux)
    })
    
    # # sliced raster stack list with landsat for comparison of prediction and original values
    # lo <- seq(1,(length(names(ras))-1),by=4)
    # hi <- lo+3
    # rasslice_landsat <- lapply(seq(hi), function(x){
    #   stack(ras[[lo[x]:hi[x]]], aux, swir)
    # })
    
    
    
    ########################### PREDICT ###################################################################
    
    
    # SPATIAL PREDICTION 
    
    # take only MODIS scene blocks that are not duplicates!
    allmod <- stack(lapply(rasslice, '[[', 1))
    names(allmod)
    
    duplicates_check <- nchar(names(allmod))==40
    non_duplicated <- !duplicated(substring(names(allmod), 1,40))
    
    

    
    lapply(seq(hi), function(k){
      if(non_duplicated[k]==TRUE){
        print(k)
        rm(prednam)
        prednam <- names(rasslice[[k]])[grepl("MOD", names(rasslice[[k]]))]
        if(length(prednam)<1){
          prednam <- names(rasslice[[k]])[grepl("MYD", names(rasslice[[k]]))]
        }
        names(rasslice[[k]])
        
        
        names(rasslice[[k]])[1:3] <- c("Modis", "ia", "hs")
        sp <- predict(rasslice[[k]],final_model)
        writeRaster(sp, paste0(predoutpath, "pred_", method, "_", prednam, ".tif"), overwrite=T)
      } else {
        print("skipping duplicate")
      }

    })

})


