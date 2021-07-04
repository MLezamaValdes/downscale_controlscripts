
library(rasterVis)
library(ggplot2)

outpath <- "D:/downscaling_after_talk/spatial_predictions_rf/"
modelpath <- "D:/downscaling_after_talk/models/"
figurepath <- "C:/Users/mleza/OneDrive/Documents/PhD/work_packages/auto_downscaling_30m/paper/paper_draft/figures/new/"

############ get model ######################################

load(paste0(modelpath, "final_model_rf_150000SE_F.RData"))

############ FOR rf unscaled non-dummy ######################################
df <- list.files(stackpath, pattern="hs_ia", full.names = T)


months <- substring(list.files(stackpath, pattern="hs_ia"), 13, 19)

TeAqNum <- aux_sc[[1]]
names(TeAqNum) <- "TeAqNum"


pred <- lapply(seq(months), function(m){
  
  print(paste0("~~~~~~~~~~~ starting with ", months[m], " ~~~~~~~~~~~~~~~~~~~~~~"))
  
  sf <- list.files(stackpath, pattern=months[m], full.names = T)
  dyn <- stack(sf[grepl("hs_ia", sf)])
  snam <- read.csv2(sf[grepl("names_sat_ia", sf)])
  names(dyn) <-snam$x
  
  ### cut into scenes #####
  nsceneblocks <- nlayers(dyn)/4
  
  sceneblockid <- unlist(lapply(seq(nsceneblocks), function(i){
    rep(i,4)}))
  
  scenelist <- lapply(seq(nsceneblocks), function(i){
    
    x <- dyn[[which(sceneblockid==i)]]
    modname <- names(x)[2]
    
    names(x) <- c("Landsat", "Modis", "ia", "hs")
    
    if(grepl("MYD",modname)){
      TeAqNum[] <- 1
    } else {
      TeAqNum[] <- 0
    }
    
    allras <- stack(x, TeAqNum)
    
    l <- setNames(list(allras),modname)
    
    return(l)
  })
  
  
  print(paste0("~~~~~~~~~~~ starting to predict and write ", months[m], " n = ", length(scenelist), " ~~~~~~~~~~~~~~~~~~~~~~"))
  
  # & write out
  lapply(seq(scenelist), function(j){
    print(paste0("~~~~~~~~~~~ starting ", j, " ~~~~~~~~~~~~~~~~~~~~~~"))
    
    nam <- substring(names(scenelist[[j]]), 11, 22)
    s <- stack(scenelist[[j]][[1]], aux)
    
    print(paste0("~~~  predicting ", j, " ~~~"))
    
    ps <- s[[model_final$finalModel$xNames]]
    pred <- predict(ps, model_final)
    
    predstack <- stack(pred, s$Modis, s$Landsat)
    names(predstack) <- c("downscaled_Modis", "original_Modis", "reference_Landsat")
    
    writeRaster(predstack, 
                paste0(outpath, "plots/", "prediction_", months[m], "_", nam, ".tif"),
                overwrite=T)
    
    #Raster to matrix
    dscm <-as.matrix(predstack$downscaled_Modis)
    prop_NA_m <-length(which(is.na(dscm)==TRUE))/(length(dscm))
    prop_NA_l <-length(which(is.na(dscm)==TRUE))/(length(dscm))
    
    ### make a figure if there is good coverage in both Landsat and Modis
    
    if(prop_NA_m < 0.6 & prop_NA_l < 0.6){ 
          predstack$reference_Landsat[is.na(predstack$downscaled_Modis)] <- NA
          
          
          exp <- gplot(predstack)+
            geom_tile(aes(fill = value)) +
            facet_wrap(~ variable) +
            scale_fill_gradientn(colours = viridis(100)) +
            coord_equal()
          
          ggsave(paste0(figurepath, "spatial_improvement", nam, ".png"),
                 plot = exp, dpi=500)
        
    }
  
  })
  
  
})
