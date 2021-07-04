########## make predictor stacks from satstacks ########################



library(RStoolbox)
library(raster)

predstackdir <- "D:/downscaling_after_talk/stacks_for_spatial_predictions/"

########### functions ##############################################
scaling01 <- function(x, xmaxval, xminval){
  (x - xminval) / (xmaxval - xminval)
}

scale_metric_in_all_datasets <- function(dataset, metricpredictors){
    
    if(class(dataset)=="data.frame"){
      scaledvars <- lapply(seq(metricpredictors), function(i){
        
    
      x <- scaling01(dataset[,metricpredictors[i]], 
                   xmaxval = mmtab[,metricpredictors[i]][2],
                   xminval = mmtab[,metricpredictors[i]][1])
      print(summary(x, na.rm=T))
      x

      })
      
      scaleddf <- data.frame(scaledvars)
      # TO DO: set those over 1 and under 0 to NA
      names(scaleddf) <- paste0(metricpredictors, "_sc")
      dataset_out <- cbind(dataset, scaleddf)
      
      return(dataset_out)
      
    } else if(class(dataset)=="RasterStack"){
      dataset_out <-  lapply(seq(metricpredictors), function(i){
      
      vals <- dataset[[metricpredictors[i]]][]
      
      x <- scaling01(vals, 
                     xmaxval = mmtab[,metricpredictors[i]][2],
                     xminval = mmtab[,metricpredictors[i]][1])
      
      x[x<0] <- NA
      x[x>1] <- NA
      
      dataset[[metricpredictors[i]]] <- x
      
      dataset[[metricpredictors[i]]]
      
      })
      
      return(dataset_out)
      
    }
  
}


############## paths ##############################################


# modelpath <- "/scratch/tmp/llezamav/modeling_after_talk/"
stackpath <- "/scratch/tmp/llezamav/satstacks_ngb/"
auxpath <- "/scratch/tmp/llezamav/satstacks/"



# auxpath <- "D:/downscaling_after_talk/clean_data/auxdat/"
# stackpath <- "D:/downscaling_after_talk/clean_data/satstacks_ngb/"

rasterOptions(tmpdir=paste0(predstackdir, "run/"))



# load(paste0(modelpath, "final_model_nnet_150000dummyrun.RData"))

#### get min max scaling values
mmtab <- readRDS("add_files/overall_minmaxvals_train_valid.Rds")
################# get aux stack #####################


# get aux
aux <- stack(paste0(auxpath, "aux_stack_xy_final.tif"))
names(aux) <-  c("dem", "slope", "aspect", "TWI", "soilraster", 
                 "landcoverres", "spatialblocks","x", "y")




################ rescale aux #########################
########### rescale from 0 to 1 ################

metricpredictorsstat <- c("dem", "slope", "aspect", "TWI")

scaled_aux_list <-  scale_metric_in_all_datasets(dataset = aux, metricpredictors = metricpredictorsstat)
scaled_aux <- stack(scaled_aux_list)
names(scaled_aux) <- paste0(names(scaled_aux), "_sc")

########### make dummys ################

soiras <- aux[["soilraster"]]
landcoverres <- aux[["landcoverres"]]

# make dummy for open soil (0.94)
landcoverres100 <- landcoverres*100 # making integers
landcover_oneHot <- oneHotEncode(landcoverres100, classes=c(97, 94),
                                 na.rm=T)
# rename as in train
names(landcover_oneHot[[1]]) <- "landcoverres.1"

#mapview(landcover_oneHot$landcoverres.1)

# make dummy for soil raster
uvs <- unique(values(soiras))
soiras_oneHot <- oneHotEncode(soiras, classes=uvs, na.rm=T)
srnams <- paste0("soilraster.", uvs)
names(soiras_oneHot) <- srnams


aux_scaled_complete <- stack(scaled_aux, soiras_oneHot, landcover_oneHot$landcoverres.1)
names(aux_scaled_complete)
writeRaster(aux_scaled_complete, paste0(predstackdir, "aux_scaled_dummies_complete.tif"))


aux_helpdata <- stack(aux[[c("spatialblocks", "x", "y")]])
writeRaster(aux_helpdata, paste0(predstackdir, "aux_helpdata.tif"))




############### prep dynamic data scaling ##################################



months <- substring(list.files(stackpath, pattern="hs_ia"), 13, 19)


lapply(c(4:length(months)), function(m){
  
  print(paste0("~~~~~~~~~~~ starting with ", months[m], " ~~~~~~~~~~~~~~~~~~~~~~"))
  sf <- list.files(stackpath, pattern=months[m], full.names = T)
  stack <- stack(sf[grepl("hs_ia", sf)])
  snam <- read.csv2(sf[grepl("names_sat_ia", sf)])
  
  names(stack) <-snam$x
  
  
  
  ########## cut stack into sets #####################
  
  nsceneblocks <- nlayers(stack)/4
  
  sceneblockid <- unlist(lapply(seq(nsceneblocks), function(i){
    rep(i,4)}))
  
  scenelist <- lapply(seq(nsceneblocks), function(i){
    stack[[which(sceneblockid==i)]]
  })
  
  
  ################# rescale dyn stack #################
  metricpredictorsdyn <- c("Modis","ia", "hs")
  
  scaled_scenes_list <-  lapply(seq(scenelist), function(i){
    modname <- names(scenelist[[i]])[2]
    scenenam <- paste(names(scenelist[[i]])[1],  names(scenelist[[i]])[2],
                      names(scenelist[[i]])[3], names(scenelist[[i]])[4])
    names(scenelist[[i]]) <- c("Landsat", "Modis", "ia", "hs")
    scaled_scene_i_list <- scale_metric_in_all_datasets(dataset = scenelist[[i]], metricpredictors = metricpredictorsdyn)
    
    scaled_scene_i <- stack(scaled_scene_i_list)
    names(scaled_scene_i) <- paste0(metricpredictorsdyn, "_sc")
    
    TeAqNum <- scaled_scene_i[[1]]
    names(TeAqNum) <- "TeAqNum.1"
    
    if(grepl("MYD",modname)){
      TeAqNum[] <- 1
    } else {
      TeAqNum[] <- 0
    }
    
    allras <- stack(scaled_scene_i, TeAqNum)
    
    l <- setNames(list(allras), modname)
    return(l)
  })
  
  print(paste0("~~~~~~~~~~~ starting to write ", months[m], " n = ", length(scaled_scenes_list), " ~~~~~~~~~~~~~~~~~~~~~~"))
  
  # & write out
  lapply(seq(scaled_scenes_list), function(j){
    print(paste0("~~~~~~~~~~~ writing ", j, " ~~~~~~~~~~~~~~~~~~~~~~"))
    
    nam <- substring(names(scaled_scenes_list[[j]]), 11, 22)
    writeRaster(scaled_scenes_list[[j]][[1]], 
                paste0(predstackdir, "predictors_", months[m], "_", nam, "_scaled.tif"),
                overwrite=T)
  })
  
})



########## bind aux and dynamic rasters together for predictions ################


############ FOR SCALED AND DUMMY VARIABLES ######################################

aux_sc <- stack(paste0(predstackdir, "aux_scaled_dummies_complete.tif"))
names(aux_sc) <- c("dem_sc", "slope_sc", "aspect_sc", "TWI_sc", 
                "soilraster.12", "soilraster.5" , "soilraster.9" , "soilraster.10", "soilraster.24",
                "soilraster.NA" ,"soilraster.4" , "soilraster.21" ,"soilraster.7" , "soilraster.3" ,
                "soilraster.19", "soilraster.1" , "soilraster.20", "soilraster.13",
                "landcoverres.1")

predfiles <- list.files(predstackdir, pattern="predictors_", full.names = T)

# i would be for the scene you're interested in
dyn <- stack(predfiles[i])
names(dyn) <- c("Modis_sc","ia_sc", "hs_sc", "TeAqNum.1") 

dynauxstack <- stack(dyn, aux_sc)
names(dynauxstack)


############ FOR rf unscaled non-dummy ######################################
df <- list.files(stackpath, pattern="hs_ia", full.names = T)


months <- substring(list.files(stackpath, pattern="hs_ia"), 13, 19)

TeAqNum <- aux_sc[[1]]
names(TeAqNum) <- "TeAqNum.1"

lapply(seq(months), function(m){
  
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
  
  
  print(paste0("~~~~~~~~~~~ starting to write ", months[m], " n = ", length(scenelist), " ~~~~~~~~~~~~~~~~~~~~~~"))
  
  # & write out
  lapply(seq(scenelist), function(j){
    print(paste0("~~~~~~~~~~~ writing ", j, " ~~~~~~~~~~~~~~~~~~~~~~"))
    
    nam <- substring(names(scenelist[[j]]), 11, 22)
    s <- stack(scenelist[[j]][[1]], aux)
    writeRaster(s, 
                paste0(predstackdir, "predictors_", months[m], "_", nam, ".tif"),
                overwrite=T)
  })
  
})
