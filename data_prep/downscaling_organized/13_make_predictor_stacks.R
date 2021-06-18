########## make predictor stacks from satstacks ########################




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
# auxpath <- "D:/downscaling_after_talk/clean_data/auxdat/"
# stackpath <- "D:/downscaling_after_talk/clean_data/satstacks_ngb/"


# modelpath <- "/scratch/tmp/llezamav/modeling_after_talk/"
stackpath <- "/scratch/tmp/llezamav/satstacks_ngb/"
auxpath <- "/scratch/tmp/llezamav/satstacks/"




# load(paste0(modelpath, "final_model_nnet_150000dummyrun.RData"))

#### get min max scaling values
mmtab <- readRDS("add_files/overall_minmaxvals_train_valid.Rds")


sf <- list.files(stackpath, pattern="2017-02", full.names = T)
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
aux_scaled_complete <- stack(scaled_aux, aux[[c("soilraster", "landcoverres", "spatialblocks", "x", "y")]])

names(aux)
writeRaster(aux_scaled_complete, paste0(predstackdir, "aux_scaled_complete.tif"))


