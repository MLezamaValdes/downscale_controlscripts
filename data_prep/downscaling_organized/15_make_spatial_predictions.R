


library(rasterVis)
library(ggplot2)

options(digits = 15)

outpath <- "D:/downscaling_after_talk/spatial_predictions_rf/"
modelpath <- "D:/downscaling_after_talk/models/"
figurepath <- "C:/Users/mleza/OneDrive/Documents/PhD/work_packages/auto_downscaling_30m/paper/paper_draft/figures/new/"
stackpath <- "D:/downscaling_after_talk/clean_data/satstacks_ngb/"
auxpath <- "D:/downscaling_after_talk/clean_data/auxdat/"
omdir <- paste0("D:/downscaling_after_talk/data_download_preprocessing/MODIS/",
                "2018-11", "/LST/")
############ get model ######################################

load(paste0(modelpath, "final_model_rf_150000factorfast_mtry.RData"))

head(model_final$trainingData)
typeof(model_final$trainingData$landcoverres)

############ FOR rf unscaled non-dummy ######################################
df <- list.files(stackpath, pattern="hs_ia", full.names = T)


months <- substring(list.files(stackpath, pattern="hs_ia"), 13, 19)

m <- which(months=="2018-11")

TeAqNum <- raster(df[1])
names(TeAqNum) <- "TeAqNum"

aux <- stack(paste0(auxpath, "aux_stack_xy_final.tif"))
names(aux) <-  c("dem", "slope", "aspect", "TWI", "soilraster", 
                 "landcoverres", "spatialblocks","x", "y")

############  prepping soilraster ##############

aux$soilraster[aux$soilraster==3 | aux$soilraster==19] <- NA
# # just checking
# soiltab <- sort(table(ratify(aux$soilraster)[]))
# levels(model_final$trainingData$soilraster) %in% levels(ratify(aux$soilraster))[[1]]$ID
aux$soilraster <- ratify(aux$soilraster)

is.factor(aux$soilraster)
levels(aux$soilraster)

############  prepping landcoverres ##############

lc <- aux$dem
lc[aux$landcoverres[]==aux$landcoverres[1]] <- 0.970000028610229
lc[aux$landcoverres[]==aux$landcoverres[1090]] <- 0.939999997615814
unique(lc[])



# dataType(lc)
# dataType(aux$dem)

# writeRaster(lc, paste0(auxpath, "lc_FLT8S.grd"), datatype='FLT8S')
# lc <- raster(paste0(auxpath, "lc_FLT8S.grd"))
dataType(lc)

# aux$landcoverres[][aux$landcoverres[]==aux$landcoverres[1]] <- 2
# aux$landcoverres[][aux$landcoverres[]==aux$landcoverres[1090]] <- 1

lc_rat <- ratify(aux$landcoverres)
lc_rat



# levels(lc_rat) <- list(data.frame(ID=c(0.939999997615814, 0.970000028610229)))

aux$landcoverres <- lc_rat
is.factor(aux$landcoverres)

levels(aux$landcoverres)




# 
# unique(aux$landcoverres[])
# 
# aux$landcoverres <- ratify(aux$landcoverres)
# 
# is.factor(aux$landcoverres)
# levels(aux$landcoverres)

############  checking whether levels coincide in model and rasters ##############

# auxdf <- aux[]
# auxdf <- data.frame(auxdf)
# str(auxdf)

# levels(aux$landcoverres)
# levels(aux$soilraster)


# lab_sr <- levels(aux$soilraster)
# code_sr <-  sort(unique(as.numeric(aux$soilraster[]))) # raster contains codes 
# # that are the same than labels in training data, i.e. I think 
# # I must make a raster, where
# # code_rs in train_df_sr is in the raster actually 
# 
# 
# 
# 
# 
# lab_sr <- levels(model_final$trainingData$soilraster)
# code_sr <-  sort(unique(as.numeric(model_final$trainingData$soilraster)))
# 
# train_df_sr <- data.frame(code_sr,lab_sr)
# train_df_sr
# 
# lab_lc <- levels(model_final$trainingData$landcoverres)
# code_lc <-  sort(unique(as.numeric(model_final$trainingData$landcoverres)))
# 
# train_df_lc <- data.frame(code_lc,lab_lc)
# train_df_lc

############  putting aux for predictors together ##############

takeaux <- which(names(aux) %in% model_final$finalModel$xNames )

aux <- aux[[takeaux]]
names(aux)

# pred <- lapply(seq(months), function(m){
  
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
      TeAqNum[] <- 2
      TeAqNum[1] <- 1
    } else {
      TeAqNum[] <- 1
      TeAqNum[1] <- 2
    }
    
    
    TeAqNum <- ratify(TeAqNum)
    
    if(TeAqNum[1]==1){
      TeAqNum[1] <- 2
    } else {
      TeAqNum[1] <- 1
    }
    
    allras <- stack(x, TeAqNum)
    
    l <- setNames(list(allras),modname)
    
    return(l)
  })
  
  prevdir <- "D:/downscaling_after_talk/spatial_predictions_rf/preview/"
  
  # lapply(seq(scenelist), function(s){
  #   png(filename=paste0(prevdir, names(scenelist[[s]]), ".png"))
  #   par(mfrow=c(1,2))
  #   plot(scenelist[[s]][[1]]$Landsat)
  #   plot(scenelist[[s]][[1]]$Modis)
  #   dev.off()
  # })
  # 
  print(paste0("~~~~~~~~~~~ starting to predict and write ", months[m], " n = ", length(scenelist), " ~~~~~~~~~~~~~~~~~~~~~~"))
  
  j=8
  # & write out
  lapply(seq(scenelist), function(j){
    nam <- "2018316.1350.006.2"
    
    if(grepl( "2018316.1350.006.2", names(scenelist[[j]]))){
      print(paste0("~~~~~~~~~~~ starting ", j, " ~~~~~~~~~~~~~~~~~~~~~~"))
      
      # nam <- substring(names(scenelist[[j]]), 11, 22)
      
      

      orfmodf <- list.files(omdir,
                            pattern=nam, full.names = T)
      orgmodf <- orfmodf[grepl("c_warp",orfmodf)]

      om <- raster(orgmodf)
      bilinmod <- resample(om, TeAqNum, method="bilinear")
      names(bilinmod) <- "Modis"
      
      modngb <- scenelist[[j]][[1]]$Modis
      scenelist[[j]][[1]]$Modis <- bilinmod


      
      s <- stack(scenelist[[j]][[1]], aux)
      
      print(paste0("~~~  predicting ", j, " ~~~"))
      
      
      str(model_final$trainingData)
      
      ps <- s[[model_final$finalModel$xNames]]
      ps

      
      ############### CONVERT TO DATAFRAME TO DEAL WITH FACTORS IN A CLEAN MANNER ################
      psdf <- ps[]
      psdf <- data.frame(psdf)
      str(psdf)

      psdff <- psdf
      psdff$soilraster <- factor(psdff$soilraster)
      psdff$landcoverres <- factor(psdff$landcoverres)
      
      
      # to get 2 levels 
      if(psdff$TeAqNum[1]==1){
        psdff$TeAqNum[1] <- 2
      } else {
        psdff$TeAqNum[1] <- 1
      }
      
      psdff$TeAqNum <- factor(psdff$TeAqNum)
      
      
      if(psdff$TeAqNum[5]==1){
        psdff$TeAqNum[1] <- 1
      } else {
        psdff$TeAqNum[1] <- 2
      }
      
      
      iscomp <- complete.cases(psdff)
      psdffpred <- psdff[iscomp,]
      
      str(psdffpred)
      any(is.na(psdffpred))
      
      ### predict model_final with data frame from predictor stack
      pred <- predict(model_final, psdffpred)
      
      
      pred_allval <- iscomp
      pred_allval[pred_allval==TRUE] <- pred
      pred_allval[pred_allval==FALSE]<- NA
      
      predras <- ps[[1]]
      predras[] <- pred_allval
      
      predstack <- stack(modngb, predras, s$Landsat)
      names(predstack) <- c("original_Modis", "downscaled_Modis", "reference_Landsat")
      
      writeRaster(predstack, 
                  paste0(outpath, "plots/", "prediction_", months[m], "_", nam, ".grd"),
                  overwrite=T)
      
      # #Raster to matrix
      # dscm <-as.matrix(predstack$downscaled_Modis)
      # prop_NA_m <-length(which(is.na(dscm)==TRUE))/(length(dscm))
      # prop_NA_l <-length(which(is.na(dscm)==TRUE))/(length(dscm))
      # 
      # ### make a figure if there is good coverage in both Landsat and Modis
      # 
      # if(prop_NA_m < 0.6 & prop_NA_l < 0.6){ 
      #   predstack$reference_Landsat[is.na(predstack$downscaled_Modis)] <- NA
      #   
      #   
      #   exp <- gplot(predstack)+
      #     geom_tile(aes(fill = value)) +
      #     facet_wrap(~ variable) +
      #     scale_fill_gradientn(colours = viridis(100)) +
      #     coord_equal()
      #   
      #   ggsave(paste0(figurepath, "spatial_improvement", nam, ".png"),
      #          plot = exp, dpi=500)
      #   
      # }
      
    }
    
 
  })
  
  
# })
