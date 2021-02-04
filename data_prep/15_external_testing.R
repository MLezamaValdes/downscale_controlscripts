##############################################################
########### external testing of models #######################
##############################################################

rm(list=ls())

library(ggplot2)
library(viridis)
library(gridExtra)
library(grid)
library(raster)

modelpath <-"C:/Users/mleza/OneDrive/Documents/PhD/work_packages/auto_downscaling_30m/final_150000/"
regressionStatsRsenal <- source("C:/Users/mleza/OneDrive/Documents/PhD/ML_dist/ML_dist/regressionstats_Rsenal.R")
figurepath <- "C:/Users/mleza/OneDrive/Documents/PhD/work_packages/auto_downscaling_30m/paper/paper_draft/figures/"
rasterpath <- "D:/new_downscaling/clean_data/satstacks/"
predoutpath <- "C:/Users/mleza/OneDrive/Documents/PhD/work_packages/auto_downscaling_30m/raster_predictions/"
swirpath <- "D:/new_downscaling/SWIR/composites/" 



test1 <- read.csv2(paste0(modelpath, "test1.csv"), sep=",", dec=".")
test2 <- read.csv2(paste0(modelpath, "test2.csv"), sep=",", dec=".")
test3 <- read.csv2(paste0(modelpath, "test3.csv"), sep=",", dec=".")

testlist <- list(test1, test2, test3)

sizetests <- c(nrow(test1), nrow(test2), nrow(test3))
# sizetab <- as.table(sizetests)
# names(sizetab) <- c("test1", "test2", "test3")
# barplot(sizetab, main="test subset sizes")

methods <- c("rf",
             "nnet")

testtitle <- c("test areas all months", "training areas 6 testing months",
               "test areas 6 testing months")

i=1


#for (i in 1:length(methods)){
  
  method <- methods[i]
  print(method)
  
  n <- 150000
  
  final_model <- get(load(paste0(modelpath,"final_model_",method,"_", n, ".RData")))
  print(paste0("model name = ", "final_model_",method,"_", n, ".RData"))
  
  # external statistical evaluation
  ex_test_1to3 <- lapply(seq(3), function(j){
    
    pred <- predict(final_model, newdata = testlist[[j]])
    
    saveRDS(pred, paste0(modelpath, "prediction_", method, "_test", j, ".RDS"))
    
    s <- regressionStats(pred,testlist[[j]]$Landsat)
    
    sround <- round(s, digits=2)
    
    df <- data.frame(pred, testlist[[j]]$Landsat)
    names(df) <- c("pred", "obs")
    
    
    p <- ggplot(df, aes(obs,pred))+
      #xlab(paste("observed LST in test", j))+ylab(paste("predicted LST in test", j))+
      #ggtitle(testtitle[j])+
      labs(x=paste("observed LST in test", j), 
             y=paste("predicted LST in test", j),
             title=paste0(testtitle[j], " n=", sizetests[j]),
             subtitle=paste0("R²=",sround$Rsq, " RMSE=",sround$RMSE))+
      stat_binhex(bins=600)+
      geom_abline(slope=1,intercept=0)+
      scale_x_continuous(expand=c(0,0))+
      scale_y_continuous(expand=c(0,0))+
      scale_fill_gradientn(name="datapoints", trans="log", colours=viridis(10), 
                           breaks=10^(0:4))
    
    return(list(p, df, s))
    
  })
  
  plots <- lapply(ex_test_1to3, '[[', 1)

  eg <- grid.arrange(plots[[1]], plots[[2]], plots[[3]], nrow = 2,
                     top=textGrob(paste("external evaluation final", method, 
                                        "model, trained on 150000 observations"),
                                  gp=gpar(fontsize=16,font=2))
  )
  
  ggsave(paste0(figurepath, "external_eval", method, ".png"), plot = eg)
  
  
  
  #### spatial prediction ############################################################
  ####################################################################################
  
  
  #### prepare predictor raster stack ################################################
  
  # which predictors are to be used? 
  predictornames_final_model <- final_model$finalModel$xNames
  
  # get auxiliary stack
  aux <- stack("D:/new_downscaling/auxiliary/aux_stack_xy_final.tif")
  names(aux) <-  c("dem", "slope", "aspect", "TWI", "soilraster", "landcoverres", 
                   "spatialblocks","x", "y")
  
  # subset aux by predictornames
  
  aux <- aux[[predictornames_final_model]]
  
  # get MOD, L, hs, ia stacks
  raspos <- which(grepl("2019-01" ,list.files(rasterpath, pattern="L_MOD_hs_ia")))
  ras <- stack(list.files(rasterpath, pattern="L_MOD_hs_ia", full.names = T)[raspos][1])
  
  rasnampos <- which(grepl("2019-01" ,list.files(rasterpath, pattern="names_sat_ia_hs")))
  rasnam <- read.csv(list.files(rasterpath, pattern="names_sat_ia_hs", full.names = T)[rasnampos])
  
  length(rasnam$x) == nlayers(ras)
  names(ras) <- rasnam$x
  
  lo <- seq(1,(length(names(ras))-1),by=4)
  hi <- lo+3
  lo <- lo +1
  
  # # get SWIR stacks
  # all_swir <- list.files(swirpath, pattern="_tc_", full.names = T)
  # swir <- stack(all_swir[which(grepl("2019-01", list.files(swirpath, pattern="_tc_")))])
  # names(swir) <- c("swir6","swir7")
  # 
  # # sliced raster stack list for prediction
  # rasslice <- lapply(seq(hi), function(x){
  #   stack(ras[[lo[x]:hi[x]]], aux, swir)
  # })
  
  # sliced raster stack list for prediction
  rasslice <- lapply(seq(hi), function(x){
    stack(ras[[lo[x]:hi[x]]], aux)
  })
  
  # sliced raster stack list with landsat for comparison of prediction and original values
  lo <- seq(1,(length(names(ras))-1),by=4)
  hi <- lo+3
  rasslice_landsat <- lapply(seq(hi), function(x){
    stack(ras[[lo[x]:hi[x]]], aux)
  })
  
  # # sliced raster stack list with landsat for comparison of prediction and original values
  # lo <- seq(1,(length(names(ras))-1),by=4)
  # hi <- lo+3
  # rasslice_landsat <- lapply(seq(hi), function(x){
  #   stack(ras[[lo[x]:hi[x]]], aux, swir)
  # })
  

  
  ########################### PREDICT ##########################################
  
  
  # SPATIAL PREDICTION 
  
  # take only MODIS scene blocks that are not duplicates!
  lapply(seq(hi), function(k){
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
  })
  

  
#}
  
  
  
####### making one good image for a scene that is pretty complete ########
########################################################################## 
  
nam_rasslice <- sapply(seq(rasslice), function(i){
  names(rasslice[[i]])[1]
})

pat <- substring(nam_rasslice, 11,22)

pos_pot <- sapply(seq(pat),function(i){
  max(which(grepl(pat[i], nam_rasslice)))
})


# get MODIS together
rasslice

allmodis <-lapply(rasslice, '[[', 1)

plot(stack(allmodis))


prediction_response <- lapply(seq(pos_pot), function(i){
  sp <- raster(list.files(predoutpath, pattern=pat[i], full.names=T)[1])
  plotstack <- stack(rasslice[[pos_pot[i]]][[1]], sp, rasslice_landsat[[pos_pot[i]]][[1]])
  names(plotstack) <- c(paste0("MODIS", pat[i]), "prediction", "Landsat")

  
  # library(rasterVis)
  # exp <- gplot(plotstack)+
  #   geom_tile(aes(fill = value)) +
  #   facet_wrap(~ variable) +
  #   scale_fill_gradientn(colours = viridis(100)) +
  #   coord_equal()
  # 
  # ggsave(paste0(figurepath, "spatial_improvement.png"), 
  #        plot = exp, dpi=500)
  
  return(plotstack)
  
})

#prediction_response <- stack(prediction_response)
#plot(prediction_response[[1]])
#e <- drawExtent()
#prediction_response <- crop(prediction_response, e)
modislist <- lapply(prediction_response, `[[`, 1)
modis <- stack(modislist)

predlist <- lapply(prediction_response, `[[`, 2)
pred <- stack(predlist)


moddf <- extract(modis, c(1:ncell(modis)))
preddf <- extract(pred, c(1:ncell(pred)))


moddf <- modis[]
preddf <- pred[]

saveRDS(moddf, paste0(predoutpath, "mod_pred_scatter/moddf.RDS"))
saveRDS(preddf, paste0(predoutpath, "mod_pred_scatter/preddf.RDS"))


moddf <- readRDS(paste0(predoutpath, "mod_pred_scatter/moddf.RDS"))
preddf <- readRDS(paste0(predoutpath, "mod_pred_scatter/preddf.RDS"))


moddf <- data.frame(moddf)
preddf <- data.frame(preddf)

head(moddf)
head(preddf)


preddfc <- preddf[complete.cases(preddf),]
moddfc <- moddf[complete.cases(moddf),]


plot(moddf$MODIS2019001.1315, preddf$prediction.1)
library(ggplot2)
library(viridis)
library(raster)
library(mapview)


pat <- "2019002.2030"

k <- rasslicepos <- which(grepl(pat, nam_rasslice))

orgmodloc <- "D:/new_downscaling/data_download_preprocessing/MODIS/2019-01/LST/"

#small_proj_MOD11_L2.A2019001.1315.006.2019029180309.tif

modorglistfiles <- list.files(orgmodloc, pattern=pat, full.names = T)
orgmod <- raster(modorglistfiles[grepl("small_proj", modorglistfiles)])

mapview( prediction_response[[rasslicepos]]$prediction) + mapview(orgmod)


mapview(prediction_response[[1]][[1]])+mapview(prediction_response[[1]]$prediction)+
  mapview(prediction_response[[1]]$Landsat)


i=1
k = 2
mapview(prediction_response[[k]][[1]])+mapview(prediction_response[[k]]$prediction)+
  mapview(prediction_response[[k]]$Landsat)


df <-data.frame( cbind(moddf[,1], preddf[,i]))
names(df) <- c("mod", "ds")
df$mod[df$mod < (-50)] <- NA
df <- df[complete.cases(df),]


ggplot(df, aes(df[,1],df[,2]))+
  xlab("MODIS original")+ylab("downscaled")+
  ggtitle(paste0("downscaled LST vs original ", names(moddf)[i]))+
  stat_binhex(bins=1000)+
  geom_abline(slope=1,intercept=0)+
  scale_x_continuous(expand=c(0,0))+
  scale_y_continuous(expand=c(0,0))+
  scale_fill_gradientn(name="datapoints", trans="log", colours=viridis(10),
                       breaks=10^(0:4))


ggplot(df,aes(df[,1], ))



