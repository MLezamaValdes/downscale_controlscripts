
library(ggplot2)
library(viridis)
library(gridExtra)
library(grid)
library(caret)
library(raster)

modelpath <- "D:/downscaling_after_talk/models/"
trainvalidpath <- "D:/downscaling_after_talk/clean_data/train_valid/"
regressionStatsRsenal <- source("C:/Users/mleza/OneDrive/Documents/PhD/ML_dist/ML_dist/regressionstats_Rsenal.R")

figurepath <- "C:/Users/mleza/OneDrive/Documents/PhD/work_packages/auto_downscaling_30m/paper/paper_draft/figures/new/"

rasterpath <- "D:/downscaling_after_talk/clean_data/satstacks_ngb/"
predoutpath <- "D:/downscaling_after_talk/predictions/"
predstackdir <- "D:/downscaling_after_talk/spatial_predictions_rf/predstacks/"


# train <- read.csv2(paste0(trainvalidpath, "train_LHS_150000_scaled.csv"))
# 
# test1 <- read.csv2(paste0(trainvalidpath, "validation_1_LHS_150000_scaled.csv"))
# test2 <- read.csv2(paste0(trainvalidpath, "validation_2_LHS_150000_scaled.csv"))
# test3 <- read.csv2(paste0(trainvalidpath, "validation_3_LHS_150000_scaled.csv"))
# 
# 
# train <- read.csv2(paste0(trainvalidpath, "train_LHS_150000.csv"))
# 
# test1 <- read.csv2(paste0(trainvalidpath, "validation_1_LHS_150000.csv"))
# test2 <- read.csv2(paste0(trainvalidpath, "validation_2_LHS_150000.csv"))
# test3 <- read.csv2(paste0(trainvalidpath, "validation_3_LHS_150000.csv"))
# # 
##############################################################


# testlist_unscaled <- lapply(seq(testlist), function(i){
#   testlist[[i]]$TeAq <- as.factor(substring(testlist[[i]]$Mscene,1,3))
#   testlist[[i]]$TeAqNum <- as.numeric(testlist[[i]]$TeAq)
#   testlist[[i]]
# })

# sizetab <- as.table(sizetests)
# names(sizetab) <- c("test1", "test2", "test3")
# barplot(sizetab, main="test subset sizes")

# methods <- c("rf",
#              "nnet",
#              "svmLinear")
methods <- c("rf")

# testtitle <- c("spatial validation", "temporal validation",
#                "spatiotemporal validation")
# 
# 
# # testtitle <- c("validation set 1", "validation set 2", "validation set 3")
# 
# i=1
# 
# 
# # #### var imp & pred obs plots ############################################################
# # ####################################################################################
# # 
# 
# 
# 
# 
# 
# 
# 
# # to do: scale predictors
# 
# 
# 
# testlist <- list(test1, test2, test3)
# 
#   testlist <- testlist
#   sizetests <- c(nrow(testlist[[1]]), nrow(testlist[[2]]), nrow(testlist[[3]]))
#   

i=1

  method <- methods[i]
  print(method)
  
  fm <- list.files(modelpath, pattern=method, full.names=T)
  fm <- fm[grepl("final_", fm)]
  fm <- fm[grepl("SE_F", fm)]
  
  load(fm)
  
  n <- 150000

  # #### spatial prediction ############################################################
  # ####################################################################################
  # 

# for (i in 1:length(methods)){
  
  
  #### prepare predictor raster stack ################################################
  
  # which predictors are to be used? 
  predictornames_model_final <- model_final$finalModel$xNames
  
  # get auxiliary stack
  aux <- stack("E:/new_downscaling/auxiliary/aux_stack_xy_final.tif")
  names(aux) <-  c("dem", "slope", "aspect", "TWI", "soilraster", "landcoverres", 
                   "spatialblocks","x", "y")
  
  # subset aux by predictornames
  
  aux <- aux[[predictornames_model_final]]
  
  # get MOD, L, hs, ia stacks
  raspos <- which(grepl("2018-11" ,list.files(rasterpath, pattern="L_MOD_hs_ia")))
  ras <- stack(list.files(rasterpath, pattern="L_MOD_hs_ia", full.names = T)[raspos][1])
  
  rasnampos <- which(grepl("2018-11" ,list.files(rasterpath, pattern="names_sat_ia_hs")))
  rasnam <- read.csv(list.files(rasterpath, pattern="names_sat_ia_hs", full.names = T)[rasnampos])
  
  length(rasnam$x) == nlayers(ras)
  names(ras) <- rasnam$x
  
  names(ras)
  
  lo <- seq(1,(length(names(ras))-1),by=4)
  hi <- lo+3
  lo <- lo +1

  
  # sliced raster stack list for prediction
  rasslice <- lapply(seq(hi), function(x){
    stack(ras[[(lo[x]):hi[x]]], aux)
  })
  
  

  # sliced raster stack list with landsat for comparison of prediction and original values
  lo <- seq(1,(length(names(ras))-1),by=4)
  hi <- lo+3
  rasslice_landsat <- lapply(seq(hi), function(x){
    stack(ras[[lo[x]:hi[x]]], aux)
  })
  
  # allmodis <-lapply(rasslice, '[[', 1)
  # alllandsat <- lapply(rasslice_landsat, '[[', 1)
  # 
  # windows(10,20)
  # par(mfrow=c((length(alllandsat)/2),2))
  # 
  # # for 11 2018 2 and 8 perhaps seem to look good
  # 
  # lapply(seq(alllandsat), function(i){
  #   plot(allmodis[[i]])
  #   plot(alllandsat[[i]])
  # })

  
  # # sliced raster stack list with landsat for comparison of prediction and original values
  # lo <- seq(1,(length(names(ras))-1),by=4)
  # hi <- lo+3
  # rasslice_landsat <- lapply(seq(hi), function(x){
  #   stack(ras[[lo[x]:hi[x]]], aux, swir)
  # })
  

  
  ########################### PREDICT ##########################################
  
  
  # SPATIAL PREDICTION 
  
  # take only MODIS scene blocks that are not duplicates!
  k=8
  lapply(seq(hi), function(k){
      print(k)
      rm(prednam)
      prednam <- names(rasslice[[k]])[grepl("MOD", names(rasslice[[k]]))]
      if(length(prednam)<1){
        prednam <- names(rasslice[[k]])[grepl("MYD", names(rasslice[[k]]))]
      }
      modname <- prednam
      names(rasslice[[k]])[1:3] <- c("Modis", "ia", "hs")
      
      TeAqNum <- rasslice[[k]][[1]]
      #names(TeAqNum) <- "TeAqNum.1"
      names(TeAqNum) <- "TeAqNum"
      
      if(grepl("MYD",modname)){
        TeAqNum[] <- 1
      } else {
        TeAqNum[] <- 0
      }
      
      
      rasslice[[k]] <- stack(rasslice[[k]], TeAqNum)
      names(rasslice[[k]])
      model_final$finalModel$xNames
      
      omdir <- paste0("D:/downscaling_after_talk/data_download_preprocessing/MODIS/", 
                      "2018-11", "/LST/")
      orfmodf <- list.files(omdir, 
                            pattern=prednam, full.names = T)
      orgmodf <- orfmodf[grepl("c_warp",orfmodf)]
      om <- raster(orgmodf)
      bilinmod <- resample(om, sp, method="bilinear")
      names(bilinmod) <- "Modis"
      
      predstack <- rasslice[[k]]
      predstack[[1]] <- bilinmod
      writeRaster(predstack, paste0(predstackdir, "predstack_", prednam,".tif"), overwrite=T)
      write.csv2(names(predstack), paste0(predstackdir, "names_predstack_", prednam,".csv"),
                 row.names=F)
      
      
      sp <- predict(predstack,model_final)
      writeRaster(sp, paste0(predoutpath, "pred_", method, "_", prednam, "bilinmod.tif"), overwrite=T)
  })
  

  
  


####### making one good image for a scene that is pretty complete ########
########################################################################## 
  
# nam_rasslice <- sapply(seq(rasslice), function(i){
#   names(rasslice[[i]])[1]
# })
# 
# pat <- substring(nam_rasslice, 11,22)
# 
# pos_pot <- sapply(seq(pat),function(i){
#   max(which(grepl(pat[i], nam_rasslice)))
# })


# get MODIS together
# 
# 
# library(lubridate)
# 
# "2009-06-03 19:30"
# pb.txt <- "2014312.1335"  
# 
# pb.date <- as.POSIXct(pb.txt, format=c("%Y%j.%H%M"), tz="UTC") 
# 
# with_tz(pb.date, tzone = "NZ")



################ MAKE FIGURE FOR PAPER ######################################
  
  
################## TO DO: CHOOSE HIGHLY TEMPORALLY MATCHING SCENE AND 
################### ONE THAT IS ALSO PRETTY COMPLETE '''''''''''
  
  # prediction_response <- lapply(seq(pos_pot), function(i){
i=k
  sporg <- raster(paste0(predoutpath, "pred_rf_MYD11_L2.A2018316.1350.006.2.tif"))
  sp <- raster(paste0(predoutpath, "pred_rf_MYD11_L2.A2018316.1350.006.2bilinmod.tif"))
  spnam <- names(sp)

  # sp <- raster(list.files(predoutpath, pattern=pat[i], full.names=T)[1])
  plotstack <- stack(rasslice_landsat[[k]][[1]], sp,rasslice[[k]][[1]] )
  
 

  plot(plotstack[[2]])
  e2 <- drawExtent()
  # e <- c(377567.5, 390992.4,  -1290792 , -1282961)
  plotstack_small_1 <- crop(plotstack, e2)
  
  
  names(plotstack) <- c("Landsat", "downscaled_MODIS", "MODIS")
  
  
  diff <- plotstack$Landsat-sporg
  mapview(diff)


  library(rasterVis)
  
  
  e1sf <- st_as_sf(st_as_sfc(st_bbox(e1, crs = st_crs(plotstack))))
  e2sf <- st_as_sf(st_as_sfc(st_bbox(e2, crs = st_crs(plotstack))))
  
  e1sf$area <- as.numeric(st_area(e1sf))
  e2sf$area <- as.numeric(st_area(e2sf))
  
  
  exp <- gplot(plotstack)+
    geom_tile(aes(fill = value)) +
    facet_wrap(~ variable) +
    ggtitle("a")+
    scale_fill_gradientn(colours = viridis(100), 
                         na.value = "white",
                         name="LST (?C)") +
    coord_equal()+
    theme_minimal()+
    
    theme(strip.background =element_rect(fill="gray56"),
          strip.text = element_text(colour = "white"),
            axis.text = element_text(size=7))
  
  
  expboxes <- exp + 
    geom_polygon(data = data.frame(x = c(419639.7, 419639.7,  435980.8,  435980.8), #e1
                                                   y = c(-1330038, -1310686, -1310686, -1330038 )),
                         aes(x = x, y = y), colour = "firebrick3", fill=NA, size = 1)+
    geom_polygon(data = data.frame(x = c(414049.3, 414049.3,   430820.4 ,   430820.4 ), #e2
                                   y = c(-1275424 , -1256933 , -1256933 , -1275424  )),
                   aes(x = x, y = y), colour = "navy",  fill=NA, size = 1)
  
  

  # ggsave(paste0(figurepath, "spatial_improvement_", prednam,".png"),
  #        plot = expboxes, dpi=300, width = 20, height=15, units = "cm")
  
  # plot(plotstack[[1]])
  e2 <- extent(414049.3, 430820.4 , -1275424 ,-1256933)
  plotstack_small_2 <- crop(plotstack, e2)
  names(plotstack_small_2) <- c("Landsat", "downscaled_MODIS", "MODIS")
  
  exps2 <- gplot(plotstack_small_2)+
    geom_tile(aes(fill = value)) +
    facet_wrap(~ variable) +
    ggtitle("c")+
    scale_fill_gradientn(colours = viridis(100), 
                         na.value = "white",
                         name="LST (?C)") +    coord_equal()+
    theme_minimal()+
    theme(strip.background =element_rect(fill="navy"),
          strip.text = element_text(colour = "white"),
          axis.text = element_text(size=7))
  
  exps2
  
  # ggsave(paste0(figurepath, "spatial_improvement_", prednam,"small_extent_2.png"),
  #        plot = exps2, dpi=300, width = 20, height=15, units = "cm")
  
  
  e1 <- c( 419639.7, 435980.8 , -1330038, -1310686 )
  plotstack_small_1 <- crop(plotstack, e1)
  names(plotstack_small_1) <- c("Landsat", "downscaled_MODIS", "MODIS")
  
  
  exps <- gplot(plotstack_small_1)+
    geom_tile(aes(fill = value)) +
    facet_wrap(~ variable) +
    theme_minimal()+
    ggtitle("b")+
    scale_fill_gradientn(colours = viridis(100), 
                         na.value = "white",
                         name="LST (?C)") +    
    coord_equal() +
    theme(strip.background =element_rect(fill="firebrick3"),
          strip.text = element_text(colour = "white"),
          axis.text = element_text(size=7))
    
  # exps
  # 
  # ggsave(paste0(figurepath, "spatial_improvement_", prednam,"small_extent_1.png"),
  #        plot = exps,dpi=300, width = 20, height=15, units = "cm")
  
  plotlist <- list(expboxes, exps, exps2)

  png(paste0(figurepath, "spatial_improvement_", prednam,"full_figure.png"),
      width = 22, height=25, units = "cm", res=300)
  
  grid.arrange(
    grobs = plotlist,
    # widths = c(2, 1, 1),
    layout_matrix = rbind(c(1),
                          c(2),
                          c(3))

  )
  dev.off()
  
  
  
  
  
# })

# #prediction_response <- stack(prediction_response)
# #plot(prediction_response[[1]])
# #e <- drawExtent()
# #prediction_response <- crop(prediction_response, e)
# modislist <- lapply(prediction_response, `[[`, 1)
# modis <- stack(modislist)
# 
# predlist <- lapply(prediction_response, `[[`, 2)
# pred <- stack(predlist)
# 
# 
# moddf <- extract(modis, c(1:ncell(modis)))
# preddf <- extract(pred, c(1:ncell(pred)))
# 
# 
# moddf <- modis[]
# preddf <- pred[]
# 
# saveRDS(moddf, paste0(predoutpath, "mod_pred_scatter/moddf.RDS"))
# saveRDS(preddf, paste0(predoutpath, "mod_pred_scatter/preddf.RDS"))
# 
# 
# moddf <- readRDS(paste0(predoutpath, "mod_pred_scatter/moddf.RDS"))
# preddf <- readRDS(paste0(predoutpath, "mod_pred_scatter/preddf.RDS"))
# 
# 
# moddf <- data.frame(moddf)
# preddf <- data.frame(preddf)
# 
# head(moddf)
# head(preddf)
# 
# 
# preddfc <- preddf[complete.cases(preddf),]
# moddfc <- moddf[complete.cases(moddf),]

# 
# plot(moddf$MODIS2019001.1315, preddf$prediction.1)
# library(ggplot2)
# library(viridis)
# library(raster)
# library(mapview)
# 
# 
# pat <- "2019002.2030"
# 
# k <- rasslicepos <- which(grepl(pat, nam_rasslice))
# 
# orgmodloc <- "D:/new_downscaling/data_download_preprocessing/MODIS/2019-01/LST/"
# 
# #small_proj_MOD11_L2.A2019001.1315.006.2019029180309.tif
# 
# modorglistfiles <- list.files(orgmodloc, pattern=pat, full.names = T)
# orgmod <- raster(modorglistfiles[grepl("small_proj", modorglistfiles)])
# 
# mapview( prediction_response[[rasslicepos]]$prediction) + mapview(orgmod)
# 
# 
# mapview(prediction_response[[1]][[1]])+mapview(prediction_response[[1]]$prediction)+
#   mapview(prediction_response[[1]]$Landsat)
# 
# 
# i=1
# k = 2
# mapview(prediction_response[[k]][[1]])+mapview(prediction_response[[k]]$prediction)+
#   mapview(prediction_response[[k]]$Landsat)
# 
# 
# df <-data.frame( cbind(moddf[,1], preddf[,i]))
# names(df) <- c("mod", "ds")
# df$mod[df$mod < (-50)] <- NA
# df <- df[complete.cases(df),]
# 
# 
# ggplot(df, aes(df[,1],df[,2]))+
#   xlab("MODIS original")+ylab("downscaled")+
#   ggtitle(paste0("downscaled LST vs original ", names(moddf)[i]))+
#   stat_binhex(bins=1000)+
#   geom_abline(slope=1,intercept=0)+
#   scale_x_continuous(expand=c(0,0))+
#   scale_y_continuous(expand=c(0,0))+
#   scale_fill_gradientn(name="datapoints", trans="log", colours=viridis(10),
#                        breaks=10^(0:4))
# 
# 
# ggplot(df,aes(df[,1], ))



