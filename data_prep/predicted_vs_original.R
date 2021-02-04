# original MODIS vs. predicted MODIS
library(raster)
library(rasterVis)

predpath <- "C:/Users/mleza/OneDrive/Documents/PhD/work_packages/auto_downscaling_30m/raster_predictions/"

date <- c("2019-11")



########################### GET unique PREDICTED RASTERS #################################################
prednam <- list.files(paste0(predpath, date), full.names = F)
predfiles <- list.files(paste0(predpath, date), full.names = T)
predfilepos <- which(!duplicated(substring(prednam, 1, 44)))
pred <- stack(predfiles[predfilepos])

########################### GET Landsat Modis RASTERS #############################################

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
lo <- lo


# sliced raster stack list for prediction
rasslice <- lapply(seq(hi), function(x){
  stack(ras[[lo[x]:hi[x]]])
})

rasslice_renamed <- lapply(seq(rasslice), function(i){
  names(rasslice[[i]][[1]]) <- paste0("L_", substring(names(rasslice[[i]][[1]]), 18,25))
  names(rasslice[[i]][[2]]) <- paste0("M_", substring(names(rasslice[[i]][[2]]), 11,22))
  rasslice[[i]]
})


########################### GET ORIGINAL RASTERS #################################################


orgmodpath <- "D:/new_downscaling/data_download_preprocessing/MODIS/"
orgmodpath_date <- paste0(orgmodpath, date, "/LST/")

orgmod <- stack(list.files(orgmodpath_date, pattern="small_proj", full.names = T))
names(orgmod)

# with which original image to pare prediction
orgmodnames_pos_modstack <- sapply(seq(nlayers(pred)), function(x){
  which(grepl(substring(names(pred[[x]]),20,40), names(orgmod)))
})

# MODIS original vs. downscaled
for(j in seq(nlayers(pred))){  
  orgnamj <- substring(names(orgmod[[orgmodnames_pos_modstack[[j]]]]), 22, 33)
  prednamj <- substring(names(pred[[j]]), 19, 30)
  tiff(paste0(figurepath, "pred_org/original_pred_", orgnamj, ".tif"), compression = "none",
       width=1200, height=1000)
  #plot(orgmod[[orgmodnames_pos_modstack[[j]]]], main=paste0("original ", orgnamj))
  p1 <- levelplot(orgmod[[orgmodnames_pos_modstack[[j]]]], main=paste0("original ", orgnamj))
  #plot(pred[[j]], main=paste0("prediction ", prednamj))
  p2 <- levelplot(pred[[j]], main=paste0("prediction ", prednamj))
  grid.arrange(p1, p2, ncol=2)
  dev.off()
}


orgmod_for_plot <- orgmod
names(orgmod_for_plot) <- paste0("M",substring(names(orgmod), 26,33))
bwp1 <- bwplot(orgmod_for_plot, main="MOD11_L2 Nov 2019")
predunique <- which(!duplicated(substring(names(pred), 1,48)))
pred_for_plot <- pred[[predunique]]
names(pred_for_plot) <- paste0("p", substring(names(pred[[predunique]]), 23,30))

bwp2 <- bwplot(pred_for_plot, main="Pred Nov 2019")

grid.arrange(bwp1, bwp2)


####### take original MODIS and resample to Landsat resolution ######

# original Landsat (x axis)
template <- raster("D:/new_downscaling/template_new.tif")

# find out which rasslice corresponds to the orgmod we're looking at
rasslice_pos <- lapply(seq(nlayers(orgmod)), function(i){
  sapply(seq(rasslice), function(j){
    if(length(which(grepl(substring(names(orgmod[[i]]), 22,33), names(rasslice[[j]])))) > 0){
      which(grepl(substring(names(orgmod[[i]]), 22,33), names(rasslice[[j]])))
    } else {
      0
    }
  })
})

# find out which pred corresponds to the orgmod we're looking at
pred_pos <- lapply(seq(nlayers(orgmod)), function(i){
      which(grepl(substring(names(orgmod[[i]]), 22,33), names(pred)))
})

# for i in orgmod

# nearest neighbor o.Ä. resampled MODIS (y plot 1)
orgmod_resampled <- resample(orgmod, template, method="ngb")


############################# SPATIAL COMPARISON PLOTS #########################################
istack <- lapply(seq(nlayers(orgmod)), function(i){
  print(i)
  rm(predi)
  orgmodi <- orgmod_resampled[[i]]
  orgid <- substring(names(orgmodi), 22,33)
  
  # get Landsat
  matchpos <- which(rasslice_pos[[i]]!=0)
  if(length(matchpos)==1){
    x <- 1
    rm(predi)
    response <- rasslice_renamed[[matchpos]][[1]]
    
    # get pred
    predi <- pred[[pred_pos[[i]]]]
    
    names(orgmodi) <-paste0("M_",  substring(names(orgmodi), 22,33))
    names(predi) <- paste0("pred_",substring(names(predi), 19,30))
    
    
    istack <- stack(orgmodi, predi, response)
    
    png(paste0(figurepath, "Org_resp_downscaled/M_pred_L_", orgid, ".png"),
        width=1200, height=1000)
    p <- spplot(istack, main="Original Modis, downscaling result and Landsat 8")
    print(p)
    dev.off()
    
  } else {
    p <- lapply(seq(length(matchpos)), function(x){
      rm(predi)
      response <- rasslice_renamed[[matchpos[x]]][[1]]
      
      # get pred
      predi <- pred[[pred_pos[[i]]]]
      
      names(orgmodi) <-paste0("M_",  substring(names(orgmodi), 22,33))
      names(predi) <- paste0("pred_",substring(names(predi), 19,30))
      
      
      istack <- stack(orgmodi, predi, response)
      
      png(paste0(figurepath, "Org_resp_downscaled/M_pred_L_", orgid, ".png"),
          width=1200, height=1000)
      p <- spplot(istack, main="Original Modis, downscaling result and Landsat 8")
      print(p)
      dev.off()
      
    })
  }    
  return(istack)
})

istack <- plots

############################# SCATTERPLOTS #########################################


for(i in seq(istack)){
  # make scatterplots
  istackval <- data.frame(istack[[i]][])
  istackval_comp <- istackval[complete.cases(istackval),]
  
  scat1 <- ggplot(istackval_comp, aes(istackval_comp[,3],istackval_comp[,1]))+
    xlab("Landsat LST (response)")+ylab("MODIS resampled to 30m")+
    ggtitle("Landsat vs. MODIS RESAMPLED to 30m")+
    stat_binhex(bins=300)+
    geom_abline(slope=1,intercept=0)+
    scale_x_continuous(expand=c(0,0))+
    scale_y_continuous(expand=c(0,0))+
    scale_fill_gradientn(name="datapoints", trans="log", colours=viridis(10), 
                         breaks=10^(0:4))
  
  scat2 <- ggplot(istackval_comp, aes(istackval_comp[,3],istackval_comp[,2]))+
    xlab("Landsat LST (response)")+ylab("MODIS downscaled to 30m")+
    ggtitle("Landsat vs. MODIS DOWNSCALED to 30m")+
    stat_binhex(bins=300)+
    geom_abline(slope=1,intercept=0)+
    scale_x_continuous(expand=c(0,0))+
    scale_y_continuous(expand=c(0,0))+
    scale_fill_gradientn(name="datapoints", trans="log", colours=viridis(10), 
                         breaks=10^(0:4))
  
  ggsave(paste0(figurepath, "Scatter_resampling_vs_downscaling_", i, ".png"), 
      width=1000, height=460)
  grid.arrange(scat1,scat2, ncol=2)
  dev.off()
  
}


############################### make RMSE / R² plots ######################

rsqdf <- lapply(seq(istack), function(i){
  mdate <- substring(names(istack[[i]])[1], 3)
  istackval <- data.frame(istack[[i]][])
  istackval_comp <- istackval[complete.cases(istackval),]
  
  cormat <- cor(istackval_comp)^2
  LMOD <- cormat[3,1]
  Ldownsc <- cormat[3,2]
  
  data.frame(mdate=rep(mdate, 2), 
             rsq= c(LMOD, Ldownsc), 
             dwnsc_rsmpl=c("LMOD","Ldownsc"))
})

rsqdf_comp <- do.call("rbind", rsqdf)
head(rsqdf_comp)

ggplot(rsqdf_comp, aes(x=mdate, y=rsq, color=dwnsc_rsmpl))+
  geom_point()


