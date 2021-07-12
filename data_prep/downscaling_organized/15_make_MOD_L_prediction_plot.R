
library(ggplot2)
library(viridis)
library(gridExtra)
library(grid)
library(caret)
library(raster)

modelpath <- "D:/downscaling_after_talk/models/"
# trainvalidpath <- "D:/downscaling_after_talk/clean_data/train_valid/"
regressionStatsRsenal <- source("C:/Users/mleza/OneDrive/Documents/PhD/ML_dist/ML_dist/regressionstats_Rsenal.R")

figurepath <- "C:/Users/mleza/OneDrive/Documents/PhD/work_packages/auto_downscaling_30m/paper/paper_draft/figures/new/"

rasterpath <- "D:/downscaling_after_talk/clean_data/satstacks_ngb/"
predoutpath <- "D:/downscaling_after_talk/predictions/"
predstackdir <- "D:/downscaling_after_talk/spatial_predictions_rf/predstacks/"



method <- "rf"

fm <- list.files(modelpath, pattern="mtry", full.names=T)
load(fm)

n <- 150000

outpath <- "D:/downscaling_after_talk/spatial_predictions_rf/plots/"

plotstack <- stack(list.files(paste0(outpath, "plots/"), pattern="2.grd", full.names = T))


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
# i=k
#   sporg <- raster(paste0(predoutpath, "pred_rf_MYD11_L2.A2018316.1350.006.2.tif"))
#   sp <- raster(paste0(predoutpath, "pred_rf_MYD11_L2.A2018316.1350.006.2bilinmod.tif"))
#   spnam <- names(sp)
# 
#   # sp <- raster(list.files(predoutpath, pattern=pat[i], full.names=T)[1])
#   plotstack <- stack(rasslice_landsat[[k]][[1]], sp,rasslice[[k]][[1]] )
#   
# #  
#   plotstack <- list.files()

  plot(plotstack[[1]])
  plot(plotstack[[3]])

names(plotstack) <- c("MODIS", "downscaled_MODIS","Landsat")

  e1 <- drawExtent()
  
  xmin       : 407395.233177807 
  xmax       : 424710.511878379 
  ymin       : -1307003.31721416 
  ymax       : -1287956.51064354 
    # e1 <- c(377567.5, 390992.4,  -1290792 , -1282961)

  plotstack_small_1 <- crop(plotstack, e1)

  e2 <- extent(414049.3, 430820.4 , -1275424 ,-1256933)
  plotstack_small_2 <- crop(plotstack, e2)
  
  
  library(rasterVis)
  library(sf)
  
  e1sf <- st_as_sf(st_as_sfc(st_bbox(e1, crs = st_crs(plotstack))))
  e2sf <- st_as_sf(st_as_sfc(st_bbox(e2, crs = st_crs(plotstack))))
  
  e1sf$area <- as.numeric(st_area(e1sf))
  e2sf$area <- as.numeric(st_area(e2sf))
  plot(e2sf,add=T)
  plot(e1sf,add=T)
  
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
