# testing correspondence between L_MOD_ia_hs & original MODIS images
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

date <- c("2019-11")
i=1

########################### GET STACKS FOR MODEL #################################################
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


# sliced raster stack list for prediction
rasslice <- lapply(seq(hi), function(x){
  stack(ras[[lo[x]:hi[x]]])
})

allmod <- stack(lapply(rasslice, '[[', 1))
allmodnames <- names(allmod)

########################### GET ORIGINAL DATA #################################################
orgmodpath <- "D:/new_downscaling/data_download_preprocessing/MODIS/"
orgmodpath_date <- paste0(orgmodpath, date, "/LST/")

orgmod <- stack(list.files(orgmodpath_date, pattern="small_proj", full.names = T))
names(orgmod)

orgmodnames_pos_modstack <- sapply(seq(names(orgmod)), function(x){
  which(grepl(substring(names(orgmod)[x],20,40), allmodnames))
})

par(mfrow=c(1,2))
j = 4
plot(orgmod[[j]], main=paste0("original ",
                              names(orgmod[[j]])))
plot(allmod[[orgmodnames_pos_modstack[[j]]]])


