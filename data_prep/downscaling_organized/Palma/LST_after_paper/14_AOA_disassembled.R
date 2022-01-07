install.packages("/home/l/llezamav/R/CAST_0.5.1.tar.gz", 
                                   repos = NULL, lib="/home/l/llezamav/R/")
library(raster)
library(caret,lib.loc="/home/l/llezamav/R/")
library(CAST,lib.loc="/home/l/llezamav/R/")
library(parallel)
library(doParallel)

##############################################################################

aoapath <- "/scratch/tmp/llezamav/aoa/disassembly/"
modelpath <- "/scratch/tmp/llezamav/modeling_after_talk/"
predstackdir <- "/scratch/tmp/llezamav/predstacks/"
tempdir()

# modelpath <- "D:/downscaling_after_talk/models/"
# trainpath <- "/scratch/tmp/llezamav/train_valid/"
# stackpath <- "/scratch/tmp/llezamav/satstacks_ngb/"
# aoapath <- "D:/downscaling_after_talk/aoa/"
# predstackdir <- "D:/downscaling_after_talk/spatial_predictions_rf/predstacks/"

method <- "rf"

load(paste0(modelpath, "final_model_rf_150000factorfast_mtry.RData"))
print(model_final$finalModel$xNames)

########## aoa on spatial predictors #################################

if(method=="nnet"){
  predictrs <- model_final$finalModel$coefnames
} else if (method=="rf") {
  predictrs <-model_final$finalModel$xNames
} else if (method == "gbm"){
  predictrs <- model_final$finalModel$xNames
}

# opts_slurmR$verbose_on()


# read predstack
predstack <- stack(list.files(predstackdir, pattern="predstack_MYD11_L2.A2018316.1350.006.2.tif$", full.names = T))
psnams <- read.csv2(list.files(predstackdir, pattern=".csv", full.names = T))

names(predstack) <- psnams$x

modname <- basename(list.files(predstackdir, pattern="predstack_MYD11_L2.A2018316.1350.006.2.tif$", full.names = T))
# TeAqNum <- predstack[[1]]

TeAqNum <- predstack$TeAqNum
if(grepl("MYD",modname)){
  TeAqNum[] <- 2
  TeAqNum[1] <- 1
} else {
  TeAqNum[] <- 1
  TeAqNum[1] <- 2
}

predstack$TeAqNum <- TeAqNum
names(predstack)
predstack <- predstack[[model_final$finalModel$xNames]]

################## CONVERTING TO DF ###########################################
print("raster to df now")

predstack_df <- predstack[]
predstack_df <- data.frame(predstack_df)

# activate for last model
predstack_df$soilraster <- factor(predstack_df$soilraster)
predstack_df$landcoverres <- factor(predstack_df$landcoverres)
predstack_df$TeAqNum <- factor(predstack_df$TeAqNum)

if(predstack_df$TeAqNum[5]==1){
  predstack_df$TeAqNum[1] <- 1
} else {
  predstack_df$TeAqNum[1] <- 2
}

str(predstack_df)

predstack_df <- predstack_df[model_final$finalModel$xNames]
withinextent <- !is.na(predstack_df$dem)
predstack_df_within_ex <- predstack_df[withinextent,]

print(nrow(predstack_df_within_ex))

######################################################################################
print(paste0("amount of samples aoa has to be calculated for: ", ncell(predstack)))

cores <- 5
cl <- makeCluster(cores, 
                  outfile=paste0("/home/l/llezamav/LST_after_paper/cluster_outfile_AOA.txt"))
registerDoParallel(cl)

################### aoa on test 3 ###################################
print("starting to run aoa")

# DI <- trainDI(model_final)
# saveRDS(DI, paste0(aoapath, "DI_", method, ".RDS"))
DI <- readRDS(paste0(aoapath, "DI_", method, ".RDS"))
aoa_spat = aoa(newdata = predstack_df_within_ex, model = model_final, 
                  trainDI = DI, 
                  cl=cl)

saveRDS(aoa_spat, paste0(aoapath, "aoa_spatial_", method, ".RDS"))

print("aoa rds saved, proceeding to generate aoa raster")

aoa_ras <- predstack[[1]]
# aoa_ras[] <- NA
# fullexAOA <- withinextent
# fullexAOA[fullexAOA==TRUE] <- aoa_spat$AOA
# aoa_ras[] <- fullexAOA
aoa_ras[] <- aoa_spat$AOA 

print("writing aoa raster")

writeRaster(aoa_ras, paste0(aoapath, "aoa_spatial_", method, ".tif"),
            overwrite=T)

