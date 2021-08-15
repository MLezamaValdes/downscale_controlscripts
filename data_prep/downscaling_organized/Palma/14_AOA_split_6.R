
# library(slurmR, lib.loc="/home/l/llezamav/R/")
library(raster)
library(caret,lib.loc="/home/l/llezamav/R/")
library(CAST,lib.loc="/home/l/llezamav/RCast/")
library(parallel)
library(doParallel)

##############################################################################

aoapath <- "/scratch/tmp/llezamav/aoa/"
modelpath <- "/scratch/tmp/llezamav/modeling_after_talk/"
predstackdir <- "/scratch/tmp/llezamav/predstacks/"
tempdir()

# modelpath <- "D:/downscaling_after_talk/models/"
# trainpath <- "/scratch/tmp/llezamav/train_valid/"
# stackpath <- "/scratch/tmp/llezamav/satstacks_ngb/"
# aoapath <- "D:/downscaling_after_talk/aoa/"
# predstackdir <- "D:/downscaling_after_talk/spatial_predictions_rf/predstacks/"

method <- "rf"

########## aoa on test 3 #############################################


load(paste0(modelpath, "final_model_rf_150000factorfast_mtry.RData"))
print(model_final$finalModel$xNames)

# test1 <- read.csv2(paste0(trainpath, "validation_1_LHS_150000.csv"))
# test2 <- read.csv2(paste0(trainpath, "validation_2_LHS_150000.csv"))
# test3 <- read.csv2(paste0(trainpath, "validation_3_LHS_150000.csv"))
# 
# testlist <- list(test1, test2, test3)
# 
# aoatest1to3 <- lapply(seq(testlist), function(i){
#   test <- testlist[[i]]
#   test$TeAq <- as.factor(substring(test$Mscene,1,3))
#   test$TeAqNum <- as.numeric(test$TeAq)
#   print(names(test))
#   
#   # add this in when final model 
#   # test$soilraster <- factor(test$soilraster)
#   # test$TeAqNum <- factor(test$TeAqNum)
#   # test$landcoverres <- factor(test$landcoverres)
#   
#   
#   #take only predictors
#   test <- test[,model_final$finalModel$xNames]
#   print("test, i.e. new data looks like this now:")
#   print(head(test))
#   
#   print(paste0("starting with aoa ", i, "now"))
#   aoa <- aoa(newdata=test, model=model_final)
#   
#   print(paste0("saving aoa ", i, "now"))
#   saveRDS(aoa, paste0(aoapath, "aoa_", method, "_", i, ".RDS"))
#   
#   return(table(aoa$AOA))
# })
# 
# saveRDS(aoatest1to3, paste0(aoapath, "table_aoa_test1to3.RDS"))

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
predstack <- stack(list.files(predstackdir, pattern="split_6.tif", full.names = T))
psnams <- read.csv2(list.files(predstackdir, pattern=".csv", full.names = T))

names(predstack) <- psnams$x


# ################## FACTOR TROUBLE CHECKKING ###########################################
# 
# str(model_final$trainingData)
# 
# predstack_org <- predstack
# 
# table(as.numeric(model_final$trainingData$soilraster)) # that are the numbers that need to go in the raster I suppose
# table(model_final$trainingData$soilraster) # these are the factor "names", i.e. what is now in the raster 
# 
# 
# # check which soil values are available in prediction raster
# av_un_pred <- sort(unique(predstack$soilraster[]))
# 
# # which soil values are missing in prediction raster relative to training dataset
# av_training_lev <- sort(unique(model_final$trainingData$soilraster[]))
# av_training_num <- sort(unique(as.numeric(model_final$trainingData$soilraster[])))
# 
# lev_num_soil_training <- data.frame(av_training_lev, av_training_num)
# 
# miss <- av_training_lev %in%  av_un_pred 
# miss_cat_in_pred <- as.numeric(as.character(av_training_lev[miss==FALSE]))
# 
# # add x soil values missing in prediction raster to the vector at places 
# # length(vector)+x, # convert to factor 
# fac_pred_soil <- factor(c(predstack$soilraster[], miss_cat_in_pred))
# levels(fac_pred_soil)
# 
# # eliminate the values that were not present in the raster
# # check whether all went well
# 
# length(predstack$soilraster[])
# tail(fac_pred_soil, n=50)
# 
# fac_pred_soil_crop <- fac_pred_soil[1:length(predstack$soilraster[])]
# tail(fac_pred_soil_crop, n=50)
# 
# # write the factor names (1-11) back into the raster 
# predstack$soilraster[] <- fac_pred_soil_crop
# 
# table(predstack$soilraster[])
# table(predstack_org$soilraster[])
# 
# table(as.numeric(factor(predstack$soilraster[])))
# 
# # activate for last model
# predstack$soilraster <- factor(predstack$soilraster)
# predstack_df$TeAqNum <- factor(predstack_df$TeAqNum)
# predstack_df$landcoverres <- factor(predstack_df$landcoverres)
# 

modname <- basename(list.files(predstackdir, pattern="split_6.tif", full.names = T))
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

######################################################################################
print(paste0("amount of samples aoa has to be calculated for: ", ncell(predstack)))

cores <- 40
cl <- makeCluster(cores)
registerDoParallel(cl)

################### aoa on test 3 ###################################
print("starting to run aoa")

aoa_spat <- aoa(newdata=predstack_df_within_ex, model=model_final,
                cl=cl)

saveRDS(aoa_spat, paste0(aoapath, "aoa_spatial_", method, "_", "split_6.RDS"))

print("aoa rds saved, proceeding to generate aoa raster")

aoa_ras <- predstack[[1]]
# aoa_ras[] <- NA
# fullexAOA <- withinextent
# fullexAOA[fullexAOA==TRUE] <- aoa_spat$AOA
# aoa_ras[] <- fullexAOA
aoa_ras[] <- aoa_spat$AOA 

print("writing aoa raster")

writeRaster(aoa_ras, paste0(aoapath, "aoa_spatial_", method, "_", "split_6.tif"),
            overwrite=T)
