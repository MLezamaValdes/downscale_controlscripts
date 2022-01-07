install.packages("/home/l/llezamav/R/CAST_0.5.1.tar.gz", 
                                   repos = NULL, lib="/home/l/llezamav/R/")

# library(slurmR, lib.loc="/home/l/llezamav/R/")
library(raster)
library(caret,lib.loc="/home/l/llezamav/R/")
library(CAST,lib.loc="/home/l/llezamav/R/")
library(parallel)
library(doParallel)

##############################################################################

aoapath <- "/scratch/tmp/llezamav/aoa/disassembly/"
modelpath <- "/scratch/tmp/llezamav/modeling_after_talk/"
predstackdir <- "/scratch/tmp/llezamav/predstacks/"
scriptpath <- "/home/l/llezamav/LST_after_paper/"
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

print("starting to calculate DI")

DI <- trainDI(model)
saveRDS(DI, paste0(aoapath, "DI_", method, ".RDS"))


# read predstack
predstack <- stack(list.files(predstackdir, 
                              pattern="predstack_MYD11_L2.A2018316.1350.006.2.tif", 
                              full.names = T))
psnams <- read.csv2(list.files(predstackdir, 
                               pattern=".csv", full.names = T))

names(predstack) <- psnams$x

modname <- tools::file_path_sans_ext(basename(list.files(predstackdir, 
                  pattern="predstack_MYD11_L2.A2018316.1350.006.2.tif$", 
                  full.names = T)))

# split predstack into 9 to run aoa on
source(paste0(scriptpath, "SplitRas.R"))
spoutdir <- paste0(predstackdir, "splits/")
outnam <- modname
dir.create(spoutdir)

SplitRas(raster=predstack, ppside=3, save=TRUE, plot=FALSE,
                       outdir=spoutdir, outnam=outnam)

splitfiles <- list.files(spoutdir, full.names=T)

# make cluster
cores <- 8
cl <- makeCluster(cores)
registerDoParallel(cl)

# per piece of the split prediction stack: apply AOA using DI 
lapply(seq(splitfiles), function(s){
  print(paste0("starting to apply aoa to split ", s))
  predstack <- stack(splitfiles[[s]])
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

  
  ################### aoa on test 3 ###################################
  
  print("apply AOA to raster")
  aoa_spat = aoa(newdata = predstack_df_within_ex, model = model_final, 
                 trainDI = DI, cl=cl)
  
  saveRDS(aoa_spat, paste0(aoapath, "aoa_spatial_", modname, "_split_", s,".RDS"))
  
  print("aoa rds saved, proceeding to generate aoa raster")
  
  aoa_ras <- predstack[[1]]
  # aoa_ras[] <- NA
  # fullexAOA <- withinextent
  # fullexAOA[fullexAOA==TRUE] <- aoa_spat$AOA
  # aoa_ras[] <- fullexAOA
  aoa_ras[] <- aoa_spat$AOA 
  
  print("writing aoa raster")
  
  writeRaster(aoa_ras, paste0(aoapath, "aoa_spatial_", modname, "_split_", s,".tif"),
              overwrite=T)
})

# mosaic 
aoalist <- raster(list.files(aoapath, full.names = T, pattern=".tif$"))
mos <- do.call(mosaic, raster.list)
writeRaster(mos, paste0(aoapath, "aoa_spatial_", modname, ".tif"),
            overwrite=T)