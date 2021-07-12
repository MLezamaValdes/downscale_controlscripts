
# library(slurmR, lib.loc="/home/l/llezamav/R/")
library(raster)
library(caret,lib.loc="/home/l/llezamav/R/")
library(CAST,lib.loc="/home/l/llezamav/RCast/")
library(parallel)
library(doParallel)


##############################################################################


modelpath <- "/scratch/tmp/llezamav/modeling_after_talk/"
trainpath <- "/scratch/tmp/llezamav/train_valid/"
stackpath <- "/scratch/tmp/llezamav/satstacks_ngb/"
aoapath <- "/scratch/tmp/llezamav/aoa/"
predstackdir <- "/scratch/tmp/llezamav/predstacks/"
tempdir()

method <- "rf"

########## aoa on test 3 #############################################


load(paste0(modelpath, "final_model_rf_150000factorfast_mtry.RData"))
print(model_final$finalModel$xNames)

# read predstack
predstack <- stack(list.files(predstackdir, pattern="1350.006.2.tif$", full.names = T))
psnams <- read.csv2(list.files(predstackdir, pattern=".csv", full.names = T))

names(predstack) <- psnams$x


# 
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
#   test$soilraster <- factor(test$soilraster)
#   test$TeAqNum <- factor(test$TeAqNum)
#   test$landcoverres <- factor(test$landcoverres)
#   str(soilraster)
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

# ########## aoa on spatial predictors #################################


modname <- basename(list.files(predstackdir, pattern="1350.006.2.tif$"))
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

ncell(predstack[[1]])/150000

predstack_lowres <- aggregate(predstack, fact=18.5)

print(paste0("n for aggregated run = ", ncell(predstack_lowres)))


################## CONVERTING TO DF ###########################################
print("raster to df now")

predstack_df <- predstack_lowres[]
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

cores <- 50
cl <- makeCluster(cores)
registerDoParallel(cl)

################### aoa on test 3 ###################################
print("starting to run aoa")

aoa_spat <- aoa(newdata=predstack_df_within_ex, model=model_final,
                cl=cl)

saveRDS(aoa_spat, paste0(aoapath, "aoa_spatial_", method, "_", "resample.tif"))

print("aoa rds saved, proceeding to generate aoa raster")


aoa_ras <- predstack_lowres[[1]]
aoa_ras[] <- NA

fullexAOA <- withinextent
fullexAOA[fullexAOA==TRUE] <- aoa_spat$AOA
aoa_ras[] <- fullexAOA

print("writing aoa raster")

writeRaster(aoa_ras, paste0(aoapath, "aoa_spatial_", method, "_", "split_5.tif"))

stopCluster(cl)