# modelpath <- "D:/downscaling_after_talk/models/"
# trainpath <- "D:/downscaling_after_talk/clean_data/train_valid/"
# stackpath <- "D:/downscaling_after_talk/clean_data/satstacks_ngb/"

library(raster)

modelpath <- "/scratch/tmp/llezamav/modeling_after_talk/"
trainpath <- "/scratch/tmp/llezamav/train_valid/"
stackpath <- "/scratch/tmp/llezamav/satstacks_ngb/"

test1_sc <- read.csv2(paste0(trainpath, "validation_1_LHS_150000_scaled.csv"))
test2_sc <- read.csv2(paste0(trainpath, "validation_2_LHS_150000_scaled.csv"))
test3_sc <- read.csv2(paste0(trainpath, "validation_3_LHS_150000_scaled.csv"))

load(paste0(modelpath, "final_model_nnet_150000dummyrun.RData"))

