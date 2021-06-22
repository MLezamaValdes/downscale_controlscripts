modelpath <- "D:/downscaling_after_talk/models/"
trainpath <- "D:/downscaling_after_talk/clean_data/train_valid/"
stackpath <- "D:/downscaling_after_talk/clean_data/satstacks_ngb/"

library(raster)
library(CAST)

# modelpath <- "/scratch/tmp/llezamav/modeling_after_talk/"
# trainpath <- "/scratch/tmp/llezamav/train_valid/"
# stackpath <- "/scratch/tmp/llezamav/satstacks_ngb/"
# aoapath <- "/scratch/tmp/llezamav/aoa"

test1 <- read.csv2(paste0(trainpath, "validation_1_LHS_150000_scaled.csv"))
test2 <- read.csv2(paste0(trainpath, "validation_2_LHS_150000_scaled.csv"))
test3 <- read.csv2(paste0(trainpath, "validation_3_LHS_150000_scaled.csv"))


# TO DO: ADD SVMLINEAR

methods <- c("nnet", "rf", "gbm")
for(i in seq(methods)){
  method <- methods[i]
  
  
  
  if(method=="rf"){
    load(paste0(modelpath, "final_model_rf_150000SE_F.RData"))
  } else {
    load(paste0(modelpath, "final_model_", method, "_150000dummyrun.RData"))
  }
  
  if(method=="rf"){
    test1 <- read.csv2(paste0(trainpath, "validation_1_LHS_150000.csv"))
    test2 <- read.csv2(paste0(trainpath, "validation_2_LHS_150000.csv"))
    test3 <- read.csv2(paste0(trainpath, "validation_3_LHS_150000.csv"))
    
  }
  
  ########## aoa on test 3 #############################################
  
  aoa <- aoa(newdata=test3, model=model_final)
  saveRDS(aoa, paste0(aoapath, "aoa_", method, ".RDS"))
  
  # ########## aoa on spatial predictors #################################
  # 
  # if(method=="nnet"){
  #   predictrs <- model_final$finalModel$coefnames
  # } else if (method=="rf") { 
  #   predictrs <- rownames(model_final$finalModel$importance)
  # } else if (method == "gbm"){
  #   predictrs <- model_final$finalModel$xNames
  # }
  # 
  # # put auxiliary and dynamic data together
  # 
  # if(method=="rf"){ # use unscaled data 
  #   
  # } else { # use scaled data 
  #   
  # }
  # 
  # 
  # ################### aoa on test 3 ###################################
  # aoa <- aoa(newdata=test3, model=model_final)
  # 
  # ################### spatial aoa ###################################
  # 

}


# aoa_spatial <- 