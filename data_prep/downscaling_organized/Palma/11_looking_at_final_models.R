
library(caret)
modelpath <- "D:/downscaling_after_talk/models/"

### ffs models 
load(paste0(modelpath, "ffs_model_rf_150000SE_F_factor.RData"))
load(paste0(modelpath, "ffs_model_gbm_150000dummys.RData"))
load(paste0(modelpath, "ffs_model_nnet_150000dummys.RData"))

ffs_model

ffs_model$finalModel$importance
plot(varImp(ffs_model))


### look into models
load(paste0(modelpath, "final_model_gbm_150000dummyrun.RData"))
load(paste0(modelpath, "final_model_nnet_150000dummyrun.RData"))

model_final
model_final$bestTune
format(5e-01 ,scientific=F)
0.5