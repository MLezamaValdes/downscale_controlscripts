#### external evaluation 

rm(list=ls())
print("loading Palma libs and paths")

n <- 1500


loc="Laptop"
if(loc=="Palma"){
  
  outpath <- "/scratch/tmp/llezamav/modelling/"
  trainpath <- "/scratch/tmp/llezamav/satstacks/extraction_result_new/train_test/"
  
  
  library(CAST,lib.loc="/home/l/llezamav/R/")
  library(caret,lib.loc="/home/l/llezamav/R/")
  library(gbm,lib.loc="/home/l/llezamav/R/")
  library(parallel)
  library(doParallel)
  library(kernlab,lib.loc="/home/l/llezamav/R/")
  testsubset <- read.csv2("/scratch/tmp/llezamav/satstacks/extraction_result_new/test_n2000.csv")
  modelpath <- "/scratch/tmp/llezamav/modelling/"
  
}
if(loc=="Laptop"){
  
  library(parallel)
  library(doParallel)
  library(randomForest)
  library(gbm)
  library(CAST)
  library(caret)
  trainpath <- "C:/Users/mleza/OneDrive/Desktop/"
  outpath <- trainpath
  testsubset <- read.csv2(paste0(trainpath, "test_n2000.csv"))
  modelpath <- "C:/Users/mleza/OneDrive/Documents/PhD/work_packages/auto_downscaling_30m/testmodel_allyears/"
  regressionStatsRsenal <- source("C:/Users/mleza/OneDrive/Documents/PhD/ML_dist/ML_dist/regressionstats_Rsenal.R")
  
  
}


# read stack



print("libraries loaded")

n <- 15000

print("loading datasets")
train <- read.csv2(paste0(trainpath, "training_complete_150000_clhs.csv"))

metric <- "RMSE" # Selection of variables made by either Rsquared or RMSE
methods <- c("rf",
             "gbm",
             "nnet",
             "svmLinear")
response <- train$Landsat

for (i in 1:length(methods)){
  
  method <- methods[i]
  print(method)
  tuneLength <- 2
  tuneGrid <- NULL
  
  ffs_model <- get(load(paste0(modelpath, "ffs_model_",method,"_", n, ".RData")))
  predictornames <- ffs_model$selectedvars
  predictors <- train[,which(names(train)%in%predictornames)]
  print(paste0("amount predictors for ", method, " = ", ncol(predictors)))
  
  regressionStatsRsenal
  predictions <- ffs_model$pred[ffs_model$pred$mtry==ffs_model$bestTune$mtry,c("pred","obs")]
  s <- regressionStats(predictions$pred,predictions$obs)
  
  
  
  spatialpred <- predict(predictors,ffs_model)
  
  

}

#### spatial prediction


