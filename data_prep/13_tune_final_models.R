########## tune final models ###############################


# see Hanna's script: 
# https://github.com/HannaMeyer/AntAir/blob/0dcfb14dd93f2c44467e4450be685cca6f9f66b4/AntAir/02_TrainAndPredict/08_finalModel.R

rm(list=ls())

loc="Laptop"
if(loc=="Palma"){
  datpath <- "/scratch/tmp/llezamav/satstacks/train_test/"
  trainingDat <- readRDS(paste0(datpath, "train_DI__2019-10.rds"))
  
  library(CAST,lib.loc="/home/l/llezamav/R/")
  library(caret,lib.loc="/home/l/llezamav/R/")
  library(gbm,lib.loc="/home/l/llezamav/R/")
  library(parallel)
  library(doParallel)
  library(kernlab,lib.loc="/home/l/llezamav/R/")

}
if(loc=="Laptop"){
  datpath <- "C:/Users/mleza/OneDrive/Documents/PhD/work_packages/auto_downscaling_30m/model_ffs_Jan19/"
  trainingDat <- readRDS(paste0(datpath, "train_DI__2019-10.rds"))
  
  library(parallel)
  library(doParallel)
  library(randomForest)
  library(gbm)
  library(CAST)
  library(caret)
}


################################################################################
# Settings
################################################################################
seed <- 100
cores <- detectCores()-3
k <- length(unique(trainingDat$spatialblocks))
variables_within_SE <- FALSE # use only variables from ffs within SE of best model
methods <- c("rf",
             "gbm",
             "nnet",
             "svmLinear")

################################################################################
# help functions etc
################################################################################
scaling <- function(predictors,scaleStats){
  for (i in 1:ncol(predictors)){
    rowID <- which(row.names(scaleStats)==names(predictors)[i])
    predictors[,i] <- (predictors[,i]-scaleStats$mean[rowID])/scaleStats$sd[rowID]
  }
  return(predictors)
}
cl <- makeCluster(cores)
registerDoParallel(cl)
################################################################################
# Train models
################################################################################
n <- 15000

for (method in methods){
  ffs_model <- get(load(paste0(modelpath, "ffs_model_",method,"_", n, ".RData")))
  
  
  
  kval <- min(length(unique(train$time_num)), length(unique(train$spatialblocks)))
  
  # split training cuarter into various blocks for cv during training
  foldids <- CreateSpacetimeFolds(train, spacevar="spatialblocks", timevar = "time_num",
                                  k=kval,seed=100)
  
  # as in 11
  
  predictornames <- ffs_model$selectedvars

  
  predictors <- trainingDat[,which(names(trainingDat)%in%predictornames)]
  
  # tune finer
  
  if (method=="gbm"){
    tuneGrid <-  expand.grid(interaction.depth = seq(3,14,2), 
                             n.trees = c(100,200,300,400,500),
                             shrinkage = c(0.01,0.05,0.1),
                             n.minobsinnode = 10)
    
  }

  if (method=="rf"){
    tuneGrid <-  expand.grid(mtry = c(seq(2,ncol(predictors),2)))
  }
  if (method=="nnet"){
    predictors <- scaling(predictors,ffs_model$scaleStats)
    tuneGrid <- expand.grid(size = seq(2,ncol(predictors),1),
                            decay = seq(0,0.1,0.01))
  }
  
  response <- trainingDat$Landsat
  
  ctrl <- trainControl(method = "cv",
                       index = folds$index,
                       indexOut = folds$indexOut,
                       savePredictions = TRUE,
                       verboseIter = TRUE,
                       returnResamp = "all")
  
    if (method!="nnet"){
      model_final <- train(predictors,
                           response,
                           method = method,
                           tuneGrid = tuneGrid,
                           trControl = ctrl)
    }else{
      model_final <- train(predictors,
                           response,
                           method = method,
                           tuneGrid = tuneGrid,
                           trControl = ctrl,
                           trace = FALSE, #relevant for nnet
                           linout = TRUE)	
      
    }
  
  model_final$scaleStats <- ffs_model$scaleStats
  writeRDS(model_final,file=paste0(modelpath,"model_final_",method, "_", n,".Rds"))
}


stopCluster(cl)