########## tune final models ###############################

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
  
}





print("libraries loaded")

print("loading datasets")
train <- read.csv2(paste0(trainpath, "training_complete_150000_clhs.csv"))

print("loaded train dataset")

print(paste0("nrow train = ", nrow(train)))

testing=TRUE
if(testing){
  ### just for testing ###############
  
  # only if subset
  trainsubset <- train[sample(nrow(train),n), ]
  saveRDS(trainsubset, paste0(outpath, "train_subset_", n, ".rds"))
  train <- trainsubset 
  
  
} else {
  n <- nrow(train)
}


# see Hanna's script: 
# https://github.com/HannaMeyer/AntAir/blob/0dcfb14dd93f2c44467e4450be685cca6f9f66b4/AntAir/02_TrainAndPredict/08_finalModel.R


################################################################################
# Settings
################################################################################
seed <- 100
cores <- detectCores()-3

kval <- 4
print(paste("k crossvalidation folds:", kval))

train$ymo <- as.factor(train$ymo)

# split training cuarter into various blocks for cv during training
foldids <- CreateSpacetimeFolds(train, spacevar="spatialblocks", timevar = "ymo",
                                k=kval,seed=100)


(trainlength <- sapply(seq(foldids$indexOut), function(i){
  length(foldids$indexOut[[i]])
}))

set.seed(100)

metric <- "RMSE" # Selection of variables made by either Rsquared or RMSE
methods <- c("rf",
             "gbm",
             "nnet",
             "svmLinear")
withinSE <- FALSE # favour models with less variables or not?
response <- train$Landsat
n <- 15000

cl <- makeCluster(cores)
registerDoParallel(cl)
################################################################################
# Train models
################################################################################

for (i in 1:length(methods)){
  
  method <- methods[i]
  print(method)
  tuneLength <- 2
  tuneGrid <- NULL
  
  ffs_model <- get(load(paste0(modelpath, "ffs_model_",method,"_", n, ".RData")))
  predictornames <- ffs_model$selectedvars
  predictors <- train[,which(names(train)%in%predictornames)]
  print(paste0("amount predictors for ", method, " = ", ncol(predictors)))
  
  
  tctrl <- trainControl(method="cv", 
                        savePredictions = TRUE,
                        returnResamp = "all",
                        verboseIter=TRUE,
                        index=foldids$index,
                        indexOut=foldids$indexOut)
  
  

  
  if (method=="rf"){
    #   tuneLength <- 1
    tuneGrid <-  expand.grid(mtry = c(seq(2,ncol(predictors),2)))
    
  
    # Create A Data Frame From All Combinations Of Factor Variables, 
    # mtry: Number of variables randomly sampled as2-10 candidates at each split
  }
  if (method=="gbm"){
    tuneGrid <-  expand.grid(interaction.depth = seq(3,14,2), 
                             n.trees = c(100,200,300,400,500),
                             shrinkage = c(0.01,0.05,0.1),
                             n.minobsinnode = 10)
    
    #tuneLength <- 10
    predictors <- data.frame(scale(train[,c("Modis","ia", "hs", "dem",
                                            "slope", "aspect", "TWI",
                                            "soilraster",
                                            "swir6", "swir7")]))
    
    tctrl <- trainControl(method="repeatedcv",
                          number=kval,
                          repeats=kval,
                          savePredictions = TRUE,
                          returnResamp = "all",
                          verboseIter=TRUE,
                          index=foldids$index,
                          indexOut=foldids$indexOut)
    
  }
  if (method=="nnet"){
    #   tuneLength <- 1
    predictors <- data.frame(scale(predictors))
    # tuneGrid <- expand.grid(size = seq(2,ncol(predictors),2),
    #                         decay = seq(0,0.1,0.025))
    # 
    tuneGrid <- expand.grid(size = seq(2,ncol(predictors),1),
                            decay = seq(0,0.1,0.01))
    
    
  }
  
  if (method=="svmLinear"){ # C controls how large the support vectors, i.e. margins are
    #   tuneLength <- 1
    tuneGrid <- expand.grid(C= 2^c(0:4))
  }
  
  
  if(method=="gbm"){ # importance = TRUE kills it
    ffs_model <- ffs(predictors,response, 
                     method="gbm", 
                     trControl=tctrl,
                     tuneGrid=tuneGrid,
                     withinSE = FALSE,
                     tuneLength=tuneLength,           
                     metric=metric)
    
    
  } else {
    ffs_model <- ffs(predictors,
                     response,
                     metric=metric,
                     withinSE = withinSE,
                     method = method,
                     importance =TRUE,
                     tuneLength = tuneLength,
                     tuneGrid = tuneGrid,
                     trControl = tctrl,
                     trace = FALSE, #relevant for nnet
                     linout = TRUE) #relevant for nnet
    
    
  }
  
  save(ffs_model,file=paste0(outpath,"ffs_model_",method,cvmode,"_", n, ".RData"))
}

stopCluster(cl)














for (method in methods){
  ffs_model <- get(load(paste0(modelpath, "ffs_model_",method,"_", n, ".RData")))
  predictornames <- ffs_model$selectedvars
  predictors <- train[,which(names(trainingDat)%in%predictornames)]
  paste0("amount_predictors: ", ncol(predictors))
  
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
  
  response <- train$Landsat
  
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