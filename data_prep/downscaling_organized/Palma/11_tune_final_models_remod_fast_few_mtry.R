########## tune final models ###############################

# library(Rmpi)  # R implementation of MPI interface
# library(doMPI,lib.loc="/home/l/llezamav/R/") # interface for foreach construct to run in MPI parallel mode

library(slurmR,lib.loc="/home/l/llezamav/R/") # This also loads the parallel package


# 
# # Load the R MPI package if it is not already loaded.
# if (!is.loaded("mpi_initialize")) {
#   library("Rmpi")
# }
# 
# ns <- mpi.universe.size()
# print(ns)
# mpi.spawn.Rslaves(nslaves=ns)
# #
# # In case R exits unexpectedly, have it automatically clean up
# # resources taken up by Rmpi (slaves, memory, etc...)
# .Last <- function(){
#   if (is.loaded("mpi_initialize")){
#     if (mpi.comm.size(1) > 0){
#       print("Please use mpi.close.Rslaves() to close slaves.")
#       mpi.close.Rslaves()
#     }
#     print("Please use mpi.quit() to quit R")
#     .Call("mpi_finalize")
#   }
# }
# # Tell all slaves to return a message identifying themselves
# mpi.bcast.cmd( id <- mpi.comm.rank() )
# mpi.bcast.cmd( ns <- mpi.comm.size() )
# mpi.bcast.cmd( host <- mpi.get.processor.name() )
# mpi.remote.exec(paste("I am",mpi.comm.rank(),"of",mpi.comm.size()))

# rm(list=ls())
print("loading Palma libs and paths")


n <- 150000


loc="Palma"
if(loc=="Palma"){
  
  outpath <- "/scratch/tmp/llezamav/modeling_after_talk/"
  trainpath <- "/scratch/tmp/llezamav/train_valid/"
  modelpath <- outpath
  

  library(CAST,lib.loc="/home/l/llezamav/R/")
  library(caret,lib.loc="/home/l/llezamav/R/")
  library(gbm,lib.loc="/home/l/llezamav/R/")
  library(parallel)
  library(doParallel)
  library(kernlab,lib.loc="/home/l/llezamav/R/")
  #testsubset <- read.csv2("/scratch/tmp/llezamav/satstacks/extraction_result_new/test_n2000.csv")
  
  
  print("loading datasets")
  train <- read.csv2(paste0(trainpath, "train_LHS_150000.csv"))
  
  print("loaded train dataset")

}
if(loc=="Laptop"){

  library(parallel)
  library(doParallel)
  library(randomForest)
  library(gbm)
  library(CAST)
  library(caret)
  trainpath <- "D:/downscaling_after_talk/clean_data/train_valid/"
   modelpath <- "D:/downscaling_after_talk/models/"
   outpath <- modelpath
 
  
  train <- read.csv2(paste0(trainpath, "train_LHS_150000.csv"))

}





print("libraries loaded")


print(paste0("nrow train = ", nrow(train)))

testing=FALSE


# see Hanna's script: 
# https://github.com/HannaMeyer/AntAir/blob/0dcfb14dd93f2c44467e4450be685cca6f9f66b4/AntAir/02_TrainAndPredict/08_finalModel.R


################################################################################
# Settings
################################################################################
seed <- 50
# cores <- detectCores()-3

kval <- 10
print(paste("k crossvalidation folds:", kval))



############## ADD MOD/MYD info as predictor ##################################
train$ymo <- as.factor(train$ymo)

train$TeAq <- as.factor(substring(train$Mscene,1,3))
train$TeAqNum <- as.numeric(train$TeAq)

train$soilraster <- factor(train$soilraster)
train$TeAqNum <- factor(train$TeAqNum)
train$landcoverres <- factor(train$landcoverres)

  

# split training cuarter into various blocks for cv during training
foldids <- CreateSpacetimeFolds(train, spacevar="spatialblocks", timevar = "ymo",
                                k=kval,seed=50)


(trainlength <- sapply(seq(foldids$indexOut), function(i){
  length(foldids$indexOut[[i]])
}))

set.seed(100)

metric <- "RMSE" # Selection of variables made by either Rsquared or RMSE
# methods <- c("rf",
#              "nnet",
#              #"svmLinear",
#              "gbm")
methods <- c("rf")
# methods <- c("gbm")
response <- train$Landsat
n <- 150000


################################################################################
# Train models
################################################################################
rf_remodelling <- c("factor")

i=1



# for (i in 1:length(rf_remodelling)){
  
  method <- "rf"
  print(method)
  
  mp <- list.files(modelpath, pattern=rf_remodelling[i], full.names=T)
  print(basename(mp))
  #load(paste0(modelpath, "ffs_model_",method,"time_only_", n, ".RData"))
  load(mp)
  
  tuneLength <- 2
  tuneGrid <- NULL
  
  print(paste0("model name = ", "ffs_model_",method,"_", n, ".RData"))
  predictornames <- ffs_model$selectedvars
  print(predictornames)
  print(varImp(ffs_model))
  predictors <- train[,which(names(train)%in%predictornames)]
  
  str(predictors)

  print(paste0("amount predictors for ", method, " = ", ncol(predictors)))
  pv <- ncol(predictors)
  
  
  tctrl <- trainControl(method="cv", 
                        savePredictions = TRUE,
                        returnResamp = "all",
                        verboseIter=TRUE,
                        index=foldids$index,
                        indexOut=foldids$indexOut)
  
  #### tuning settings ##############
  
  if (method=="rf"){
    #   tuneLength <- 1
    tuneGrid <-  expand.grid(mtry = c(seq(2,(pv-4),1)))
    
  
    # Create A Data Frame From All Combinations Of Factor Variables, 
    # mtry: Number of variables randomly sampled as2-10 candidates at each split
  }
  if (method=="gbm"){
    tuneGrid <-  expand.grid(interaction.depth = seq(3,14,2), 
                             n.trees = c(100,200,300,400,500),
                             shrinkage = c(0.01,0.05,0.1),
                             n.minobsinnode = 10)
    
    #tuneLength <- 10
    predictors <- data.frame(scale(predictors))
    
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
    # tuneGrid <- expand.grid(size = seq(2,ncol(predictors),1),
    #                         decay = seq(0,0.1,0.01))
    
    tuneGrid <- expand.grid(decay = c(0.5, 0.1, 1e-2, 1e-3, 1e-4, 1e-5, 1e-6, 1e-7),
                size = c(1, 2, 3, 5, 10, 20))
    
    
  }
  
  if (method=="svmLinear"){ # C controls how large the support vectors, i.e. margins are
    #   tuneLength <- 1
    tuneGrid <- expand.grid(C= c(0:16))

  }
  
  
  #### training ##############
  
  
  if(method=="nnet"){ # importance = TRUE kills it
    model_final <- train(predictors,
                         response,
                         method = method,
                         tuneGrid = tuneGrid,
                         trControl = tctrl,
                         trace = FALSE, #relevant for nnet
                         linout = TRUE) # relevant for nnet
  

  }  else if(method=="gbm"){
    
    model_final <- train(predictors[-6],
                         response, 
                         method=method, 
                         trControl=tctrl,
                         tuneGrid=tuneGrid,
                         withinSE = FALSE,
                         tuneLength=tuneLength,           
                         metric=metric)


  } else {
    
    
    # Making the cluster, and exporting the variables
    cl <- makeSlurmCluster(145, outfile="/home/l/llezamav/11_final_rf.txt")
    
    # Approximation
    jnk = clusterEvalQ(cl,{library(caret,lib.loc="/home/l/llezamav/R/"); 
      library(raster)})
    clusterExport(cl, list("modelpath", "predictors", "response", "method", 
                           "tuneGrid", "tctrl", "n", "rf_remodelling"))
    
    
  
    model_final <- train(predictors,
                         response,
                         method = method,
                         tuneGrid = tuneGrid,
                         trControl = tctrl,
                         ntree=400,
                         trace = FALSE, #relevant for nnet
                         linout = TRUE)	

  }
  
  save(model_final,file=paste0(modelpath,"final_model_",method,"_", 
                               n, rf_remodelling[i], "fast_mtry.RData"))
# }

  
  # ## MPI-save version of R quit
  # mpi.quit()
stopCluster(cl)
