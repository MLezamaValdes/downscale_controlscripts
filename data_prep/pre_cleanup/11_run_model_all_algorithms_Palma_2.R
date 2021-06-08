

print("loading Palma libs and paths")

outpath <- "/scratch/tmp/llezamav/modelling/"
trainpath <- "/scratch/tmp/llezamav/satstacks/extraction_result_new/train_test/"

install.packages("/home/l/llezamav/R/kernlab_0.9-29.tar.gz", repos = NULL, lib="/home/l/llezamav/R/")
                 
                 
library(parallel)
library(doParallel)
library(CAST,lib.loc="/home/l/llezamav/R/")
library(caret,lib.loc="/home/l/llezamav/R/")
library(gbm,lib.loc="/home/l/llezamav/R/")
library(Cubist,lib.loc="/home/l/llezamav/R/")
library(pls,lib.loc="/home/l/llezamav/R/")
library(kernlab,lib.loc="/home/l/llezamav/R/")

testing=FALSE


print("libraries loaded")
print("loading datasets")

train <- read.csv2(paste0(trainpath, "training_complete_150000_clhs.csv"))

print("loaded train dataset")

if(testing){
  ### just for testing ###############
  n <- 150000
  
  # only if subset
  trainsubset <- train[sample(nrow(train),n), ]
  saveRDS(trainsubset, paste0(outpath, "train_subset_", n, ".rds"))
  train <- trainsubset 
  
  testsubset <- read.csv2("/scratch/tmp/llezamav/satstacks/extraction_result_new/test_n2000.csv")
  
} else {
  n <- nrow(train)
}

#################

# kval <- min(length(unique(train$time_num)), length(unique(train$spatialblocks)))

kval <- 3 # for faster ffs, then in final model more k
print(paste("k crossvalidation folds:", kval))

train$ymo <- as.factor(train$ymo)

# split training cuarter into various blocks for cv during training
# if(cvmode=="time_only"){
#   foldids <- CreateSpacetimeFolds(train, timevar = "ymo",
#                                   k=kval,seed=100)
# } else {
  foldids <- CreateSpacetimeFolds(train, spacevar="spatialblocks", timevar = "ymo",
                                  k=kval,seed=100)
# }

(trainlength <- sapply(seq(foldids$indexOut), function(i){
  length(foldids$indexOut[[i]])
}))

set.seed(100)

metric <- "RMSE" # Selection of variables made by either Rsquared or RMSE
# methods <- c("rf",
#              "gbm",
#              "nnet",
#              "svmLinear")
methods <- c("svmLinear")
withinSE <- FALSE # favour models with less variables or not?
response <- train$Landsat
predictors <- train[,c("Modis","ia", "hs", "dem", 
                       "slope", "aspect", "TWI", 
                       "soilraster", "landcoverres", 
                       "swir6", "swir7")]
length(predictors)

cores <- detectCores()
print(paste("cores = ", cores))
cl <- makeCluster(cores-3, outfile="/home/l/llezamav/par_algorithms.txt")
registerDoParallel(cl)

for (i in 1:length(methods)){
  
  tctrl <- trainControl(method="cv", 
                        savePredictions = TRUE,
                        returnResamp = "all",
                        verboseIter=TRUE,
                        index=foldids$index,
                        indexOut=foldids$indexOut)
  
  
  method <- methods[i]
  print(method)
  tuneLength <- 2
  tuneGrid <- NULL
  
  if (method=="rf"){
    #   tuneLength <- 1
    tuneGrid <- expand.grid(mtry=seq(2,3))
    # Create A Data Frame From All Combinations Of Factor Variables, 
    # mtry: Number of variables randomly sampled as2-10 candidates at each split
  }
  if (method=="gbm"){
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
    tuneGrid <- expand.grid(size = seq(2,ncol(predictors),2),
                            decay = seq(0,0.1,0.025))
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
    
  } else if(method=="rf"){ # importance = TRUE kills it
    ffs_model <- ffs(predictors,response, 
                     method=method, 
                     ntree=100,
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
  
  save(ffs_model,file=paste0(outpath,"ffs_model_",method,"_", n, ".RData"))
}

stopCluster(cl)

