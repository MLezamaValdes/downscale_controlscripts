

print("loading Palma libs and paths")

outpath <- "/scratch/tmp/llezamav/modeling_after_talk/"
trainpath <- "/scratch/tmp/llezamav/train_valid/"
# trainpath <- "D:/downscaling_after_talk/clean_data/train_test_DI/train_valid/"
# outpath <- paste0(trainpath, "modeling_after_talk/")


# install.packages("/home/l/llezamav/R/kernlab_0.9-29.tar.gz", 
#                       repos = NULL, lib="/home/l/llezamav/R/")
# install.packages("/home/l/llezamav/R/CAST_0.4.3.tar.gz", 
#                  repos = NULL, lib="/home/l/llezamav/R/")
# install.packages("/home/l/llezamav/R/caret_6.0-85.tar.gz", 
#                  repos = NULL, lib="/home/l/llezamav/R/")
# install.packages("/home/l/llezamav/R/gbm_2.1.5.tar.gz", 
#                  repos = NULL, lib="/home/l/llezamav/R/")
install.packages("/home/l/llezamav/R/Cubist_0.3.0.tar.gz", 
                 repos = NULL, lib="/home/l/llezamav/R/")
# install.packages("/home/l/llezamav/R/pls_2.7-3.tar.gz", 
#                  repos = NULL, lib="/home/l/llezamav/R/")


# library(parallel)
# library(doParallel)
# library(CAST)
# library(caret)
# library(gbm)
# library(Cubist)
# library(pls)
# library(kernlab) 
                 
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


# train <- read.csv2(list.files(trainpath, pattern="train", full.names=T), nrow=3000000)
train <- read.csv2(paste0(trainpath, "train_LHS_150000_scaled.csv"))
n <- 150000
print("loaded train dataset")

if(testing){
  ### just for testing ###############
  n <- 5000
  
  # only if subset
  trainsubset <- train[sample(nrow(train),n), ]
  saveRDS(trainsubset, paste0(outpath, "train_subset_", n, ".rds"))
  train <- trainsubset 
  
  testsubset <- read.csv2("D:/downscaling_after_talk/clean_data/train_test_DI/test_all_samples_2018-12.csv", nrow=5000)
  # testsubset <- read.csv2("/scratch/tmp/llezamav/satstacks/extraction_result_new/test_n2000.csv")
  
} else {
  n <- nrow(train)
}



# kval <- min(length(unique(train$time_num)), length(unique(train$spatialblocks)))

kval <- 3 # for faster ffs, then in final model more k
print(paste("k crossvalidation folds:", kval))


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
methods <- c("nnet")
response <- train$Landsat


# in goes:  TeAqNum.2 (MYD),  landcoverres.1 (for soil) and all soil categories apart from 12 (snow)
predictors <- train[,c("Modis_sc","ia_sc", "hs_sc", "dem_sc", 
                       "slope_sc", "aspect_sc", "TWI_sc", 
                       "soilraster.4",  "soilraster.5" ,  "soilraster.7" ,  "soilraster.9" ,  "soilraster.10" ,
                       "soilraster.13","soilraster.20","soilraster.21","soilraster.24",
                       "landcoverres.1" ,"TeAqNum.2")]
length(predictors)

cores <- detectCores()
print(paste("cores = ", cores))

cl <- makeCluster(cores-3, outfile="/home/l/llezamav/scripts_new/par_algorithms.txt")

#cl <- makeCluster(cores-3, outfile="/home/l/llezamav/par_algorithms.txt")
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
    # predictors <- data.frame(scale(train[,c("Modis","ia", "hs", "dem",
    #                                         "slope", "aspect", "TWI",
    #                                         "soilraster","landcoverres",
    #                                         "TeAqNum")]))
    
    #####
    
    length(predictors)
    
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
                     withinSE = TRUE,
                     tuneLength=tuneLength,           
                     metric=metric)
    
  } else if(method=="rf"){ # importance = TRUE kills it
    ffs_model <- ffs(predictors,response, 
                     method=method, 
                     ntree=100,
                     trControl=tctrl,
                     tuneGrid=tuneGrid,
                     withinSE = TRUE,
                     tuneLength=tuneLength,           
                     metric=metric)
    
  } else {
    ffs_model <- ffs(predictors,
                     response,
                     metric=metric,
                     withinSE = TRUE,
                     method = method,
                     importance =TRUE,
                     tuneLength = tuneLength,
                     tuneGrid = tuneGrid,
                     trControl = tctrl,
                     trace = FALSE, #relevant for nnet
                     linout = TRUE) #relevant for nnet
    
    
  }
  
  save(ffs_model,file=paste0(outpath,"ffs_model_",method,"_", n, "dummys.RData"))
}

stopCluster(cl)

