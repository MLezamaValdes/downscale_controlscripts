
y=1
m=1
# 
# loc="Palma"
testing=TRUE
print("loading Palma libs and paths")
library(raster)
library(rgdal)
datpath <- "/scratch/tmp/llezamav/satstacks/"
aoipath <- "/scratch/tmp/llezamav/aoi/"
time_range <- readRDS("/scratch/tmp/llezamav/time_range.rds")
cddir <- "/scratch/tmp/llezamav/satstacks/"
iahsrespath <- "/scratch/tmp/llezamav/ia_hs_res/"
swiroutpath <- paste0(datpath, "swir/")
time_range <- readRDS("/scratch/tmp/llezamav/time_range.rds")
outpath <- "/scratch/tmp/llezamav/modelling/"

expath <- "/scratch/tmp/llezamav/stack_extraction/"
library(parallel)
library(doParallel)
library(CAST,lib.loc="/home/l/llezamav/R/")
library(caret,lib.loc="/home/l/llezamav/R/")
library(gbm,lib.loc="/home/l/llezamav/R/")

library(Cubist,lib.loc="/home/l/llezamav/R/")
library(pls,lib.loc="/home/l/llezamav/R/")
library(kernlab,lib.loc="/home/l/llezamav/R/")
print("libraries loaded")

ym <- substring(time_range[[y]][[m]][[1]][[1]], 1, 7)
`%notin%` <- Negate(`%in%`)
print("notin done")

print("loading rds")
train <- readRDS(paste0(expath, "train_DI__2019-01.rds"))
print("loaded train rds")

if(testing){
  ### just for testing ###############
  n <- 6000
  
  # only if subset
  trainsubset <- train[sample(nrow(train),n), ]
  saveRDS(trainsubset, paste0(outpath, "train_", ym, "_subset_", n, ".rds"))
  train <- trainsubset 
  
  testsubset <- readRDS(paste0(expath, "test_ds_2019-01.rds"))
} else {
  n <- nrow(train)
}

#################

kval <- min(length(unique(train$time_num)), length(unique(train$spatialblocks)))

# split training cuarter into various blocks for cv during training
foldids <- CreateSpacetimeFolds(train, spacevar="spatialblocks", timevar = "time_num",
                                k=kval,seed=100)

(trainlength <- sapply(seq(foldids$indexOut), function(i){
  length(foldids$indexOut[[i]])
}))

set.seed(100)

metric <- "RMSE" # Selection of variables made by either Rsquared or RMSE
# gbm didn't work
methods <- c("rf",
             "gbm",
             "nnet",
             "svmLinear", # sampes >> features -> AI@WWU: use linear kernel
             #"dnn", # deep neural network
             #"mlpKerasDropout", 
             #"mlpSGD" # gradient descent
)
withinSE <- TRUE # favour models with less variables or not?
# last time this was false, but I think it would be better to try it with T
response <- train$Landsat
predictors <- train[,c("Modis","ia", "hs", "dem", 
                       "slope", "aspect", "TWI", 
                       "soilraster", "landcoverres", 
                       "swir6", "swir7")]
length(predictors)

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
    tuneGrid <- expand.grid(mtry=seq(2,ncol(predictors)))
    # Create A Data Frame From All Combinations Of Factor Variables, 
    # mtry: Number of variables randomly sampled as candidates at each split
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
  
  
  cores <- detectCores()
  cl <- makeCluster(cores-3)
  registerDoParallel(cl)
  
  
  
  if(method="gbm"){ # importance = TRUE kills it
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
  
  save(ffs_model,file=paste0(outpath,"ffs_model_",method,"_", n, ".RData"))
  stopCluster(cl)
}
