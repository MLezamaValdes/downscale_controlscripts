

print("loading Palma libs and paths")

outpath <- "/scratch/tmp/llezamav/modeling_after_talk/"
trainpath <- "/scratch/tmp/llezamav/train_valid/"
# trainpath <- "D:/downscaling_after_talk/clean_data/train_test_DI/train_valid/"
# outpath <- paste0(trainpath, "modeling_after_talk/")

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
train <- read.csv2(paste0(trainpath, "train_LHS_150000.csv"))

str(train)

print("loaded train dataset")
n <- 150000
############## ADD MOD/MYD info as predictor ##################################
train$TeAq <- as.factor(substring(train$Mscene,1,3))
train$TeAqNum <- as.numeric(train$TeAq)


train$soilraster <- factor(train$soilraster)
train$TeAqNum <- factor(train$TeAqNum)
train$landcoverres <- factor(train$landcoverres)


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
methods <- c("rf")
response <- train$Landsat
predictors <- train[,c("Modis","ia", "hs", "dem", 
                       "slope", "aspect", "TWI", 
                       "soilraster", "landcoverres", "TeAqNum")]
length(predictors)

cores <- detectCores()
print(paste("cores = ", cores))

cl <- makeCluster(cores-3, outfile="/home/l/llezamav/scripts_new/par_algorithms_remodelling.txt")

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
  
    #   tuneLength <- 1
    tuneGrid <- expand.grid(mtry=seq(2,3))
    # Create A Data Frame From All Combinations Of Factor Variables, 
    # mtry: Number of variables randomly sampled as2-10 candidates at each split

    ffs_model <- ffs(predictors,response, 
                     method=method, 
                     ntree=100,
                     trControl=tctrl,
                     tuneGrid=tuneGrid,
                     withinSE = FALSE,
                     tuneLength=tuneLength,           
                     metric=metric)
  
    save(ffs_model,file=paste0(outpath,"ffs_model_",method,"_", n, "SE_F_factor.RData"))
}
# 
# print("~~~~~~~~~~~~~~~~~~~~~~~~~~~ DONE WITH SE=F MODEL, PROCEEDING TO EXCLUDING SLOPE ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
# 
# predictors <- train[,c("Modis","ia", "hs", "dem", 
#                        "aspect", "TWI", 
#                        "soilraster", "landcoverres", "TeAqNum")]
# for (i in 1:length(methods)){
#   
#   tctrl <- trainControl(method="cv", 
#                         savePredictions = TRUE,
#                         returnResamp = "all",
#                         verboseIter=TRUE,
#                         index=foldids$index,
#                         indexOut=foldids$indexOut)
#   
#   
#   method <- methods[i]
#   print(method)
#   tuneLength <- 2
#   tuneGrid <- NULL
#   
#   #   tuneLength <- 1
#   tuneGrid <- expand.grid(mtry=seq(2,3))
#   # Create A Data Frame From All Combinations Of Factor Variables, 
#   # mtry: Number of variables randomly sampled as2-10 candidates at each split
#   
#   ffs_model <- ffs(predictors,response, 
#                    method=method, 
#                    ntree=100,
#                    trControl=tctrl,
#                    tuneGrid=tuneGrid,
#                    withinSE = TRUE,
#                    tuneLength=tuneLength,           
#                    metric=metric)
#   
#   save(ffs_model,file=paste0(outpath,"ffs_model_",method,"_", n, "no_slope.RData"))
# }
# 
# print("~~~~~~~~~~~~~~~~~~~~~~~~~~~ DONE WITH SE=F MODEL & EXCLUDING SLOPE, TRYING MORE TREES NOW ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
# 
# 
# predictors <- train[,c("Modis","ia", "hs", "dem", 
#                        "slope", "aspect", "TWI", 
#                        "soilraster", "landcoverres", "TeAqNum")]
# for (i in 1:length(methods)){
#   
#   tctrl <- trainControl(method="cv", 
#                         savePredictions = TRUE,
#                         returnResamp = "all",
#                         verboseIter=TRUE,
#                         index=foldids$index,
#                         indexOut=foldids$indexOut)
#   
#   
#   method <- methods[i]
#   print(method)
#   tuneLength <- 2
#   tuneGrid <- NULL
#   
#   #   tuneLength <- 1
#   tuneGrid <- expand.grid(mtry=seq(2,3))
#   # Create A Data Frame From All Combinations Of Factor Variables, 
#   # mtry: Number of variables randomly sampled as2-10 candidates at each split
#   
#   ffs_model <- ffs(predictors,response, 
#                    method=method, 
#                    ntree=500,
#                    trControl=tctrl,
#                    tuneGrid=tuneGrid,
#                    withinSE = TRUE,
#                    tuneLength=tuneLength,           
#                    metric=metric)
#   
#   save(ffs_model,file=paste0(outpath,"ffs_model_",method,"_", n, "no_slope.RData"))
# }

stopCluster(cl)

