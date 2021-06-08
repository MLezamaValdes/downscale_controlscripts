

y=1
m=1
# 
# loc="Palma"
testing=TRUE

# if(loc=="Palma"){
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

# install.packages("/home/l/llezamav/R/kernlab_0.9-29.tar.gz", repos = NULL,
#                  lib="/home/l/llezamav/R/")
# install.packages("/home/l/llezamav/R/pls_2.7-3.tar.gz", repos = NULL,
#                  lib="/home/l/llezamav/R/")
# install.packages("/home/l/llezamav/R/Cubist_0.2.2.tar.gz", repos = NULL,
#                  lib="/home/l/llezamav/R/")
# install.packages("/home/l/llezamav/R/gbm_2.1.5.tar.gz", repos = NULL,
#                  lib="/home/l/llezamav/R/")
# install.packages("/home/l/llezamav/R/caret_6.0-85.tar.gz", repos = NULL,
#                  lib="/home/l/llezamav/R/")
# install.packages("/home/l/llezamav/R/CAST_0.4.3.tar.gz", repos = NULL,


# TO DO: INSTALL elmNNRcpp 




#                  lib="/home/l/llezamav/R/")
library(parallel)
library(doParallel)
library(CAST,lib.loc="/home/l/llezamav/R/")
library(caret,lib.loc="/home/l/llezamav/R/")
library(gbm,lib.loc="/home/l/llezamav/R/")
library(Cubist,lib.loc="/home/l/llezamav/R/")
library(pls,lib.loc="/home/l/llezamav/R/")
library(kernlab,lib.loc="/home/l/llezamav/R/")
print("libraries loaded")



# } else if(loc=="Laptop"){
#   print("loading Laptop libs and paths")
#   
#   library(raster)
#   library(rgdal)
#   library(CAST)
#   library(caret)
#   library(gbm)
#   library(Cubist)
#   library(pls)
#   library(kernlab)
#   iahsrespath <- paste0(cddir, "ia_hs_res/")
#   aoipath <- "D:/new_downscaling/aoi/"
#   auxpath <-  "D:/new_downscaling/auxiliary/"
#   datpath <- auxpath
#   swiroutpath <- "D:/new_downscaling/SWIR/composites/" # TO DO!!!!!!!!!!!!!!!!!!!
# 
#   outpath <- "D:/new_downscaling/modelling/"
#   expath <- "D:/new_downscaling/extraction/"
# } 

ym <- substring(time_range[[y]][[m]][[1]][[1]], 1, 7)
`%notin%` <- Negate(`%in%`)
print("notin done")


######################################### Look up train and test #################################################

# # for now just a simulation with random samples
# testsites <- c(26, 63, 43, 12, 35, 40, 31, 79,  5,  2, 60, 11)
# 
# datpath <- "D:/new_downscaling/extraction/"
# 
# #test <- read.csv2(paste0(datpath, "test_ds_", ym, ".csv"))
# #train <- read.csv2(paste0(datpath, "train_DI_", ym, ".csv"))
# 
# train <- readRDS(paste0(datpath, "train_DI_3.5_", ym, ".rds"))
# 
# #ds <- read.csv2(paste0(datpath, "pott3_new_", ym, ".csv"))
# 
# test <- subset(ds, ds$spatialblocks %in% testsites)
# train <- subset(ds, ds$spatialblocks %notin% testsites)


######################################### RUN MODEL #################################################
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
withinSE <- FALSE # favour models with less variables or not?
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
































# 
# 
# 
# 
# ffsModel <- ffs(train[,c(1:3, 9:14, 16:17)],
#                 train$Landsat,
#                 method = "rf",
#                 ntree=250,
#                 trControl=tctrl,
#                 tuneGrid=expand.grid(mtry=2))
# 
# #stop cluster and save model:
# saveRDS(ffsModel,file=paste0(outpath,"ffsModel_2019_01_model_n7000.rds"))
# 
# #ffsModel <- readRDS(paste0(outpath,"ffsModel_2019_01_model_n7000.rds"))
# 
# finalModel <- train(train[,ffsModel$selectedvars],
#                     train$Landsat,
#                     method = "rf",
#                     trControl=tctrl,
#                     tuneLength=length(ffsModel$selectedvars))
# 
# stopCluster(cl)
# 
# saveRDS(finalModel,file=paste0(outpath,"finalModel_2019_01_model_n7000.rds"))
# #ffsModel <- readRDS(paste0(outpath,"ffsModel_fake_model.rds"))
# 
# finalModel
# 
# ##################### TEST & PREDICT ######################
# 
# #test <- readRDS(paste0(outpath, "test.rds"))
# pred <- predict(finalModel, newdata = testsubset)
# saveRDS(pred, file=paste0(outpath, "prediction_testdata_n7000.rds"))
# 
# source("C:/Users/mleza/OneDrive/Documents/PhD/ML_dist/ML_dist/regressionstats_Rsenal.R")
# 
# #visualize
# regressionStats(pred, testsubset$Landsat)
# df <- data.frame(pred, testsubset$Landsat)
# 
# 
# 
# ggplot(df, aes(testsubset.Landsat,pred))+
#   xlab("Observed LST in Testdata")+ylab("predicted LST with Testdata")+
#   ggtitle("Observed vs. Predicted LST in Testdata, 
#           trained on 7000 observations, 250 trees")+
#   stat_binhex(bins=300)+
#   geom_abline(slope=1,intercept=0)+
#   scale_x_continuous(expand=c(0,0))+
#   scale_y_continuous(expand=c(0,0))+
#   scale_fill_gradientn(name="datapoints", trans="log", colours=viridis(10), 
#                        breaks=10^(0:4))
# 
# # complete stack
# auxpath <- "D:/new_downscaling/auxiliary/"
# tempdyn <- raster::stack(paste0("D:/new_downscaling/clean_data/", "new_L_MOD_hs_ia", ym, ".tif"))
# tdnam <- read.csv(paste0(cddir, "names_sat_ia_hs_2019-01.csv"))
# names(tempdyn) <- tdnam$x
# 
# aux <- stack(paste0(auxpath, "aux_stack_xy_swir67.tif"))
# names(aux) <- c("dem", "slope", "aspect", "TWI", "soilraster", "landcoverres", "spatialblocks", "swir6", "swir7", "x", "y")
# sc <- stack(tempdyn, aux)
# 
# 
# 
# #################### PREDIICTIONS #####################################################
# 
# 
# ffsModel$selectedvars
# 
# iasenes <- which(grepl("LC08_",names(sc)))
# 
# auxmodstack <- sc[[ffsModel$selectedvars]]
# 
# 
# for(i in seq(iasenes)){
#   # ffspredstack <- stack(sc[[c(iasenes[i])]], auxmodstack)
#   # names(ffspredstack) <- c("ia", names(ffspredstack)[2:5])
#   pred_ras <- predict(auxmodstack, finalModel)
#   
#   png(filename=paste0(outpath, "Landsat_prediction_", i, ".png"))
#   par(mfrow=c(1,2))
#   plot(sc[[iasenes[i]-2]], main=paste0("Landsat ", substring(names(sc[[iasenes[i]-1]]), 11,22)))
#   plot(pred_ras, main="prediction")
#   dev.off()
# }
# 
# 
# 
