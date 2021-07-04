# modelpath <- "D:/downscaling_after_talk/models/"
# trainpath <- "D:/downscaling_after_talk/clean_data/train_valid/"
# stackpath <- "D:/downscaling_after_talk/clean_data/satstacks_ngb/"
# 
# library(raster)
# library(CAST)
# 
# install.packages("/home/l/llezamav/R/doMPI_0.2.2.tar.gz",
#                 repos = NULL, lib="/home/l/llezamav/R/")
# 

##############################################################################
library(Rmpi)  # R implementation of MPI interface
library(doMPI,lib.loc="/home/l/llezamav/R/") # interface for foreach construct to run in MPI parallel mode


# Load the R MPI package if it is not already loaded.
if (!is.loaded("mpi_initialize")) {
  library("Rmpi")
}

ns <- mpi.universe.size()
print(ns)
mpi.spawn.Rslaves(nslaves=ns)
#
# In case R exits unexpectedly, have it automatically clean up
# resources taken up by Rmpi (slaves, memory, etc...)
.Last <- function(){
  if (is.loaded("mpi_initialize")){
    if (mpi.comm.size(1) > 0){
      print("Please use mpi.close.Rslaves() to close slaves.")
      mpi.close.Rslaves()
    }
    print("Please use mpi.quit() to quit R")
    .Call("mpi_finalize")
  }
}
# Tell all slaves to return a message identifying themselves
mpi.bcast.cmd( id <- mpi.comm.rank() )
mpi.bcast.cmd( ns <- mpi.comm.size() )
mpi.bcast.cmd( host <- mpi.get.processor.name() )
mpi.remote.exec(paste("I am",mpi.comm.rank(),"of",mpi.comm.size()))

# # Test computations
# x <- 5
# x <- mpi.remote.exec(rnorm, x)
# length(x)
# x
# 

# cl <- startMPIcluster() # start cluster (link R instances together)
# num.cluster <- clusterSize(cl)
# registerDoMPI(cl) # tells foreach to use MPI parallel mode
# print(paste("Running in parallel mode on",num.cluster,"worker nodes."))


library(raster)
library(CAST,lib.loc="/home/l/llezamav/R/")
library(caret,lib.loc="/home/l/llezamav/R/")
library(parallel)
library(doParallel)




##############################################################################


modelpath <- "/scratch/tmp/llezamav/modeling_after_talk/"
trainpath <- "/scratch/tmp/llezamav/train_valid/"
stackpath <- "/scratch/tmp/llezamav/satstacks_ngb/"
aoapath <- "/scratch/tmp/llezamav/aoa/"
predstackdir <- "/scratch/tmp/llezamav/predstacks/"


method <- "rf"

########## aoa on test 3 #############################################


load(paste0(modelpath, "final_model_rf_150000SE_F.RData"))
print(model_final$finalModel$xNames)

# test1 <- read.csv2(paste0(trainpath, "validation_1_LHS_150000.csv"))
# test2 <- read.csv2(paste0(trainpath, "validation_2_LHS_150000.csv"))
# test3 <- read.csv2(paste0(trainpath, "validation_3_LHS_150000.csv"))
# 
# testlist <- list(test1, test2, test3)
# 
# aoatest1to3 <- lapply(seq(testlist), function(i){
#   test <- testlist[[i]]
#   test$TeAq <- as.factor(substring(test$Mscene,1,3))
#   test$TeAqNum <- as.numeric(test$TeAq)
#   print(names(test))
#   
#   # add this in when final model 
#   # test$soilraster <- factor(test$soilraster)
#   # test$TeAqNum <- factor(test$TeAqNum)
#   # test$landcoverres <- factor(test$landcoverres)
#   
#   
#   #take only predictors
#   test <- test[,model_final$finalModel$xNames]
#   print("test, i.e. new data looks like this now:")
#   print(head(test))
#   
#   print(paste0("starting with aoa ", i, "now"))
#   aoa <- aoa(newdata=test, model=model_final)
#   
#   print(paste0("saving aoa ", i, "now"))
#   saveRDS(aoa, paste0(aoapath, "aoa_", method, "_", i, ".RDS"))
#   
#   return(table(aoa$AOA))
# })
# 
# saveRDS(aoatest1to3, paste0(aoapath, "table_aoa_test1to3.RDS"))

########## aoa on spatial predictors #################################

if(method=="nnet"){
  predictrs <- model_final$finalModel$coefnames
} else if (method=="rf") {
  predictrs <-model_final$finalModel$xNames
} else if (method == "gbm"){
  predictrs <- model_final$finalModel$xNames
}

# read predstack
predstack <- stack(list.files(predstackdir, pattern=".tif", full.names = T))
psnams <- read.csv2(list.files(predstackdir, pattern=".csv", full.names = T))

names(predstack) <- psnams$x

################### aoa on test 3 ###################################
aoa_spat <- aoa(newdata=predstack, model=model_final)
saveRDS(aoa_spat, paste0(aoapath, "aoa_", method, "_", "aoa_spat.RDS"))

################### spatial aoa ###################################


## MPI-save version of R quit
mpi.quit()