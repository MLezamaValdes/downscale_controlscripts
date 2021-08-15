# modelpath <- "D:/downscaling_after_talk/models/"
# trainpath <- "D:/downscaling_after_talk/clean_data/train_valid/"
# stackpath <- "D:/downscaling_after_talk/clean_data/satstacks_ngb/"
# 
# library(raster)
# library(CAST)

library(raster)
library(caret,lib.loc="/home/l/llezamav/R/")
library(CAST,lib.loc="/home/l/llezamav/RCast/")
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


load(paste0(modelpath, "final_model_rf_150000factorfast_mtry.RData"))
print(model_final$finalModel$xNames)

test1 <- read.csv2(paste0(trainpath, "validation_1_rand_150000.csv"))
test2 <- read.csv2(paste0(trainpath, "validation_2_rand_150000.csv"))
test3 <- read.csv2(paste0(trainpath, "validation_3_rand_150000.csv"))

testlist <- list(test1, test2, test3)


cores <- 40
cl <- makeCluster(cores)
registerDoParallel(cl)


aoatest1to3 <- lapply(c(2,3), function(i){
  test <- testlist[[i]]
  test$TeAq <- as.factor(substring(test$Mscene,1,3))
  test$TeAqNum <- as.numeric(test$TeAq)
  print(names(test))

  test$soilraster <- factor(test$soilraster)
  test$TeAqNum <- factor(test$TeAqNum)
  test$landcoverres <- factor(test$landcoverres)
  
  #take only predictors
  test <- test[,model_final$finalModel$xNames]
  
  print("test, i.e. new data looks like this now:")
  print(head(test))
  str(test)

  # equalize factor levels
  trained_with <- model_final$trainingData[1,-ncol(model_final$trainingData)]
  str(trained_with)
  trained_with <- rbind(model_final$trainingData[1,-ncol(model_final$trainingData)], test)
  
  if(any(!levels(test$soilraster)  %in% levels(trained_with$soilraster))){
    notintrainlevels <- levels(test$soilraster)[which(!levels(test$soilraster) %in% levels(trained_with$soilraster))]
    remove <- test$soilraster[test$soilraster %in% notintrainlevels]
    test$soilraster[test$soilraster %in% notintrainlevels] <- NA
    test$soilraster <- droplevels(test$soilraster)
    print(levels(test$soilraster))
    print("because these soil factor levels were not within the training boundaries, the following samples will be set to NA")
    print(table(droplevels(remove)))
  }
  
  str(trained_with)
  str(test)
  
  print(paste0("starting with aoa ", i, "now"))
  
  aoa <- aoa(newdata=test, model=model_final, cl=cl)

  print(paste0("saving aoa ", i, "now"))
  saveRDS(aoa, paste0(aoapath, "aoa_", method, "_rand_", i, ".RDS"))
  
  test$AOA <- aoa$AOA
  test$DI <- aoa$DI
  
  write.csv2(test, paste0(trainpath, "validation_", i, "_rand_150000_aoa.RDS"))

  return(table(aoa$AOA))
})

saveRDS(aoatest1to3, paste0(aoapath, "table_aoa_test1to3.RDS"))

stopCluster(cl)