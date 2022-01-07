library(caret)

trainvalidpath <- "D:/LST_after_paper/train_valid/"

train <- read.csv2(paste0(trainvalidpath, "train_LHS_50000.csv"))


test1 <- read.csv2(paste0(trainvalidpath, "validation_1_rand_50000.csv"))
test2 <- read.csv2(paste0(trainvalidpath, "validation_2_rand_50000.csv"))
test3 <- read.csv2(paste0(trainvalidpath, "validation_3_rand_50000.csv"))

adddummys <- function(dataset){
  
  
  ############## ADD MOD/MYD info as predictor ##################################
  dataset$TeAq <- as.factor(substring(dataset$Mscene,1,3))
  dataset$TeAqNum <- as.numeric(dataset$TeAq)
  
  dataset$ymo <- as.factor(dataset$ymo)
  ############### make dummys from categorical predictors ######################
  dsdummys <- data.frame(factor(dataset$soilraster), 
                         factor(dataset$landcoverres), 
                         factor(dataset$TeAqNum))
  names(dsdummys) <- c("soilraster", "landcoverres", "TeAqNum")
  
  levels(dsdummys$landcoverres) <- c("1", "2")
  
  dummyform <- caret::dummyVars(" ~ .", data=dsdummys)
  dummydata <- data.frame(predict(dummyform, newdata = dsdummys)) 
  #
  dataset <- cbind(dataset, dummydata)
  
  return(dataset)
}

train <- adddummys(train)

test1 <- adddummys(test1)
test2 <- adddummys(test2)
test3 <- adddummys(test3)



############### SCALING ###################################################

tvlist <- list(train, test1, test2, test3)
tvlistnam <- c("train", "test1", "test2", "test3")

minmaxvals <- lapply(seq(4), function(i){
  
  min_vals <- apply(tvlist[[i]][,c("Modis","ia", "hs", "dem",
                                   "slope", "aspect", "TWI")],2,min)
  max_vals <- apply(tvlist[[i]][,c("Modis","ia", "hs", "dem",
                                   "slope", "aspect", "TWI")],2,max)
  
  minmaxvals <- data.frame(rbind(min_vals, max_vals))
  minmaxvals$mm <- c("min", "max")
  
  return(minmaxvals)
  
})

names(minmaxvals) <- tvlistnam

mmv <- do.call("rbind", minmaxvals)
saveRDS(mmv,"add_files/minmaxvals_train_valid.Rds")

mintab <- mmv[mmv$mm == "min",]
mintab
maxtab <- mmv[mmv$mm == "max",]
maxtab

overallmax <- apply(maxtab, 2, max)
overallmin <- apply(mintab, 2, min)

mmtab <- data.frame(rbind(as.numeric(overallmin), as.numeric(overallmax)))
names(mmtab) <- names(mintab)
mmtab <- mmtab[,1:7]

addfilespath <- paste0(trainvalidpath, "add_files/")
dir.create(addfilespath)

saveRDS(mmtab,paste0(addfilespath, "overall_minmaxvals_train_valid.Rds"))

########### rescale from 0 to 1 ################

scaling01 <- function(x, xmaxval, xminval){
  (x - xminval) / (xmaxval - xminval)
}

metricpredictors <- c("Modis","ia", "hs", "dem",
                      "slope", "aspect", "TWI")

scaled_train <- train

scale_metric_in_all_datasets <- function(dataset){
  scaledvars <- lapply(seq(metricpredictors), function(i){
    
    x <- scaling01(dataset[,metricpredictors[i]], 
                   xmaxval = mmtab[,metricpredictors[i]][2],
                   xminval = mmtab[,metricpredictors[i]][1])
    print(summary(x))
    x
  })
  
  scaleddf <- data.frame(scaledvars)
  names(scaleddf) <- paste0(metricpredictors, "_sc")
  dataset <- cbind(dataset, scaleddf)
  return(dataset)
}

train <- scale_metric_in_all_datasets(train)
test1 <- scale_metric_in_all_datasets(test1)
test2 <- scale_metric_in_all_datasets(test2)
test3 <- scale_metric_in_all_datasets(test3)

write.csv2(train, paste0(trainvalidpath, "train_LHS_50000_scaled.csv"))

write.csv2(test1, paste0(trainvalidpath, "validation_1_rand_50000_scaled.csv"))
write.csv2(test2, paste0(trainvalidpath, "validation_2_rand_50000_scaled.csv"))
write.csv2(test3, paste0(trainvalidpath, "validation_3_rand_50000_scaled.csv"))

