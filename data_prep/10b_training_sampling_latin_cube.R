#loc <- "Laptop"
loc <- "Palma"

if(loc=="Laptop"){
  tdipath <- "C:/Users/mleza/OneDrive/Desktop/"
  testpath <- "C:/Users/mleza/OneDrive/Desktop/testsubsets/"
  
  library(doParallel)
  library(parallel)
  library(CAST)
  library(caret)
  library(clhs)
  
} else if (loc=="Palma"){
  library(doParallel)
  library(parallel)
  library(CAST,lib.loc="/home/l/llezamav/R/")
  library(caret,lib.loc="/home/l/llezamav/R/")
  tdipath <- "/scratch/tmp/llezamav/satstacks/extraction_result_new/train_test/"
  testpath <- "/scratch/tmp/llezamav/satstacks/extraction_result_new/"
}


train <- read.csv2(paste0(paste0(tdipath, "training_complete.csv")))

names(train)

rhs <- randomLHS(150000, ncol(train))
dim(rhs)

library(dplyr)
trainvars <- train %>% select(Landsat, Modis, ia, hs, dem, slope, aspect, TWI, soilraster, 
         landcoverres, swir6, swir7)

trainsubset <- clhs(trainvars, size=150000 ) 
trainsubset_comp <- train[trainsubset,]

all(rownames(trainsubset_comp) %in% trainsubset)

write.csv2(trainsubset_comp, paste0(tdipath, "training_complete_150000_clhs.csv"))


