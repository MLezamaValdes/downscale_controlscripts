# DI on Palma

y=1
m=1


# install.packages("/home/l/llezamav/R/CAST_0.4.3.tar.gz", repos = NULL,
#                 lib="/home/l/llezamav/R/")
library(doParallel)
library(parallel)
library(CAST, lib.loc="/home/l/llezamav/R/")

`%notin%` <- Negate(`%in%`)

nc <- detectCores()
cl <- makeCluster(nc-3)

datpath <- "/scratch/tmp/llezamav/stack_extraction/"
#datpath <- "D:/new_downscaling/extraction/"

# set month to look at
time_range <- readRDS("/scratch/tmp/llezamav/time_range.rds")
ym <- substring(time_range[[y]][[m]][[1]][[1]], 1, 7)

###################### START WITH POTT3 ALREADY FROM SERVER ###########################################


###################### prep variables ###########################################

pott3 <- read.csv2(paste0(datpath, "pott3_new_",ym, ".csv"))

# convert time to numeric
pott3$time_num <- as.numeric(as.factor(pott3$time))
# get only relevant variables for DI 
divars <- names(pott3)[c(1:4, 6:7, 9:14, 16:17, 20)]
print(divars)
potDI <- pott3[divars]

##### 1 get 150.000 random samples per month ###########################################

set.seed(100)
# to maintain data distribution
rnds <- sample(rownames(potDI), 150000)
randsamples <- potDI[rnds,]

###################### put rest of samples in potDI ###########################################

potDI_ds <- subset(potDI, rownames(potDI) %notin% rnds)

### 2 sample selection via DI ###################################################################

### TEST DI LOOP ###################################################################


    # # # choose 10 (new) random points to start with
    # trn <- sample(rownames(potDI_ds), 10) # training data (n=10) rownames
    # DItrain <- potDI_ds[trn,]
    # 
    # # dissimilarity is being calculated for the rest of the dataset
    # # what to do about chosen and discarded ones? 
    # ndatrn <- !(rownames(potDI_ds) %in% trn) # which in potDI_ds are not in DItrain?
    # DInewdata <- potDI_ds[ndatrn,]
    # 
    # potDI_ds <- DInewdata
    # randsamples <- DItrain

#### test end #################

registerDoParallel(cl)

samples <- randsamples

i=0
t0prev <- 0
x <- 3.5
repeat {
  di <- aoa(newdata=potDI_ds, train=samples, cl=cl)
  t <- table(di$AOA)
  
  diff_out_aoa <- t[1]-t0prev
  t0prev <- t[1]
  print(paste0("diff out of AOA to previous step: ", diff_out_aoa))
  
  print(paste0("iteration = ", i))
  print("AOA out / in: ")
  print(t)
  
  choose <- round(log(t[1])**x)
  print(paste0("choose ", choose, "most dissimilar samples"))
  
  most_diss <- tail(sort(di$DI), choose)
  
  # add most dissimlar to samples
  most_diss_ds <- subset(potDI_ds, rownames(potDI_ds) %in% names(most_diss))
  samples <- rbind(samples, most_diss_ds)
  
  # take most dissimlar away from potDI_ds 
  potDI_ds <- subset(potDI_ds, rownames(potDI_ds) %notin% names(most_diss))
  
  if(any(di$AOA==0)==FALSE){
    break
  }
  
  i=i+1
}


# take dataset at the chosen rownames to get full dataset
trainDS <- pott3[rownames(samples),]
rownames(trainDS)==rownames(samples)

saveRDS(trainDS, paste0(datpath, "train_DI_", "_" , ym, ".rds"))
write.csv2(trainDS, paste0(datpath, "train_DI_", "_" ,  ym, ".csv"))



