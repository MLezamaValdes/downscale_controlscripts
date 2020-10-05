# DI on Palma

y=1
m=5


# install.packages("/home/l/llezamav/R/CAST_0.4.3.tar.gz", repos = NULL,
#                 lib="/home/l/llezamav/R/")
library(doParallel)
library(parallel)
library(CAST, lib.loc="/home/l/llezamav/R/")

`%notin%` <- Negate(`%in%`)

nc <- detectCores()
cl <- makeCluster(nc-3)

datpath <- "/scratch/tmp/llezamav/satstacks/"
#datpath <- "D:/new_downscaling/extraction/"

# set month to look at
time_range <- readRDS("/scratch/tmp/llezamav/time_range.rds")

###################### START WITH POTT3 ALREADY FROM SERVER ###########################################

di_log_choosing <- function(y,m){
  
  ym <- substring(time_range[[y]][[m]][[1]][[1]], 1, 7)
  
  pat <- paste0("few_samples_",ym)
  if((length(list.files(dirpath, pattern=pat))>0)==F){
    ###################### prep variables ###########################################
    pott3 <- read.csv2(paste0(datpath, "extraction_result/pott3_",ym, ".csv"))
    
    # convert time to numeric
    pott3$time_num <- as.numeric(as.factor(pott3$modtime))
    
    # get only relevant variables for DI 
    potDI <- pott3[c("Landsat", "Modis", "ia", "hs",
                     "dem", "slope", "aspect", "TWI", "soilraster", "x", "y", "swir6", "swir7",
                     "modtime", "time_num")]
    print(paste0("nrow(potDI) = ", nrow(potDI)))
    
    ##### 1 get 150.000 random samples per month ###########################################
    
    if(nrow(potDI)<150000){
      n <- nrow(potDI)*0.75
    } else {
      n <- 150000
    }
    
    set.seed(100)
    # to maintain data distribution
    rnds <- sample(rownames(potDI), n)
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
      
      # stop if either all samples are in the AOA or if there are less remaining samples than chosen in this step
      if(any(di$AOA==0)==FALSE | nrow(potDI_ds) < choose){
        break
      }
      
      i=i+1
    }
    
    
    # take dataset at the chosen rownames to get full dataset
    trainDS <- pott3[rownames(samples),]
    rownames(trainDS)==rownames(samples)
    
    saveRDS(trainDS, paste0(datpath, "train_test/train_DI_", "_" , ym, ".rds"))
    write.csv2(trainDS, paste0(datpath, "train_test/train_DI_", "_" ,  ym, ".csv"))
    
  }
}


for(y in seq(year)){
  for(m in seq(month)){
    print(c(y,m))
    di_log_choosing(y,m)
  }
}


