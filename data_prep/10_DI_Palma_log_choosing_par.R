# DI on Palma


library(doParallel)
library(parallel)
library(CAST,lib.loc="/home/l/llezamav/R/")
library(caret,lib.loc="/home/l/llezamav/R/")

datpath <- "/scratch/tmp/llezamav/satstacks/extraction_result_new/"
#datpath <- "D:/new_downscaling/extraction/"


year <- c(2019:2013)
month <- c("01","02","03","04", "09", "10","11", "12")


# set month to look at
time_range <- readRDS("/scratch/tmp/llezamav/time_range.rds")

###################### START WITH POTT3 ALREADY FROM SERVER ###########################################

di_log_choosing <- function(y,m){
  
  ym <- substring(time_range[[y]][[m]][[1]][[1]], 1, 7)
  `%notin%` <- Negate(`%in%`)
  
  pat <- paste0("few_samples_",ym)
  print(paste0("proceeding: ",(length(list.files(datpath, pattern=pat))>0)==F))
  if((length(list.files(datpath, pattern=pat))>0)==F){
    ###################### prep variables ###########################################
    potpath <- paste0(datpath, "pott3_",ym, ".csv")
    print(paste0("path to pott3",potpath))
    pott3 <- read.csv2(potpath, nrow=50)
    
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
    
    samples <- randsamples
    
    cli <- makeCluster(3)
    registerDoParallel(cli)
    
    i=0
    t0prev <- 0
    x <- 3.5
    repeat {
      di <- aoa(newdata=potDI_ds, train=samples, cl=cli)
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
      print(paste0("stopping criterion met? all in AOA=1? ", any(di$AOA==0)==FALSE,
                   "or less samples to choose from than needed in last choose?", nrow(potDI_ds) < choose))
      if(any(di$AOA==0)==FALSE | nrow(potDI_ds) < choose){
        break
      }
      
      
      i=i+1
    }
    
    
    stopCluster(cli)
    
    write.csv2(samples, paste0(datpath, "train_test/samples_train_DI_", "_" ,  ym, ".csv"))
    
    # take dataset at the chosen rownames to get full dataset
    trainDS <- pott3[rownames(samples),]
    all(rownames(trainDS)==rownames(samples))
    
    saveRDS(trainDS, paste0(datpath, "train_test/train_DI_", "_" , ym, ".rds"))
    write.csv2(trainDS, paste0(datpath, "train_test/train_DI_", "_" ,  ym, ".csv"))
    
  }
}



########################### RUN ###########################################

# !!!!!! ##############
month <- c("02","03","04", "09", "10","11", "12")
# !!!!!! ##############


no_cores <- 8
cl <- makeCluster(no_cores)
y=1

jnk = clusterEvalQ(cl, {
  library(doParallel);
  library(parallel);
  library(CAST,lib.loc="/home/l/llezamav/R/");
  library(caret,lib.loc="/home/l/llezamav/R/")})
clusterExport(cl, list("cl","di_log_choosing", "time_range","month", "year", "datpath", "y"))



parLapply(cl, seq(length(month)), function(m){
  
#lapply( seq(length(month)), function(m){
      
  print(paste0("starting yearindex ", y, "month: ", m))
      print(c(y,m))
      di_log_choosing(y,m)
})

stopCluster(cl)




