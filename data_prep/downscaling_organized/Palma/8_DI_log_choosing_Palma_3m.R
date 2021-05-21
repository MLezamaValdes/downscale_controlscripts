# DI on Palma

rm(list=ls())
# location="Palma"
# install.packages("/home/l/llezamav/R/caret_6.0-85.tar.gz", repos = NULL,
#                  lib="/home/l/llezamav/R/")
# install.packages("/home/l/llezamav/R/CAST_0.4.3.tar.gz", repos = NULL,
#                  lib="/home/l/llezamav/R/")


library(doParallel)
library(parallel)
library(CAST,lib.loc="/home/l/llezamav/R/")
library(caret,lib.loc="/home/l/llezamav/R/")

datpath <- "/scratch/tmp/llezamav/"
inputpath <- paste0(datpath, "extraction/")
outputpath <- paste0(datpath, "train_test_DI_3m/")

############################################################################################
###################### put away temporal testing ###########################################
############################################################################################


# take only those months that go into training 
avym <- read.csv2("/scratch/tmp/llezamav/available_months_scenes_test.csv")
ex_files <- list.files(inputpath, pattern="extr_complete_cases", full.names = T)
ex_files_use <- substring(list.files(inputpath, pattern="extr_complete_cases", full.names = F), 21,27)
testdates <- avym$date[avym$test==1]

`%notin%` <- Negate(`%in%`)
ex_files <- ex_files[ex_files_use %notin% testdates]


############################################################################################
###################### put away spatial testing ###########################################
############################################################################################
blocktestsample <- read.csv2("/scratch/tmp/llezamav/blocktest_samples_test.csv")
print(paste0("blocktestsample = ", blocktestsample$x))

###################### START WITH complete cases extraction ALREADY FROM SERVER ###########################################



di_log_choosing <- function(extr, ym){
    
    # convert time to numeric
    extr$time_num <- as.numeric(as.factor(extr$modtime))
    
    # get only relevant variables for DI 
    potDI <- extr[c("Landsat", "Modis", "ia", "hs",
                     "dem", "slope", "aspect", "TWI", "soilraster", "x", "y",
                      "time_num")]
    print(paste0("nrow(potDI) = ", nrow(potDI)))
    
    
    vartest <- sapply(seq(potDI), function(i){
      var(potDI[,i])
    })
    

    if(any(vartest==0)){
      novarpos <- which(vartest==0)
      
      novarnams <- names(potDI)[novarpos]
      write.csv2(novarnams, paste0(outputpath, "novar_varnames_",  ym, ".csv"))
      
      potDI[,novarpos] <- NULL
      
    }
    
    
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
    
    `%notin%` <- Negate(`%in%`)
    potDI_ds <- subset(potDI, rownames(potDI) %notin% rnds)
    
    ### 2 sample selection via DI ###################################################################
    
    samples <- randsamples
    
    
    # if(location=="Laptop"){
    #       cli <- makeCluster(2, outfile="C:/Users/mleza/test.txt")
    # } else {
      cli <- makeCluster(27, outfile="/home/l/llezamav/scripts_new/par_aoa_out.txt")
      #cli <- makeCluster(2, outfile="/home/l/llezamav/scripts_new/par_aoa_out.txt")
      
    # }
    
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
      `%notin%` <- Negate(`%in%`)
      potDI_ds <- subset(potDI_ds, rownames(potDI_ds) %notin% names(most_diss))
      
      # stop if either all samples are in the AOA or if there are less remaining samples than chosen in this step
      print(paste0(ym, " stopping criterion met? 1: All in AOA=1? ", any(di$AOA==0)==FALSE,
                   " or 2: less samples to choose from than needed in last choose? nrow(potDI_ds) )", 
                   nrow(potDI_ds), " or too little to choose? choose=", choose, " (should be 0 and then stop) - i.e. ", nrow(potDI_ds) < choose))
      if(any(di$AOA==0)==FALSE | nrow(potDI_ds) < choose | choose < 5){
        break
      }
      
      i=i+1
    }
    
    #write.csv2(samples, paste0(datpath, "train_test/s_train_DI-",  ym, "se.csv"))
    
    # take dataset at the chosen rownames to get full dataset
    trainDS <- extr[rownames(samples),]
    all(rownames(trainDS)==rownames(samples))
    
    saveRDS(trainDS, paste0(outputpath, "train_DI_", ym, ".rds"))
    write.csv2(trainDS, paste0(outputpath, "train_DI_",  ym, ".csv"))
    
    
    stopCluster(cli)
    
    
}



########################### RUN ###########################################

no_cores <- 2

cl <- makeCluster(no_cores, outfile="/home/l/llezamav/scripts_new/par_log_choosing_ym_out.txt")

registerDoParallel(cl)
jnk = clusterEvalQ(cl, {
  library(doParallel);
  library(parallel);
  library(CAST,lib.loc="/home/l/llezamav/R/");
  library(caret,lib.loc="/home/l/llezamav/R/")})
clusterExport(cl, list("cl","di_log_choosing", "datpath", "ex_files",
                       "inputpath", "outputpath", "blocktestsample"))

parLapply(cl, seq(ex_files), function(m){
  
  
      `%notin%` <- Negate(`%in%`)
      ym <- substring(basename(ex_files[m]), 21, 27)
      print(paste0("yearmonth = ", ym))
      extr_all <- read.csv2(ex_files[m])
      
      ### separate by spatial blocks (blocktestsample) into test and potential trainingssamples
      test <- extr_all[extr_all$spatialblocks %in% blocktestsample$x,]
      write.csv2(test, paste0(outputpath, "test_all_samples_", ym, ".csv"))
      
      pottrain <- extr_all[!extr_all$spatialblocks %in% blocktestsample$x,]
      
      # get 3.15 mio samples for pottrain 
      if(nrow(pottrain)<3150000){
        ntrain <- nrow(pottrain)
      } else {
        ntrain <- 3150000
      }
      
      extr_3mi <- sample(rownames(pottrain), ntrain)
      pott3 <- pottrain[extr_3mi,]
      
      ## run DI sampling for potential training samples
      di_log_choosing(extr=pott3, ym)
})



stopCluster(cl)





