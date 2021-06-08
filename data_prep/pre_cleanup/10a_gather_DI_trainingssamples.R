# ########### 10a gather DI trainingssamples #################


#loc <- "Laptop"
loc <- "Palma"

if(loc=="Laptop"){
  tdipath <- "C:/Users/mleza/OneDrive/Desktop/"
  testpath <- "C:/Users/mleza/OneDrive/Desktop/testsubsets/"

  library(doParallel)
  library(parallel)
  library(CAST)
  library(caret)
  
} else if (loc=="Palma"){
  library(doParallel)
  library(parallel)
  library(CAST,lib.loc="/home/l/llezamav/R/")
  library(caret,lib.loc="/home/l/llezamav/R/")
  tdipath <- "/scratch/tmp/llezamav/satstacks/extraction_result_new/train_test/"
  testpath <- "/scratch/tmp/llezamav/satstacks/extraction_result_new/"
}



# 
# f <- list.files(tdipath, pattern="^train_DI", full.names=T)
# f <- f[grepl(f, pattern = ".csv")]
# 
# l <- sapply(seq(testmonths), function(i){
#   grep(testmonths[i], f)
# })
# 
# ll <- unlist(l)
# 
# takemonths <- !seq(f) %in% ll
# 
# tdis <- lapply(seq(f), function(i){
#   if(takemonths[i]==TRUE){
#       read.csv2(f[i])
#   }
# })
# 
# length(tdis)
# 
# alldat <- do.call(rbind, tdis)
# 
# 
# write.csv2(alldat, paste0(tdipath, "training_complete.csv"))
# 
# rm(alldat)
# 
# 
# ########### 10a gather DI test samples #################
# 
# ft <- list.files(testpath, pattern="csv", full.names=T)
# ft <- ft[grepl(ft, pattern = "testsubset_ds")]
# ft
# 
# tests <- lapply(seq(ft), function(i){
#   read.csv2(ft[i])
# })
# 
# alltestdat <- do.call(rbind, tests)
# 
# 
# write.csv2(alltestdat, paste0(testpath, "test_complete.csv"))
# 
# rm(alltestdat)

#################### CALL FINAL AOA DI SELECTION ON GATHERED TRAINING DATASET ################

train <- read.csv2(paste0(paste0(tdipath, "training_complete.csv")))

### put test months away ###########################################################
####################################################################################
`%notin%` <- Negate(`%in%`)

testmonths <- c("2019-09","2018-10", "2016-01", "2015-02", "2014-11", "2013-12")

trainmonth_samples <- train$ymo %notin% testmonths
table(trainmonth_samples)

trainmonths <- train[trainmonth_samples,]

potDI <- trainmonths

print(paste0("nrow of whole dataset = ", nrow(potDI)))

# to maintain data distribution
n <- 150000

rnds <- sample(rownames(potDI), n)
randsamples <- potDI[rnds,]
samples <- randsamples
print(paste0("nrow samples = ", nrow(samples)))


potDI_ds <- subset(potDI, rownames(potDI) %notin% rnds)
print(paste0("nrow potDI_ds = ", nrow(potDI_ds)))

#### cutting potDI_ds into chunks to be able to run this thing #############
N <- 4

print(paste0("splitting potential DS file into ", N, " equal parts results in dataframes of nrow ", 
             round(nrow(potDI_ds)/N), ", procedure can handle 2850000"))

potDI_ds_split <- split(potDI_ds, sample(1:N, nrow(potDI_ds), replace=T))


for(i in seq(potDI_ds_split)){
  print(paste0("subsample nrows: ", (nrow(potDI_ds_split[[i]]))))
}


if(loc=="Laptop"){
  cli <- makeCluster(2, outfile="C:/Users/mleza/test.txt")
} else {
  nc <- detectCores()-3
  cli <- makeCluster(nc, outfile="/home/l/llezamav/gather.txt")
}

registerDoParallel(cli)


# start looping by subsample here

lapply(seq(potDI_ds_split), function(k){
  i=0
  t0prev <- 0
  x <- 3.5
  repeat {
    di <- aoa(newdata=potDI_ds_split[[k]], train=samples, cl=cli)
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
    most_diss_ds <- subset(potDI_ds_split[[k]], rownames(potDI_ds_split[[k]]) %in% names(most_diss))
    samples <- rbind(samples, most_diss_ds)
    
    # take most dissimlar away from potDI_ds_split[[k]] 
    potDI_ds_split[[k]] <- subset(potDI_ds_split[[k]], rownames(potDI_ds_split[[k]]) %notin% names(most_diss))
    
    # stop if either all samples are in the AOA or if there are less remaining samples than chosen in this step
    print(paste0("y=", y, " m=", m, " stopping criterion met? 1: All in AOA=1? ", any(di$AOA==0)==FALSE,
                 " or 2: less samples to choose from than needed in last choose? nrow(potDI_ds_split[[k]]) )", 
                 nrow(potDI_ds_split[[k]]), " or too little to choose? choose=", choose, " (should be 0 and then stop) - i.e. ", nrow(potDI_ds_split[[k]]) < choose))
    if(any(di$AOA==0)==FALSE | nrow(potDI_ds_split[[k]]) < choose | choose < 5){
      break
    }
    
    i=i+1
  }
  
  # take dataset at the chosen rownames to get full dataset
  trainDS <- pott3[rownames(samples),]
  all(rownames(trainDS)==rownames(samples))
  
  saveRDS(trainDS, paste0(datpath, "train_test/train_DI_complete_ ", k ,".rds"))
  write.csv2(trainDS, paste0(datpath, "train_test/train_DI_complete_ ", k ,".csv"))
  
  
  
})



stopCluster(cli)


ps <- list.files(datpath, pattern="train_DI_complete_", full.names = T)
all_csv <- lapply(seq(ps), function(i){
  read.csv2(ps[i])
})

comp_comp <- do.call("rbind", all_csv)
write.csv2(comp_comp, paste0(datpath, "train_test/train_DI_complete_all_datasets.csv"))


# to maintain data distribution
n <- 500000

rnds <- sample(rownames(comp_comp), n)
randsamples <- comp_comp[rnds,]
samples <- randsamples
print(paste0("nrow samples = ", nrow(samples)))


potDI_ds <- subset(comp_comp, rownames(comp_comp) %notin% rnds)
print(paste0("nrow potDI_ds = ", nrow(potDI_ds)))



nc <- detectCores()-3
cli <- makeCluster(nc, outfile="/home/l/llezamav/gather.txt")

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
  print(paste0("y=", y, " m=", m, " stopping criterion met? 1: All in AOA=1? ", any(di$AOA==0)==FALSE,
               " or 2: less samples to choose from than needed in last choose? nrow(potDI_ds) )", 
               nrow(potDI_ds), " or too little to choose? choose=", choose, " (should be 0 and then stop) - i.e. ", nrow(potDI_ds) < choose))
  if(any(di$AOA==0)==FALSE | nrow(potDI_ds) < choose | choose < 5){
    break
  }
  
  i=i+1
}


write.csv2(comp_comp, paste0(datpath, "train_test/train_DI_complete_all_datasets_DI.csv"))

stopCluster(cli)

