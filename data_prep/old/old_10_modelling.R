############ Modelling
y=1
m=1

library(devtools)
library(caret)
library(CAST)
modpath <- "D:/new_downscaling/modelling/"

### get extraction data ###################################################################
expath <- "D:/new_downscaling/extraction/"
exdf <- read.csv2(paste0(expath, "extr_complete_cases_2019-01.csv"), header=T, nrow=10000000) # 10 million samples for now, change!
str(exdf)
names(exdf)

exdforg <- exdf
exdf <- exdf[,c(1:6, 9:ncol(exdf))] # aux ID rausschmeißen
names(exdf) <- c("id","Landsat", "Modis", "ia", "hs", "time", "dem", "slope", "aspect", 
                 "TWI", "soilraster", "landcoverres", "spatialblocks", "swir")
head(exdf)
str(exdf)

exdf$time <- as.numeric(as.factor(exdf$time))
exdf$Modis[exdf$Modis <= -30] <- NA
exdf <- exdf[complete.cases(exdf),]

######### plot #############################################################################

par(mfrow=c(4,4), mar=c(5,3,2,2)+2)
for(i in seq(ncol(potDI))){
  print(i)
  if(typeof(potDI[,i])=="factor"){
    barplot(table(as.factor(potDI[,i])), main=names(potDI)[i])
  } else {
    boxplot(potDI[,i], main=names(potDI)[i])
  }
}

### get raster data ###################################################################
tempdyn <- stack(paste0(cddir, "L_MOD_hs_ia", substring(time_range[[y]][[m]][[1]][[1]], 1, 7), ".tif"))
tdnam <- read.csv("D:/new_downscaling/clean_data/names_sat_ia_hs_2019-01.csv")
names(tempdyn) <- tdnam$x
aux <- stack("D:/new_downscaling/auxiliary/aux_stack.tif")
names(aux) <- c("dem", "slope", "aspect", "TWI", "soilraster", "landcoverres", "spatialblocks")

### 1 select test data ###################################################################

### make a raster ######################

usb <- unique(exdf$spatialblocks) 
# sbs <- sample(usb,length(usb)*0.2)
sbs <- c(64,77,54,19,31,78,61,62)

testsites <- aux$spatialblocks
testsites_vals <- ifelse(testsites[] %in% sbs,1,0)
testsites[] <- testsites_vals
plot(testsites)
writeRaster(testsites, "D:/new_downscaling/modelling/testsites_2019_01.tif")


exdf$test <- ifelse(exdf$spatialblocks %in% sbs,1,0)

test <- subset(exdf,exdf$test==1)
saveRDS(test, paste0(modpath, "test.rds"))

pottrain <- subset(exdf, exdf$test==0)
saveRDS(pottrain, paste0(modpath, "pottrain_before_sampling.rds"))

### 2.0 random sample selection ###################################################################

# get 3 Mio random samples per month to choose from in random and DI picking
splpot <- sample(rownames(pottrain), 3000000)
pott3 <- pottrain[splpot,]

# get 150.000 random samples per month to honour data distribution
rnds <- sample(rownames(pott3), 150000)
randsamples <- pott3[rnds,]

pott3$inrandsamples <- ifelse(rownames(pott3) %in% rnds,1,0)
potDI <- pott3[pott3$inrandsamples == 0,]

### 2 sample selection via DI ###################################################################

# check whether variability is given 


# choose 10 (new) random points
trn <- sample(rownames(potDI), 10)
DItrain <- potDI[trn,]

# dissimilarity is being calculated for the rest of the dataset
# what to do about chosen and discarded ones? 
ndatrn <- !(rownames(potDI) %in% trn)
DInewdata <- potDI[ndatrn,]
di <- aoa(newdata=DInewdata, train=DItrain)

# iterieren

while(any(aoa$AOA==0)==FALSE){
  
}
# Abbruch, wenn AOA überall 1 

#### PART OF HANNA#s AOA FUNCTION ######################################

#### Scale data and weight data if applicable:
train <- pottrain
train <- scale(train)
scaleparam <- attributes(train)
# if(!inherits(weight, "error")){
#   train <- sapply(1:ncol(train),function(x){train[,x]*unlist(weight[x])})
# }
newdata <- test
newdata <- scale(newdata,center=scaleparam$`scaled:center`,#scaleparam$`scaled:center`
                 scale=scaleparam$`scaled:scale`)

# if(!inherits(weight, "error")){
#   newdata <- sapply(1:ncol(newdata),function(x){newdata[,x]*unlist(weight[x])})
# }

#### For each pixel caclculate distance to each training point and search for
#### min distance:
#mindist <- apply(newdata,1,FUN=function(x){


distfun <- function(x){
  if(any(is.na(x))){
    return(NA)
  }else{
    tmp <- FNN::knnx.dist(t(matrix(x)),train,k=1)
    return(min(tmp))
  }
}



# ### for test run
# e <- extent(c(383828.5, 389660.8, -1294332, -1288986))
# tempdyn <- crop(tempdyn, e)
# aux <- crop(aux,e)


# # take those blocks, that contain a sufficiently high amount of data, of those a third for testing
# t <- table(exdf$blockmask)
# set.seed(100)
# testids <- sample(t[t>70000],round(length(t[t>70000])*0.3))
# testext <- lapply(seq(testids), function(i){
#   exdf[exdf$blockmask==as.numeric(names(testids))[i],]
# })
# 
# test <- do.call("rbind",testext) 
# testdfids <- rownames(test)

testdfids <- seq(1,length(unique(exdf$id))*0.20) # take first 20% of pixels for testing (update with spatial blocks)

### make test and train datasets  ###################################################################
exdf$test <- 0
exdftestids <- which(rownames(exdf) %in% testdfids)
exdf$test[exdftestids] <- 1
table(exdf$test) # 0 training, 1 test

# make blocks
bm <- cv
#bm[bm==2000] <- NA
x <- (min(nrow(bm), ncol(bm)))
y = round(x/sqrt(10))
bmagg <- aggregate(bm, fact=y, fun=min)
bmagg[] <- seq(1:ncell(bmagg))
plot(bmagg)

bmaggaux <- resample(bmagg, aux[[1]], method="ngb")
eb <- extract(bmaggaux, extent(bmaggaux))

ebb <- c(eb,eb,eb,eb,eb)
exdf$block <-ebb[1:nrow(exdf)]

train <- exdf[exdf$test == 0,]
test <- exdf[exdf$test == 1,]

## make a visual representation of the test sites
testareasblockmask <- exdf$id[exdf$test==1]
cv <- aux$spatialblocks
cv[][testareasblockmask] <- 2000
plot(cv)





outpath <- "C:/Users/mleza/OneDrive/Documents/PhD/work_packages/auto_downscaling_30m/model_testing/"
writeRaster(cv, paste0(outpath, "train_test.tif"), overwrite=T)
writeRaster(bmagg, paste0(outpath, "spatialblocks_training.tif"), overwrite=T)

saveRDS(train, paste0(outpath, "train.rds"))
saveRDS(test, paste0(outpath, "test.rds"))


######################################### RUN MODEL #################################################

trainsubset <- train[sample(nrow(train),5000), ]
saveRDS(trainsubset, paste0(outpath, "train_subset_5000.rds"))
train <- trainsubset 
testsubset <- test[sample(nrow(test),5000),]

train$time <- as.numeric(train$time)

# split training cuarter into various blocks for cv during training
foldids <- CreateSpacetimeFolds(train, spacevar="block", timevar = "time",
                                k=ncell(bmagg),seed=100)
trainlength <- sapply(seq(foldids$indexOut), function(i){
  length(foldids$indexOut[[i]])
})
tctrl <- trainControl(method="cv", savePredictions = TRUE,
                      returnResamp = "all",
                      verbose=TRUE,
                      index=foldids$index,
                      indexOut=foldids$indexOut)

#start parallel processing on all cores except 3:
library(parallel)
library(doParallel)
library(ggplot2)

cores <- detectCores()
cl <- makeCluster(cores-3)
registerDoParallel(cl)

set.seed(100)
ffsModel <- ffs(train[,c(2:4, 7:12)],
                train$Landsat,
                method = "rf",
                ntree=100,
                trControl=tctrl,
                tuneGrid=expand.grid(mtry=2))

#stop cluster and save model:
saveRDS(ffsModel,file=paste0(outpath,"ffsModel_fake_model_n5000.rds"))

ffsModel <- readRDS(paste0(outpath,"ffsModel_fake_model_n5000.rds"))

finalModel <- train(train[,ffsModel$selectedvars],
                    train$Landsat,
                    method = "rf",
                    trControl=tctrl,
                    tuneLength=length(ffsModel$selectedvars))

stopCluster(cl)

saveRDS(finalModel,file=paste0(outpath,"finalModel_train_L8_30m.rds"))
#ffsModel <- readRDS(paste0(outpath,"ffsModel_fake_model.rds"))

finalModel
ffsModel$selectedvars

##################### TEST & PREDICT ######################

#test <- readRDS(paste0(outpath, "test.rds"))
pred <- predict(finalModel, newdata = testsubset)
saveRDS(pred, file=paste0(outpath, "prediction_testdata_n5000.rds"))

source("C:/Users/mleza/OneDrive/Documents/PhD/ML_dist/ML_dist/regressionstats_Rsenal.R")

#visualize
regressionStats(pred, testsubset$Landsat)
df <- data.frame(pred, testsubset$Landsat)



ggplot(df, aes(testsubset.Landsat,pred))+
  xlab("Observed LST in Testdata")+ylab("predicted LST with Testdata")+
  ggtitle("Observed vs. Predicted LST in Testdata, 
          trained on 5000 observations")+
  stat_binhex(bins=300)+
  geom_abline(slope=1,intercept=0)+
  scale_x_continuous(expand=c(0,0))+
  scale_y_continuous(expand=c(0,0))+
  scale_fill_gradientn(name="datapoints", trans="log", colours=viridis(10), 
                       breaks=10^(0:4))

# complete stack
sc <- stack(tempdyn, aux)

iasenes <- which(grepl("ia_",names(sc)))

for(i in seq(iasenes)){
  ffspredstack <- sc[[c(iasenes[i],45,50,49,46)]]
  names(ffspredstack) <- c("ia", names(ffspredstack)[2:5])
  pred_ras <- predict(ffspredstack, finalModel)
  
  png(filename=paste0(outpath, "Landsat_prediction_", i, ".png"))
  par(mfrow=c(1,2))
  plot(sc[[iasenes[i]-2]], main=paste0("Landsat ", substring(names(sc[[iasenes[i]-1]]), 11,22)))
  plot(pred_ras, main="prediction")
  dev.off()
}



