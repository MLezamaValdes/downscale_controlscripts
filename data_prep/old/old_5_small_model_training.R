
# get datasets
# raster data
sres <- readRDS(paste0(main, "auxiliary_stack_30m.rds"))


# extracted data
exdf <- readRDS(paste0(paste0("C:/Users/mleza/OneDrive/Documents/PhD/work_packages/auto_downscaling_30m/", "ex_fake_model_final.rds")))

s <- readRDS(paste0(main, "comp_stack.rds"))

# select training and testdata based on the testblock
library(devtools)
#install_github("HannaMeyer/CAST")
library(caret)
library(CAST)

boxplot(exdf$blockmask)
quantile(exdf$blockmask)

# take those blocks, that contain a sufficiently high amount of data, of those a third for testing
t <- table(exdf$blockmask)
set.seed(100)
testids <- sample(t[t>70000],round(length(t[t>70000])*0.3))

testext <- lapply(seq(testids), function(i){
  exdf[exdf$blockmask==as.numeric(names(testids))[i],]
})

test <- do.call("rbind",testext) 

testdfids <- rownames(test)

exdf$test <- 0
exdftestids <- which(rownames(exdf) %in% testdfids)
exdf$test[exdftestids] <- 1

train <- exdf[exdf$test == 0,]
test <- exdf[exdf$test == 1,]

## !!!!! has to be done!
testareasblockmask <- as.numeric(names(testids))
cv <- blockmask
cv[cv==testareasblockmask] <- 2000
plot(cv)


# # random räumliche Bereiche raus
# traintestras <- s$blockmask
# traintestras[traintestras<46] <- 1
# traintestras[traintestras>=46] <- 0
# 
# par(mfrow=c(1,1))
# plot(traintestras, main="train and test areas (75% train from cleaned dataset)")
# plot(aoianta, add=T)
# 
# # 75% = 46
# 
# # plot(s$blockmask)
# # plot(aoianta, add=T)
# 
# # train = 1, test=0
# exdf$traintest <- NA
# exdf$traintest[exdf$blockmask<46] <- 1
# exdf$traintest[exdf$blockmask>=46] <- 0
# 
# 
# train <- exdf[exdf$traintest==1,]
# test <- exdf[exdf$traintest==0,]
# 
# nrow(train)

opp <- "C:/Users/mleza/OneDrive/Documents/PhD/work_packages/auto_downscaling_30m/fake_model/"
# saveRDS(train, paste0(opp, "train.rds"))
# saveRDS(test, paste0(opp, "test.rds"))
# 
saveRDS(train, paste0(opp, "train_new.rds"))
saveRDS(test, paste0(opp, "test_new.rds"))

######################################### RUN MODEL #################################################

df[sample(nrow(df), 3), ]

#saveRDS(train, paste0(opp, "train_50000.rds"))
train <- readRDS( paste0(opp, "train_new.rds"))
test <- readRDS(paste0(opp, "test_new.rds"))

trainsubset <- train[sample(nrow(train),5000), ]
saveRDS(trainsubset, paste0(opp, "train_subset_5000.rds"))
train <- trainsubset 
testsubset <- test[sample(nrow(test),5000),]

# split training cuarter into various blocks for cv during training
foldids <- CreateSpacetimeFolds(train, spacevar="blockmask",
                                k=5,seed=100)
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
ffsModel <- ffs(train[,c(2:6)],
                train$L8,
                method = "rf",
                ntree=100,
                trControl=tctrl,
                tuneGrid=expand.grid(mtry=2))

#stop cluster and save model:
saveRDS(ffsModel,file=paste0(opp,"ffsModel_fake_model_n5000.rds"))

ffsModel <- readRDS(paste0(opp,"ffsModel_fake_model_n5000.rds"))

finalModel <- train(train[,ffsModel$selectedvars],
                    train$L8,
                    method = "rf",
                    trControl=tctrl,
                    tuneLength=length(ffsModel$selectedvars))

stopCluster(cl)

saveRDS(finalModel,file=paste0(opp,"finalModel_train_L8_30m.rds"))
#ffsModel <- readRDS(paste0(opp,"ffsModel_fake_model.rds"))

##################### TEST & PREDICT ######################

#test <- readRDS(paste0(opp, "test.rds"))
pred <- predict(finalModel, newdata = testsubset)
saveRDS(pred, file=paste0(opp, "prediction_testdata_n5000.rds"))

source("C:/Users/mleza/OneDrive/Documents/PhD/ML_dist/ML_dist/regressionstats_Rsenal.R")

#visualize
regressionStats(pred, testsubset$L8)
df <- data.frame(pred, testsubset$L8)



ggplot(df, aes(testsubset.L8,pred))+
  xlab("Observed LST in Testdata")+ylab("predicted LST with Testdata")+
  ggtitle("Observed vs. Predicted LST in Testdata, 
          trained on 5000 observations")+
  stat_binhex(bins=300)+
  geom_abline(slope=1,intercept=0)+
  scale_x_continuous(expand=c(0,0))+
  scale_y_continuous(expand=c(0,0))+
  scale_fill_gradientn(name="datapoints", trans="log", colours=viridis(10), 
                       breaks=10^(0:4))

############### predict whole raster ##########################################


# go on here! if not read tifs
finalModel <- readRDS(paste0(opp,"finalModel_train_L8_30m.rds"))
l <- stack(list.files(paste0(main, "LANDSAT_model/"), full.names = T))
mres <- stack(paste0(main, "MODIS_model/modis_resampled.tif"))
satstack <- stack(l, mres)

#satstack <- stack(paste0(main, "satstack.rds"))
sres <- stack(paste0(main, "auxiliary_stack_30m.tif"))

s <- stack(satstack, sres)
smask <- mask(s, aoianta)
writeRaster(smask, paste0(main, "complete_stack_small_model_mask.tif"))

for(i in seq(nlayers(s))){
  print(filename(s[[i]]))
}

s18 <- smask[[c(3, 5:8)]]
names(s18) <-  c("MODIS", "DEM", "slope", "TWI" , "hillsh" )
pred_18 <- predict(s18, finalModel)

#pred_19 <- predict(smask[[c(4:8)]], finalModel)

resstack <- stack(s18[[1]], smask[[2]], pred_18)
names(resstack) <- c("MODIS","L8","MOD_downscaled")
spplot(resstack)

plot(resstack[[1]])
e <- drawExtent()
resstacks <- crop(resstack,e)
spplot(resstacks)

par(mfrow=c(1,3))
plot(s18[[1]], main="MODIS 11.02.18")
plot(smask[[2]], main="L8 11.02.18")
plot(pred_18, main="MODIS 11.02.18 downscaled 30m")