
######################################### RUN MODEL #################################################
y=1
m=1

ym <- substring(time_range[[y]][[m]][[1]][[1]], 1, 7)


expath <- "D:/new_downscaling/extraction/"
#train <- read.csv2(paste0(expath, "train_DI_", ym, ".rds"))

### just for testing ###############
train <- 
trainsubset <- train[sample(nrow(train),5000), ]
saveRDS(trainsubset, paste0(outpath, "train_subset_5000.rds"))
train <- trainsubset 
testsubset <- test[sample(nrow(test),5000),]

#################


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



