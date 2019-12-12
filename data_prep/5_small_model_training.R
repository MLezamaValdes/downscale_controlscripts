# select training and testdata based on the testblock
library(devtools)
#install_github("HannaMeyer/CAST")
library(caret)
library(CAST)

boxplot(exdf$blockmask)
quantile(exdf$blockmask)

# random räumliche Bereiche raus
traintestras <- s$blockmask
traintestras[traintestras<46] <- 1
traintestras[traintestras>=46] <- 0

par(mfrow=c(1,1))
plot(traintestras, main="train and test areas (75% train from cleaned dataset)")
plot(aoianta, add=T)

# 75% = 46

# plot(s$blockmask)
# plot(aoianta, add=T)

# train = 1, test=0
exdf$traintest <- NA
exdf$traintest[exdf$blockmask<46] <- 1
exdf$traintest[exdf$blockmask>=46] <- 0


train <- exdf[exdf$traintest==1,]
test <- exdf[exdf$traintest==0,]

nrow(train)

opp <- "C:/Users/mleza/OneDrive/Documents/PhD/work_packages/auto_downscaling_30m/fake_model/"
saveRDS(train, paste0(opp, "train.rds"))
saveRDS(test, paste0(opp, "test.rds"))

######################################### RUN MODEL #################################################

df[sample(nrow(df), 3), ]

saveRDS(train, paste0(opp, "train_50000.rds"))
train <- trainsubset
train <- readRDS( paste0(opp, "train_50000.rds"))
trainsubset <- train[sample(nrow(train),1000), ]
train <- trainsubset 

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
                ntree=30,
                trControl=tctrl,
                tuneGrid=expand.grid(mtry=2))

#stop cluster and save model:
saveRDS(ffsModel,file=paste0(opp,"ffsModel_fake_model.rds"))


finalModel <- train(train[,ffsModel$selectedvars],
                    train$L8,
                    method = "rf",
                    trControl=tctrl,
                    tuneLength=length(ffsModel$selectedvars))

stopCluster(cl)
saveRDS(finalModel,file=paste0(opp,"finalModel_train_L8_30m.rds"))

list.files(opp)



