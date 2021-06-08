############## RUN EXTRACTION AND MODELLING ON 1km² TO SEE IF THATs WHY MODIS DOESNT GET IN ############
library(raster)
library(CAST)
library(caret)

########### get stacks ################

auxpath <- "D:/new_downscaling/auxiliary/"
expath <- "D:/new_downscaling/extraction/"

ym <- "2019-01"
ex1k <- paste0(expath, "test1k/")
dir.create(ex1k)

aux <- stack(list.files(auxpath, pattern="67", full.names = T))
names(aux) <- c("dem", "slope", "aspect", "TWI", "soilraster", "landcoverres", "spatialblocks", "swir6", "swir7", "x", "y")

tempdyn <- raster::stack(paste0(cddir, "new_L_MOD_hs_ia2019-01.tif"))
tdnam <- read.csv(paste0(cddir, "names_sat_ia_hs_2019-01.csv"))
names(tempdyn) <- tdnam$x

# get aoi
aoi <- readOGR(paste0(aoipath, "Levy_MDV.shp"))
antaproj <- crs("+proj=stere +lat_0=-90 +lat_ts=-71 +lon_0=0 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m
+no_defs +ellps=WGS84 +towgs84=0,0,0")
aoianta <- spTransform(aoi, antaproj)
########### resample to 1km² resolution ################

# get a MODIS template for resampling
mos <- raster("D:/new_downscaling/data_download_preprocessing/MODIS/2019-01/MDV_MODIS_LST_Mosaic.tif")
plot(mos)

t1 <- resample(tempdyn, mos)
a1 <- resample(aux, mos)
names(a1) <- names(aux)

########### extract data ################

# extract aux
auxdf <- as.data.frame(extract(a1, aoianta))
write.csv2(auxdf, paste0(ex1k, "aux_df_1km.csv"), row.names=F)

tmpdyndf <- as.data.frame(extract(t1, aoianta))
write.csv2(tmpdyndf, paste0(ex1k, "aux_df_1km.csv"), row.names=F)

# sort into useful file
new_package <- seq(1,(ncol(tmpdyndf)-2), by=4)
end <- new_package+3

dfslices <- lapply(seq(new_package), function(i){
  x <- tmpdyndf[,new_package[i]:end[i]] # get all columns for one date
  xnam <- substring(names(x)[3], 4, 18) # get time info from hs/ia
  x$time <- xnam # add time as row
  x$xd <- tmpdyndf$x
  x$yd <- tmpdyndf$y
  x$id <- seq(1:nrow(x)) # as pixel ID for tempdyn
  x <- cbind(x, auxdf) # add tempdyn for one date + aux together
  names(x) <- c("Modis", "ia", "hs", "Landsat", "time", "id","dem", "slope", "aspect",
                "TWI", "soilraster", "landcoverres", "spatialblocks", "swir6", "swir7", "xa", "ya")
  x
})

tddf <- do.call(rbind,dfslices)

tddf$Modis[tddf$Modis > 30] <- NA
tddf$Modis[tddf$Modis <= -100] <- NA
tddf$Landsat[tddf$Landsat <= -100] <- NA
tddf$Landsat[tddf$Landsat > 30] <- NA

tddfcc <- tddf[complete.cases(tddf),]
write.csv2(tddfcc, paste0(ex1k, "extr_complete_cases_2019-01_1k.csv"), row.names=F)


########### make test and training dataset  ################
# test
testsites <- c(26, 63, 43, 12, 35, 40, 31, 79,  5,  2, 60, 11)

test <- subset(tddfcc, tddfcc$spatialblocks %in% testsites)

saveRDS(test, paste0(ex1k, "test_ds_", ym, ".rds"))
write.csv2(test, paste0(ex1k, "test_ds_", ym, ".csv"))

# train
`%notin%` <- Negate(`%in%`)
pottrain <-  subset(tddfcc, tddfcc$spatialblocks %notin% testsites)

nrow(pottrain)/nrow(test)

########### start modelling  ################
train <- pottrain[sample(nrow(pottrain),5000), ]
saveRDS(train, paste0(ex1k, "train_", ym, "_subset_5000.rds"))

train$time_num <- as.numeric(as.factor(train$time))


kval <- min(length(unique(train$time_num)), length(unique(train$spatialblocks)))

# split training cuarter into various blocks for cv during training
foldids <- CreateSpacetimeFolds(train, spacevar="spatialblocks", timevar = "time_num",
                                k=kval,seed=100)

(trainlength <- sapply(seq(foldids$indexOut), function(i){
  length(foldids$index[[i]])
}))

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

prednames <- names(train[,c(1:3, 7:12, 14:15)])
prednames_nohs <- names(train[,c(1:2,7, 10:12, 14:15)])

set.seed(100)
ffsModel_hohs <- ffs(train[,c(1:2,7, 10:12, 14:15)],
                train$Landsat,
                method = "rf",
                ntree=250,
                trControl=tctrl,
                tuneGrid=expand.grid(mtry=2))

set.seed(100)
ffsModel <- ffs(train[,c(1:3, 7:12, 14:15)],
                train$Landsat,
                method = "rf",
                ntree=250,
                trControl=tctrl,
                tuneGrid=expand.grid(mtry=2))

#stop cluster and save model:
saveRDS(ffsModel,file=paste0(ex1k,"ffsModel_2019_01_model_n5000.rds"))
saveRDS(ffsModel_hohs,file=paste0(ex1k,"ffsModel_2019_01_model_n5000_nohs.rds"))

#ffsModel <- readRDS(paste0(outpath,"ffsModel_2019_01_model_n7000.rds"))

finalModel <- train(train[,ffsModel$selectedvars],
                    train$Landsat,
                    method = "rf",
                    trControl=tctrl,
                    tuneLength=length(ffsModel$selectedvars))


finalModel_nohs <- train(train[,ffsModel_hohs$selectedvars],
                    train$Landsat,
                    method = "rf",
                    trControl=tctrl,
                    tuneLength=length(ffsModel$selectedvars))

stopCluster(cl)

saveRDS(finalModel,file=paste0(ex1k,"finalModel_2019_01_model_n5000.rds"))
saveRDS(finalModel,file=paste0(ex1k,"finalModel_2019_01_model_n5000_nohs.rds"))

#ffsModel <- readRDS(paste0(outpath,"ffsModel_fake_model.rds"))

finalModel_nohs

############### TEST AND PREDICT ################################

#test <- readRDS(paste0(outpath, "test.rds"))
pred <- predict(finalModel, newdata = test)
saveRDS(pred, file=paste0(ex1k, "prediction_testdata_n5000.rds"))

pred_nohs <- predict(finalModel_nohs, newdata = test)
saveRDS(pred_nohs, file=paste0(ex1k, "prediction_testdata_n5000_nohs.rds"))

source("C:/Users/mleza/OneDrive/Documents/PhD/ML_dist/ML_dist/regressionstats_Rsenal.R")

#visualize
regressionStats(pred, test$Landsat)
df <- data.frame(pred, test$Landsat)

regressionStats(pred_nohs, test$Landsat)
df_nohs <- data.frame(pred_nohs, test$Landsat)

ggplot(df, aes(test.Landsat,pred))+
  xlab("Observed LST in Testdata")+ylab("predicted LST with Testdata")+
  ggtitle("Observed vs. Predicted LST in Testdata, 
          trained on 7000 observations, 250 trees")+
  stat_binhex(bins=300)+
  geom_abline(slope=1,intercept=0)+
  scale_x_continuous(expand=c(0,0))+
  scale_y_continuous(expand=c(0,0))+
  scale_fill_gradientn(name="datapoints", trans="log", colours=viridis(10), 
                       breaks=10^(0:4))



ggplot(df_nohs, aes(test.Landsat,pred_nohs))+
  xlab("Observed LST in Testdata")+ylab("predicted LST with Testdata")+
  ggtitle("Observed vs. Predicted LST in Testdata, 
          trained on 7000 observations, 250 trees")+
  stat_binhex(bins=300)+
  geom_abline(slope=1,intercept=0)+
  scale_x_continuous(expand=c(0,0))+
  scale_y_continuous(expand=c(0,0))+
  scale_fill_gradientn(name="datapoints", trans="log", colours=viridis(10), 
                       breaks=10^(0:4))
#################### PREDIICTIONS #####################################################


ffsModel$selectedvars

iasenes <- which(grepl("LC08_",names(sc)))

auxmodstack <- sc[[ffsModel$selectedvars]]

# stack pred rasters together



for(i in seq(iasenes)){
  # ffspredstack <- stack(sc[[c(iasenes[i])]], auxmodstack)
  # names(ffspredstack) <- c("ia", names(ffspredstack)[2:5])
  pred_ras <- predict(auxmodstack, finalModel)
  
  png(filename=paste0(outpath, "Landsat_prediction_", i, ".png"))
  par(mfrow=c(1,2))
  plot(sc[[iasenes[i]-2]], main=paste0("Landsat ", substring(names(sc[[iasenes[i]-1]]), 11,22)))
  plot(pred_ras, main="prediction")
  dev.off()
}



