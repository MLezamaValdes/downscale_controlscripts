##############################################################
########### external testing of models #######################
##############################################################


library(ggplot2)
library(viridis)
library(gridExtra)
library(grid)
library(caret)
library(raster)

modelpath <- "D:/downscaling_after_talk/models/"
trainvalidpath <- "D:/downscaling_after_talk/clean_data/train_valid/"
regressionStatsRsenal <- source("C:/Users/mleza/OneDrive/Documents/PhD/ML_dist/ML_dist/regressionstats_Rsenal.R")

figurepath <- "C:/Users/mleza/OneDrive/Documents/PhD/work_packages/auto_downscaling_30m/paper/paper_draft/figures/new/"

rasterpath <- "D:/downscaling_after_talk/clean_data/satstacks_ngb/"
predoutpath <- "D:/downscaling_after_talk/predictions/"


train <- read.csv2(paste0(trainvalidpath, "train_LHS_150000.csv"))

test1 <- read.csv2(paste0(trainvalidpath, "validation_1_LHS_150000.csv"))
test2 <- read.csv2(paste0(trainvalidpath, "validation_2_LHS_150000.csv"))
test3 <- read.csv2(paste0(trainvalidpath, "validation_3_LHS_150000.csv"))






##############################################################

# train_sc <- read.csv2(paste0(trainvalidpath, "train_LHS_150000_scaled.csv"))
# 
# test1_sc <- read.csv2(paste0(trainvalidpath, "validation_1_LHS_150000_scaled.csv"))
# test2_sc <- read.csv2(paste0(trainvalidpath, "validation_2_LHS_150000_scaled.csv"))
# test3_sc <- read.csv2(paste0(trainvalidpath, "validation_3_LHS_150000_scaled.csv"))
# 
# testlist_scaled <- list(test1_sc, test2_sc, test3_sc)

# testlist_unscaled <- lapply(seq(testlist), function(i){
#   testlist[[i]]$TeAq <- as.factor(substring(testlist[[i]]$Mscene,1,3))
#   testlist[[i]]$TeAqNum <- as.numeric(testlist[[i]]$TeAq)
#   testlist[[i]]
# })

# sizetab <- as.table(sizetests)
# names(sizetab) <- c("test1", "test2", "test3")
# barplot(sizetab, main="test subset sizes")

methods <- c("rf")

testtitle <- c("spatial validation", "temporal validation",
               "spatiotemporal validation")


# testtitle <- c("validation set 1", "validation set 2", "validation set 3")

i=1


# #### var imp & pred obs plots ############################################################
# ####################################################################################
# 



validname <- c("spatial", "temporal", "spatio-temporal")




# to do: scale predictors

opts <- c("final")

j=1

for (j in 1:length(methods)){
  
  method <- methods[j]
  print(method)
  
for (i in 1:length(opts)){
  
  testlist <- list(test1, test2, test3)
  
  
  testlist <- testlist 
  sizetests <- c(nrow(testlist[[1]]), nrow(testlist[[2]]), nrow(testlist[[3]]))
  sizetests

  testlist <- lapply(seq(testlist), function(x){
    
    testlist[[x]]$TeAq <- as.factor(substring(testlist[[x]]$Mscene,1,3))
    testlist[[x]]$TeAqNum <- as.numeric(testlist[[x]]$TeAq)
    
      testlist[[x]]$soilraster <- factor(testlist[[x]]$soilraster)
      
      killlevels <- levels(testlist[[x]]$soilraster)[levels(testlist[[x]]$soilraster) %in% 
                                         levels(model_final$trainingData$soilraster)==FALSE]
      
      
      levels(testlist[[x]]$soilraster)[levels(testlist[[x]]$soilraster) %in% killlevels] <- NA
      testlist[[x]]$TeAqNum <- factor(testlist[[x]]$TeAqNum)
      testlist[[x]]$landcoverres <- factor(testlist[[x]]$landcoverres)
      testlist[[x]][,c(model_final$finalModel$xNames, "Landsat")]
      testlist[[x]] <- testlist[[x]][complete.cases(testlist[[x]]),]
      testlist[[x]]
    
  })
  str(testlist[[1]])
  sizetests <- c(nrow(testlist[[1]]), nrow(testlist[[2]]), nrow(testlist[[3]]))
  sizetests
#
  levels(model_final$trainingData$soilraster) == levels(testlist[[3]]$soilraster)
  levels(model_final$trainingData$TeAqNum) == levels(testlist[[3]]$TeAqNum)
  levels(model_final$trainingData$landcoverres) == levels(testlist[[3]]$landcoverres)



  # print(opts[i])
  # 
  # fm <- list.files(modelpath, pattern=method, full.names=T)
  # fm <- fm[grepl(opts[i], fm)]
  # print(fm)
  
  load(paste0(modelpath, "final_model_rf_150000factorfast_mtry.RData"))
  
  print(model_final)
  
  n <- 150000
  
  if(method=="gbm" | method=="nnet"){
    
    testlist <- testlist_scaled 
 
  }
  


  
  if(method!="svmLinear"){
      finImp <- varImp(model_final)
      plot(finImp, main=paste0("variable importance final ", method, " model (rf, n=150000)") )
  } else {
    # #create a pretty color vector for the bar plot
    # cols<-colorRampPalette(c('lightgreen','lightblue'))(num.vars)
    # 
    # #use the function on the model created above
    # par(mar=c(3,4,1,1),family='serif')
    # gar.fun('Landsat',model_final,col=cols,ylab='Rel. importance',ylim=c(-1,1))
    # 
    # 
    # library(rminer)
    # M <- fit(Landsat~predictornames, data=train[1:50000,], model="svm", kpar=list(sigma=0.10), C=model_final$finalModel@param$C)
    # svm.imp <- Importance(M, data=train[1:50000,])
    
    # library(rminer)
    # Importance(model_final,data=testlist[[1]])
    
  }
  

  
  # 
  # model_final <- get(load(paste0(modelpath,"model_final_",method,"_", n, ".RData")))
  # print(paste0("model name = ", "model_final_",method,"_", n, ".RData"))
  
  # external statistical evaluation
  ex_test_1to3 <- lapply(seq(3), function(j){
    
    # testlist[[j]]$TeAq <- as.factor(substring(testlist[[j]]$Mscene,1,3))
    # testlist[[j]]$TeAqNum <- as.numeric(testlist[[j]]$TeAq)
    # 
    pred <- predict(model_final, newdata = testlist[[j]])
    
    saveRDS(pred, paste0(predoutpath, "prediction_", method, "_test", j, ".RDS"))
    
    s <- regressionStats(pred,testlist[[j]]$Landsat)
    
    sround <- round(s, digits=2)
    
    df <- data.frame(pred, testlist[[j]]$Landsat)
    names(df) <- c("pred", "obs")
    
    
    p <- ggplot(df, aes(obs,pred))+
      #xlab(paste("observed LST in test", j))+ylab(paste("predicted LST in test", j))+
      #ggtitle(testtitle[j])+
      theme_minimal()+
      labs(x=paste("observed LST"), 
             y=paste("predicted LST"),
             title= paste0("(", letters[j], ")  ", paste0(testtitle[j], " n=", sizetests[j])),
             subtitle=paste0("R²=",sround$Rsq, " RMSE=",sround$RMSE))+
      stat_binhex(bins=300)+
      geom_abline(slope=1,intercept=0)+
      scale_x_continuous(expand=c(0,0))+
      scale_y_continuous(expand=c(0,0))+
      scale_fill_gradientn(name="count \nper bin", 
                           trans="log", 
                           colours=viridis(10), 
                           breaks=5^(0:4)
                           
                           )+
      theme(axis.text=element_text(size=10),
             axis.title=element_text(size=10),
            legend.title = element_text(size=10),
            panel.grid.minor = element_blank())
    
    return(list(p, df, s))
    
  })
  
  plots <- lapply(ex_test_1to3, '[[', 1)

  eg <- grid.arrange(plots[[1]], plots[[2]], plots[[3]], nrow = 2)
  eg
  
  ggsave(paste0(figurepath, "external_eval_new_", method, "_",opts[i], "_remodeling.png"), 
         plot = eg, width=20, height=14, units="cm", dpi=1000)
  
  
  dfs <- lapply(ex_test_1to3, '[[', 2)
  saveRDS(dfs, paste0(predoutpath, "external_eval_stats_remodeling", method, "_",opts[i], ".RDS"))
  
}
}



##################### VARIMP #################################################

vi <- varImp(model_final)

str(vi)
class(vi$importance)

library(tidyverse)
library(dplyr)
library(ggplot2)

vi <- vi$importance
vi$var <- rownames(vi)
names(vi) <- c("Importance", "Predictor")
vi <- vi[order(vi$Importance),]



vi$Predictor <- as.factor(vi$Predictor)
vi$Predictornames <- c("Terra/Aqua", "aspect","slope", "incidence angle",
                       "soil type", "landcover type", "DEM", "MODIS LST")
vi

viplot <- vi %>% 
  ggplot(aes(reorder(Predictornames, Importance), Importance)) + 
  geom_col(aes(fill = Importance)) + 
  scale_fill_viridis()+
  # scale_fill_gradient2(low = "white", 
  #                      high = "skyblue") + 
  coord_flip() + theme_minimal()+
  labs(y = "",
       x = "")+
  theme(legend.title = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text=element_text(size=15),
        legend.text = element_text(size=12))

viplot
ggsave(paste0(figurepath, "VI_plot.png"), 
       plot = viplot, 
       # width=10, height=7, 
       # units="cm", 
       dpi=1000)
