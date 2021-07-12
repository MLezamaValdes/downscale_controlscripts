
library(CAST)

loc="Laptop"

if(loc=="Palma"){
  modelpath <- "/scratch/tmp/llezamav/modelling/"
  regressionStatsRsenal <- source()
  
  library(raster)
  library(rgdal)
  library(viridis)

  time_range <- readRDS("/scratch/tmp/llezamav/time_range.rds")

  outpath <- "/scratch/tmp/llezamav/modelling/"
  modelpath <- outpath
  
  
} else if(loc=="Laptop"){
  
  modelpath <- "D:/downscaling_after_talk/models/"
  regressionStatsRsenal <- source("C:/Users/mleza/OneDrive/Documents/PhD/ML_dist/ML_dist/regressionstats_Rsenal.R")
  figurepath <- "C:/Users/mleza/OneDrive/Documents/PhD/work_packages/auto_downscaling_30m/paper/paper_draft/figures/new/"
  library(ggplot2)
  library(gridExtra)
  library(viridis)
  
  
  library(raster)
  library(rgdal)
 
} else {
  print("something's off")
}


# year <- c(2019:2013)
# month <- c("01","02","03","04", "09", "10","11", "12")
# 
# `%notin%` <- Negate(`%in%`)

# # get aux general
# if(loc=="Laptop"){
#   datpath <- auxpath
# }

# amount of training samples
n <- 150000
# methods <- c("rf",
#            "gbm",
#            "nnet",
#            "svmLinear"
#            )
methods <- c("nnet")
par(mfrow=c(2,2), mar=c(5,3,2,2)+2)


  
eval <- lapply(seq(methods), function(i){
  method <- methods[i]
  print(method)
  
  dummymodels <- list.files(modelpath, pattern="dummys", full.names=T)

  dummymodels <- dummymodels[grepl("ffs", dummymodels)]
  
  #load(paste0(modelpath, "ffs_model_",method,"time_only_", n, ".RData"))
  load(dummymodels[which(grepl(method, dummymodels))])
  
  if(method=="rf"){
    predictions_rf <- ffs_model$pred[ffs_model$pred$mtry==ffs_model$bestTune$mtry,c("pred","obs")]
    s <- regressionStats(predictions_rf$pred,predictions_rf$obs)
    
    p <- ggplot(predictions_rf, aes(obs,pred)) + 
      stat_binhex(bins=100)+ggtitle(paste(method, "RMSE= ", round(round(s$RMSE, digits=2), digits=2), "R²= ", round(s$Rsq, digits=2)))+
      xlim(min(predictions_rf),max(predictions_rf))+ylim(min(predictions_rf),max(predictions_rf))+
      xlab("Measured LST 30m")+
      ylab("Predicted LST 30m")+
      geom_abline(slope=1, intercept=0,lty=2)+
      scale_fill_gradientn(name = "data points", colors=viridis(10))+theme_minimal()
    df <- data.frame(ffs_model$selectedvars, c(0,ffs_model$selectedvars_perf), c(0, ffs_model$selectedvars_perf_SE))
    df$type <- ffs_model$modelInfo$label
    names(df) <- c("selvars", "selvars_perf", "selvars_perf_SE", "type")
    df
    
    cvp <- plot(ffs_model, main=method)
    
    ffsp <- plot_ffs(ffs_model) + ggtitle(method)
     plot_ffs(ffs_model, plotType="selected", main = method)
    
  }
  
  if(method=="gbm"){
    predictions_gbm <- ffs_model$pred[ffs_model$pred$shrinkage==ffs_model$bestTune$shrinkage&
                                        ffs_model$pred$interaction.depth==ffs_model$bestTune$interaction.depth&
                                        ffs_model$pred$n.trees==ffs_model$bestTune$n.trees&
                                        ffs_model$pred$n.minobsinnode==ffs_model$bestTune$n.minobsinnode,
                                      c("pred","obs")]
    s <- regressionStats(predictions_gbm$pred,predictions_gbm$obs)
    
    p <- ggplot(predictions_gbm, aes(obs,pred)) + 
      stat_binhex(bins=100)+ggtitle(paste(method, "RMSE= ", round(s$RMSE, digits=2), "R²= ", round(s$Rsq, digits=2)))+
      xlim(min(predictions_gbm),max(predictions_gbm))+ylim(min(predictions_gbm),max(predictions_gbm))+
      xlab("Measured LST 30m")+
      ylab("Predicted LST 30m")+
      geom_abline(slope=1, intercept=0,lty=2)+
      scale_fill_gradientn(name = "data points", colors=viridis(10))+theme_minimal()
    #
    df <- data.frame(ffs_model$selectedvars, c(0,ffs_model$selectedvars_perf), c(0,ffs_model$selectedvars_perf_SE))
    
    cvp <- plot(ffs_model, main=method)
    ffsp <- plot_ffs(ffs_model) + ggtitle(method)
    

    df$type <- ffs_model$modelInfo$label
    names(df) <- c("selvars", "selvars_perf", "selvars_perf_SE", "type")
    df
    plot_ffs(ffs_model, main = method)
    
  }
  
  if(method=="nnet"){
    predictions_nnet <- ffs_model$pred[ffs_model$pred$size==ffs_model$bestTune$size& 
                                         ffs_model$pred$decay==ffs_model$bestTune$decay,
                                       c("pred", "obs")]
    s <- regressionStats(predictions_nnet$pred,predictions_nnet$obs)
    p <- ggplot(predictions_nnet, aes(obs,pred)) + 
      stat_binhex(bins=100)+ggtitle(paste(method, "RMSE= ", round(s$RMSE, digits=2), "R²= ", round(s$Rsq, digits=2)))+
      xlim(min(predictions_nnet),max(predictions_nnet))+ylim(min(predictions_nnet),max(predictions_nnet))+
      xlab("Measured LST 30m")+
      ylab("Predicted LST 30m")+
      geom_abline(slope=1, intercept=0,lty=2)+
      scale_fill_gradientn(name = "data points", colors=viridis(10))+theme_minimal()
    df <- data.frame(ffs_model$selectedvars, c(0,ffs_model$selectedvars_perf), c(0, ffs_model$selectedvars_perf_SE))
    
    df$type <- ffs_model$modelInfo$label
    names(df) <- c("selvars", "selvars_perf", "selvars_perf_SE", "type")
    df
    
    cvp <- plot(ffs_model, main=method)
    ffsp <- plot_ffs(ffs_model) + ggtitle(method)
    
    plot_ffs(ffs_model, main = method)
    
  }
  
  if(method=="svmLinear"){
    predictions_svm <- ffs_model$pred[ffs_model$pred$C==ffs_model$bestTune$C,
                                      c("pred", "obs")]
    s <- regressionStats(predictions_svm$pred,predictions_svm$obs)
    p <- ggplot(predictions_svm, aes(obs,pred)) + 
      stat_binhex(bins=100)+
      ggtitle(paste(method, "RMSE= ", round(s$RMSE, digits=2), "R²= ", round(s$Rsq, digits=2)))+
      xlim(min(predictions_svm),max(predictions_svm))+ylim(min(predictions_svm),max(predictions_svm))+
      xlab("Measured LST 30m")+
      ylab("Predicted LST 30m")+
      geom_abline(slope=1, intercept=0,lty=2)+
      scale_fill_gradientn(name = "data points", colors=viridis(10))+theme_minimal()
    
    df <- data.frame(ffs_model$selectedvars, c(0,ffs_model$selectedvars_perf), c(0,ffs_model$selectedvars_perf_SE))
    
    df$type <- ffs_model$modelInfo$label
    names(df) <- c("selvars", "selvars_perf", "selvars_perf_SE", "type")
    df
    
    cvp <- plot(ffs_model, main=method)
    par(mar=c(c(5, 4, 6, 2) + 0.1))
    ffsp <- plot_ffs(ffs_model) + ggtitle(method)
    plot_ffs(ffs_model,main = method)
    text(1,1,"selected variables and RMSE",cex=2,font=2)
    
  }
  
  print(i)
  
  return(list(s,df,p, cvp,ffsp))
  
})



stats <- lapply(eval, `[[`, 1)
perf <- lapply(eval, `[[`, 2)
plots <- lapply(eval, `[[`, 3)
cv_plots <- lapply(eval, `[[`, 4)
ffsp <- lapply(eval, `[[`, 5)

library(gridExtra)
library(grid)


# in case one method only
i=1
print(methods[i])
eg <- plots[[1]]
ggsave(paste0(figurepath, "internal_ffs_eval_dummyrun_", methods[i], ".png"), plot = eg)
saveRDS(perf, paste0("add_files/selected_predictors_FFS_dummyrun", methods[i], ".Rds"))
saveRDS(stats, paste0("add_files/stats_FFS_dummyrun", methods[i], ".Rds"))






# plot scatter
eg <- grid.arrange(plots[[1]], plots[[2]], plots[[3]], plots[[4]], nrow = 2)
ggsave(paste0(figurepath, "internal_ffs_eval_dummyrun.png"), plot = eg)


# plot tuning
cv_p <- cv_plots[[1]]
ggsave(paste0(figurepath, "tuning_ffs_eval_dummyrun_gbm.png"), plot = cv_p)


cv_p <- grid.arrange(cv_plots[[1]], cv_plots[[2]], cv_plots[[3]], cv_plots[[4]],nrow = 2)
ggsave(paste0(figurepath, "tuning_ffs_eval_dummyrun.png"), plot = cv_p)


# plot rmse
ffsplots <- grid.arrange(ffsp[[1]], ffsp[[2]], ffsp[[3]], ffsp[[4]],nrow = 2)

ggsave(paste0(figurepath, "rmse_ffs_eval_dummyrun.png"), plot = ffsplots)


saveRDS(perf, "add_files/selected_predictors_FFS_dummyrun.Rds")
saveRDS(stats, "add_files/stats_FFS_dummyrun.Rds")

