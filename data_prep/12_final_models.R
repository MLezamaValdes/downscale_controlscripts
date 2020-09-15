# 12_final models

modelpath <- "D:/new_downscaling/modelling/"
methods <- c("rf", "lm",
             "gbm", # Stochastic Gradient Boosting,  not working for now
             "pls", # Partial Least Squares
             "nnet",
             "cubist", 
             "svmLinear" # sampes >> features -> AI@WWU: use linear kernel
)
  
load(paste0(modelpath, "ffs_model_",method,"_", n, ".RData"))

predictions_rf <- ffs_model$pred[ffs_model$pred$mtry==ffs_model$bestTune$mtry,c("pred","obs")]
regressionStats(predictions_rf$pred,predictions_rf$obs)

ggplot(predictions_rf, aes(obs,pred)) + 
  stat_binhex(bins=100)+
  xlim(min(predictions_rf),max(predictions_rf))+ylim(min(predictions_rf),max(predictions_rf))+
  xlab("Measured Tair (°C)")+
  ylab("Predicted Tair (°C)")+
  geom_abline(slope=1, intercept=0,lty=2)+
  scale_fill_gradientn(name = "data points", colors=viridis(10))
