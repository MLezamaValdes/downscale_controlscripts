# 14_external_evaluation_prediction.R
modelpath <-"C:/Users/mleza/OneDrive/Documents/PhD/work_packages/auto_downscaling_30m/final_150000/"

methods <- c("rf",
             "nnet")

for (i in 1:length(methods)){
  
  method <- methods[i]
  print(method)
  
  n <- 150000
  
  final_model <- get(load(paste0(modelpath,"final_model_",method,"_", n, ".RData")))
  print(paste0("model name = ", "final_model_",method,"_", n, ".RData"))
  
  
  
}