##############################################################
#################    20    ##################################
#########    take model and MODIS time series   #################
#### to predict high resolution LST ###########################
##############################################################
library(raster)
method <- "rf"
n <- "150000"

# get final model
modelpath <-"C:/Users/mleza/OneDrive/Documents/PhD/work_packages/auto_downscaling_30m/final_150000/"
final_model <- get(load(paste0(modelpath,"final_model_",method,"_", n, ".RData")))

# get & prepare auxiliary stack
aux <- stack("D:/new_downscaling/auxiliary/aux_stack_xy_final.tif")
names(aux) <-  c("dem", "slope", "aspect", "TWI", "soilraster", "landcoverres", 
                 "spatialblocks","x", "y")
predpos <- match(c("dem", "slope","aspect","landcoverres"), names(aux))
aux_predictors <- aux[[predpos]]


# get and preprocess MODIS scene



# 

sp <- predict(rasslice[[k]],final_model)
