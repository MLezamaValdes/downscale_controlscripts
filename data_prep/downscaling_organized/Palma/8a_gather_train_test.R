
# install.packages("/home/l/llezamav/R/clhs_0.8.1.tar.gz", repos = NULL,
#                  lib="/home/l/llezamav/R/")
# 
# install.packages("/home/l/llezamav/R/dplyr_1.0.5.tar.gz", repos = NULL,
#                  lib="/home/l/llezamav/R/")
library(clhs, lib.loc="/home/l/llezamav/R/")
library(dplyr, lib.loc="/home/l/llezamav/R/")

DIdir <- "/scratch/tmp/llezamav/train_test_DI_3m/"
outdir <- "/scratch/tmp/llezamav/train_valid/"
# DIdir <- paste0(cddir, "train_test_DI/")
# outdir <- paste0(cddir, "train_valid/")

n = 150000
valmonths <- c("2014-11", "2015-01", "2017-02", "2018-12", "2019-03")

# list training files 
f <- list.files(DIdir, pattern="train_DI_", full.names = T)
f <- f[grepl(".csv", f)]

# list validation files 
fv <- list.files(DIdir, pattern="test_all_", full.names = T)
fv <- fv[grepl(".csv", fv)]


myLHSfun <- function(ds, desiredsize, outnam){
  vars <- ds %>% select(Landsat, Modis, ia, hs, dem, slope, aspect, TWI, soilraster, 
                        landcoverres)
  subs_vars <- clhs(vars, size=desiredsize,progress=T,simple=TRUE) 
  ds_lhs <- ds[subs_vars,]
  
  # testlines <- all(rownames(ds_lhs) %in% subs_vars)
  # 
  # if (testlines == FALSE){ 
  #   stop("something went wrong here")
  # }
  
  write.csv2(ds_lhs, paste0(outdir, outnam, "_LHS_", format(n, scientific = F), ".csv"))
}



################################ TRAINING ##########################################
################################ TRAINING ##########################################
################################ TRAINING ##########################################

#####################################################################
# reading training datasets while excluding validation months
#####################################################################

# can any of the valmonths patterns be found in f[i]?
(isvalmonth <- sapply(seq(f), function(i){
  any(sapply(seq(valmonths), function(j){
    grepl(valmonths[j],basename(f[i]))
  }))
}))

# f_trainmonth <- f[!isvalmonth]
# 
# print(f_trainmonth)
# 
# alltrain <- lapply(seq(f_trainmonth), function(i){
# 
#      read.csv2(f_trainmonth[i])
# })
# 
# length(alltrain)
# for(i in seq(length(alltrain))){
#   print(head(alltrain[[i]]))
# }
# 
# 
# print("starting rbind of train")
# alltrainall <- do.call("rbind", alltrain)
# 
# print("alltrainall looking like this: ")
# print(head(alltrainall))
# 
# write.csv2(alltrainall, paste0(outdir, "train_DI_full_pre_LHS.csv"),dec=".",sep=";")
# 
# alltrainall <- read.csv2( paste0(outdir, "train_DI_full_pre_LHS.csv"),dec=".",sep=";")
# print(paste0("nrow (alltrainall) = ", nrow(alltrainall)))
# 
# if(nrow(alltrainall)<n){
#   n <- nrow(alltrainall)
# }
# # LHS
# print("starting LHSfun of train")
# myLHSfun(alltrainall, desiredsize = n, outnam="train")
# print(paste0("LHS train done"))

################################ VALIDATION ##########################################
################################ VALIDATION ##########################################
################################ VALIDATION ##########################################


############################### VALIDATION 1 ##########################################


#############################################################
# reading all month's test datasets
#############################################################
# alltest <- lapply(seq(fv), function(i){
#     read.csv2(fv[i])
# })
# print("starting rbind of validation 1")
# alltestall <- do.call("rbind", alltest)
# 
# write.csv2(alltestall, paste0(DIdir, "val_1_full_pre_LHS.csv"))

alltestall <- read.csv2(paste0(DIdir, "val_1_full_pre_LHS.csv"))

rnds <- sample(rownames(alltestall), 5000000)
alltestall_samp <- alltestall[rnds,]

rm(alltestall)

# LHS
print("starting LHSfun of validation 1")
myLHSfun(alltestall_samp, desiredsize = n, outnam="validation_1")
print("LHS validation 1 done")



# # ################################ VALIDATION 2 ##########################################
# 
# #############################################################
# # reading training datasets only for validation months
# #############################################################
# 
# f_valmonth <- f[isvalmonth]
# 
# print(f_valmonth)
# 
# val2 <- lapply(seq(f_valmonth), function(i){
# 
#     read.csv2(f_valmonth[i])
# 
# })
# 
# print("starting rbind of validation 2")
# val2all <- do.call("rbind", val2)
# 
# print("this is val2:")
# head(val2all)
# 
# write.csv2(val2all, paste0(outdir, "val_2_full_pre_LHS.csv"))
# 
# # LHS
# print("starting LHSfun of validation 2")
# myLHSfun(val2all, desiredsize = n, outnam="validation_2")
# print("LHS validation 2 done")
# 
# rm(val2)
# rm(val2all)
# gc()
# ################################ VALIDATION 3 ##########################################
# 
# #############################################################
# # reading test datasets only for validation months 
# #############################################################
# 
# # can any of the valmonths patterns be found in fv[i]? 
# (isvalmonth_validation <- sapply(seq(fv), function(i){
#   any(sapply(seq(valmonths), function(j){
#     grepl(valmonths[j],basename(fv[i]))
#   }))
# }))
# 
# 
# fv_valmonth <- fv[isvalmonth_validation]
# 
# print(fv_valmonth) 
# 
# 
# val3 <- lapply(seq(fv_valmonth), function(i){
# 
# 
#     read.csv2(fv_valmonth[i])
#  
# })
# 
# print("starting rbind of validation 3")
# val3all <- do.call("rbind", val3)
# 
# write.csv2(val3all, paste0(outdir, "val_3_full_pre_LHS.csv"))
# 
# # LHS 
# print("starting LHSfun of validation 3")
# myLHSfun(val3all, desiredsize = n, outnam="validation_3")
# print("LHS validation 3 done")
# 
# 
# 
