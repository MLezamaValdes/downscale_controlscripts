
install.packages("/home/l/llezamav/R/slurmR_0.4-1.tar.gz",
                 repos = NULL, lib="/home/l/llezamav/R/")

library(CAST,lib.loc="/home/l/llezamav/RCast/")

library(raster)
library(caret,lib.loc="/home/l/llezamav/R/")
library(parallel)
library(doParallel)
library(slurmR, lib.loc="/home/l/llezamav/R/")
sessionInfo()

