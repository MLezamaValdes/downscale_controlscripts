


path1k <- "/scratch/tmp/llezamav/satstacks_1k/"
path <- "/scratch/tmp/llezamav/satstacks/"

library(raster)
library(parallel)

# get original resolution MODIS raster template
template <- raster(paste0(path1k, "template_1000.tif"))

# get L_MOD_hs_ia final stack
fp <- list.files(path, pattern="L_MOD_hs_ia", full.names=T)
alldatfiles <- list.files(path, pattern=substring(basename(fp), 13,19), full.names=T)
fpnam <- alldatfiles[grepl(alldatfiles,pattern="csv")]


no_cores <- detectCores()-2
print(paste0("number of cores=", no_cores))
cl <- makeCluster(no_cores)

jnk = clusterEvalQ(cl, {library(raster)})

clusterExport(cl, list("fp", "fpnam", "template", "path1k", "path"))

parLapply(cl, seq(fp), function(i){
  
  rs <- stack(fp[i])
  n <- read.csv2(fpnam[i])
  names(rs) <- n$x
  
  # resample whole stack to 1k
  rs_1k <- resample(rs,template)
  writeRaster(rs_1k, paste0(path1k, "L_MOD_hs_ia_", substring(basename(fp), 13,19), ".tif"))
  
})







