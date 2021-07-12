# 13a_cut_predstack_for_AOA


# get stacks
predstackdir <- "D:/downscaling_after_talk/spatial_predictions_rf/predstacks/"
outdir <- "D:/downscaling_after_talk/spatial_predictions_rf/predstacks/"

ps <- stack(list.files(predstackdir, pattern=".tif", full.names=T))
nps <- read.csv2(list.files(predstackdir, pattern=".csv", full.names=T))
names(ps) <- nps$x

# ncell(ps[[1]])/5000000 # using less than 5 Mio, i.e. making 8 pieces would be good, 
# # making 9 
# ncell(ps[[1]])/9
# #--> 4.3 M samples per piece

outnam <- basename(list.files(predstackdir, pattern=".tif", full.names=T))

# split raster
SplitRas <- function(raster,ppside,save,plot,outdir,outnam){
  h        <- ceiling(ncol(raster)/ppside)
  v        <- ceiling(nrow(raster)/ppside)
  print(paste0("starting to aggregate ", i))
  agg      <- aggregate(raster,fact=c(h,v))
  agg[]    <- 1:ncell(agg)
  print(paste0("making Polygons ", i))
  agg_poly <- rasterToPolygons(agg)
  names(agg_poly) <- "polis"
  r_list <- list()
  for(i in 1:ncell(agg)){
    e1          <- extent(agg_poly[agg_poly$polis==i,])
    r_list[[i]] <- crop(raster,e1)
  }
  print(paste0("starting to save ", i))
  if(save==T){
    for(i in 1:length(r_list)){
      writeRaster(r_list[[i]],filename=paste0(outdir, outnam, "_split_",i, ".tif"),
                  format="GTiff",overwrite=TRUE)  
    }
  }
  if(plot==T){
    par(mfrow=c(ppside,ppside))
    for(i in 1:length(r_list)){
      plot(r_list[[i]],axes=F,legend=F,bty="n",box=FALSE)  
    }
  }
  
  return(paste0(outdir, outnam, "_split_",i, ".tif"))
}


splitfiles <- SplitRas(ps, ppside=3, save=TRUE, plot=FALSE,
         outdir=outdir, outnam=outnam)

