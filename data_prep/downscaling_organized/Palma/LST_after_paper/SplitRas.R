# split raster
SplitRas <- function(raster,ppside,save,plot,outdir,outnam){
  for(i in seq(raster)){
    
  }
  h        <- ceiling(ncol(raster)/ppside)
  v        <- ceiling(nrow(raster)/ppside)
  print(paste0("starting to aggregate "))
  agg      <- aggregate(raster,fact=c(h,v))
  agg[]    <- 1:ncell(agg)
  print(paste0("making Polygons "))
  agg_poly <- rasterToPolygons(agg)
  names(agg_poly) <- "polis"
  r_list <- list()
  for(i in 1:ncell(agg)){
    print(paste0("cropping ", i, "/ ", ncell(agg)))
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

