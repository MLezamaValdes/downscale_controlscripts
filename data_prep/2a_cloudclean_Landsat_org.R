# cloudclean relevant Landsat scenes 
y=3
m=1

cloud_shadow <- c(328, 392, 840, 904, 1350)
cld <- c(352, 368, 416, 432, 480, 864, 880, 928, 944, 992)
mc_cloud <- c(386, 388, 392, 400, 416, 432, 898, 900, 904, 928, 944)
hc_cloud <- c(480, 992)
hc_cirrus <- c(834, 836, 840, 848, 864, 880, 898, 900, 904, 912, 928, 944, 992)
lc_cirrus <- c(322, 324, 328, 336, 352, 368, 386, 388, 392, 400, 416, 432, 480)
lc_cloud <- c(322, 324, 328, 336, 352, 368, 834, 836, 840, 848, 864, 880)

cloud <- c(cloud_shadow,cld,mc_cloud,hc_cloud,hc_cirrus)

L_cloudclean <- function(y, m){
  
  L8scenepath <- paste0(main, "L8/", substring(time_range[[y]][[m]][[1]][[1]], 1, 7), "/")
  btscenepath <- paste0("D:/new_downscaling/data_download_preprocessing/L8/", substring(time_range[[y]][[m]][[1]][[1]], 1, 7), 
                        "/get_data/LANDSAT/BT/")
  
  # get the Landsat scenes
  L8LSTpath <- paste0(L8scenepath, "LST/")
  l8r <- list.files(L8LSTpath, full.names=T,  pattern='bt_band10')
  
  # get respective cloud rasters
  # take those dirs that match a scene in the satelite stack
  btscenepaths <- list.files(btscenepath, full.names=T)[match(substring(list.files(btscenepath),1,40), substring( basename(l8r), 1,40))]
  btscenepaths_dir <- list.files(btscenepaths, pattern="Bt$", full.names=T)
  
  # get the pixel quality for those scenes 
  pqa_files <- list.files(btscenepaths_dir, pattern = "pixel_qa", full.names = T)
  
  qa <- lapply(seq(pqa_files), function(i){ # for each tile in this month
    x <- raster(pqa_files[i])
    lst <- raster(l8r[i])
    
    # check whether rasters coincide
    try(substring(names(x), 1, 34) != substring(names(lst), 1,34), stop("rasters don't belong together"))
    
    # cloud mask
    c <- x
    cs <- is.element(values(c),cloud)
    c[] <- cs
    
    
    cres <- resample(c, template)
    lres <- resample(lst, template)
    
    s <- stack(lres, cres)
    
    # clean out clouds
    s[[1]][s[[2]]==1] <- NA
    
    writeRaster(s[[1]],paste0(L8LSTpath, substring(names(lst), 1,40), "_cloud_rm.tif"),overwrite=T)
    
    print(paste0(i, " / ", length(pqa_files)))
    
  })
  
}


# do complete 1 as well :) 

for(y in c(2:length(year))){
  for(m in seq(month)){
    print(c(y,m))
    L_cloudclean(y,m)
    gc()
  }
}


