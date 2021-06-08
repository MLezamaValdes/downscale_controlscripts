# cloudclean relevant Landsat scenes 
y=1
m=1

L8scenepath <- paste0(main, "L8/", substring(time_range[[y]][[m]][[1]][[1]], 1, 7), "/")
btscenepath <- paste0("D:/new_downscaling/data_download_preprocessing/L8/", substring(time_range[[y]][[m]][[1]][[1]], 1, 7), 
                      "/get_data/LANDSAT/BT/")
cloud_shadow <- c(328, 392, 840, 904, 1350)
cld <- c(352, 368, 416, 432, 480, 864, 880, 928, 944, 992)
mc_cloud <- c(386, 388, 392, 400, 416, 432, 898, 900, 904, 928, 944)
hc_cloud <- c(480, 992)
hc_cirrus <- c(834, 836, 840, 848, 864, 880, 898, 900, 904, 912, 928, 944, 992)
lc_cirrus <- c(322, 324, 328, 336, 352, 368, 386, 388, 392, 400, 416, 432, 480)
lc_cloud <- c(322, 324, 328, 336, 352, 368, 834, 836, 840, 848, 864, 880)

cloud <- c(cloud_shadow,cld,mc_cloud,hc_cloud,hc_cirrus)


# get Landsat names in dynamic satellite stacks
n <- read.csv(paste0(cddir, "names_sat_ia_hs_", substring(time_range[[y]][[m]][[1]][[1]], 1, 7),".csv"))

# get satellite stacks to fill in cleaned Landsat scenes
lst <- stack(paste0(cddir, "L_MOD_hs_ia", substring(time_range[[y]][[m]][[1]][[1]], 1, 7), ".tif"))
names(lst) <- n$x

# get landsat names in sat ia hs stack (object lst)
li <- seq(1, length(n$x), by=4)
lnam_stack <- n$x[li] 

# find unique landsat scenes
l_unique <- unique(substring(lnam_stack, 1,40))

# which directories are represented in stack
all_lst <- list.files(paste0(L8scenepath, "LST/"), full.names=F, pattern="band10.tif$")
m_ras_to_files <- match(substring(all_lst, 1, 40),l_unique) # which position in stack are the tif files in? 

all_lst <- list.files(paste0(L8scenepath, "LST/"), full.names=T, pattern="band10.tif$")

# get respective cloud rasters
# take those dirs that match a scene in the satelite stack
btscenepaths <- list.files(btscenepath, full.names=T)[match(substring(list.files(btscenepath),1,40), l_unique)]
btscenepaths_dir <- list.files(btscenepaths, pattern="Bt$", full.names=T)

pqa_files <- list.files(btscenepaths_dir, pattern = "pixel_qa", full.names = T)

# make it so that we clean up all files that are supposed to go into the stack
# file name should be something like 4, before we run make_L8_MOD_stack() in fun_hillsh_incidence
qa <- lapply(seq(pqa_files), function(i){
  x <- raster(pqa_files[i])
  
  # cloud mask
  c <- x
  cs <- is.element(values(c),cloud)
  c[] <- cs
  
  stack_i <- which(grepl(substring(names(c),1,40), substring(names(lst), 1,40)))
  # just to check
  try(if(substring(names(c),1,40) != substring(names(lst[[stack_i[1]]]), 1,40)) stop("L8 and L8 cloud removed not matching"))
  
  cres <- resample(c, template)
  s <- stack(lst[[stack_i[1]]], cres)
  
  # clean out clouds
  s[[1]][s[[2]]==1] <- NA
  
  writeRaster(s[[1]],paste0(L8scenepath, "LST/", substring(names(lst[[stack_i[1]]]), 1,40), "_cloud_rm.tif"),overwrite=T)
  
  # if(length(stack_i)>1){
  #   
  #   for(j in length(stack_i)){
  #     lst[[(stack_i[j])]] <- s[[1]]
  #   }
  # 
  # } else {
  #   lst[[stack_i]] <- s[[1]]
  # }
  # 
  # print(i)
  # s[[1]]
  
})



writeRaster(lst, paste0(cddir, "new_L_MOD_hs_ia", substring(time_range[[y]][[m]][[1]][[1]], 1, 7), ".tif"),
            overwrite=T)


# par(mfrow=c(3,4))
# plot(lst[[li]])

# par(mfrow=c(2,5))
# for(i in seq(qa)){
#   plot(qa[[i]])
# }

# get the cloud cleaned Landsat rasters
rmf <- list.files(paste0(L8scenepath, "LST/"), pattern="rm.tif")
rmf_comp <- list.files(paste0(L8scenepath, "LST/"), pattern="rm.tif", full.names=T)

match(substring(rmf, 1,40), substring(lnam_stack, 1,40))



# put cloud cleaned files into stacks
ind <- seq(1, length(names(lst)), by=4)
rmfs <- match(substring(names(lst[[ind]]), 1,40), substring(rmf, 1,40))

rmcloud <- lapply(seq(rmfs), function(i){
  x <- raster(rmf_comp[rmfs[i]], full.names = T)
  try(if(substring(names(x), 1,40) != substring(names(tempdyn[[(ind[i])]]),1,40)) stop("L8 and L8 cloud removed not matching"))
  lst[[(ind[i])]] <- x
})


