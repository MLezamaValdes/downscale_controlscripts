## TO DO:
# * add monthwise processing routine
# * also try band 6
# * unzipping routine
# * routine for using the right hillshading raster

swirmonthpath <- "D:/new_downscaling/SWIR/new/"
swiroutpath <- paste0(cddir, "SWIR/")

#hs <- stack(paste0(cddir, "ia_hs_res/ia_hs_2019-01-26-1940.tif"))

library(XML)
y=1
m=1

stp <- list.files(swirmonthpath, full.names = T, pattern=".tar$")
stpaths <- grep(list.files(stp, full.names = T), pattern='.tar$', invert=TRUE, value=TRUE)

aoianta

# cloud_cirrus <- c(322, 324, 328, 336, 352, 368, 834, 836, 840, 848, 864, 880, 
#                   386, 388, 392, 400, 416, 432, 898, 900, 904, 928, 944, 480, 
#                   992, 322, 324, 328, 336, 352, 368, 386, 388, 392, 400, 416, 
#                   432, 480, 834, 836, 840, 848, 864, 880, 898, 900, 904, 912, 
#                   928, 944, 992, 1346, 1348, 1350, 1352)

cloud_shadow <- c(328, 392, 840, 904, 1350)
cld <- c(352, 368, 416, 432, 480, 864, 880, 928, 944, 992)
mc_cloud <- c(386, 388, 392, 400, 416, 432, 898, 900, 904, 928, 944)
hc_cloud <- c(480, 992)
hc_cirrus <- c(834, 836, 840, 848, 864, 880, 898, 900, 904, 912, 928, 944, 992)
lc_cirrus <- c(322, 324, 328, 336, 352, 368, 386, 388, 392, 400, 416, 432, 480)
lc_cloud <- c(322, 324, 328, 336, 352, 368, 834, 836, 840, 848, 864, 880)

cloud <- c(cloud_shadow,cld,mc_cloud,hc_cloud,hc_cirrus)

b7files <- lapply(seq(stpaths), function(i){
  # get pixel quality assessment and make a cloud mask
  bqa <- list.files(stpaths[i], pattern="pixel_qa", full.names = T)
  c <- raster(bqa)
  cs <- is.element(values(c),cloud)
  c[] <- cs
  
  # get raster tile
  x <- list.files(stpaths[i], pattern="band7", full.names = T)
  s <- stack(x, c)
  # clean out clouds
  s[[1]][s[[2]]==1] <- NA
  s

})


#generate command for merging all the tiles
cm <- lapply(seq(b7files), function(i){
  if(i < seq(b7files)[length(seq(b7files))]){
    print(paste0("b7files[[", i, "]][[1]], "))
  } else {
    print(paste0("b7files[[", i, "]][[1]]"))
  }
})

mrg <- paste(cm[2:length(cm)], sep="", collapse="")
commd <- paste("merge(b7files[[1]][[1]],", mrg,")")

mos <- eval(parse(text=commd))

mc <- crop(mos, aoianta)
mm <- mask(mc, aoianta)

plot(mm)
plot(aoianta,add=T)

# write mosaic original
ym <- substring(time_range[[y]][[m]][[1]][1],1,7)
writeRaster(mm, paste0(swiroutpath, "swir_", ym, ".tif"), overwrite=T)

mm <- raster(paste0(swiroutpath, "swir_", ym, ".tif"))

# Topo correction
library(landsat)
library(lubridate)
library(satellite)

# extract info from swir metadata 
sun <- lapply(seq(stpaths), function(i){
  met <- list.files(stpaths[i], pattern="xml", full.names = T)
  meta <- readMeta(met, raw=T)
  s <- meta$global_metadata$solar_angles
  d <- meta$global_metadata$acquisition_date
  t <- meta$global_metadata$scene_center_time
  list(s,d,t)
})

sundf <- as.data.frame(matrix(unlist(sun), ncol=5, byrow=T))
names(sundf) <- c("zenith", "azimuth", "units", "date", "time")
sundf$azimuth <- as.numeric(as.character(sundf$azimuth))
sundf$sun_elev <- 90-as.numeric(sundf$zenith)

# get info concerning which hs file to choose
dt <- paste0(sundf$date, "_", substring(sundf$time, 1, 8))
sundf$datetime <- as.POSIXlt(dt, format="%Y-%m-%d_%H:%M:%S")
sundf$timepos <- as.POSIXct(substring(sundf$time, 1, 8), format="%H:%M:%S")
h <- hour(mean(sundf$timepos))
m <- minute(mean(sundf$timepos))
hm <- paste0(h, m)
d <- max(day(sundf$datetime)) # get day that is most often in composit
hs_files <- list.files(paste0(cddir, "ia_hs_res/"))
day <- hs_files[which(substring(hs_files, 15,16)==d)]
td <- as.numeric(hm) - as.numeric(substring(hs_files, 18,21))
whichhsfile <- which(td==min(td))

# get hillshading file
hs <- stack(list.files(paste0(cddir, "ia_hs_res/"), full.names = T)[whichhsfile])

# resample mosaic to fit to hs
mmres <- resample(mm, hs[[2]])
# topographic correction
swir_tc <- calcTopoCorr(mmres, hillsh = hs[[2]])

# write tc mosaic
writeRaster(swir_tc, paste0(swiroutpath, "swir_tc_", ym, ".tif"), overwrite=T)
