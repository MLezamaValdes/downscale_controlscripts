## TO DO:
# * add monthwise processing routine
# * also try band 6
# * unzipping routine
# * routine for using the right hillshading raster

library(XML)
library(landsat)
library(lubridate)
library(satellite)
library(stringr)

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



swir_downloadpath <- "D:/new_downscaling/SWIR/downloaded_scenes/"

y=1
m=2

ym <- substring(time_range[[y]][[m]][[1]][[1]], 1, 7)
swiroutpath <- paste0(swir_downloadpath, ym, "/")
dir.create(swiroutpath)

# extract zip files
sc <- list.files(swir_downloadpath, pattern="tar.gz$", full.names = T)
orgpath <- paste0(swiroutpath, "org/")
dir.create(orgpath)

already_extracted <- list.files(orgpath)

sc_nam <- list.files(swir_downloadpath, pattern="tar.gz$", full.names = F)
sc_nam <- substring(sc_nam, 1,39)

# for(i in seq(sc)){
#   if(!any(grepl(sc_nam[i], already_extracted))){
#       untar(sc[i],  compressed = 'gzip', 
#         exdir=paste0(orgpath, tools::file_path_sans_ext(tools::file_path_sans_ext(basename(sc[i])))))
#   }
# }
monthpat <- str_remove(ym, "-")

tiles <- list.files(orgpath, full.names = T)
allscenes <- list.files(tiles, pattern="pixel")

swirdf <- data.frame(tiles, allscenes)
monthdf <- swirdf[grepl(monthpat, swirdf$allscenes),]

b6 <- list.files(monthdf$tiles, pattern="band6", full.names = T)
b7 <- list.files(monthdf$tiles, pattern="band7", full.names = T)
bqa <- list.files(monthdf$tiles, pattern="pixel_qa.tif", full.names = T)

############################ CLEAN OUT CLOUDS ##################################################################

swirbands <- lapply(seq(b6), function(i){
  b <- stack(b6[i], b7[i])
  q <- raster(bqa[i])
  
  c <- q
  cs <- is.element(values(c),cloud)
  c[] <- cs
  
  s <- stack(b,c)
  b[c==1] <- NA 
  
  #writeRaster(b, paste0(swiroutpath, substring(basename(b6[i]), 1,nchar(basename(b6[i]))-10), ".tif"))
  b
})

# # here a check for actual cloudfree status? 
# swirbands <- swirbands[1:3]

############################ MERGE ALL TILES ##################################################################

#generate command for merging all the tiles
cm <- lapply(seq(swirbands), function(i){
  if(i < seq(swirbands)[length(seq(swirbands))]){
    print(paste0("swirbands[[", i, "]][[1]], "))
  } else {
    print(paste0("swirbands[[", i, "]][[1]]"))
  }
})

mrg <- paste(cm[2:length(cm)], sep="", collapse="")
commd <- paste("merge(swirbands[[1]][[1]],", mrg,")")
mos6 <- eval(parse(text=commd))


cm <- lapply(seq(swirbands), function(i){
  if(i < seq(swirbands)[length(seq(swirbands))]){
    print(paste0("swirbands[[", i, "]][[2]], "))
  } else {
    print(paste0("swirbands[[", i, "]][[2]]"))
  }
})

mrg <- paste(cm[2:length(cm)], sep="", collapse="")
commd <- paste("merge(swirbands[[1]][[2]],", mrg,")")
mos7 <- eval(parse(text=commd))

############################ MASK AOI ##################################################################
mc6 <- crop(mos6, aoianta)
mc7 <- crop(mos7, aoianta)

mm6 <- mask(mc6, aoianta)
mm7 <- mask(mc7, aoianta)

# plot(mm6)
# plot(mm7)
# plot(aoianta,add=T)

# write mosaic original
ym <- substring(time_range[[y]][[m]][[1]][1],1,7)
writeRaster(mm6, paste0(swiroutpath, "swir_6_", ym, ".tif"), overwrite=T)
writeRaster(mm7, paste0(swiroutpath, "swir_7_", ym, ".tif"), overwrite=T)

#mm <- raster(paste0(swiroutpath, "swir_", ym, ".tif"))


############################ TOPOGRAPHIC CORRECTION ##################################################################

# extract info from swir metadata 
sun <- lapply(seq(monthdf$tiles), function(i){
  met <- list.files(monthdf$tiles[i], pattern="xml", full.names = T)
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

hs_files <- list.files(paste0(cddir, "ia_hs_res/"), pattern=ym)
day <- hs_files[which(substring(hs_files, 15,16)==d)]
td <- as.numeric(hm) - as.numeric(substring(hs_files, 18,21))
whichhsfile <- which(td==min(td))

# get hillshading file
hs <- stack(list.files(paste0(cddir, "ia_hs_res/"), full.names = T)[whichhsfile])

# resample mosaic to fit to hs
mm <- stack(mm6, mm7)
mmres <- resample(mm, hs[[2]])

# topographic correction
swir_tc67 <- calcTopoCorr(mmres, hillsh = hs[[2]])

# write tc mosaic
writeRaster(swir_tc67, paste0(swiroutpath, "swir_tc_67", ym, ".tif"), overwrite=T)
