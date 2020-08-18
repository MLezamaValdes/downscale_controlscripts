library(raster)
library(mapview)
library(rgdal)

aoipath <-  "D:/new_downscaling/aoi/MDV/"
l8p <- "D:/new_downscaling/data_download_preprocessing/L8/2019-01/"
mp <- "D:/new_downscaling/data_download_preprocessing/MODIS/2019-01/"
L8scenepath <- paste0(main, "L8/", substring(time_range[[y]][[m]][[1]][[1]], 1, 7), "/")

# get aoi
aoip <- list.files(aoipath, pattern="adp.shp", full.names = T)
aoi <- readOGR(aoip)
antaproj <- crs("+proj=stere +lat_0=-90 +lat_ts=-71 +lon_0=0 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m
+no_defs +ellps=WGS84 +towgs84=0,0,0")
aoianta <- spTransform(aoi, antaproj)
aoiutm <- spTransform(aoi, crs("+proj=utm +zone=57 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs "))


# get data together

# L8 cropped to AOI LST with same extent
l8r <- list.files(paste0(l8p, "LST/"), full.names=T)
L8ras <- stack(l8r)

mr <- list.files(paste0(mp, "LST/"), pattern="small", full.names=T)
mr <- mr[grep('tif$', mr)]
mras <- stack(mr)


# MATCHFILE DOESN'T WORK!!!

# # separate matchfile$modnam
# matchfile <- read.csv2(paste0(L8scenepath, "downloaded_days_MODmatch.csv"))
# 
# split1 <- strsplit(as.character(matchfile$modnam), "AND")
# L8match <- lapply(seq(split1), function(i){
#   strsplit(split1[[i]], ";")
# })
# 
# whichmod <- lapply(seq(nrow(matchfile)), function(i){
#   grepl("11_L2",unlist(L8match[[i]]))
# })
# 
# # what goes wrong here????
# matchstack <- lapply(seq(nrow(matchfile)), function(i){ # for each row, i.e. each Landsat scene
#   mav <- unlist(L8match[[i]])[whichmod[[i]]] # which modis files do we have here
#   lapply(seq(mav), function(j){ # for all available modis scenes
#     if(any(grepl(mav[j],names(mras)))){
#           data.frame(L8=matchfile$fnam[grep(mav[j], names(mras))],
#                 MOD=mav[j], mras=names(mras)[grep(mav[j], names(mras))],
#                 mraspos = grep(mav[j], names(mras)))
#     }
#   
#   })
# })
# 
# L8stackpos <- lapply(seq(nrow(matchfile)), function(i){
#   # where is the matchfile L8 file in L8ras stack?
#   grep(substring(matchfile$fnam, 22,36)[i],substring(names(L8ras), 11,25))
# })
# 
# # automatize next step!
# 
# #9,1 MODIS raster 1
# L8stackpos[[9]]
# #L8stackpos: 10,7; 13,3; 17,13; 
# 
# # find L8 equivalent
# matchfile$fnam[17]
# names(L8ras)[13]
# 
# # take MODIS stack position from matchfile
# matchfile[13,]


names(mras[[1]]) #15:15
names(L8ras)[2] #13:49

plot(L8ras[[2]])
plot(mras[[1]], add=T)

modl8res <- resample(mras[[1]], L8ras[[2]])
modl8resval <- modl8res
modl8resval[modl8resval < (-100)] <- NA
satstack <- stack(L8ras[[2]], modl8resval)

# stack MODIS and L8, DEM, TWI, (aspect), hillshade, spatial blocks for CV
dempath <- "D:/new_downscaling/tiles_westcoast/"
dem <- raster(paste0(dempath, "DEM_8m_", areaname,"_clean_filled_15.tif"))

# resample to landsat resolution from 8m 




blockmask <- raster(paste0(dempath, "blockmask.tif"))
slope <- raster(paste0(dempath, "slopeMDV.tif"))
hillsh <- raster(paste0(dempath, "hillshading_8m.tif"))
hillshade <- crop(hillsh, dem)
twipath <- "D:/Antarctica/runoff_paths/"
twi <- raster(paste0(twipath, "TWI_Rslope.tif"))
twires <- resample(twi, dem)
stack(dem, slope, twi, hillshade)


plot(dem)
plot(slope, add=T)
plot(twi, add=T)
plot(hillshade, add=T)


s <- stack(dem, slope, twires, hillshade, blockmask)
plot(s)

# resample s to fit with satstack
sres <- resample(s, satstack[[1]])

stack.complete <- stack(satstack,sres)
writeRaster(stack.complete, paste0(main, "Jan_01_stack.tif"))

test <- stack(paste0(main, "Jan_01_stack.tif"))
names(stack.complete) <- c("L8", "MOD", "DEM", "slope","TWI","hillsh","blockmask")

# extract
exdat <- extract(stack.complete, aoianta)

# exdat <- exdat[[1]]
# # write raster acquisition time into data
# exdat$date <- rep(20190101, nrow(exdat))
# exdat$modtime <- rep(1515, nrow(exdat))
# exdat$l8time <- rep(1349, nrow(exdat))
# 
# # save exdat
# saveRDS(exdat, paste0(main, "exdat_jan_01_19_t.rds"))
# 
# # check exdat
# ed <- exdat[complete.cases(exdat),]
# et <- head(exdat)
# 
# et[complete.cases(et)]