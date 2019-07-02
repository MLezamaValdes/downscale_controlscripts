# 2a get DEM
library(raster)
library(mapview)


# get DEM

dempath <- "D:/DEM_8m/tiles_westcoast/"

zipf <- list.files(dempath, full.names = T, pattern=".tar.gz")
for(i in seq(zipf)){
  untar(zipf[i],  compressed = 'gzip', exdir=paste0(dempath, tools::file_path_sans_ext(tools::file_path_sans_ext(basename(zipf[i])))))
}

# get all the dem tiles
# sdirs <- list.files(dempath, pattern="m$", full.names = T)
# dems <- list.files(sdirs, pattern="dem.tif$", full.names = T)
# 
demtilepath <- paste0(dempath, "all/")
# file.copy(dems, paste0(demtilepath, basename(dems)))

dt <- list.files(demtilepath, full.names=T)
dem_tiles <- lapply(seq(dt), function(i){
  r <- raster(dt[i])
  r[r<(-300)] <- NA
  r
})

dem_tiles

#generate command for merging all the tiles
cm <- lapply(seq(dem_tiles), function(i){
    if(i < seq(dem_tiles)[length(seq(dem_tiles))]){
    print(paste0("dem_tiles[[", i, "]], "))
  } else {
    print(paste0("dem_tiles[[", i, "]]"))
  }
})

mrg <- paste(cm[2:length(cm)], sep="", collapse="")
commd <- paste("merge(dem_tiles[[1]],", mrg, ", ext=NULL)")

mos <- merge(dem_tiles[[1]], dem_tiles[[2]], dem_tiles[[3]], dem_tiles[[4]], dem_tiles[[5]], dem_tiles[[6]], dem_tiles[[7]], dem_tiles[[8]], dem_tiles[[9]], dem_tiles[[10]], dem_tiles[[11]], dem_tiles[[12]], dem_tiles[[13]], dem_tiles[[14]], dem_tiles[[15]], dem_tiles[[16]], dem_tiles[[17]], dem_tiles[[18]] , ext=NULL)
writeRaster(mos, paste0(dempath, "dem_mosaic_8m_westcoast_corrected.tif"), format="GTiff")


# make a mosaic
mos <- raster(paste0(dempath, "dem_mosaic_8m_westcoast.tif"))
# filter out senseless values

# filter mosaic
mos_3 <- focal(mos, w=matrix(1/9,nrow=3,ncol=3))
writeRaster(mos_3, paste0(dempath, "dem_mosaic_8m_westcoast_filter_3x3_corr.tif"), format="GTiff", overwrite=T)

# correct for negative NA values
mos_c <- raster(paste0(dempath, "dem_mosaic_8m_westcoast_filter_3x3_corr.tif"))
mos_c[mos_c < (-300)] <- NA
writeRaster(mos_c, paste0(dempath, "dem_mosaic_8m_westcoast_filter_3x3_corr2.tif"), format="GTiff", overwrite=T)


# run again!
# resample to 30m
mos_3_30 <- aggregate(mos_3, fact=(30/8))
writeRaster(mos_3_30, paste0(dempath, "dem_mosaic_30m_westcoast_filter_3x3_corr.tif"), format="GTiff", overwrite=T)


# takes too much time and doesn't work sometimes
# # make a template polygon, where DEM is not NA
# tpl_hs <- mos
# tpl_hs <- setValues(tpl_hs, ifelse(!is.na(tpl_hs[]), 1, NA))
# 
# 
# gdal_polygonizeR <- function(x, outshape=NULL, gdalformat = 'ESRI Shapefile', pypath=NULL, readpoly=TRUE, quiet=TRUE){
#   if (isTRUE(readpoly)) require(rgdal)
#   if (is.null(pypath)) {
#     pypath <- Sys.which('gdal_polygonize.py')}
#   if (!file.exists(pypath)) stop("Can't find gdal_polygonize.py on your system.")
#   owd <- getwd()
#   on.exit(setwd(owd))
#   setwd(dirname(pypath))
#   if (!is.null(outshape)) {
#     outshape <- sub('\\.shp$', '', outshape)
#     f.exists <- file.exists(paste(outshape, c('shp', 'shx', 'dbf'), sep='.'))
#     if (any(f.exists))
#       stop(sprintf('File already exists: %s',
#                    toString(paste(outshape, c('shp', 'shx', 'dbf'),
#                                   sep='.')[f.exists])), call.=FALSE)
#   } else outshape <- tempfile()
#   if (is(x, 'Raster')) {
#     require(raster)
#     writeRaster(x, {f <- tempfile(fileext='.tif')})
#     rastpath <- normalizePath(f)
#   } else if (is.character(x)) {
#     rastpath <- normalizePath(x)
#   } else stop('x must be a file path (character string), or a Raster object.')
#   system2('python', args=(sprintf('"%1$s" "%2$s" -f "%3$s" "%4$s.shp"',
#                                   pypath, rastpath, gdalformat, outshape)))
#   if (isTRUE(readpoly)) {
#     shp <- readOGR(dirname(outshape), layer = basename(outshape), verbose=!quiet)
#     return(shp)
#   }
#   return(NULL)
# }
# 
# tpl <- gdal_polygonizeR(tpl_hs, pypath="C:/OSGeo4W64/bin/gdal_polygonize.py")