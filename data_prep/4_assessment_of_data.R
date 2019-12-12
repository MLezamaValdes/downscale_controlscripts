##### assessment routine for downloaded datasets ############
library(raster)

# check if coverage is good 
# exclude cloudy areas
# assign some kind of measurement (how secure MODIS data is e.g.)



# setup

## time range 
year <- c(2019:2013)
month <- c("01","02","03","04", "09", "10","11", "12")
day <- c(17:22)
time_range <- lapply(seq(year), function(j){
  lapply(seq(month), function(i){
    y <- paste(paste0(year[j],"-",month[i],"-",day), 
               paste0(year[j],"-",month[i],"-",day))
    strsplit(y, " ")
  })
})


main <- "E:/downscaling/data_download_preprocessing/"
lpath <- paste0(main, "L8/")
mpath <- paste0(main, "MODIS/")
datemain <- "E:/downscaling/data_download_preprocessing/timediff/"
nam <- c("L8", "MOD")


y=2
m=2

for(y in seq(year)){
  for(m in seq(month)){
    # get all rasters for a certain month in
    lsdirs <- paste0(lpath, list.files(lpath, pattern=as.character(year[y])), "/")[1:8] # has to be changed if months change
    msdirs <- paste0(mpath, list.files(mpath, pattern=as.character(year[y])), "/")[1:8]
    
    # read LST rasters for Landsat and MODIS
    lf <- list.files(paste0(lsdirs[m],"bt/"), pattern="BTC", full.names = T)
    lr <- lapply(seq(lf), function(i){
      raster(lf[i])
    })
    
    mf <- list.files(paste0(msdirs[m],"LST/"), pattern="small", full.names = T)
    mr <- lapply(seq(mf), function(i){
      raster(mf[i])
    })
    
    # compare times they are accessible for
    datefiles <- list.files(paste0(datemain, substring(time_range[[y]][[m]][[1]][[1]], 1, 7), "/"), full.names = T)
    for(i in seq(datefiles)){
      assign( paste0("dat", nam[i]), read.csv2(datefiles[i]))
    }
    
    # check if order of rasters is the same as in date file
    if(all(sapply(seq(datMOD$fnam), function(i){
          datMOD$fnam[i]==substring(basename(names(mr[[i]])), 12, nchar(basename(names(mr[[i]]))))
    }))==T){
      
      # find those MODIS files, that are closest to the available L8 ones
      modmatchl8 <- sapply(seq(datL8$doy), function(i){
              doymatch <- which(datMOD$doy==datL8$doy[i])
              for(i in seq(doymatch)){
                datMOD$minday
                datL8$minday
              }
      })
      
      # make only as much scenes, as there are matches in the day of year
      
      length(modmatchl8[[2]])

      # are those found to be closest together in time also overlapping?
      
      
    } else {
      print("something wrong in order of files")
    }
  
  }
}



# cut cloud covered areas for L8


# extract rasters 
