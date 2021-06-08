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

swiroutpath <-"D:/new_downscaling/SWIR/composites/"
orgpath <- "D:/new_downscaling/SWIR/downloaded_scenes/2019-02/org/"

y=7
m=1

all <- read.csv2("D:/new_downscaling/all_comb_scenes.csv")

tiles <- list.files(orgpath, full.names = T)
allscenes <- list.files(tiles, pattern="pixel")




# # for the first round only (extraction etc.)
# dir.create(swiroutpath)
# 
# # extract zip files
# sc <- list.files(swir_downloadpath, pattern="tar.gz$", full.names = T)
# dir.create(orgpath)
# 
# already_extracted <- list.files(orgpath)
# 
# sc_nam <- list.files(swir_downloadpath, pattern="tar.gz$", full.names = F)
# sc_nam <- substring(sc_nam, 1,39)
# 
# # for(i in seq(sc)){
# #   if(!any(grepl(sc_nam[i], already_extracted))){
# #       untar(sc[i],  compressed = 'gzip', 
# #         exdir=paste0(orgpath, tools::file_path_sans_ext(tools::file_path_sans_ext(basename(sc[i])))))
# #   }
# # }


missing_swir_scene <- NA
make_SWIR_composites <- function(y, m){
  
  ym <- substring(time_range[[y]][[m]][[1]][[1]], 1, 7)
  monthpat <- str_remove(ym, "-")
  
  
  np <- paste0(swiroutpath, ym, "/")
  dir.create(np)
  
  swirdf <- data.frame(tiles, allscenes, allscenessubst)
  monthdf <- swirdf[grepl(monthpat, swirdf$allscenessubst),]
  
  if(nrow(monthdf)>1){
  
      b6 <- list.files(monthdf$tiles, pattern="band6", full.names = T)
      b7 <- list.files(monthdf$tiles, pattern="band7", full.names = T)
      bqa <- list.files(monthdf$tiles, pattern="pixel_qa.tif", full.names = T)
      
      b6folder <- sapply(strsplit(dirname(b6), "/org/"), "[", 2 )
      b7folder <- sapply(strsplit(dirname(b7), "/org/"), "[", 2 )
      bqafolder <- sapply(strsplit(dirname(bqa), "/org/"), "[", 2 )
      
      
      filedf <- data.frame(bqafolder)
      
      b7short <- substring(basename(b7), 11, 25)
      b6short <- substring(basename(b6), 11, 25)
      bqashort <- substring(basename(bqa), 11, 25)
      
      fpos <- match(bqashort, b7short)
      filedf$b7 <- b7[fpos]
      filedf$b6 <- b6[fpos]
      
      qpos <- match(bqashort, bqashort)
      filedf$bqa <- bqa[qpos]
    
      matchtest <- sapply(seq(nrow(filedf)), function(i){
        m1 <- all(grepl(filedf$bqafolder[i], filedf$b6[i]))
        m2 <- all(grepl(substring(basename(filedf$bqa[i]), 1, 25), filedf$b6[i]))
        m1&m2
      })
      
      # filedf <- filedf[c(5,6),]
      
      if(any(matchtest)==FALSE){stop("files not matching")}
      
      ############################ CLEAN OUT CLOUDS ##################################################################
      
      print("starting to clean up clouds")
      
      swirbands <- lapply(seq(nrow(filedf)), function(i){
          
        if(!any(is.na(filedf[i,]))){
          print(paste0(i, "/", nrow(filedf)))
          
          b <- tryCatch({stack(filedf$b6[i], filedf$b7[i])})
          bqaband <- try(raster(filedf$bqa[i]), silent=T)
          
          if(class(bqaband)!="try-error" & class(b)!="try-error"){
            print("fileset available")
            c <- bqaband
            cs <- is.element(values(c),cloud)
            c[] <- cs
            
            s <- stack(b,c)
            b[c==1] <- NA
            
            b <- try(crop(b, aoianta))
            
            if(class(b)!="try-error"){

              writeRaster(b, paste0(np, substring(basename(b6[i]), 1,nchar(basename(b6[i]))-10), "_c_rm.tif"),
                          overwrite=T)
              b
            }
          }
        } 
      })
      
      # saveRDS(swirbands, paste0(swiroutpath, "swirbands_", ym, ".tif"))
      
      # # here a check for actual cloudfree status? 
      # swirbands <- swirbands[1:3]
      
      ############################ MERGE ALL TILES ##################################################################
      
      
      # which are viable rasters
      swirbands_noterror <- sapply(seq(swirbands), function(i){
        class(swirbands[[i]])!="try-error"
      })

      swirbands <- swirbands[swirbands_noterror]
      
      # which are viable rasters
      swirbands_notNA <- sapply(seq(swirbands), function(i){
        print(i)
        if(!is.null(swirbands[[i]])){
                  if(is.na(maxValue(swirbands[[i]]) | maxValue(swirbands[[i]]))){
                  FALSE
                  } else {
                  TRUE
                }
        } else {
          FALSE
        }
      })
      
      swirbands <- swirbands[swirbands_notNA]
      
      if(length(swirbands) > 1){
        
        print("starting to merge")
        
        #generate command for merging all the tiles
        cm <- lapply(seq(swirbands), function(i){
          if(i < seq(swirbands)[length(seq(swirbands))]){
            print(paste0("swirbands[[", i, "]][[1]], "))
          } else {
            print(paste0("swirbands[[", i, "]][[1]]"))
          }
        })
        
        mrg <- paste(cm[2:length(cm)], sep="", collapse="")
        commd <- paste("raster::merge(swirbands[[1]][[1]],", mrg,")")
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
        
      } else {
        mos6 <- swirbands[[1]][[1]]
        mos7 <- swirbands[[1]][[2]]
      }
      
      
      ############################ MASK AOI ##################################################################
      print("starting to mask AOI")
      
      mm6 <- mask(mos6, aoianta)
      mm7 <- mask(mos7, aoianta)
      
      if(!is.na(maxValue(mm7)&minValue(mm7)) &  !is.na(maxValue(mm6)&minValue(mm6))){
      
      # plot(mm6)
      # plot(mm7)
      # plot(aoianta,add=T)
      
      # write mosaic original
      ym <- substring(time_range[[y]][[m]][[1]][1],1,7)
      writeRaster(mm6, paste0(swiroutpath, "swir_6_", ym, ".tif"), overwrite=T)
      writeRaster(mm7, paste0(swiroutpath, "swir_7_", ym, ".tif"), overwrite=T)
      
      #mm <- raster(paste0(swiroutpath, "swir_", ym, ".tif"))
      
      
      ############################ TOPOGRAPHIC CORRECTION ##################################################################
      print("starting topographic correction")
      
      # extract info from swir metadata 
      sun <- lapply(seq(monthdf$tiles), function(i){
        print(i)
        tryCatch(met <- list.files(monthdf$tiles[i], pattern="xml", full.names = T))
        if(length(met)>0){
          meta <- readMeta(met, raw=T)
          s <- meta$global_metadata$solar_angles
          d <- meta$global_metadata$acquisition_date
          t <- meta$global_metadata$scene_center_time
          list(s,d,t)
          }
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
      mins <- minute(mean(sundf$timepos))
      hm <- paste0(h, mins)
      d <- max(day(sundf$datetime)) # get day that is most often in composit
      
      hs_files <- list.files(paste0(cddir, "ia_hs_res/"), pattern=ym)
      if(length(hs_files)<1){print("no ia_hs_files available for that month")
      } else {
        day <- hs_files[which(substring(hs_files, 19,20)==as.character(d))]
        if(length(day)<1){
          as.numeric(substring(hs_files, 19,20))
          diff <- abs(as.numeric(substring(hs_files, 19,20))-d)
          md <- min(diff)
          pos <- which(diff==md)[1]
          substring(hs_files, 19,20)[pos]
        }
        td <- as.numeric(hm) - as.numeric(substring(hs_files, 22,25))
        whichhsfile <- which(td==min(td))
        whichhsfile <- whichhsfile[1]
        
        # get hillshading file
        hs <- stack(list.files(paste0(cddir, "ia_hs_res/"), full.names = T)[whichhsfile])
        
        print("resampling topographically corrected SWIR mosaic")
        
        # resample mosaic to fit to hs
        mm <- stack(mm6, mm7)
        mmres <- resample(mm, hs[[2]])
        
        # topographic correction
        swir_tc67 <- calcTopoCorr(mmres, hillsh = hs[[2]])
        
        # write tc mosaic
        print("writing topographically corrected SWIR mosaic")
        writeRaster(swir_tc67, paste0(swiroutpath, "swir_tc_67", ym, ".tif"), overwrite=T)
        missing_swir_scene <- NA
          
        }
 
      }
  
  } else {
    print("no SWIR scene here")
    missing_swir_scene <- c(missing_swir_scene, ym)
  }
}


####################### CHECKING FOR STUFF THAT MIGHT HAVE GONE WRONG ##################################



allscenessubst <- substring(allscenes, 18, 23)
#allscenessubst[order(as.numeric(gsub("[^20-26]+", "", allscenessubst)))]
sas <- rev(allscenessubst[order(as.numeric(substring(allscenessubst, 1,26)))])
table(sas)

#swirresult <- lapply(seq(year), function(y){
lapply(c(7), function(y){
  #lapply(seq(month), function(m){
  lapply(seq(month), function(m){
    res <- NA
    ym <- substring(time_range[[y]][[m]][[1]][[1]], 1, 7)
    monthpat <- str_remove(ym, "-")
    print(ym)
    res <- c(res, ym)
    if(nrow(all[grepl(ym,substring(all$L8date, 1,7)),]) > 0){
      print("there should be a SWIR scene")
      nec <- 1
      res <- c(res, "there should be a SWIR scene")
    } else {
      nec <- 0
      print("no SWIR scene necessary here, no sat either")
      res <- c(res, "no SWIR scene necessary here, no sat either")}
    if(nec==1){
      rasterOptions(tmpdir="D:/new_downscaling/run/swir/")
      make_SWIR_composites(y,m)
      file.remove(list.files("D:/new_downscaling/run/swir/", full.names = T))
    }
    nec <- NA
    return(res) 
    })
 })

saveRDS(swirresult, paste0(swiroutpath, "missing_scenes.rds"))
