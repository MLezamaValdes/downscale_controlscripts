library(getSpatialData)
library(rgdal)

scripts <- "C:/Users/mleza/OneDrive/Documents/PhD/work_packages/auto_downscaling_30m/downscale_controlscripts/data_prep/"
batchrunpath <- "C:/Users/mleza/HEG/HEG_Win/bin/BatchRunning/BatchRunning/"
batchoutdir <- paste0(batchrunpath, "outdir/")
batchindir <- paste0(batchrunpath, "indir/")
time_range <- readRDS(list.files(scripts, pattern="time_range.rds", full.names = T))
modispath <- "C:/Users/mleza/OneDrive/Documents/PhD/work_packages/auto_downscaling_30m/downscaling_MOD_ts/"
# run HEG tool to tranlate swaths to geotiff
tplpath <- list.files(batchoutdir, pattern="unix.prm", full.names = T)

aoipath <- "E:/new_downscaling/aoi/"
aoi <- readOGR(list.files(aoipath, pattern="actually.shp", full.names = T))

y <- 1
m <- 1
d <- 1

login_USGS(username="MaiteLezama", password = "Eos300dmmmmlv")
login_earthdata(username = "Mlezama", password = "Eos300dm")


get_processMODIS_for_ts <- function(time_range, modispath, username, password, aoi){
  
        wgsproj <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

    

        #L8datetime <- as.POSIXlt(downloadedday$datetime, format="%Y:%j:%H:%M:%S")
        
        
        print("STARTING MODIS DOWNLOAD AND PREP")
        
        modisscenepath <- paste0(modispath, substring(time_range[[y]][[m]][[1]][[1]], 1, 7), "/")
        hdfpath <- paste0(modisscenepath, "hdfs/")
        MODtifHDFoutpath <- paste0(modisscenepath, "translated/")
        MODLSTpath <- paste0(modisscenepath, "LST/")
        
        
        ## set archive directory
        set_archive(modisscenepath)
        
        print("basic settings done")
        
        
        ####### GET DATA ONLINE ##############################################################################################
        
        ## set aoi and time range for the query
        set_aoi(aoi)
        aoiwgs <- spTransform(aoi, wgsproj)
        
        product <- c("MODIS_MYD11_L2_V6", "MODIS_MOD11_L2_V6")

        records <- getMODIS_records(time_range = time_range[[y]][[m]][[d]], name = product,
                                   username="MaiteLezama", password = "Eos300dmmmmlv",
                                   aoi=get_aoi())
        
        #previews <- get_previews(records) 
        #view_previews(previews)
        
        #records <- records[records$level == "Level-2A" | records$level == "sr",]
        #records_cc_aoi <- calc_cloudcov(records) 
        #records_ts <- select_timeseries(records)
        
        records_av <- check_availability(records)
        records_av$level <- NA
        records_order <- order_data(records_av)
        records_download <- get_data(records_order)
        
        
        wgsproj <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
        print(paste0("downloaded", records$summary))
   
        
        dir.create(file.path(modisscenepath, "hdfs/"))
        
        
        # put all hdfs into one folder (hdfpath)
        
        downloaded_hdfs <- paste0(modisscenepath, 
                                  list.files(modisscenepath ,pattern = ".hdf$", recursive = TRUE))
        
        downloaded_hdfs <- downloaded_hdfs[!grepl("hdfs", downloaded_hdfs)]
        
        nl <- paste0(hdfpath, basename(downloaded_hdfs))
        
        file.copy(from=downloaded_hdfs, to=nl,
                  overwrite = TRUE)
        
        print("MODIS data downloaded and in place")
        
        if(length(list.files(hdfpath))!=0){
          
          
          ######### BATCH TRANSLATING SWATH TO GEOTIFF WITH HEG TOOL ####################################################
          
          # get template prm file
          tpl <- read.delim(list.files(batchoutdir, pattern="unix.prm", full.names = T),
                            sep="=", col.names = c("nam", "val"), header = F, stringsAsFactors = F)
          
          
          # list all files in hdf dir
          filescomp <- list.files(hdfpath, full.names=T, pattern="11_L2")
          filescomp <- filescomp[grepl("hdf$", filescomp)]
          

          # clean out batchoutdir if something there from previous run
          a <- character(0)
          if(!identical(a, list.files(batchoutdir, pattern=".tif"))){
            file.remove(list.files(batchoutdir, pattern=".tif", full.names = T))
          }
          
          # run HEG tool
          library(R.utils)
          
          source("C:/Users/mleza/OneDrive/Documents/PhD/work_packages/auto_downscaling_30m/downscaleRS/R/runheg.R")
          try(runheg(files=filescomp, indir=batchindir, 
                     outdir=batchoutdir, tplpath=tplpath, layer = "LST|"))
          
          
          # transport results to other filespath
          dir.create(file.path(MODtifHDFoutpath)) # create MODtifHDFoutpath
          MODtiffiles <- list.files(batchoutdir, pattern=".tif$", full.names=T)
          MODtifHEG <- paste0(MODtifHDFoutpath, basename(MODtiffiles))
          file.copy(from=MODtiffiles, to=MODtifHEG,
                    overwrite = TRUE)
          
          print("batch translating hdf to tif done")
          
          ######## GO ON PROCESSING LST IN R ########################################################################################
          
          # get tif files
          lst <- lapply(seq(list.files(MODtifHDFoutpath, pattern=".tif$")), function(i){
            raster(list.files(MODtifHDFoutpath, pattern=".tif$", full.names=T)[i])
          })
          
          # cut to & mask by aoi
          
          
          # convert values to valid range and degree C
          print("converting values to valid range and degree Celsius")
          dir.create(MODLSTpath)
          lst_c <- lapply(seq(lst), function(i){
            lstc <- crop(lst[[i]],aoiwgs)
            lstcm <- mask(lstc, aoiwgs)
            
            # Valid Range = 7500-65535
            lstcm[lstcm == 0 ] <- NA
            lstcm[[i]][lstcm[[i]] < 7500 & lstcm[[1]] > 65535] <- NA
            
            # scale factor = 0.02
            lst_1_conv <- lstcm*0.02
            
            # convert to degree C
            lstc <- lst_1_conv - 273.15
            
            writeRaster(lstc, paste0(MODLSTpath, "cels_", names(lst[[i]]), ".tif"), format="GTiff",
                        overwrite=T)
            
            print(i)
            return(lstc)
          })
          
          # save projected and resampled rasters
          dir.create(file.path(MODLSTpath)) # create MODtifHDFoutpath
          
          
          # project rasters
          print("projecting rasters - will take quite a while")
          lapply(seq(lst_c), function(i){
            print(i)
            x <- projectRaster(lst_c[[i]], crs = antaproj)
            writeRaster(x, paste0(MODLSTpath, "proj_", names(lst[[i]]), ".tif"), format="GTiff",
                        overwrite=T)
            x
          })
          
          
          rm(lst_c)
          gc()
          
          projfiles <- list.files(MODLSTpath, pattern="proj", full.names = T)
          projfiles <- projfiles[  sapply(seq(projfiles), function(i){
            !grepl("small", projfiles[i])
          })]
          
          lst_cp <- lapply(seq(projfiles), function(i){
            raster(projfiles[i])
          })
          tmplras <- template
          
          if(length(lst_cp)>1){
            # get extent of files
            MODext <- lapply(seq(lst_cp), function(i){
              p <- as(extent(aoianta), 'SpatialPolygons')
              crs(p) <- antaproj
              p
            })
            me <- do.call(bind, MODext)
            
            # make a template to force 1x1km pixels
            extent(tmplras) <- extent(aoianta)
          }
          
          res(tmplras) <- c(1000, 1000)
          tmplras[] <- 1
          
          
          # resample all rasters to 1km x 1km resolution
          lst_res <- lapply(seq(lst_cp), function(i){
            print(i)
            x <- resample(lst_cp[[i]], tmplras)
            writeRaster(x, paste0(MODLSTpath, names(lst[[i]]), ".tif"), format="GTiff",
                        overwrite=T)
            x
          })
          
          print("rasters resampled to 1x1km resolution")
          
          print("values, resolution and projection adapted")
          
          ############# PATCH IMAGES ####################################################################################
          
          if(length(lst_res) > 1){
            # write mosaic command
            mrg <- character()
            for(i in seq(lst_res)){
              mrg[i] <- paste0("lst_res[[", i, "]]")
            }
            #mrg[length(mrg)] <- paste0("lst_res[[", length(mrg), "]]")
            mrg <- paste(mrg, sep="", collapse=",")
            cm <- paste("raster::mosaic(", mrg, ", tolerance=0.9,
                    fun=mean, overwrite=T, overlap=T, ext=NULL)")
            
            mosaic <- eval(parse(text=cm))
            mosaic[mosaic < -90] <- NA # correct for too low values
            
            writeRaster(mosaic, paste0(modisscenepath, areaname, "_MODIS_LST_Mosaic.tif"), format="GTiff",
                        overwrite=T, bylayer=T)
            #mosaic <- raster(paste0(modisscenepath, areaname, "_MODIS_LST_Mosaic.tif"))
            
            # visualize mosaic
            #mapview(mosaic, col.regions = viridis(500), legend = TRUE)
            
            print("MODIS images patched")
          }
          
          
          ############ MAKE A RASTER WHICH GIVES INFO ON INPUT RASTER ##########################################################
          
          
          # find max bounding box
          newextent <- extent(tmplras)
          
          # bring them all to new extent
          lst_ex <- lapply(seq(lst_res), function(i){
            x <- crop(lst_res[[i]], newextent)
            names(x) <- names(lst_res[[i]])
            x
          })
          
          lst_ex <- stack(lst_ex)
          
          # put in correct names if any got missing during processing
          testnam <- which(grepl("layer",names(lst_ex)))
          for(i in seq(testnam)){
            names(lst_ex[[testnam[i]]]) <- names(lst[[testnam[i]]])
          }
          
          
          # crop everything to aoi - perhaps here?
          lst_s <- crop(lst_ex, aoianta)
          
          # write
          for(i in seq(nlayers(lst_s))){
            print(i)
            writeRaster(lst_s[[i]], paste0(MODLSTpath, "small_", names(lst_res[[i]]), ".tif"), format="GTiff",
                        overwrite=T)
          }
          
          print("indidvidual MODIS images stacked and cut to aoi")
          
          ################ MAKE DATE RASTERS #################################
          
          #lst_s <- stack(list.files(MODLSTpath, pattern="small", full.names=T))
          
          fnams <- sapply(seq(lst), function(i){
            names(lst[[i]])
          })
          
          utcdates <- lapply(seq(fnams), function(i) {
            fnam <- fnams[i]
            # get UTC date from fnam
            su <- strsplit(fnam, "A")
            su <- su[[1]][length(su[[1]])]
            org <- paste0(substring(su, 1,4), "-01-01")
            utcday <- as.Date((as.numeric(substring(su, 5,7))-1), origin=org)
            
            if(grepl("v", su)){
              utctime <- NULL
              utcdate <- utcday
            } else {
              utctime <- paste0(substring(su, 9, 10), ":", substring(su, 11, 12))
              utcdate <- strptime(paste0(utcday,utctime), format='%Y-%m-%d %H:%M', tz="UTC")
            }
            print(i)
            return(utcdate)
          })
          
          # test
          v <- NULL
          for(i in seq(utcdates)){
            v[i] <- as.character(utcdates[[i]])
          }
          
          namdate <- data.frame(fnams=fnams, utc = v)
          
          
          # MODdate contains dayofyear, minutesofday as data and as rasters as well
          MODdate <- datestodoymod(utcdates, fnams, lst_s)
          
          print("MODIS time done")
          
          ############# GOODNESS OF FIT OF ACQUISITION TIME (L8 / MODIS) ##############################
          
          # timeex <- data.frame(extract(tstack, extent(tstack)))
          
          
          L8time <- read.csv(paste0(L8datpath, "L8_date_", areaname, time_range[[y]][[m]][[1]][1], ".csv"))
          L8date <- lapply(seq(nrow(L8time)), function(i){
            strptime(paste(L8time$date[i], L8time$time[i]), format='%Y-%m-%d %H:%M:%S', tz="UTC")
          })
          
          # convert L8 time to minute of day
          fnamsL8 <- list.files(paste0(L8scenepath, "bt/"), pattern="BTC")
          
          
          #L8dates <- datestodoymod(L8date, fnamsL8, lst_s)
          doy <- sapply(seq(L8date), function(i){
            strftime(L8date[[i]], format = "%j")
          })
          
          minutes <- sapply(seq(L8date), function(i){
            minute(L8date[[i]])
          })
          
          hours <- sapply(seq(L8date), function(i){
            hour(L8date[[i]])
          })
          
          
          # make L8 date df
          minutesofday <- (hours*60)+minutes
          L8datedf <- L8time
          
          #L8datedf$fnam <- fnamsL8
          L8datedf$doy <- doy
          L8datedf$min <- minutes
          L8datedf$hrs <- hours
          L8datedf$minday <- minutesofday
          
          # make MODIS date df
          moddate <- character()
          for(i in seq(utcdates)){
            moddate[i] <- as.character( utcdates[[i]][1])
          }
          
          modL8datedf <- data.frame(doy=MODdate$dayofyear, minday=MODdate$minutesofday, date=moddate, fnam=fnams)
          
          dir.create(paste0(main, "timediff/", substring(time_range[[y]][[m]][[1]][[1]], 1, 7), "/"))
          
          write.csv2(L8datedf, paste0(main, "timediff/", substring(time_range[[y]][[m]][[1]][[1]], 1, 7), "/", "dates_L8.csv"))
          write.csv2(modL8datedf, paste0(main, "timediff/", substring(time_range[[y]][[m]][[1]][[1]], 1, 7), "/", "dates_MODIS.csv"))
          
          
          
          print("timedifference to L8 written")
          
          
        }
        
      }
      
    } else {print("no temporally matching scenes")
      
      #file.rename(L8scenepath, paste0(substring(L8scenepath, 1, (nchar(L8scenepath)-nchar(basename(L8scenepath))-1)),
      #                                paste0("no_tmatch_",basename(L8scenepath))))
    } # for if there are MODIS scenes within <2h of L8
  }
  
  gc()
  
} # function