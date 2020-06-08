
################################### ADD SCENES TOGETHER FOR EXTRACTION #######################
library(lubridate)
library(suncalc)
library(oce)

y=1
m=3
time_range[[y]][[m]]

# deprecated! hillshade and lc missing
aux <- stack(paste0("D:/new_downscaling/clean_data/aux_stack_8m.tif"))
names(aux) <- c("dem", "slope", "twires", "hillshade", "lc", "blockmask")

# slope <- terrain(aux[[1]],opt="slope", unit="degrees")
# aspect <- terrain(aux[[1]],opt="aspect", unit="degrees")
# 
# writeRaster(slope, paste0(dempath, "slope", areaname,".tif"), overwrite=T)
# writeRaster(aspect, paste0(dempath, "aspect", areaname,".tif"), overwrite=T)

sl <- raster(paste0(dempath, "slope", areaname,".tif"))
as <- raster(paste0(dempath, "aspect", areaname,".tif"))

aoilonlat <- spTransform(aoianta, CRS="+proj=longlat +ellps=WGS84")
lon <- mean(extent(aoilonlat)[c(1,2)])
lat <- mean(extent(aoilonlat)[c(3,4)])


iapath <- paste0(cddir, "ia/")
hspath <- paste0(cddir, "hs/")



########################### FUNCTIONS #####################################
make_L8_MOD_stack <- function(y, m, timethres){
  
  L8scenepath <- paste0(main, "L8/", substring(time_range[[y]][[m]][[1]][[1]], 1, 7), "/")
  modisscenepath <- paste0(modispath, substring(time_range[[y]][[m]][[1]][[1]], 1, 7), "/")
  L8LSTpath <- paste0(L8scenepath, "LST/")
  MODLSTpath <- paste0(modisscenepath, "LST/")
  
  if(length(list.files(L8scenepath, pattern="msel.csv", full.names=T))!=0){
    #try(timediff_msel <- read.csv2(list.files(L8scenepath, pattern="msel.csv", full.names=T)))
    try(timediff_comp <- read.csv2(list.files(L8scenepath, pattern="timediff_comp.csv", full.names=T)))
    
    print("read timediff csvs")
    
    if(exists("timediff_comp")){
      
      # make file lists of all downloaded LST scenes for L and M
      l8r <- list.files(L8LSTpath, full.names=T)
      mr <- list.files(MODLSTpath, pattern="small", full.names=T)
      
      if(length(mr)!=0){
        mr <- mr[grep('tif$', mr)]
        
        
        ########## get stacks of all downloaded LST data in ############
        
        L8ras <- stack(l8r)
        mras <- stack(mr)
        
        names(mras) <- substring(basename(mr), 12, 51)
        names(L8ras) <- substring(basename(l8r), 1,40)
        
        print("read stacks")
        
        
        ########## get timediff csvs ############
        tdpath <- paste0(main, "timediff/", substring(time_range[[y]][[m]][[1]][[1]], 1, 7), "/")
        #tdf <- list.files(tdpath, full.names = T)
        #l8dates <- read.csv2(tdf[1])
        #mdates <- read.csv2(tdf[2])
        timediff <- timediff_comp
        
        ########## take only closely matching ############
        tdthres <- timediff[timediff$timediff < timethres,]
        
        ########## which in the raster stacks are the closely matching scenes? ############
        
        # which of timediff_comp$MODname is actually in names(mras)? only those scenes available, mark in timediff_df
        MODavailable <- sapply(seq(nrow(tdthres)), function(i){
          any(grepl(substring(as.character(tdthres$MODname[i]), 1, 40), names(mras)))
        })
        L8available <- sapply(seq(nrow(tdthres)), function(i){
          any(grepl(substring(tdthres$L8name[i], 1, 40), names(L8ras)))
        })
        
        tdthres$Mcomp <- MODavailable
        tdthres$Lcomp <- L8available
        
        
        tdthres[tdthres$Mcomp==FALSE,]
        
        tdthres <- tdthres[tdthres$Lcomp == T & tdthres$Mcomp ==T,]
        
        modnames <- substring(as.character(tdthres$MODname), 1, nchar(as.character(tdthres$MODname))-4)
        l8names <- as.character(tdthres$L8name)
        
        write.csv2(tdthres, paste0(L8scenepath, "timediff_comp_comp.csv"))
        
        # mlayer and llayer stem from tdthres
        mlayer <- sapply(seq(modnames), function(i){
          which(grepl(modnames[i], names(mras)))
        })
        mlayer <- unlist(mlayer)
        
        llayer <- sapply(seq(l8names), function(i){
          which(grepl(l8names[i], names(L8ras)))
        })
        llayer <- unlist(llayer)
        
        # stack
        
        mstack <- mras[[mlayer]]
        lstack <- L8ras[[llayer]]
        
        print("selected matching scenes")
        
        ########## resample MODIS to L8 resolution ############
        print("starting resampling of MODIS")
        
        mstackres <- resample(mstack, template)
        
        print("starting resampling of MODIS")
        
        lstackres <- resample(lstack, template)
        
        #LSTstack <- stack(mstackres, lstack)
        
        #mstackres <- mstackres[[c(1,2,3,4,5,6,7,8,11)]]
        #lstackres <- lstackres[[c(5,4,3,2,9,8,6,7,1)]]
        
        ########## put L8 and corresponding MODIS together ############
        
        
        # TO DO: PUT THEM TOGETHER RIGHT AWAY
        
        s <- lapply(seq(nlayers(lstackres)), function(i){
          stack(lstackres[[i]], mstackres[[i]])
        })
        
        satstack <- stack(s)
        
        print("matched scenes")
        
        write.csv2(names(satstack), paste0(cddir, "satnames_", substring(time_range[[y]][[m]][[1]][[1]], 1, 7), ".csv"),
                   row.names = F)
        
        print("starting to write sat stack")
        writeRaster(satstack, paste0(cddir, "L_MOD_", substring(time_range[[y]][[m]][[1]][[1]], 1, 7), ".tif"), overwrite=T)
      } else {
        print("No MODIS scenes available")
      }

      
    } else {
      print("No scenes for this time step")
    }
    
  } else {
    print("no scenes available for stacking")
  }
  
}

########## make hillshading and incidence angle raster ############
make_hs_ia <- function(y,m){
  
  L8scenepath <- paste0(main, "L8/", substring(time_range[[y]][[m]][[1]][[1]], 1, 7), "/")
  timediff_comp <- read.csv2(paste0(L8scenepath, "timediff_comp_comp.csv"))
  
  # for d in seq(nrow(timediff))
  uniqueMODscenes <- unique(timediff_comp$MODname)
  
  MD <- substring(uniqueMODscenes,11,22)
  MODDate <- as.POSIXct(MD, tz="UTC",format = "%Y%j.%H%M")
  
  #lapply(c(1:8), function(d){
  lapply(seq(uniqueMODscenes), function(d){
    # get time from MODIS (all times are UTC https://modis-images.gsfc.nasa.gov/products_filename.html)
    
    MD <- substring(uniqueMODscenes[d],11,22)
    MODDate <- as.POSIXct(MD, tz="UTC",format = "%Y%j.%H%M")
    
    doy <- substring(MD, 5,7)
    h <- hour(MODDate)
    min <- minute(MODDate)
    
    doymin <- as.numeric(doy)+((h+min/60)/24)
    
    #time should be in UTC
    sa <- sunAngle(MODDate, lon=lon, lat=lat)
    
    hs <- hillShade(slope = sl, aspect = as, angle = sa$altitude, direction = sa$azimuth)
    dateaschar <- paste0(substring(as.character(MODDate),1,10), "-", substring(as.character(MODDate),12,13),substring(as.character(MODDate),15,16))
    
    writeRaster(hs, paste0(hspath, "hs_", dateaschar, ".tif"), overwrite=T)
    
    # check whether daylight saving applies
    
    MODDateNZ <- with_tz(MODDate, tzone = "Pacific/Auckland")
    if(dst(MODDateNZ)==TRUE){
      dstnz <- 60
    } else {
      dstnz <- 0
    }
    
    ia <- Incidence(DOY = doymin, Lat=lat, Lon=lon, SLon=180, DS=dstnz, Slope = sl, Aspect = as)
    writeRaster(ia, paste0(iapath, "ia_", dateaschar, ".tif"), overwrite=T)
    
    gc()
  })
} # ACHTUNG D NOCH NICHT RICHTIG ITERIERT

########## put allstacks + hs + ia together ############
match_sat_ia_hs <- function(y,m){
  L8scenepath <- paste0(main, "L8/", substring(time_range[[y]][[m]][[1]][[1]], 1, 7), "/")
  timediff_comp <- read.csv2(paste0(L8scenepath, "timediff_comp_comp.csv"))
  
  # get satellite stack for y and m and rename correctly
  satstack <- stack(paste0(cddir, "L_MOD_", substring(time_range[[y]][[m]][[1]][[1]], 1, 7), ".tif"))
  namdat <- read.csv2(paste0(cddir, "satnames_", substring(time_range[[y]][[m]][[1]][[1]], 1, 7), ".csv"))
  names(satstack) <- namdat$x
  
  # get matching table 
  try(timediff_comp_comp <- read.csv2(paste0(L8scenepath, "timediff_comp_comp.csv")))
  uniqueMODscenes <- unique(timediff_comp$MODname)
  
  # get naming of ia and hs
  dateaschar <- lapply(seq(uniqueMODscenes), function(i){
    MD <- substring(uniqueMODscenes[i],11,22)
    MODDate <- as.POSIXct(MD, tz="UTC",format = "%Y%j.%H%M")
    paste0(substring(as.character(MODDate),1,10), "-", substring(as.character(MODDate),12,13),substring(as.character(MODDate),15,16))
  })
  
  # read ia
  ia <- lapply(seq(uniqueMODscenes), function(i){
    try(raster(paste0(iapath, "ia_", dateaschar[[i]], ".tif")))
  })

  
  ras_available <- sapply(seq(ia), function(i){
    class(ia[[i]])!="try-error"
  })
  
  ia <- stack(ia)
  
  # read hs
  hs <- lapply(seq(uniqueMODscenes), function(i){
    raster(paste0(hspath, "hs_", dateaschar[[i]], ".tif"))
  })
  hs <- stack(hs)
  
  # resample stacks
  iares <- resample(ia, satstack[[1]])
  hsres <- resample(hs, satstack[[1]])
  
  # test MODIS names coinciding
  namsat <- names(satstack)[c(2,4,6,8,10,12,14,16,18,20,22)]
  namia <- sapply(seq(ia), function(i){
    names(ia[[i]])
  })
  namhs <- sapply(seq(hs), function(i){
    names(hs[[i]])
  })
  
  terraindatnams <- data.frame(namhs, namia)
  terraindatnams$uniqueMODscenes <- uniqueMODscenes
  
  # match satstack names with terraindatnams
  
  nassub <- substring(namsat,22,33)
  
  iahspos <- lapply(seq(namsat), function(i){
      which(grepl(nassub[i], terraindatnams$uniqueMODscenes)==T)
  })
  
  satind <- seq(1, nlayers(satstack), 2)
  
  allstacks <- lapply(seq(length(satind)), function(i){
    stack(satstack[[c(satind[i], satind[i]+1)]], iares[[iahspos[[i]]]], hsres[[iahspos[[i]]]])
  })
  allstacks <- stack(allstacks)
  
  write.csv2(names(allstacks), paste0(cddir, "names_sat_ia_hs_", substring(time_range[[y]][[m]][[1]][[1]], 1, 7), ".csv"),
             row.names = F)
  
  print("starting to write complete stack")
  writeRaster(allstacks, paste0(cddir, "L_MOD_hs_ia", substring(time_range[[y]][[m]][[1]][[1]], 1, 7), ".tif"),
              overwrite=T)
  

}

########################### RUN #####################################

  y=2
  m=5
  time_range[[y]][[m]]
  
  
  for(y in c(2:7)){
    for(m in c(5:8)){
            make_L8_MOD_stack(y,m,timethres)
      gc()
    }
  }
  

  
#make_hs_ia(y,m)
  #gc()
  
  # match_sat_ia_hs(y,m)
  # gc()


############################## CHECK ###################################

satstack <- stack(paste0(cddir, "L_MOD_2019-3.tif"))


sldeg <- terrain(dem, opt="slope", unit="degrees")
asdeg <- terrain(dem, opt="aspect", unit="degrees")


iatest <- raster(paste0(iapath, "ia_", dateaschar, "test.tif"))
hstest <- raster(paste0(hspath, "hs_", dateaschar, "test.tif"))


par(mfrow=c(2,2))
plot(as, main="aspect",col=grey(1:100/100))
plot(sl, main="slope",col=grey(1:100/100))
plot(iatest, main="incidence angle",col=grey(1:100/100))
plot(hstest, main="hillshading",col=grey(1:100/100))

nv <- normalvector(sldeg[], asdeg[])
jd = JD(ISOdate(2019,3,4,13)) # Julian Day from POSIXct, one for each hour

deg <- sapply(seq(nrow(nv)), function(i){
  degrees(acos(sunvector(jd,lat,lon,11) %*% as.vector(nv[i,])))
})

iatestnew <- sldeg
iatestnew[] <- deg[]
plot(iatestnew)

par(mfrow=c(2,3))
plot(dem, main="dem", col=grey(1:100/100))
plot(as, main="aspect",col=grey(1:100/100))
plot(sl, main="slope",col=grey(1:100/100))
plot(iatest, main="incidence angle",col=grey(1:100/100))
plot(iatestnew, main="incidence angle new", col=grey(1:100/100))
plot(hstest, main="hillshading",col=grey(1:100/100))
