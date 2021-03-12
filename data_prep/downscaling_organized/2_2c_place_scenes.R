# put already downloaded scenes in order

# get all LST band 10 files that are in the list of scenes in ri and place them in their corresponding folder on D

library(stringr)
tdcomp <- read.csv2(paste0(tdpath, "all_timediffs.csv"))
monthl <- str_split(tdcomp$monthyear, "_")

tdyearmonth <- sapply(seq(monthl), function(i){
  if(nchar(monthl[[i]][1])<2){
   x <- paste0(0, monthl[[i]][1])
  } else {
   x <- monthl[[i]][1]
  }
  paste0(monthl[[i]][2],  "-", x )
})

months <- unique(tdyearmonth)
  
tdcomp$tdyearmonth <-tdyearmonth

# list files on E
dirs <- grep(".csv$", list.files("E:/new_downscaling/data_download_preprocessing/L8/", full.names = T), 
             value = TRUE, invert = TRUE)
lstdirs <- paste0(dirs, "/LST/")[dir.exists(paste0(dirs, "/LST/"))]

paths_to_copy <- lapply(seq(months), function(i){
  mon <- months[i]
  L8scenes_month <- tdcomp$L8scene[tdcomp$tdyearmonth==mon]
  
  bands10paths <- list.files(lstdirs[grepl(mon, lstdirs)], pattern="band10.tif$", full.names = T)
  
  pos <- unlist(sapply(seq(L8scenes_month), function(j){
    which(grepl(L8scenes_month[j],basename(bands10paths)))
  }))
  bands10paths[pos]
})

# actualize tdcomp: which of the newly queried scenes are found in the old, processed scenes? 
tdcomp_rows_found_in_old_scenes <- unlist(sapply(seq(unlist(paths_to_copy)), function(i){
  which(grepl(substring(basename(unlist(paths_to_copy)), 1,40)[i], tdcomp$L8scene))
}))
tdcomp$found <- NA
tdcomp$found[tdcomp_rows_found_in_old_scenes] <- TRUE
write.csv2(tdcomp, paste0(tdpath, "all_timediffs.csv"))


# make paths to copy dirs & copy 97 scenes there
ptc <- unique(paste0(L8datpath, substring(unlist(paths_to_copy),51, 62)))
for(i in seq(ptc)){
  dir.create(ptc[i], recursive = T)
}
to <- paste0(L8datpath, substring(unlist(paths_to_copy),51, 62), basename(unlist(paths_to_copy)))
file.copy(unlist(paths_to_copy), to)

