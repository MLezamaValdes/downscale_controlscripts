# get SWIR data

swirpath <- paste0(main, "L8/SWIR/")
print(swirpath)

y=1
m=2

login_USGS("MaiteLezama", "Eos300dmmmmlv")
## set aoi and time range for the query
l8proj <- crs("+proj=utm +zone=57 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs ")
aoiutm <- spTransform(aoi, l8proj)


####### GET IDs for ordering DATA ONLINE ##############################################################################################

getSWIRpi <- function(y,m, swirpath){

  set_aoi(aoiutm)
  set_archive(swirpath)
  product <- "LANDSAT_8_C1"
  
  ## query records for AOI, time range and product
  nodat <- list(0)
  day <- seq(length(time_range[[y]][[m]]))  
  query <- lapply(seq(day), function(d){
    try(getLandsat_records(time_range = time_range[[y]][[m]][[d]], name = product,
                           aoi=get_aoi()), silent=T)
  })
  
  
  # get <2 cloud cover
  promising_query <- lapply(seq(query), function(i){
    if(length(query[[i]])>0){
      print(i)
      qsr <- query[[i]][query[[i]]$level=="sr",]
      whichgood <- qsr$cloudcov_land < 2 & qsr$cloudcov < 5
      lcq <- qsr[whichgood,]
      if(nrow(lcq)<1){
        lcq <- NULL
      }
      lcq
    }
  })

  id <- lapply(seq(promising_query), function(i){
    if(!is.null(promising_query[[i]])){
      lapply(nrow(promising_query[[i]]), function(j){
          promising_query[[i]][j,]$record_id
      })
    } else {NULL}
  })
  
  
  sum_pq <- lapply(seq(promising_query), function(i){
    if(!is.null(promising_query[[i]])){
      lapply(nrow(promising_query[[i]]), function(j){
        promising_query[[i]][j,]$summary
      })
    } else {NULL}
  })
  
  
  ri <- unlist(id)
  sum_pq <- unlist(sum_pq)
  write.table(ri, paste0(swirpath, "swir_ri_", substring(time_range[[y]][[m]][[1]][[1]], 1, 7), ".txt"), 
              quote=F,row.names = F, col.names = F)
  
  write.table(sum_pq, paste0(swirpath, "swir_sum_pq_", substring(time_range[[y]][[m]][[1]][[1]], 1, 7), ".txt"), 
              quote=F,row.names = F, col.names = F)
  
  # content can be copied into https://espa.cr.usgs.gov/ordering/new/ for ordering level 2 products 
  
}


for(y in seq(year)){
  for(m in seq(month)){
    getSWIRpi(y,m,swirpath)
  }
}

########### collect all scene identifiers for submission in espa order ################################
idlist <- list.files(swirpath, pattern="_ri_", full.names=T)
idlistfiles <- lapply(seq(idlist), function(i){
  print(i)
  try(
    t <- read.table(idlist[i]))
  t[,1]
})

all_ids <- unlist(idlistfiles)

all_ids <- rev(all_ids[order(as.numeric(substring(all_ids, 18,25)))])
all_ids <- unique(all_ids)

write.table(all_ids, paste0(swirpath, "all_swir_ids.txt"), 
            quote=F,row.names = F, col.names = F)

################# order everything via https://espa.cr.usgs.gov/ordering/new/ manually with all_swir_ids.txt ####################################################################

################# download from espa ####################################################################

#install.packages("espa.tools", repos="http://R-Forge.R-project.org")
library(espa.tools)
swirdownloadpath <- "D:/new_downscaling/SWIR/downloaded_scenes/"

earthexplorer_download(usgs_eros_username="MaiteLezama",
                       usgs_eros_password="Eos300dmmmmlv",
                       output_folder = swirdownloadpath, verbose=T,
                       ordernum = "espa-mlezamavaldes@gmail.com-08102020-074516-892")

