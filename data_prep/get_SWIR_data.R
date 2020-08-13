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
  
  ## get available products and select one
  #product_names <- getLandsat_names(username="MaiteLezama", password = "Eos300dmmmmlv")
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

  
  # L8scenepath <- paste0(main, "L8/", substring(time_range[[y]][[m]][[1]][[1]], 1, 7), "/")
  # dd <- read.csv(paste0(L8scenepath, "downloaded_days.csv"))
  # 
  # dd <- dd[dd$lcc < 2,]
  # lcc <- 2
  # 
  # if(nrow(dd)<1){
  #   dd[dd$lcc <10,]
  #   lcc <- 10
  # }
  # 
  # promising_query <- lapply(seq(nrow(dd)), function(i){
  #   lapply(seq(query), function(j){
  #     if(any(grepl(substring(dd$fnam[i], 12,nchar(dd$fnam[i])), query[[j]]$summary))){
  #       q <- query[[j]][which(grepl(substring(dd$fnam[i], 12,nchar(dd$fnam[i])), query[[j]]$summary)),]
  #       q[q$level=="sr",]
  #     }
  #   })
  # })
  
  
  
  # not_working_due_to_package <- lapply(seq(promising_query), function(i){
  #   lapply(seq(promising_query[[i]]), function(j){
  #     if(!is.null(promising_query[[i]][[j]])){
  #       order_data(promising_query[[i]][[j]], wait_to_complete = F)
  # 
  #       try(getLandsat_data(records=promising_query[[i]][[j]],
  #                          espa_order=NULL,
  #                          source="auto"))
  #     }
  #   })
  # })

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
  
  
  # meta_url <- lapply(seq(promising_query), function(i){
  #   lapply(seq(promising_query[[i]]), function(j){
  #     if(!is.null(promising_query[[i]][[j]])){
  #       
  #       promising_query[[i]][[j]]$meta_url_fgdc 
  #       
  #     }
  #   })
  # })
  
  ri <- unlist(id)
  sum_pq <- unlist(sum_pq)
  write.table(ri, paste0(swirpath, "swir_ri_", substring(time_range[[y]][[m]][[1]][[1]], 1, 7), ".txt"), 
              quote=F,row.names = F, col.names = F)
  
  write.table(sum_pq, paste0(swirpath, "swir_sum_pq_", substring(time_range[[y]][[m]][[1]][[1]], 1, 7), ".txt"), 
              quote=F,row.names = F, col.names = F)
  
  # content can be copied into https://espa.cr.usgs.gov/ordering/new/ for ordering level 2 products 
  
}


# y=c(2:xxx)
for(y in c(2:length(year))){
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


################# download from espa ####################################################################

#install.packages("espa.tools", repos="http://R-Forge.R-project.org")
library(espa.tools)
swirdownloadpath <- "D:/new_downscaling/SWIR/downloaded_scenes/"

earthexplorer_download(usgs_eros_username="MaiteLezama", 
                       usgs_eros_password="Eos300dmmmmlv", 
                       output_folder = swirdownloadpath, verbose=T,
                       ordernum = "espa-mlezamavaldes@gmail.com-08102020-074516-892")

