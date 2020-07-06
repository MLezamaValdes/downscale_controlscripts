# get SWIR data

swirpath <- paste0(main, "L8/SWIR/")
print(swirpath)

y=1
m=1


####### GET DATA ONLINE ##############################################################################################

print("STARTING LANDSAT SWIR DOWNLOAD AND PREP")

## set aoi and time range for the query
l8proj <- crs("+proj=utm +zone=57 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs ")
aoiutm <- spTransform(aoi, l8proj)
set_aoi(aoiutm)

## set archive directory
set_archive(swirpath)

## get available products and select one
#product_names <- getLandsat_names(username="MaiteLezama", password = "Eos300dmmmmlv")
product <- "LANDSAT_8_C1"

## query records for AOI, time range and product
nodat <- list(0)

login_USGS("MaiteLezama", "Eos300dmmmmlv")


day <- seq(length(time_range[[y]][[m]]))  
query <- lapply(seq(day), function(d){
  try(getLandsat_query(time_range = time_range[[y]][[m]][[d]], name = product,
                       aoi=get_aoi()), silent=T)
})

te <- sapply(seq(query), function(x){
  class(query[[x]])
})

L8scenepath <- paste0(main, "L8/", substring(time_range[[y]][[m]][[1]][[1]], 1, 7), "/")
dd <- read.csv(paste0(L8scenepath, "downloaded_days.csv"))

dd[dd$lcc < 2,]
lcc <- 2

if(nrow(dd)<1){
  dd[dd$lcc <10,]
  lcc <- 10
}

promising_query <- lapply(seq(query), function(i){
  lapply(seq(nrow(dd)), function(j){
    if(any(grepl(dd$fnam[j], query[[i]]$summary))){
      query[[i]][which(grepl(dd$fnam[j], query[[i]]$summary)),]
    }
  })
})

product_identifyer <- lapply(seq(query), function(i){
  lapply(seq(nrow(dd)), function(j){
    if(any(grepl(dd$fnam[j], query[[i]]$summary))){

      x <- unlist(strsplit(promising_query[[i]][[j]]$summary  , ","))[1]
      unlist(strsplit(x, ": "))[2]

      }
  })
})

pi <- unlist(product_identifyer)

lapply(seq(query), function(i){
  lapply(seq(nrow(dd)), function(j){
    if(!is.null(promising_query[[i]][[j]])){
      id <- getLandsat_data(records=promising_query[[i]][[j]], 
                      level=c("sr","pixel_qa"), espa_order=NULL,
                      source="auto")
    }
  })
})
