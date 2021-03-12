



########### collect all scene identifiers for submission in espa order ################################
idlist <- list.files(paste0(L8datpath, "ri_txt/"), pattern="bt_ri_", full.names=T)
idlistfiles <- lapply(seq(idlist), function(i){
  print(i)
  try(
    t <- read.table(idlist[i]))
  t[,1]
})

all_ids <- unlist(idlistfiles)

all_ids <- rev(all_ids[order(as.numeric(substring(all_ids, 18,25)))])
all_ids <- unique(all_ids)

write.table(all_ids, paste0(L8datpath, "ri_txt/all_swir_ids.txt"), 
            quote=F,row.names = F, col.names = F)



################# order everything via https://espa.cr.usgs.gov/ordering/new/ manually with all_swir_ids.txt ####################################################################

############### REMEMBER / WRITE DOWN ORDER NUMBER AS BELOW ################################
