# gather comp_comp files

dir.create()

ccpath <- paste0(main, "cc/")

################ COPY ###################################
# ccfiles <- lapply(seq(year), function(y){
#   lapply(seq(month), function(m){
#     L8scenepath <- paste0(main, "L8/", substring(time_range[[y]][[m]][[1]][[1]], 1, 7), "/")
#     cc <- paste0(L8scenepath, "timediff_comp_comp.csv")
#     file.copy(cc, paste0(ccpath, "tdcc_", substring(time_range[[y]][[m]][[1]][[1]], 1, 7), ".csv"))
#     #try(read.csv2(cc))
#   })
# })


############## take a look at contents ###################

cpf <- list.files(ccpath, full.names=T)
tdcc <- lapply(seq(cpf), function(i){
  read.csv2(cpf[i])
})

df <- do.call("rbind", tdcc)
nrow(df)



write.csv2(df, "D:/new_downscaling/all_comb_scenes.csv")

# when MODIS scenes are double, take only one

um <- unique(df$MODname)

unmod <- sapply(seq(um), function(i){
  which(grepl(um[i], df$MODname))[1]
})

dfuniqueMOD <- df[unmod,]


uniqueMODscenes <- unique(dfuniqueMOD$MODname)
MD <- sapply(seq(uniqueMODscenes), function(d){
  substring(uniqueMODscenes[d],11,22)
})
MODDate <- as.POSIXct(MD, tz="UTC",format = "%Y%j.%H%M")
dateaschar <- paste0(substring(as.character(MODDate),1,10), "-", substring(as.character(MODDate),12,13),substring(as.character(MODDate),15,16))

dfuniqueMOD$dateaschar <- dateaschar

write.csv2(dfuniqueMOD, "D:/new_downscaling/uniqueMOD_comb_scenes.csv")
