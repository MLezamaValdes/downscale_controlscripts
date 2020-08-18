
# ASTER LST DATA

main <- "E:/ASTER/LST/"
list.files(main, pattern = "zip")

subdirs <- paste0(grep(list.files(main, full.names = T), pattern='zip', inv=T, value=T), "/")

alst <- lapply(seq(subdirs), function(i){
  raster(list.files(subdirs[i], pattern = ".tif$", full.names = T))
})

length(alst)
