#### 4b check if all mydLST are in LST, if not generate LST dir and add MYD ################


mdirs <- list.files("D:/new_downscaling/data_download_preprocessing/MODIS/", full.names = T)
mdirs <- paste0(mdirs, "/")

for(i in seq(mdirs)){
  mdirmonth <- list.files(mdirs[i])
  mdirmonthlong <- list.files(mdirs[i], full.names = T)
  
  LSTfiles <- list.files(mdirmonthlong[which(grepl("^LST$", mdirmonth))])
  
  print(basename(mdirs)[i])
  if(length(LSTfiles)>0){ 
    print("there is a LST folder already")
    smallfiles <- LSTfiles[grepl("small_proj", LSTfiles)]
    print(paste0("MYD files in LST?: ", any(grepl("MYD", smallfiles)))) # test whether there are already MYD files in .../LST/
    # if so, nothing has to be done. If not, .../LST/ has to be generated and files copied
    if(any(grepl("MYD", smallfiles))==FALSE & dir.exists(paste0(mdirs[i], "mydLST/"))){ 
      print("there is a folder mydLST and nothing in LST yet")
      list.files(paste0(mdirs[i], "mydLST/"), full.names=T, pattern="small_proj")
      file.copy(ftc, paste0(mdirs[i], "LST/", basename(ftc)), overwrite = T)
    }
  } else {
    print("creating LST folder and writing mydLST small proj files into it")
    dir.create(paste0(mdirs[i], "LST/"))
    ftc <- list.files(paste0(mdirs[i], "mydLST/"), full.names=T, pattern="small_proj")
    file.copy(ftc, paste0(mdirs[i], "LST/", basename(ftc)), overwrite = T)
  }

}






