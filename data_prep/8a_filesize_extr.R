# big files preparation for extraction
library(raster)
############################ make a file containing split info ############################################

datpath <- "D:/new_downscaling/clean_data/satstacks/"

fs_tab <- data.frame(list.files(datpath, pattern="L_MOD_hs_ia_", full.names = T))
fs_tab$filesize <- file.info(list.files(datpath, pattern="L_MOD_hs_ia_", full.names = T))$size

names(fs_tab) <- c("path", "filesize")
fs_tab <- fs_tab[order(fs_tab$filesize),]

fs_tab$ym <- substring( basename(fs_tab$path), 13, 19)

fs_tab$sep <- ifelse(fs_tab$filesize > 4000000000, 1, 0)
fs_tab$year <- substring( basename(fs_tab$path), 13, 16)
fs_tab$month <- substring( basename(fs_tab$path), 18, 19)

#write.csv2(fs_tab, "D:/new_downscaling/clean_data/satstacks/fs_tab.csv")

fs_tab <- read.csv2("D:/new_downscaling/clean_data/satstacks/fs_tab.csv")

############################ split stacks ############################################

#for(i in seq(nrow(fs_tab))){
for(i in c(36)){
  print(i)
  if(fs_tab[i,]$sep==1){
    print("starting splitting")
    rs <- stack(fs_tab$path[i])
    
    # max 15 packages of 4 layers per subfile
    maxpackages <- 15
    (npackages <- nlayers(rs)/4)
    splits <- ceiling(npackages/maxpackages)
    print(paste0("n splits = ", splits))
    if(splits==1){
      fs_tab$sep[i] <- 2
    } else {
      print("splitting now")
      new_package_split <- seq(1,(nlayers(rs)-2), by=(maxpackages*4))
      end_split <- new_package_split+(maxpackages*4-1)
      
      end_split[length(end_split)] <- nlayers(rs)
      
      for(x in seq(splits)){
        splitx <- rs[[new_package_split[x]:end_split[x]]]
        print(paste0("writing raster ", tools::file_path_sans_ext(basename(fs_tab[i,]$path)), "_", x, ".tif") )
        writeRaster(splitx, paste0("D:/new_downscaling/clean_data/satstacks/", 
                    tools::file_path_sans_ext(basename(fs_tab[i,]$path)), "_", x, ".tif" ),
                    overwrite=T)
      }

    }
  }
}

write.csv2(fs_tab, "D:/new_downscaling/clean_data/satstacks/fs_tab.csv")

