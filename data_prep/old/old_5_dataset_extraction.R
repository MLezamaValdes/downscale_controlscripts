
# get data 
L8datpath <- "E:/Antarctica/testrun_loop_2/L8/"
modispath <- "E:/Antarctica/testrun_loop_2/MODIS/"
timestep <- "2018-12"
l8_date_path <- paste0(L8datpath, timestep, "/")
l8_bt_path <- paste0(l8_date_path, "bt/")
m_date_path <- paste0(modispath, timestep, "/")
m_LST_path <- paste0(m_date_path, "LST/")

# go on here!
dn <- lapply(seq(list.files(m_LST_path, full.names=T)[grepl("^M",basename(list.files(m_LST_path, full.names=T)))]), function(i){
    raster(list.files(m_LST_path, full.names=T)[grepl("^M",basename(list.files(m_LST_path, full.names=T)))][i])
})


l8merged <- raster(list.files(l8_bt_path, pattern="merged", full.names=T))

# make extraction table
tf <- readOGR(list.files("E:/workflow_example/", pattern="training.shp", 
                         full.names = T))
tf@data$id <- NULL
tf$ID <- c(1:nrow(tf))

exmica <- extract(mica_s, tf)
exmic_type <- lapply(seq(exmica), function(i){
  x <- as.data.frame(exmica[[i]])
  x$type <- as.character(tf$type[i])
  x
})
