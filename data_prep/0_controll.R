

####### OVERALL CONTROLL SCRIPT ######## 

######## GENERAL INFO ABOUT RUN AND AREA ######## 
# newarea can be 0 (no) or 1 (yes). If area is not new, it is assumed that 
# the static portion of the script is already available and doesn't need to be unzipped etc.
newarea <- 0

areaname <- "MDV"


######## translating to SAGA ######## 
path_saga_norm <- "C:/OSGeo4W64/apps/saga-ltr/"
sagaCmd <- paste0(path_saga_norm, "saga_cmd.exe")

saga_outpath <- "E:/Antarctica/runoff_paths/"