############ Modelling
y=1
m=1

library(devtools)
library(caret)
library(CAST)
modpath <- "D:/new_downscaling/modelling/"

### get extraction data ###################################################################
expath <- "D:/new_downscaling/extraction/"

#exdf <- read.csv2(paste0(expath, "extr_complete_cases_2019-01.csv"), header=T, nrow=10000000) # 10 million samples for now, change!
#str(exdf)
#names(exdf)

#exdforg <- exdf
#exdf <- exdf[,c(1:6, 9:ncol(exdf))] # aux ID rausschmeißen
#names(exdf) <- c("id","Landsat", "Modis", "ia", "hs", "time", "dem", "slope", "aspect", 
#                 "TWI", "soilraster", "landcoverres", "spatialblocks", "swir")
#head(exdf)
#str(exdf)

#exdf$time <- as.numeric(as.factor(exdf$time))
#exdf$Modis[exdf$Modis <= -30] <- NA
#exdf <- exdf[complete.cases(exdf),]

######### plot #############################################################################

# par(mfrow=c(4,4), mar=c(5,3,2,2)+2)
# for(i in seq(ncol(potDI))){
#   print(i)
#   if(typeof(potDI[,i])=="factor"){
#     barplot(table(as.factor(potDI[,i])), main=names(potDI)[i])
#   } else {
#     boxplot(potDI[,i], main=names(potDI)[i])
#   }
# }

# # ### make coordinate raster ###########################################################
# aux <- stack("D:/new_downscaling/auxiliary/aux_stack.tif")
# swir <- paste0(swiroutpath, "swir_tc_", ym, ".tif")
# 
# ct <- coordinates(template)
# ct <- data.frame(ct)
# x <- template
# y <- template
# x[] <- ct$x
# y[] <- ct$y
# 
# 
# # add x and y to aux, extract and save
# aux <- stack(aux, swir, x, y)
# names(aux) <- c("dem", "slope", "aspect", "TWI", "soilraster", "landcoverres", "spatialblocks", "swir", "x", "y")
# writeRaster(aux, "D:/new_downscaling/auxiliary/aux_stack_xy.tif", overwrite=T)
# 
# auxdf <- extract(aux, aoiaux)
# write.csv2(auxdf[[1]], paste0(expath, "aux_df_swir_x_y.csv"), row.names=F)
# 
# 
# auxdf <- read.csv2(paste0(expath, "aux_df_swir_x_y.csv"))

### get raster data ###################################################################
# aux <- stack("D:/new_downscaling/auxiliary/aux_stack_xy.tif")
# names(aux) <- c("dem", "slope", "aspect", "TWI", "soilraster", "landcoverres", "spatialblocks", "swir", "x", "y")
# 
# tempdyn <- stack(paste0(cddir, "new_L_MOD_hs_ia", substring(time_range[[y]][[m]][[1]][[1]], 1, 7), ".tif"))
# tdnam <- read.csv("D:/new_downscaling/clean_data/names_sat_ia_hs_2019-01.csv")
# names(tempdyn) <- tdnam$x

### 1 select test data ###################################################################

### make a raster ######################

# usb <- unique(exdf$spatialblocks) 
# # sbs <- sample(usb,length(usb)*0.2)
# sbs <- c(64,77,54,19,31,78,61,62)
# 
# testsites <- aux$spatialblocks
# testsites_vals <- ifelse(testsites[] %in% sbs,1,0)
# testsites[] <- testsites_vals
# plot(testsites)
# writeRaster(testsites, "D:/new_downscaling/modelling/testsites_2019_01.tif")
# 
# 
# exdf$test <- ifelse(exdf$spatialblocks %in% sbs,1,0)
# 
# test <- subset(exdf,exdf$test==1)
# saveRDS(test, paste0(modpath, "test.rds"))

#pottrain <- subset(exdf, exdf$test==0)
#saveRDS(pottrain, paste0(modpath, "pottrain_before_sampling.rds"))

### 2.0 random sample selection ###################################################################
# pottrain <- readRDS(paste0(modpath, "pottrain_before_sampling.rds"))
# # match coordinates with pottrain
# # id_shape <- readOGR( paste0(main, "points_id_aoianta.shp"))
# # write.csv2(id_shape@data, paste0(main, "points_id_aoianta.csv"))
# 
# id_coords <- read.csv2(paste0(main, "points_id_aoianta.csv"))
# pottraincord <- merge(pottrain, id_coords[,3:6], by.x="id", by.y="id_extr", all.x=TRUE)
# 
# 
# cordsidinpottrain <- idcords$id %in% pottrain$id
# potidincords <- pottrain$id %in% idcords$id 
# 
# table(cordsidinpottrain)
# 
# idcords <- id_coords[,c(3:4, 6)]
# names(idcords) <- c("x", "y", "id")
# pottraincord2 <- plyr::join(pottrain,idcords,by="id",match="all",type="left")
# 
# pottraincord2$id
# comppot <- pottraincord2[complete.cases(pottraincord2),]
# 
# head(pottraincord)
# any(is.na(pottraincord2$x))
# 
# head(pottraincord)
# nrow(pottraincord)
# nrow(pottrain)
# nrow(id_coords)
# length(unique(pottraincord$id))
# length(unique(id_coords$id_extr))
# length(unique(pottrain$id))
# vidpottrainc <- sort(table(unique(pottraincord$id)))
# idpottrain <- sort(table(unique(pottrain$id)))
# 
# # get 3 Mio random samples per month to choose from in random and DI picking
# splpot <- sample(rownames(pottrain), 3000000)
# pott3 <- pottrain[splpot,]


###################### TO DO: START WITH POTT3 ALREADY FROM SERVER ###########################################
pott3 <- read.csv2(paste0(expath, "pott3_new_2019-01.csv"))

# convert time to numeric
pott3$time_num <- as.numeric(as.factor(pott3$time))
nrow(pott3)

# get only relevant variables for DI 
divars <- names(pott3)[c(1:4, 6:7, 9:14, 16:17, 20)]
potDI <- pott3[divars]
nrow(potDI)

# get 150.000 random samples per month to maintain data distribution
rnds <- sample(rownames(potDI), 150000)
randsamples <- potDI[rnds,]

potDI$inrandsamples <- ifelse(rownames(potDI) %in% rnds,1,0)
table(potDI$inrandsamples)
potDI_ds <- potDI[potDI$inrandsamples == 0,]

nrow(randsamples) # already in the dataset
nrow(potDI_ds) # perhaps to be added to dataset 
nrow(randsamples)+nrow(potDI_ds) # 3 Mio starting point

### 2 sample selection via DI ###################################################################

# --> on Palma 

