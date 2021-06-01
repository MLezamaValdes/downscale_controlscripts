

library(tidyr)
library(ggplot2)
library(plyr)
library(RColorBrewer)
library(reshape2)
library(plyr)

source("split_violin_gg.R")
#file.edit("split_violin_gg.R")


DIdir <- paste0(cddir, "train_test_DI/")
testp <- list.files(DIdir,full.names=T, pattern="test")
test <- read.csv2(testp)


####################################################################

# get test samples for now
val1samps <- sample(nrow(test), size=4000)
val2samps <- sample(nrow(test), size=4000)
val3samps <- sample(nrow(test), size=4000)
trainsamps <- sample(nrow(test), size=4000)

val1 <- test[val1samps,]
val2 <- test[val2samps,]
val3 <- test[val3samps,]
train <- test[trainsamps,]


val1$tt <- "val1"
val2$tt <- "val2"
val3$tt <- "val3"
train$tt <- "train"


comp <- rbind(train, val1, val2, val3)
comp$tt <- as.factor(comp$tt)

##################### LST #########################################################
complstm <- melt(comp, id.vars = c("tt"), 
                 measure.vars = c("Modis", "Landsat"), 
                 variable.name = "sensor", 
                 value.name = "LST")

complstm$tv <- "validation"
complstm$tv[complstm$tt=="train"] <- "training"
complstm$tv <- as.factor(complstm$tv)
complstm$val <- complstm$tt
complstm$val[complstm$val=="train"] <- NA
complstm$val <- droplevels(complstm$val)

complstm_val <- complstm[complstm$tt!="train",]
complstm_trai <- complstm[complstm$tt=="train",]
complstm_trai$tt <- droplevels(complstm_trai$tt)

LST <- ggplot(data = complstm_trai) +
  theme_minimal() +
  custom_theme+
  geom_split_violin_1(aes_string(y = 'LST', x = 'sensor',fill="tt"),alpha=0.3)+
  scale_fill_manual(values=c("chartreuse3"),
                      labels=c("training"),
                      drop=F, name="")+
  new_scale_fill() +
  geom_split_violin(data=complstm_val, aes_string(y = 'LST', x = 'sensor', fill = "val"),
                    alpha=0.2)+
  scale_fill_manual(values=c("#1b1ff5","blue","skyblue"),
                    labels=c("validation 1", "validation 2", "validation 3"),
                    drop=F, name="")
  


##################### AUX VARIABLES #########################################################

make_ggplot_per_variable <- function(variable, makelegend=FALSE,yl,a=0.2,av=0.9){
  
    compauxm <- melt(comp, id.vars = c("tt"), 
                     measure.vars = variable, 
                     variable.name = "auxvar", 
                     value.name = "auxval")
    compauxm_val <- compauxm[compauxm$tt!="train",]
    compauxm_val$val <- compauxm_val$tt
    compauxm_val$val[compauxm_val$val=="train"] <- NA
    compauxm_val$val <- droplevels(compauxm_val$val)
    compauxm_trai <- compauxm[compauxm$tt=="train",]
    compauxm_trai$tt <- droplevels(compauxm_trai$tt)
    
    if(makelegend==TRUE){
            plot <- ggplot(data = compauxm_trai) +
                    theme_minimal() +
                    custom_theme+
                    geom_split_violin_1(aes_string(y = 'auxval', x = 'auxvar',fill="tt"),alpha=a)+
                    scale_fill_manual(values=c("chartreuse3"),
                                      labels=c("training"),
                                      drop=F, name="")+
                    new_scale_fill() +
                    geom_split_violin(data=compauxm_val, aes_string(y = 'auxval', x = 'auxvar', fill = "val"),
                                      alpha=av)+xlab("")+ylab(yl)+
                    scale_fill_manual(values=c("skyblue", "#1b1ff5","navy"),
                                      labels=c("validation 1", "validation 2", "validation 3"),
                                      drop=F, name="")
            } else {
              plot <- ggplot(data = compauxm_trai) +
                      theme_minimal() +
                      custom_theme_no_legend+
                      geom_split_violin_1(aes_string(y = 'auxval', x = 'auxvar',fill="tt"),alpha=a)+
                      scale_fill_manual(values=c("chartreuse3"),
                                        labels=c("training"),
                                        drop=F, name="")+xlab("")+ylab(yl)+
                      new_scale_fill() +
                      geom_split_violin(data=compauxm_val, aes_string(y = 'auxval', x = 'auxvar', fill = "val"),
                                        alpha=av)+
                      scale_fill_manual(values=c("skyblue", "#1b1ff5","navy"),
                                        labels=c("validation 1", "validation 2", "validation 3"),
                                        drop=F, name="")
              }

    return(plot)
}

plotstodo <- c("LST", "ia", "hs", "dem", "TWI", "soilraster", "aspect", "slope")
ylables <- c("LST °C", "incidence angle (degrees)", "hillshading (intensity of illumination)", "elevation (m)", "TWI (pobability of moisture accumulation)",
             "soil class", "aspect (degrees)", "slope (degrees)")

plotlist <- lapply(seq(plotstodo), function(i){
  if(i==1){ # this is the LST plot with legend
    make_ggplot_per_variable(c("Modis", "Landsat"), makelegend = T,yl=ylables[i])
  } else {
      make_ggplot_per_variable(plotstodo[i], makelegend = F, yl=ylables[i])
  }
})

grid.arrange(
  grobs = plotlist,
  #widths = c(2, 1, 1),
  layout_matrix = rbind(c(1, 1, NA, 2,3),
                        c(4, 5, 6, 7, 8))
)



library(gridExtra)
library(ggpubr)
library(gtable)



ggarrange(plotlist[[1]], plotlist[[2]], widths = c(1.5,2))



####################### JUST CHECKING ###########################################################

# avdf <- read.csv2(list.files(DIdir,full.names=T, pattern="available"))
# avdf$date[avdf$test==1]
# 
# # test whether spatial validation extraction worked all right
# blockshape <- readOGR(paste0(cddir, "train_test_polygons.shp"))
# names(blockshape) <- "tt"
# blockmask <- raster(paste0(dempath, "blockmask_aoi.tif"))
# 
# bm <- aggregate(blockmask,20) # to make extraction go faster
# 
# blocktestsample <- sort(c(62,79,23,44,26,53,19,4,38,21,40,37,59,70,22,32,20,17,10,64,51,28,52,77,48))
# 
# bm[bm %in% blocktestsample] <- 1000
# mapview(bm)+mapview(blockshape,zcol="tt",col.regions=c("chartreuse3","#1b1ff5"))
# 
# blocktestsample
# # ALL GOOD :)





# labs(title="Land surface temperature distribution in training and validation datasets", fill=" ",
#      subtitle = paste0("n training samples=", nrow(comp[comp$tt=="train",]),
#                        "  n test samples = ", nrow(comp[comp$tt=="val1",]),
#                        
#                        "  n test samples = ", nrow(comp[comp$tt=="val2",]),
#                        "  n test samples = ", nrow(comp[comp$tt=="val3",])),
#      position_nudge(x=-1)
#      
#      )