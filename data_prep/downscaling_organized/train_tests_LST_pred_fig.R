

library(tidyr)
library(ggplot2)
library(plyr)
library(RColorBrewer)
library(reshape2)
library(plyr)
library(ggnewscale)
library(gridExtra)
library(ggpubr)
library(gtable)


source("split_violin_gg.R")
#file.edit("split_violin_gg.R")


DIdir <- paste0(cddir, "train_valid/")

train <- read.csv2(paste0(DIdir, "train_LHS_150000.csv"))
#val1 <- read.csv2(paste0(DIdir, "validation_1_LHS_150000.csv"))
val1 <- read.csv2(paste0(DIdir, "validation_1_LHS_150000.csv"))
val2 <- read.csv2(paste0(DIdir, "validation_2_LHS_150000.csv"))
val3 <- read.csv2(paste0(DIdir, "validation_3_LHS_150000.csv"))

####################################################################

# # get test samples for now
# val1samps <- sample(nrow(test), size=4000)
# val2samps <- sample(nrow(test), size=4000)
# val3samps <- sample(nrow(test), size=4000)
# trainsamps <- sample(nrow(test), size=4000)
# 
# val1 <- test[val1samps,]
# val2 <- test[val2samps,]
# val3 <- test[val3samps,]
# train <- test[trainsamps,]


val1$tt <- "val1"
val2$tt <- "val2"
val3$tt <- "val3"
train$tt <- "train"


removenamecolumns <- function(obj){
  obj <- obj[,which(names(obj)=="Landsat"):length(names(obj))]
  if(any(names(obj)=="time_num")){
    obj <- obj[,-which(names(obj)=="time_num")]
  }
  return(obj)
}


train <- removenamecolumns(train)
val1 <- removenamecolumns(val1)
val2 <- removenamecolumns(val2)
val3 <- removenamecolumns(val3)

names(train)==names(val3)

comp <- rbind(train, val1, val2, val3)
comp$tt <- as.factor(comp$tt)
# 
# ##################### LST #########################################################
# complstm <- melt(comp, id.vars = c("tt"), 
#                  measure.vars = c("Modis", "Landsat"), 
#                  variable.name = "sensor", 
#                  value.name = "LST")
# 
# complstm$tv <- "validation"
# complstm$tv[complstm$tt=="train"] <- "training"
# complstm$tv <- as.factor(complstm$tv)
# complstm$val <- complstm$tt
# complstm$val[complstm$val=="train"] <- NA
# complstm$val <- droplevels(complstm$val)
# 
# complstm_val <- complstm[complstm$tt!="train",]
# complstm_trai <- complstm[complstm$tt=="train",]
# complstm_trai$tt <- droplevels(complstm_trai$tt)
# 
# # LST <- ggplot(data = complstm_trai) +
# #   theme_minimal() +
# #   custom_theme+
# #   geom_split_violin_1(aes_string(y = 'LST', x = 'sensor',fill="tt"),alpha=0.3)+
# #   scale_fill_manual(values=c("chartreuse3"),
# #                       labels=c("training"),
# #                       drop=F, name="")+
# #   new_scale_fill() +
# #   geom_split_violin(data=complstm_val, aes_string(y = 'LST', x = 'sensor', fill = "val"),
# #                     alpha=0.2)+
# #   scale_fill_manual(values=c("#1b1ff5","blue","skyblue"),
# #                     labels=c("validation 1", "validation 2", "validation 3"),
# #                     drop=F, name="")
# #   
# 
# 
# ##################### AUX VARIABLES #########################################################

make_ggplot_per_variable <- function(variable, makelegend=FALSE,yl,a=0.15,av=0.15,bar,index){
  
    if(variable %in% c("landcoverres", "soilraster")){
      bar=TRUE
    }
  
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
    
    if(bar==TRUE){

      if(compauxm$auxvar[1] == "landcoverres"){
       
        variablename <- "landcover type"
        compauxm$auxval <- as.factor(compauxm$auxval)
        levels(compauxm$auxval) <- c("open soil", "snow and ice")
        
        mybarchart <- ggplot(data = compauxm, aes(x=auxval, fill = tt, colour=tt)) +
          theme_minimal() +
          custom_theme_barchart+
          geom_bar(position="dodge", size=1)+
          scale_fill_manual(values=c("#e8f8d9","#edf8fc", "#dddefe", "#dbe5ee"),
                            labels=c("train","validation 1", "validation 2", "validation 3"),
                            drop=F, name="")+ylab(paste0("n ", variablename, " observations"))+
          scale_color_manual(values=c("chartreuse3","skyblue", "#1b1ff5","dodgerblue4"),
                             labels=c("train","validation 1", "validation 2", "validation 3"),
                             drop=F, name="")+
          theme(legend.position = "none")+ggtitle(letters[index])
        
        plot <- mybarchart
        
      } else if(compauxm$auxvar[1] == "soilraster"){
        
        variablename <- "soil type"
        
        st <- rev(sort(table(compauxm$auxval)))
        compauxm$auxval <- factor(compauxm$auxval, levels = names(st), ordered = TRUE)
        mybarchart <- ggplot(data = compauxm, aes(x=auxval, fill = tt, colour=tt)) +
          theme_minimal() +
          custom_theme_barchart+
          geom_bar(position="dodge", size=1)+
          
          
          # old 
          #values=c("#e8f8d9","#93d3ed", "#3135f6","#124b95"),
          
          scale_fill_manual(values=c("#e8f8d9","#edf8fc", "#dddefe", "#dbe5ee"),
                            labels=c("train","validation 1", "validation 2", "validation 3"),
                            drop=F, name="")+
          scale_color_manual(values=c("chartreuse3","skyblue", "#1b1ff5","dodgerblue4"),
                            labels=c("train","validation 1", "validation 2", "validation 3"),
                            drop=F, name="")+
          ylab(paste0("n ", variablename, " observations"))+
          
          theme(legend.position = "none")+ggtitle(letters[index])
        
        plot <- mybarchart
      }
      
    } 
     
    if(bar==FALSE & makelegend==TRUE){
            plot <- ggplot(data = compauxm_trai) +
                    theme_minimal() +
                    custom_theme+
              
                    geom_split_violin(data=compauxm_val, aes_string(y = 'auxval', x = 'auxvar', fill = "val", colour="val"),
                                      alpha=av,size = 1)+
                    xlab("")+ylab(yl)+ggtitle(letters[index])+
                    scale_fill_manual(values=c("skyblue", "#1b1ff5","dodgerblue4"),
                                      labels=c("validation 1", "validation 2", "validation 3"),
                                      drop=F, name="")+
                    scale_color_manual(values=c("skyblue", "#1b1ff5","dodgerblue4"),
                                    labels=c("validation 1", "validation 2", "validation 3"),
                                    drop=F, name="")+
                    #scale_linetype_manual(values=c("dotted", "dashed","solid"))
                    new_scale_fill() +
                    new_scale_color() +
                    geom_split_violin_1(aes_string(y = 'auxval', x = 'auxvar',fill="tt", colour="tt"),
                                        alpha=a,size = 1)+
                    scale_fill_manual(values=c("chartreuse3"),
                                      labels=c("training"),
                                      drop=F, name="")+
                    scale_color_manual(values=c("chartreuse3"),
                                 labels=c("training"),
                                 drop=F, name="")
                    

            } 
    if(bar==FALSE & makelegend == FALSE) {
              plot <- ggplot(data = compauxm_trai) +
                      theme_minimal() +
                      custom_theme_no_legend+
                      geom_split_violin(data=compauxm_val, aes_string(y = 'auxval', x = 'auxvar', fill = "val", colour="val"),
                                        alpha=av,size = 1)+
                      scale_fill_manual(values=c("skyblue", "#1b1ff5","dodgerblue4"),
                                        labels=c("validation 1", "validation 2", "validation 3"),
                                        drop=F, name="")+
                      xlab("")+ggtitle(letters[index])+
                      scale_color_manual(values=c("skyblue", "#1b1ff5","dodgerblue4"),
                                         labels=c("validation 1", "validation 2", "validation 3"),
                                         drop=F, name="")+
                      new_scale_fill() +
                      new_scale_color() +
                      geom_split_violin_1(aes_string(y = 'auxval', x = 'auxvar',fill="tt",colour = "tt"),
                                          alpha=a,size = 1)+
                      scale_fill_manual(values=c("chartreuse3"),
                                        labels=c("training"),
                                        drop=F, name="")+xlab("")+ylab(yl)+
                      scale_color_manual(values=c("chartreuse3"),
                                   labels=c("training"),
                                   drop=F, name="")

              }

    return(plot)
}

plotstodo <- c("LST", "ia", "hs", "dem", "TWI", "landcoverres", "soilraster", "aspect", "slope")
ylables <- c("LST °C", "incidence angle (degrees)", "hillshading (intensity of illumination)", "elevation (m)", "TWI (pobability of moisture accumulation)",
             "snow/ice vs. open soil", "soil class", "aspect (degrees)", "slope (degrees)")

plotlist <- lapply(seq(plotstodo), function(i){
  if(i==1){ # this is the LST plot with legend
    make_ggplot_per_variable(c("Modis", "Landsat"), 
                             makelegend = T,yl=ylables[i],
                             bar=FALSE,index=i)
  } else { # for all auxiliary plots
    make_ggplot_per_variable(plotstodo[i], 
                             makelegend = F, yl=ylables[i], 
                             bar=FALSE,index=i)
  }
})

png(paste0(figurepath, "new/train_val1_val2_val3_final.png"),
    units="in", width=14, height=12, res=300)
grid.arrange(
  grobs = plotlist,
  #widths = c(2, 1, 1),
  layout_matrix = rbind(c(1, 1, 1, 2, 3),
                        c(1, 1, 1, 4, 5),
                        c(6, 7, 7, 8, 9)),
)
dev.off()




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