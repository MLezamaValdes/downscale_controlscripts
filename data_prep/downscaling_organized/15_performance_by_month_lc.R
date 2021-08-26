modelpath <- "D:/downscaling_after_talk/models/"
dp <- "D:/downscaling_after_talk/clean_data/train_valid/"
figurepath <- "C:/Users/mleza/OneDrive/Documents/PhD/work_packages/auto_downscaling_30m/paper/paper_draft/figures/new/"

load(paste0(modelpath, "final_model_rf_150000factorfast_mtry.RData"))


fl <- list.files(dp, full.names = T, pattern="rand")
fl <- c(fl, paste0(dp, "train_LHS_150000.csv"))


tv <- lapply(seq(fl), function(i){
  x <- read.csv2(fl[i])
  x[,(which(names(x)=="Landsat"):ncol(x))]
})

tv[[1]]$time_num <- NULL
tv[[2]]$time_num <- NULL
tv[[3]]$time_num <- NULL
tv[[4]]$time_num <- NULL

names(tv[[1]])
names(tv[[2]])
names(tv[[3]])
names(tv[[4]])

tv <- do.call("rbind",tv)
tv$TeAqNum <- NA

tv$TeAq <- as.factor(substring(tv$Mscene,1,3))
tv$TeAqNum <- as.numeric(tv$TeAq)
print(names(tv))

tv$soilraster[tv$soilraster==3 | tv$soilraster==19] <- NA
tv <- tv[complete.cases(tv),]

tv$soilraster <- factor(tv$soilraster)
tv$TeAqNum <- factor(tv$TeAqNum)
tv$landcoverres <- factor(tv$landcoverres)

str(tv)

# equalize factor levels
trained_with <- model_final$trainingData[1,-ncol(model_final$trainingData)]
str(trained_with)
trained_with <- rbind(model_final$trainingData[1,-ncol(model_final$trainingData)], 
                      tv[c("Modis", "ia", "dem", "slope","aspect", "soilraster",
                           "landcoverres", "TeAqNum")])

tv_ad <- trained_with[2:nrow(trained_with),]

str(tv_ad)

library(caret)
library(CAST)

predtv <- predict(model_final, newdata=tv_ad)

tv$LST_pred <- predtv
write.csv2(tv, paste0(dp, "train_valid_pred.csv"))
saveRDS(tv, paste0(dp, "train_valid_pred.rds"))




############### prepping for boxplots ##########################
library(ggplot2)
library(lubridate)
library(gridExtra)
tv$month <- month(tv$modtime, label=T, abbr=T, locale=Sys.setlocale("LC_TIME", "English"))
levels(tv$month)
tv$landcoverres <- factor(tv$landcoverres)
levels(tv$landcoverres) <- c("open soil", "snow and ice")

tv$resid <- tv$Landsat-tv$LST_pred

org <- tv[c("month", "Landsat", "resid", "landcoverres")]
org$op <- factor("org")
names(org) <- c("month", "LST",  "resid", "landcoverres", "op")

pred <- tv[c("month", "LST_pred", "resid", "landcoverres")]
pred$op <- factor("pred")
names(pred) <- c("month", "LST",  "resid", "landcoverres", "op")

residdf <- rbind(org, pred)
lstop <- rbind(org, pred)

source("C:/Users/mleza/OneDrive/Documents/PhD/work_packages/auto_downscaling_30m/downscale_controlscripts/data_prep/downscaling_organized/split_violin_gg.R")

custom_theme = theme(
  text = element_text(size = 10),
  axis.title.x = element_text(size = 14),
  axis.title.y = element_text(size = 14,face="bold", vjust=1.5),
  axis.text = element_text(size = 14),
  axis.text.x = element_text(vjust = 0.5),
  #axis.text.y = element_text(hjust = 1.2),
  legend.title=element_text(size=16),
  legend.text=element_text(size=16, hjust = 0),
  legend.direction = "vertical",
  plot.title = element_text(lineheight=.8, face="bold", size = 16),
  plot.subtitle = element_text(size = 12, face="bold"),
  panel.border = element_blank(),
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  legend.key.size = unit(1.5, "cm"),
  legend.key.width = unit(0.75,"cm"),
  plot.margin = unit(c(0,0,0,10), "lines"),
  legend.position = c(-.1,.1),
  axis.line.x = element_blank(),
  axis.ticks.x = element_blank(),
  axis.line.y = element_line(colour = 'black', size=0.3, linetype='solid'))

custom_theme_no_legend = theme(
  text = element_text(size = 10),
  axis.title.x = element_text(size = 14),
  axis.title.y = element_text(size = 14,face="bold", vjust=1.5),
  axis.text = element_text(size = 14),
  axis.text.x = element_text(vjust = 0.5),
  #axis.text.y = element_text(hjust = 1.2),
  legend.title=element_text(size=16),
  legend.text=element_text(size=16, hjust = 0),
  legend.direction = "vertical",
  plot.title = element_text(lineheight=.8, face="bold", size = 16),
  plot.subtitle = element_text(size = 12, face="bold"),
  panel.border = element_blank(),
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  #legend.key.size = unit(1.5, "cm"),
  #legend.key.width = unit(0.75,"cm"),
  #plot.margin = unit(c(0,0,0,10), "lines"),
  legend.position ="",
  axis.line.x = element_blank(),
  axis.ticks.x = element_blank(),
  axis.line.y = element_line(colour = 'black', size=0.3, linetype='solid'))

############### PLOTTING BOXPLOTS OVER TIME ##########################
lm(tv$Landsat ~ tv$resid)
offset <- -18.4442
# 
# rangepred <- max(tv$LST_pred)- min(tv$LST_pred)
# rangeobs <- max(tv$Landsat)- min(tv$Landsat)
# rangeres <- max(tv$resid)- min(tv$resid)

scalingFactor <- 0.9882 

library(dplyr)

barchart(table(residdf$month))
barchart(table(residdf$landcoverres))

nmonths <- data.frame(table(residdf$month))
nmonths <- nmonths[nmonths$Freq >0,]

nlc <- data.frame(table(residdf$landcoverres))



nrow(residdf)

sample_size = residdf %>% group_by(month) %>% summarize(num=n())


residdf <- residdf %>% 
  left_join(sample_size) %>%
  mutate(month = paste0(month, "\n", "n=", num)) 

pred <- pred %>% 
  left_join(sample_size) %>%
  mutate(month = paste0(month, "\n", "n=", num)) 

org <- org %>% 
  left_join(sample_size) %>%
  mutate(month = paste0(month, "\n", "n=", num)) 


sample_size_lc = residdf %>% group_by(landcoverres) %>% summarize(num_lc=n())


residdf <- residdf %>% 
  left_join(sample_size_lc) %>%
  mutate(landcoverres = paste0(landcoverres, "\n", "n=", num_lc)) 

pred <- pred %>% 
  left_join(sample_size_lc) %>%
  mutate(landcoverres = paste0(landcoverres, "\n", "n=", num_lc)) 

org <- org %>% 
  left_join(sample_size_lc) %>%
  mutate(landcoverres = paste0(landcoverres, "\n", "n=", num_lc)) 



mp <- ggplot() +
  theme_minimal() +
  ggtitle("a")+
  geom_boxplot(data=residdf, aes(y =(scalingFactor*resid)-20, x = month),
               alpha=1,size = 0.7,outlier.size = 0.000001, col="firebrick3")+
  geom_split_violin(data=pred, aes(y = LST , x = month, fill=op),
                    alpha=0.3,size = 0.7)+
  scale_fill_manual(values=c("gray20","gray72"),
                     labels=c("original", "predicted"),
                     drop=F, name="")+
  geom_split_violin_1(data=org, aes(y = LST, x = month, fill=op),
                      alpha=0.3,size = 0.7)+
  scale_y_continuous(name="LST (°C)",
                     sec.axis=sec_axis(~./scalingFactor+20, name="residuals")) +

  custom_theme+xlab("")+
  theme(
    plot.title = element_text(size=25),
    axis.line.y.right  = element_blank(),
    axis.title.y.right = element_blank(),
    axis.text.y.right=element_blank(),
    axis.ticks.y.right=element_blank(),
    legend.direction = "horizontal", 
    legend.margin=margin(-25,-25,-25,-25),
    legend.box.margin=margin(-25,-25,-25,-25),
    legend.position = c(0.7, 0.95))
mp

############### PLOTTING BOXPLOTS OVER LCC ##########################

# var is landcoverres
lcp <- ggplot() +
  ggtitle("b")+
  theme_minimal() +
  geom_boxplot(data=residdf, aes(y = (scalingFactor*resid)-20, x = landcoverres),
               alpha=1,size = 0.7,outlier.size = 0.000001, col="firebrick3")+
  geom_split_violin(data=pred, aes(y = LST , x = landcoverres, fill=op),
                    alpha=0.3,size = 0.7)+
  scale_fill_manual(values=c("gray20","gray72"),
                    labels=c("original", "predicted"),
                    drop=F, name="LST")+
  geom_split_violin_1(data=org, aes(y = LST, x = landcoverres, fill=op),
                      alpha=0.3,size = 0.7)+
  scale_y_continuous(name="LST (°C)",
                     sec.axis=sec_axis(~./scalingFactor+20, name="residuals")) +
  custom_theme_no_legend+xlab("")+
  theme(
    legend.position = "",
    axis.title.y.right=element_text(color="firebrick3"),
    axis.text.y.right=element_text(color="firebrick3"),
    plot.title = element_text(size=25),
    axis.line.y.left = element_blank(),
    axis.title.y.left = element_blank(),
    axis.text.y.left=element_blank(),
    axis.ticks.y.left=element_blank(),
    axis.text.x = element_text(size=14, vjust=1))


png(paste0(figurepath, "resid_month_lc_rand.png"),
    units="in", width=18, height=10, res=1000)
grid.arrange(
  grobs = list(mp, lcp),
  #widths = c(2, 1, 1),
  layout_matrix = rbind(c(1,1,2)),
)
dev.off()

## idea: make split violin from observed, predicted LST and add boxplot for residuals