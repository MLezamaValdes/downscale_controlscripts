# scene graphic
library(lubridate)
library(ggplot2)
library(reshape2)

all <- read.csv2("D:/new_downscaling/all_comb_scenes.csv")

datcharcomp <- substring(all$MODname, 11,22)
datechar <- paste0(substring(datcharcomp, 1,4), "_", substring(datcharcomp, 5,7),"_",substring(datcharcomp, 9,10),":",substring(datcharcomp, 11,12))

dat <- as.POSIXct(datechar, format="%Y_%j_%H:%M", tz="Pacific/Auckland")
all$dat <- dat
h <- hour(dat)
m <- month(dat)
all$minutesdiff <- all$timediff*60

# boxplot(h)
# boxplot(m)

# hist(m, breaks=30)
# hist(h, breaks=30)

all[all$minutesdiff<30,]
max(all$minutesdiff)
plot(all$dat, all$minutesdiff, pch=19, lwd=2,main="time difference (min) Modis and Landsat",
     xlab="years", ylab="minutes")

# all$highminutesdiff <- ifelse(all$minutesdiff>25,all$minutesdiff,NA)
# all$lowminutesdiff <- ifelse(all$minutesdiff<25,all$minutesdiff,NA)

ggplot(data=all, aes(x=dat, y=minutesdiff))+
  geom_point()+
  theme_bw() +
  theme(legend.title = element_blank(),
        #axis.text.x=element_blank(),
        #axis.ticks.x=element_blank(),
        panel.grid.major = element_blank(), 
        plot.title = element_text(lineheight=.8, face="bold", size = 16),
        plot.subtitle = element_text(size = 12, face="bold"),
        legend.text=element_text(size=14, hjust = 0),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size=14))+
  labs(title="time difference (min) Modis and Landsat and matching scenes over time", fill=" ", 
       subtitle = paste0("n=", nrow(all)))

       