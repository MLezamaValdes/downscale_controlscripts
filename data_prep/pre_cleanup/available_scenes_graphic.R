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
all$month <- as.factor(m)
all$month <- factor(all$month, levels = c("9", "10", "11", "12", "1", "2", "3"))
all$monthyear <- paste0(all$month, "_", year(dat))

all[grepl("2018",all$dat)&all$timediff<0.3,]

all$dat <- as.Date(all$dat)
# boxplot(h)
# boxplot(m)

# hist(m, breaks=30)
# hist(h, breaks=30)

# all[all$minutesdiff<30,]
# max(all$minutesdiff)
# plot(all$dat, all$minutesdiff, pch=19, lwd=2,main="time difference (min) Modis and Landsat",
#      xlab="years", ylab="minutes")

# all$highminutesdiff <- ifelse(all$minutesdiff>25,all$minutesdiff,NA)
# all$lowminutesdiff <- ifelse(all$minutesdiff<25,all$minutesdiff,NA)

ggplot(data=all, aes(x=dat, y=minutesdiff, colour=month))+
  geom_point(size=1.7)+
  scale_color_manual(values=c("black", "lightsteelblue2","coral3","hotpink4",
                                      "darkgoldenrod2", "olivedrab3", "lightcyan4"))+
  theme_bw() +
  labs(color = "month")+scale_x_date(date_labels = "%Y %m",date_breaks = "6 month")+
  theme(#axis.text.x=element_blank(),
        #axis.ticks.x=element_blank(),
        panel.grid.major = element_blank(), 
        plot.title = element_text(lineheight=.8, face="bold", size = 16),
        plot.subtitle = element_text(size = 14),
        legend.text=element_text(size=14, hjust = 0),
        legend.title = element_text(size=16, hjust=0),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        panel.grid.minor = element_blank(),
        axis.text.y = element_text(size = 11),
        axis.text.x = element_text(size=11, angle=90,face="bold"),
        strip.background = element_blank(),
        strip.text = element_text(size=14))+
  labs(title="time difference (min) in Landsat / Modis scenes over time",
       subtitle = paste0("n scenes = ", nrow(all), "; n unique months = ", length(unique(all$monthyear))))+
  xlab("date")+ylab("min")


ggsave(filename = "D:/new_downscaling/figures/timediff_time.png", 
       width = 10, units = "in", dpi=300, type = "cairo")

       