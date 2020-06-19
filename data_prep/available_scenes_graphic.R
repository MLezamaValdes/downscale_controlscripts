# scene graphic
all <- read.csv2("D:/new_downscaling/all_comb_scenes.csv")


all$dateaschar

datcharcomp <- substring(all$MODname, 11,22)
datechar <- paste0(substring(datcharcomp, 1,4), "_", substring(datcharcomp, 5,7),"_",substring(datcharcomp, 9,10),":",substring(datcharcomp, 11,12))

dat <- as.POSIXlt(datechar, format="%Y_%j_%H:%M", tz="Pacific/Auckland")
all$dat <- dat

tail(dat)
tail(datechar)

library(lubridate)
h <- hour(dat)
m <- month(dat)

boxplot(h)
boxplot(m)

hist(m, breaks=30)

hist(h, breaks=30)

all$minutesdiff <- all$timediff*60

all[all$minutesdiff<30,]

max(all$minutesdiff)
plot(all$dat, all$minutesdiff)

library(ggplot2)
p <- ggplot(all, aes(minutesdiff)) + 
  geom_boxplot()
p


geom_boxplot(outlier.colour="black", outlier.shape=16,
             outlier.size=2, notch=FALSE)
th <- table(h)


boxplot(m)
