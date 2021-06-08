######### check for environmental properties in train and test sites


library(ggplot2)
require(reshape2)

dat <- read.csv2(paste0(tdipath, "training_18_19_n4000.csv"))
test <- read.csv2(paste0(testpath, "test_18_19_n4000.csv"))

#### prep ---------------------------------------------------

dat$tt <- "train"
test$tt <- "test"

dat$time_num <- NULL

names(test)
names(dat)

df <- rbind(dat,test)

head(df)

dfnam <- c("Landsat", "Modis","ia"  ,"hs","dem","slope","aspect","TWI" ,        
            "soilraster","landcoverres","spatialblocks" ,"x","y","swir6","swir7","xd" ,
            "modtime","ymo","hmi","Lscene","Mscene","tt")

plotnam <-  c("Landsat", "Modis","ia","hs","dem","slope","aspect","TWI",
              "soilraster","landcoverres","spatialblocks", "swir6","swir7","tt")    

plotdf <- df[,plotnam]
str(plotdf)

#### Comparison training and test subsets ---------------------------------------------------


df.m <- melt(plotdf, id.var = "tt")
df.m$test <- as.factor(df.m$tt)
levels(df.m$variable) <- as.factor( c("Landsat", "Modis","incidence angle","hillshading","DEM","slope","aspect","Topographic Wetness Index",
                                      "Soil Type","Open Soil or Snow and Ice","CV blocks", "SWIR channel 6 SR","SWIR channel 7 SR","tt"))
p <- ggplot(df.m, aes(x=variable, y=value)) + 
  geom_boxplot(aes(fill=tt))+
  theme_bw() +
  theme(legend.title = element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        panel.grid.major = element_blank(), 
        plot.title = element_text(lineheight=.8, face="bold", size = 16),
        plot.subtitle = element_text(size = 12, face="bold"),
        legend.text=element_text(size=14, hjust = 0),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size=14))+
  scale_fill_manual(values=c("gray29","gray87"),
                    labels=c("Train", "Test"),
                    breaks=c(0,1))

p + facet_wrap( ~ variable, scales="free")+ylab(" ")+xlab(" ")+
  labs(title="Comparison of test and train subsets",
       subtitle = paste0("n training samples=", nrow(plotdf[plotdf$tt=="train",]), 
                         "  n test samples = ", nrow(plotdf[plotdf$tt=="test",])))

ggsave(filename = "D:/new_downscaling/figures/test_train.png", 
       width = 10, height = 8, units = "in", dpi=300, type = "cairo")

#### how many samples from which time step ---------------------------------------------------

# clean labels up etc.


year <- substring(dat$ymo, 1,4)
month <- substring(dat$ymo, 6,7)
availableMonths <- data.frame(year,month)

hist(as.numeric(availableMonths$month), main="training samples per month")
hist(as.numeric(availableMonths$year), main="training samples per year")


qplot(as.numeric(availableMonths$month), geom="histogram",
      xlab=c("Jan", "Feb", "March")) 

#### how many samples from which time step ---------------------------------------------------

# not happy with the keys yet

library('plot.matrix')

m <- c(1,2,3,9,10,11,12)
y <- c(2013,2014,2015,2016,2017,2018,2019)

# 0 = not available
# 1 = available, used in training 
# 2 = test

code <- c(0,0,0,0,0,1,2,
          0,0,0,0,0,2,1,
          1,2,0,0,1,1,0,
          2,1,0,0,1,1,1,
          1,1,0,0,1,1,1,
          1,1,0,0,2,1,1,
          1,1,1,2,1,1,1)


cdf <- data.frame(matrix(code, ncol=length(m)),
           row.names = m)
names(cdf) <- y

cdfm <- data.matrix(cdf)
plot(cdfm, xlab="year", ylab="month", col=c("#f0f0f0",  "#bdbdbd", "#636363"), 
     main="training, test and unavailable months", fmt.key="%.0f", key=list())



