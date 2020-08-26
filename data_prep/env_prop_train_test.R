######### check for environmental properties in train and test sites


library(ggplot2)
require(reshape2)
# testsite_raster <- raster("D:/new_downscaling/modelling/testsite_raster.tif")
# auxpath <- "D:/new_downscaling/auxiliary/"

#mapview(testsite_raster)+mapview(aoianta)

# aux <- stack(paste0(auxpath, "aux_stack_xy_swir67.tif"))
# names(aux) <- c("dem", "slope", "aspect", "TWI", "soilraster", "landcoverres", "spatialblocks", "swir6", "swir7", "x", "y")


auxdf <- read.csv2("D:/new_downscaling/extraction/aux_df_swir_x_y.csv")

# subset by testsites into test and train 
testsites <- c(26, 63, 43, 12, 35, 40, 31, 79,  5,  2, 60, 11)
`%notin%` <- Negate(`%in%`)
 
auxdf$test <- NA
auxdf$test[auxdf$spatialblocks %in% testsites] <- 1
auxdf$test[auxdf$spatialblocks %notin% testsites] <- 0

head(auxdf)

plotdf <- auxdf[,c(1:5, 8,9, 12)]

# just for now to test the plot
#plotdf <- plotdf[sample(nrow(plotdf), 15000), ]

df.m <- melt(plotdf, id.var = "test")
df.m$test <- as.factor(df.m$test)
levels(df.m$variable) <- as.factor(c("DEM","slope","aspect","Topographic Wetness Index","Soil Type",
                                     "SWIR channel 6 SR","SWIR channel 7 SR"))
p <- ggplot(df.m, aes(x=variable, y=value)) + 
  geom_boxplot(aes(fill=test))+
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
  labs(title="Auxiliary Variables for test and train subsets",
       subtitle = paste0("n training samples=", nrow(plotdf[plotdf$test=="0",]), 
                         "  n test samples = ", nrow(plotdf[plotdf$test=="1",])))

ggsave(filename = "D:/new_downscaling/figures/aux_test_train.png", 
       width = 10, height = 8, units = "in", dpi=300, type = "cairo")
