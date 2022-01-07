library(here)
library(raster)

predstackdir <- "D:/downscaling_after_talk/spatial_predictions_rf/predstacks/"

predstack <- stack(list.files(predstackdir, 
                              pattern="predstack_MYD11_L2.A2018316.1350.006.2.tif$", 
                              full.names = T))
psnams <- read.csv2(list.files(predstackdir, 
                               pattern=".csv", full.names = T))

names(predstack) <- psnams$x


predstack
plot(predstack[[1]])

# 38 Mio Pixel
# cut into 9 chunks
source("C:/Users/mleza/OneDrive/Documents/PhD/work_packages/auto_downscaling_30m/downscale_controlscripts/data_prep/downscaling_organized/Palma/LST_after_paper/SplitRas.R")
spoutdir <- paste0(predstackdir, "splits/")
outnam <- modname
dir.create(spoutdir)

splitfiles <- SplitRas(raster=predstack, ppside=3, save=TRUE, plot=FALSE,
                       outdir=spoutdir, outnam=outnam)

