#### predictor ranges

library("scatterplot3d") # load
library(dplyr)

main <- "C:/Users/mleza/OneDrive/Documents/PhD/work_packages/auto_downscaling_30m/final_150000/"
files <- list.files(main, pattern=glob2rx("test?.csv"), full.names = T)

test1to3 <- lapply(seq(3), function(i){
  read.csv(files[i])
})


# summaries <- lapply(seq(test1to3), function(i){
#   summary(test1to3[[i]])
# })

cubedata <- test1to3[[3]][,c(4,6,8)]
cube_samp <- sample_n(cubedata, 80)



scatterplot3d(cube_samp, pch = 16, color="steelblue")
