# test swir variance ####################


library(reshape2)
library(ggplot2)

y=2
m=7
ym <- substring(time_range[[y]][[m]][[1]][1],1,7)

swiroutpath <-"D:/new_downscaling/SWIR/composites/"

LCpath <- paste0(swiroutpath, ym, "/")

f <- list.files(LCpath, pattern="LC08", full.names = T)
files <- lapply(seq(f), function(i){
  stack(f[i])
})

b6 <- lapply(files, `[[`, 1)

days <- sapply(seq(b6), function(i){
  substring(names(b6[[i]]), 24,25)
})
print(days)




# resample by template
b6res <- lapply(seq(b6), function(i){
  try(resample(b6[[i]], template))
})


# which are viable rasters
ras_av <- sapply(seq(b6res), function(i){
  class(b6res[[i]])!="try-error"
})

# get names
ras_nam <- sapply(seq(b6res), function(i){
  if(ras_av[[i]]){
    x <- paste0("swir_",substring(names(b6res[[i]]), 18,25))
  } else {x <- NA}
  x
})

# stack
b6s <- stack(b6res[ras_av])
names(b6s) <- ras_nam[ras_av]

plot(b6s)



b6sdat <- b6s[[c(3:6, 8)]]
b6s_extract <- b6sdat[]

swirdf <- data.frame(b6s_extract)
maxna <- function(x){
    max(x, na.rm=T)
  }
  
# sum up for the same day
u <- substring(names(swirdf), 11,13)
res_u <- lapply(seq(unique(u)), function(i){
  print(i)
  nams <- c(names(swirdf)[u==unique(u)[i]])
  xu <- swirdf[,nams]
  if(!is.null(ncol(xu))){
    xu <- rowSums(xu, na.rm=T)
  }
  return(as.data.frame(xu))
})

length(res_u)

df <- data.frame(res_u[[1]], res_u[[2]], res_u[[3]], res_u[[4]], res_u[[5]])
names(df) <- unique(u)
df <- df[order(as.numeric(names(df)))]
dfc <- df[complete.cases(df),]
dfc$id <- rownames(dfc)

# take some samples
dfc.s <- dfc[sample(rownames(dfc), 500),]
colMeans(dfc[1:5])
dfc.s <- dfc.s[,c(1,5,6)]

# melt df
dfc.m <- melt(dfc.s, value.name = "SWIR", 
     variable.name = "date", id="id")


# make a trend for all developments over 1 to 3
ggplot(dfc.m, aes(x=date, y=SWIR, color=id,group=id))+
  theme(legend.position = "none") +
  #geom_line(color="grey", size=0.2)+
  geom_smooth()


cor(dfc$`101`, dfc$`119`)

# Trend with the rasters
library(greenbrown)

b6sdat <- b6sdat[[c(2,5,4,3,1)]]
mapview(b6sdat)

trend <- TrendRaster(b6sdat, start = c(2018, 11), 
                     freq = 365, method="AAT")
plot(trend)

### Only look at significant trends:
mask <- trend$SlopeSEG1
mask[trend$PvalSEG1>0.05] <- NA
masked_trend <- mask(trend$SlopeSEG1,mask)
spplot(masked_trend)

writeRaster(masked_trend, filename = "trend.tif",overwrite=TRUE)

