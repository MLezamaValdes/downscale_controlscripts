template
plot(template)
e <- drawExtent()

template <- crop(template, e)

# make pixel index
plot(template)

length(template[])

tdf <- extract(template, aoianta)
length(tdf[[1]])

mod <- raster("C:/Users/mleza/OneDrive/Desktop/testsite/modis.tif")
moddf <- extract(mod,aoianta)
length(moddf[[1]])

template_polygons <- rasterToPolygons(template)

r <- raster(ncol=36, nrow=18, vals=1:(18*36))
plot(r)
###############################
# extract values by cell number
###############################
extract(r, c(1:2, 10, 100))
s <- stack(r, sqrt(r), r/r)
extract(s, c(1, 10, 100), layer=2, n=2)
extract(s, c(1,10,100))
