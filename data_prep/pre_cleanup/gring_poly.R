sds <- get_subdatasets(hdffilepath)

gdalinfo(sds[1])

gdi <- gdalinfo(sds[1])

gdigeoloc <- gdalinfo(sds[8])



# gring define polygons
gdigeoloc[which(grepl("GRING", gdigeoloc))]
latpoints <- c(-69.9352239908737, -83.0847572338393, -77.6498107232079, -67.3282892773792, NA)
lonpoints <- c(-113.893610323958, -16.1413904059366, 123.211390385347, -164.843978001036, NA)

xym <- cbind(lonpoints, latpoints)
xym[5,] <- xym[1,]
xym

xyml <- as.list(data.frame(t(xym)))
stp <- st_polygon(list(xym))
stp <- as(stp, "Spatial")
crs(stp) <- wgsproj

stp <- as(stp, "sf")
stp

mapview(stp)
stp_anta <- st_transform(stp, st_crs(aoianta))
mapview(stp_anta)

write_sf(stp_anta, "polygonpoints.shp", overwrite=T)


### try and get info out of that
#Columns/Rows	2030 x 1354


extent(stp_anta)/2030
