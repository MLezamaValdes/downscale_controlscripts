
library(cdlTools)
library(rgdal)
library(raster)
library(rgeos)
library(stars)

############ make pixel index + coordinates shapefile and table #################################

# make pixel index shapefile 
r.to.poly_template <- sf::as_Spatial(sf::st_as_sf(stars::st_as_stars(template), 
                                         as_points = FALSE, merge = TRUE))
writeOGR(r.to.poly_template, "D:/new_downscaling/extraction/template_pixel_polygons_stars.shp")

# template_polygons <- rasterToPolygons(template)
template_polygons@data$id <- rownames(template_polygons@data)
template_polygons@data <- cbind(template_polygons@data, coordinates(template_polygons))
names(template_polygons@data) <- c("template_new","id", "x", "y")

# mapview(template_polygons, zcol="1")

# save shapefile and dataframe
writeOGR(template_polygons, "D:/new_downscaling/extraction/template_pixel_polygons.shp")
write.csv2(template_polygons@data, "template_pixel_polygons_dataframe.csv")