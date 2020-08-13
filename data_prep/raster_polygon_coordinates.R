

# link ID in template new with id in training dataset to put coordinates into the extracted dataset

# # check how extraction works
# r_dat <- test[]
# r_c <- coordinates(test)
# 
# r_df <- data.frame(r_dat, r_c)
# r_df$id <- seq(1:nrow(r_df))
# 
# r_ex <- extract(test, extent(test)) # raster extraction 
# r_df$ex <- r_ex
# 
# any(r_df$r_dat != r_df$ex)
# 
# head(r_df)

#### extract from template + coordinates ###################

temp_coord <- data.frame(coordinates(template))
temp_df <- cbind(seq(1:nrow(temp_coord)), temp_coord)

#temp_ex <- extract(template, aoianta)

xy <- temp_df[,c(2,3)]
spdf <- SpatialPointsDataFrame(coords = xy, data = temp_df,
                               proj4string = crs(aoianta))

o <- over(spdf, aoianta)

temp_df$in_aoi <- o$id
names(temp_df) <- c("id_complete", "x", "y", "in_aoi")

temp_df_ex <- temp_df[!is.na(temp_df$in_aoi),]
head(temp_df_ex)


# make shapefile
xy <- temp_df_ex[,c(2,3)]
aoianta_points <- SpatialPointsDataFrame(coords = xy, data = temp_df_ex,
                               proj4string = crs(aoianta))

aoianta_points$id_extr <- seq(1:nrow(aoianta_points@data))
head(aoianta_points@data)

writeOGR(aoianta_points, paste0(main, "points_id_aoianta.shp"), driver="ESRI Shapefile",
         layer="aoianta_points")



