library(ggspatial)
library(ggplot2)
require("rgdal") # requires sp, will use proj.4 if installed
require("maptools")
require("ggplot2")
require("plyr")
require("rosm")
library(cowplot) ## MOOOOOOO!!
require("ggmap")

# get aoi in shape for ggplot
aoi.p = fortify(aoi, region="id")
aoi.df <- join(aoi.p, aoi@data, by="id")

aoi_levy <- readOGR(list.files("D:/new_downscaling/aoi/", pattern=".shp",
                   full.names=T))
aoiL.p = fortify(aoi_levy, region="id")
aoiL.df <- join(aoiL.p, aoi@data, by="id")

# get coastline
antacoast <- st_read("D:/Anta_map_data/Coastline_medium_res_polygon_v7.1/Coastline_medium_res_polygon_v7.1.shp")

sites_box <- st_make_grid(st_bbox(aoi), n = 1) # mapview(sites_box) to verify
sites_box_anta <- st_transform(sites_box, crs=crs(antacoast))

mapview(aoianta)+mapview(sites_box_anta)
mapview(antacoast)+mapview(sites_box_anta)

anta_map <- ggplot() + 
  geom_sf(data = antacoast, fill=NA) + 
  geom_sf(data=sites_box_anta, fill = "chartreuse1", col = "chartreuse3") +
  coord_sf(datum = NA) + # to rm gridlines and axes
  theme_nothing()+ # from cowplot
  labs(x = NULL, y = NULL) +
  theme(
    #panel.grid = element_blank(), # remove gridlines,
    #plot.margin = unit(c(0.1, 0.1, 0, 0), "cm"),  #top, right, bottom, left
    panel.background = element_rect(color = "black", size=1, linetype = 1))+
  ylab("") + xlab("")


register_google(key="AIzaSyAvHjgxPXsQ7Zv7GiaAw4SzzJ11FnHOA5o")
map <- get_map(c(lon = 164, lat = -77.605660),
               zoom = 6, size = c(640,640), scale = 2, maptype = "terrain",
               source="google")


nicemap <- ggmap(map)+
  labs(x = "longitude", y="latitude")+
  theme(plot.title = element_text(colour = "gray15"),
        panel.background = element_rect(fill = "#F5F5F5"),
        axis.text.x = element_text(face="bold", color="gray15", 
                                   size=8),
        axis.text.y = element_text(face="bold", color="gray15", 
                                   size=8),
        axis.title.x = element_text(face="bold", color="gray15", 
                                    size=8),
        axis.title.y = element_text(face="bold", color="gray15", 
                                    size=8))+
  scale_x_continuous(breaks=c(160,  164,  168),limits=c(157,170.5))+
  scale_y_continuous(breaks=c(-79, -78, -77, -76),limits=c(-79,-76))+
  geom_polygon(data = aoiL.df,
               aes(long, lat, group = group),
               fill = "chartreuse1", colour = "chartreuse3", alpha = 0.2)+
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(6.5, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)



(map_with_inset <-
    ggdraw(nicemap) +
    draw_plot(anta_map, x = 0.62, y = 0.67, width = .25, height = .25))

#ggsave(filename = paste0(main, "MDV_map_b.pdf"), width = 10, height = 8, units = "in", dpi=300, device = cairo_pdf)

ggsave(filename = paste0(main, "MDV_map_b.png"), width = 10, height = 8, units = "in", dpi=300, type = "cairo")

# # to save this map:
# ggsave(filename = paste0(main, "MDV_map.png"), width = 11, height=8.5, units = "in", dpi = 300)
# 




library(USAboundaries) # get outline of CA


CA <- us_boundaries(type="state", resolution = "high", states="California")

# plot(CA$geometry) # quick test plot

# make a grid box for where our localities are, use n=1 to make single polygon
sites_box <- st_make_grid(st_bbox(snw_crop), n = 1) # mapview(sites_box) to verify

# set margins:
par(mai=c(.1,0.1,0.1,0.1))

# now make map
(ca_map <- ggplot() + 
    geom_sf(data = CA, fill=NA) + 
    geom_sf(data=sites_box, col="black", fill="gray40") +
    coord_sf(datum = NA) + # to rm gridlines and axes
    theme_nothing()+ # from cowplot
    labs(x = NULL, y = NULL) +
    theme(
      #panel.grid = element_blank(), # remove gridlines,
      #plot.margin = unit(c(0.1, 0.1, 0, 0), "cm"),  #top, right, bottom, left
      panel.background = element_rect(color = "black", size=1, linetype = 1))+
    ylab("") + xlab(""))


