library(ggspatial)
library(ggplot2)
require("rgdal")
require("maptools")
require("ggplot2")
require("plyr")
require("rosm")
library(cowplot) 
require("ggmap")
library("rmapshaper")
library("mapview")
library(raster)
library(sf)
#remotes::install_github("coolbutuseless/ggpattern")
library(ggpattern)

main <- "C:/Users/mleza/OneDrive/Documents/PhD/work_packages/auto_downscaling_30m/paper/paper_draft/figures/"

# # get aoi in shape for ggplot
# graph_abs <- readOGR("C:/Users/mleza/OneDrive/Documents/ausschnitt_graphical_abstract.shp")

#mapviewGetOption("basemaps")
#mapview(graph_abs, map.types = c("Esri.WorldImagery"))

aoi_levy <- readOGR(list.files("D:/new_downscaling/aoi/", pattern="actually.shp",
                   full.names=T))
aoi_levy <- spTransform(aoi_levy, crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

aoiL.p = fortify(aoi_levy, region="id")
aoiL.df <- join(aoiL.p, aoi_levy@data, by="id")

# get coastline
antacoast <- st_read("D:/Anta_map_data/Coastline_medium_res_polygon_v7.1/Coastline_medium_res_polygon_v7.1.shp")

simplepolys <- rmapshaper::ms_simplify(input = as(antacoast, 'Spatial'), keep=0.005) %>%
  st_as_sf()
antacoast <- simplepolys

sites_box <- st_make_grid(st_bbox(aoi_levy), n = 1) # mapview(sites_box) to verify
sites_box_anta <- st_transform(sites_box, crs=crs(antacoast))

# the overview map
anta_map <- ggplot() + 
  geom_sf(data = antacoast, fill="white") + 
  geom_sf(data=sites_box_anta, fill = "chartreuse1", col = "chartreuse3") +
  theme(panel.background = element_rect(color = "black", size=1, linetype = 1))+
  ylab("") + xlab("")+
  coord_sf(datum = NA) + # to rm gridlines and axes
  theme_nothing()+ # from cowplot
  labs(x = NULL, y = NULL) 

# terrain map
register_google(key="AIzaSyAvHjgxPXsQ7Zv7GiaAw4SzzJ11FnHOA5o")
map <- get_map(c(lon = 164, lat = -77.605660),
               zoom = 6, size = c(640,640), scale = 2, maptype = "terrain",
               source="google")
map_terrain_background <- get_map(c(lon = 164, lat = -77.605660),
                              zoom = 6, size = c(640,640), scale = 2, maptype = "terrain-background",
                              source="google")
map_sat <- get_map(c(lon = 164, lat = -77.605660),
               zoom = 6, size = c(640,640), scale = 2, maptype = "satellite",
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
               aes(x=long, y=lat, group = group, fill=id),
               fill = "chartreuse1", colour = "chartreuse3", alpha = 0.2)+
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(6.5, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)




map_with_inset <- ggdraw(nicemap) +
    draw_plot(anta_map, x = 0.62, y = 0.67, width = .25, height = .25)


# extract the legend from one of the plots
legend <- get_legend(
  # create some space to the left of the legend
  ggplot() +geom_polygon(data = aoiL.df,show.legend = T,
                         aes(x=long, y=lat, group = group, fill=id),
                         fill = "chartreuse1", colour = "chartreuse3", alpha = 0.2)+ 
    theme(legend.box.margin = margin(0, 0, 0, 12))
)

# add the legend to the row we made earlier. Give it one-third of 
# the width of one plot (via rel_widths).
plot_grid(map_with_inset, legend, rel_widths = c(3, .6))


map_with_inset


#ggsave(filename = paste0(main, "MDV_map_b.pdf"), width = 10, height = 8, units = "in", dpi=300, device = cairo_pdf)

ggsave(filename = paste0(main, "MDV_map_b_new.png"), width = 10, height = 8, units = "in", dpi=300, type = "cairo")

# # to save this map:
# ggsave(filename = paste0(main, "MDV_map.png"), width = 11, height=8.5, units = "in", dpi = 300)






##################### make test sites map #############################
testsites <- readOGR("D:/new_downscaling/modelling/only_test.shp")
testsites <- spTransform(testsites, crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

tests.p = fortify(testsites, region="id")
tests.df <- join(tests.p, testsites@data, by="id")

trainsites <- readOGR("D:/new_downscaling/modelling/trainsites_poly.shp")
trainsites <- spTransform(trainsites, crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

trains.p = fortify(trainsites, region="id")
trains.df <- join(trains.p, testsites@data, by="id") 


trains.df$tt <- "train"
tests.df$tt <- "test"

tt <- rbind(trains.df, tests.df)
tt$tt <- as.factor(tt$tt)

map_sat <- get_map(c(lon = 158.5, lat = -77.605660),
                   zoom = 6, size = c(640,640), scale = 2, maptype = "satellite",
                   source="google")

maptype = c("terrain",
            "terrain-background", "satellite", "roadmap", "hybrid", "toner",
            "watercolor", "terrain-labels", "terrain-lines", "toner-2010",
            "toner-2011", "toner-background", "toner-hybrid", "toner-labels",
            "toner-lines", "toner-lite")
map_sat_2 <- get_map(c(lon = 161.5, lat = -77.5),
                   zoom = 6, size = c(640,640), scale = 2, maptype = "toner-lite",
                   source="google")
plot_map <- get_stamenmap(as.numeric(bbox(sites_box)),
                          zoom = 7,
                          force = TRUE,
                          maptype = "terrain")




testsitesmap <- ggmap(map_sat, legend="right")+
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
  geom_polygon_pattern(data = trains.df, na.rm=T,size=1,
               aes(x=long, y=lat, group = group, fill=tt, colour=NA),
               color="black",
               fill            = NA,
               colour          = '#66CD00',
               pattern_spacing = 0.015,
               pattern_density = 0.05,
               pattern_fill    = '#66CD00',
               pattern_colour  = '#66CD00',
               pattern_angle   = 45)+
  geom_polygon_pattern(data = tests.df,na.rm=T,size=1,
                       aes(x=long, y=lat, group = group, fill=tt, colour=NA), 
                       color="black",
                       fill            = NA,
                       colour          = '#1b1ff5',
                       pattern_spacing = 0.015,
                       pattern_density = 0.05,
                       pattern_fill    = '#1b1ff5',
                       pattern_colour  = '#1b1ff5',
                       pattern_angle   = 45)+
  scale_fill_manual(name = '', values = c(test = "chartreuse3", train ="#ADE8FF"))

testsitesmap
ggsave(filename = paste0(main, "test_map.png"), width = 10, height = 8, units = "in", dpi=300, type = "cairo")
