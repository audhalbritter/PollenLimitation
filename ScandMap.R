### map scandinavia
library("raster")
library("ggplot2")
library("dplyr")

elev <- getData('worldclim', var='alt', res=2.5)
e <- extent(2,22,55,72)
elev.scand <- crop(elev, e)

# To convert your RasterLayer to a data.frame, you need to convert it to
# a SpatialPixelsDataFrame first
elev.scand.spdf <- as(elev.scand, "SpatialPixelsDataFrame")
elev.scand.df <- as.data.frame(elev.scand.spdf)

# plot China map
border <- map_data("world")
ScandMap <- ggplot() +
  geom_raster(data = elev.scand.df, aes(x=x, y=y, fill = alt)) +
  #geom_map(aes(map_id = region), data = border, map = border, fill = NA, color = "white") +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  coord_equal() +
  scale_fill_gradient(name = "Elevation", low = "grey0", high = "grey100", limits=c(0,2000)) + 
  #geom_segment(aes(x = 4, y = 60, xend = 8, yend = 62, colour = "red")) +
  #geom_rect(mapping=aes(xmin=4, xmax=8, ymin=60.5, ymax=61.5), color="black", alpha = 0) +
  #geom_point(aes(x=long, y=lat, colour  = "red"), data = coords, size=2, show.legend = FALSE) +
  labs(x = "", y = "") +
  theme_minimal()

ggsave("ScandMap.pdf")


library(maps)
x11(width = 4, height = 6.5)
par(mar = rep(0.1,4))

#force map to cover Norway, Finland and Latvia
map(region = c("Norway", "Finland", "Latvia"), proj = "sinusoidal", type = "n")
map.grid(c(0,40, 55, 80), nx = 5, ny = 5, col = "black")
#map everything that does not start with "Lake"
map(xlim = c(-130, 130), region = "(?!Lake *)", col = "grey80", fill = TRUE, proj = "", add = TRUE)

box()
