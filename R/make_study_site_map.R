### An R script to make a study site map
### Most likely will be Figure 1 of the paper

# load packages
library(ggmap)
library(ggplot2)


# get location
location <- get_map(location = c(lon = 150.379432, lat = -33.714682), zoom = 12.5, maptype = "hybrid")

# Create a small data frame to pass to ggplot
fourCorners <- expand.grid(
  lon = as.numeric(attr(location, "bb")[, c("ll.lon", "ur.lon")]), 
  lat = as.numeric(attr(location, "bb")[, c("ll.lat", "ur.lat")])
)
# The inset_raster function needs 4 data coordinates. Pull it out of your "location" that you got via get_map
xmin <- attr(location, "bb")$ll.lon
xmax <- attr(location, "bb")$ur.lon
ymin <- attr(location, "bb")$ll.lat
ymax <- attr(location, "bb")$ur.lat

# Now you are ready to plot it
mp <- ggplot(fourCorners, aes(x = lon, y = lat))+ 
  inset_raster(location, xmin, xmax, ymin, ymax)            

mp

# this gives us our basemap
# now we need to add each transect
# create a dataset which has the 
# transect name and the coords of each point

Hazelbrook <- data.frame(Transect="Hazelbrook", 
                         Point=c("A", "B", "C", "D", "E"), 
                         lat=c(-33.722225, -33.719572, -33.716511, -33.713083, -33.709178),
                         lon=c(150.455764, 150.460150, 150.46242, 150.467742, 150.470581))

Lawson <- data.frame(Transect="Lawson", 
                         Point=c("A", "B", "C", "D", "E"), 
                         lat=c(-33.722906, -33.718583, -33.714411, -33.709956, -33.705528),
                         lon=c(150.429094, 150.430475, 150.430883, 150.431783, 150.432522))

Woodford <- data.frame(Transect="Woodford", 
                       Point=c("A", "B", "C", "D", "E"), 
                       lat=c(-33.735461, -33.739739, -33.743503, -33.747961, -33.752467),
                       lon=c(150.482669, 150.484017, 150.485453, 150.486033, 150.485378))

Katoomba <- data.frame(Transect="Katoomba", 
                       Point=c("A", "B", "C", "D", "E", "F", "G", "H", "I"), 
                       lat=c(-33.714147, -33.709839, -33.705294, -33.700856, -33.696481, -33.691944, -33.687356, -33.682797, -33.678444),
                       lon=c(150.311656, 150.313153, 150.312664, 150.313456, 150.314444, 150.314814, 150.314789, 150.314722, 150.316222))



# now add the transects to the map made above
# and make the map look prettier
mp <- ggplot(fourCorners, aes(x = lon, y = lat))+ 
  inset_raster(location, xmin, xmax, ymin, ymax)+
  geom_smooth(data=Hazelbrook, aes(x=lon, y=lat), method="lm", se=FALSE, color="green2", size=2.5, alpha=0.9)+
  geom_smooth(data=Katoomba, aes(x=lon, y=lat), method="lm", se=FALSE, color="green2", size=2.5)+
  geom_smooth(data=Woodford, aes(x=lon, y=lat), method="lm", se=FALSE, color="green2", size=2.5)+
  geom_smooth(data=Lawson, aes(x=lon, y=lat), method="lm", se=FALSE, color="green2", size=2.5)+
  geom_point(data=Hazelbrook, aes(x=lon, y=lat), shape=17, size=2, color="red")+
  geom_point(data=Woodford, aes(x=lon, y=lat), shape=17, size=2, color="red")+
  geom_point(data=Lawson, aes(x=lon, y=lat), shape=17, size=2, color="red")+
  geom_point(data=Katoomba, aes(x=lon, y=lat), shape=17, size=2, color="red")+
  theme_bw()+
  xlab("Longitude")+
  ylab("Latitude")+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+
  theme(axis.ticks=element_blank())+
  coord_equal()+
  theme(axis.text=element_text(color="black"))+
  theme(panel.grid.minor=element_blank())+
  theme(panel.grid.major=element_blank())

mp

# now let's add an inset for reference
# # create bew location data
library(mapdata)

aus.map <- data.frame(map('worldHires', 'Australia')[c('x', 'y')])

mp2 <- ggplot(aus.map, aes(x = x, y = y)) +
  geom_path() +
  xlim(c(110, 155)) +
  ylim(c(-48, -10)) +
  xlab('') +
  ylab('') +
  theme_classic() +
  coord_equal()+
  theme(axis.text=element_blank())+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+
  theme(axis.ticks=element_blank())+
  geom_point(aes(x=150.379432, y=-33.714682), shape=22, color="red", size=2)

mp2

## write out both maps to put together in adobe
## this requires a bit of post-R work, but not much
## may make tweaks to this in time, but fine for now
pdf("Figures/Figure1/base.pdf", height=8, width=6.5)
print(mp)
dev.off()


ggsave(mp2, filename="Figures/Figure1/inset.png", height=3, width=3, units="in", dpi=300)













