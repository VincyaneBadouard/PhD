library(sf)
library(terra)
library(ggplot2)

raster <- terra::rast("~/PhD/R_codes/PhD/test/Paracou_P16_MNT.tif") # MNT
points <- sf::read_sf("~/PhD/SIG_data/LiDAR_Targets/BigTargets.shp") %>% # points
  sf::st_transform(crs = crs(raster)) # only x, y

plot(raster) ; plot(points, add = T)

points <- extract(raster, points, xy = T) # extract z

raster_df <- as.data.frame(raster, xy = TRUE) %>% # as df to plot
  na.omit()

ggplot() + 
  geom_raster(data = raster_df, aes(x = x, y = y, fill = `Z`)) +
  geom_point(data = points, aes(x = x, y = y)) +
  geom_text(data = points, aes(x = x, y = y, label = ID),
            vjust = -0.5, size = 3) +
  ggtitle("Paracou P16 - LiDAR big targets positions") + 
  coord_sf()


write.csv(points, "~/PhD/SIG_data/LiDAR_Targets/BigTargetsXYZ.csv")

points_sf <- st_as_sf(points,coords = c("x","y","Z"))
class(points_sf)
plot(points_sf)
