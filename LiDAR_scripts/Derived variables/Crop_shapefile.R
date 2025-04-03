
# Crop shapefile
library(sf)


ROI <- st_as_sf(vect("//amap-data.cirad.fr/work/users/VincyaneBadouard/Lidar/ALS2022/Shapefiles/Parcelles_Understory.shp")) # 4 carrés

Sb19 <- ROI %>% filter(SubPlot == 19)

plot(ROI$SubPlot)
plot(Sb19, add=T)

ggplot() +
geom_sf(data = sf::st_cast(ROI, "LINESTRING")) +
geom_sf(data = sf::st_cast(Sb19, "LINESTRING"), col="red")


write_sf(Sb19, "//amap-data.cirad.fr/work/users/VincyaneBadouard/Lidar/ALS2023/Shapefiles/P16_C19_1ha.shp")
