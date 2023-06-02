library(raster)
shpParacou <- rgdal::readORG(dsn = "C:/Users/BADOUARD/Desktop/Vincyane/Gaps2019/Gaps2019", layer = "Gaps2019")  
shpParacou <- raster::shapefile("C:/Users/BADOUARD/Desktop/Vincyane/Gaps2019/Gaps2019.shp")

library(sf)
shpParacou <- st_read("C:/Users/BADOUARD/Desktop/Vincyane/Gaps2019/Gaps2019.shp")
shpP16 <- st_read("C:/Users/BADOUARD/Desktop/Vincyane/Plots Paracou/OverallPlots.shp")


plot(shpParacou)
plot(shpParacou, col="#f2f2f2", bg="skyblue", lwd=0.25, border=0 )

library(ggplot2)
ggplot() + 
  geom_sf(data = shpParacou, size = 3, color = "black", fill = "red") + 
  ggtitle("2019 - P16 gaps") + 
  coord_sf()

ggplot() + 
  geom_sf(data = shpP16, size = 3, color = "black", fill = "red") + 
  ggtitle("2019 - P16 gaps") + 
  coord_sf()
