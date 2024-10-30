
library(lidR)
library(sf)
library(terra)
library(tidyverse)
library(raster)

ROI <- terra::vect("//amap-data.cirad.fr/work/users/VincyaneBadouard/Lidar/ALS2022/Parcelles_Understory.shp")
ROI <- st_as_sf(ROI) # as sf object
ROI <- st_set_crs(ROI, 2972) # attribuer le dernier crs
plot(ROI)

mnt <- rast("//amap-data.cirad.fr/work/users/VincyaneBadouard/Lidar/ALS2023/HighAltitudeFlight/MNT/dtm2023_4ha_HighAlt_buffer.asc")
UAV_DEM <- rast("//amap-data.cirad.fr/work/users/VincyaneBadouard/Lidar/HovermapUAV2023/AMAPVox/MNT/dtm2023_4ha_UAV_buffer.asc")

# Region of Interest (ROI)
crs(mnt) <- crs(ROI)
crs(UAV_DEM) <- crs(ROI)

UAV_DEMROI <- raster::mask(UAV_DEM, ROI)
UAV_DEMROI <- raster::crop(UAV_DEMROI, ROI)
plot(UAV_DEMROI)

mntROI <- raster::mask(mnt, ROI)
mntROI <- raster::crop(mntROI, ROI)
plot(mntROI)

ggplot() + 
  tidyterra::geom_spatraster(data = mntROI, aes(fill = dtm2023_4ha_HighAlt_buffer)) + # mnt_roi36ha_1m # dtm2023
  scale_fill_gradientn(name = "Elevation (m)",
                       colors = terrain.colors(30, rev=T),
                       na.value="white") +
  theme_classic() +
  geom_sf(data = sf::st_cast(ROI, "LINESTRING"))

diffmnt <- mntROI - UAV_DEMROI #  -2.44 to 0.59 m
diff <- as.data.frame(diffmnt)
mean(diff$dtm2023_4ha_HighAlt_buffer) # 0.03 m
sqrt(mean((diff$dtm2023_4ha_HighAlt_buffer)^2)) # RMSE = 0.09 m

ggplot() + 
  tidyterra::geom_spatraster(data = diffmnt, aes(fill = dtm2023_4ha_HighAlt_buffer)) + # mnt_roi36ha_1m # dtm2023
  scale_fill_gradientn(name = "Elevation difference (m)",
                       colors = terrain.colors(30, rev=T),
                       na.value="white") +
  theme_classic() +
  geom_sf(data = sf::st_cast(ROI, "LINESTRING")) +
  ggtitle("Paracou P16: MNT (1m) - ALS-UAV 2023 difference") +
  theme_classic() +
  coord_sf()
