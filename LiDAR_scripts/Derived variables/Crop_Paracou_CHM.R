 # CHM 25ha from Paracou CHM
library(tidyverse)
library(terra)
library(raster)
# library(lidR)
library(sf)

mask <- st_as_sf(vect("//amap-data.cirad.fr/work/users/VincyaneBadouard/Lidar/ALS2023/Shapefiles/Plot16_25ha_buffer.shp")) # + 50m buffer
mask <- st_as_sf(mask) # as sf object
mask <- st_set_crs(mask, 2972) 

CHM_Paracou <- rast("//amap-data.cirad.fr/safe/lidar/ALS/Paracou/2023/rasters/PARACOU2023_CHM_1m_filt.tif")
plot(CHM_Paracou)

# raster::crs(CHM_Paracou) <- st_set_crs(mask) # attribuer le crs

cro <- terra::crop(CHM_Paracou, mask)
CHM_P16_buffer <- terra::mask(cro, mask)

plot(CHM_P16_buffer)


terra::writeRaster(CHM_P16_buffer,
                   "//amap-data.cirad.fr/work/users/VincyaneBadouard/Lidar/ALS2023/HighAltitudeFlight/Paracou_P16_2023_25ha_50mbuffer_HighAlt_CHM_1mres.tif",
                   overwrite=TRUE)

