
library(lidR)
library(terra)
library(sf)
library(raster)

# laz
ST <- readLAS("D:/Mes Donnees/PhD/Lidar/ALS2023/HighAlt/P16_2023_25ha_HighAlt_buffer50m_intensitycor.laz"
  # "//amap-data.cirad.fr/work/users/VincyaneBadouard/Lidar/ALS2023/HighAltitudeFlight/LAZ/P16_2023_25ha_HighAlt_buffer50m_intensitycor.laz"
)
ST <- st_set_crs(ST, 2972) # attribuer le dernier crs

# Region of Interest (ROI)
ROI <- vect("//amap-data.cirad.fr/work/users/VincyaneBadouard/Lidar/ALS2023/Plot16_4ha_buffer.shp") # interest zone + 100m buffer
# plot(ROI)
class(ROI) # SpatVector (terra)

ROI <- st_as_sf(ROI) # as sf object
ROI <- st_set_crs(ROI, 2972) # attribuer le dernier crs

# ST <- catalog_intersect(ST, ROI) # if laz catalog
# plot(ST)
gc()
PC <- lidR::clip_roi(las = ST, geometry = ROI) # très long
gc()
# plot(PC)
# gc()
rm(ST, ROI); gc()

writeLAS(PC,
         "D:/Mes Donnees/PhD/Lidar/ALS2023/HighAlt/P16_2023_4ha_HighAlt_buffer_intensitycormeth2.laz"
         # "//amap-data.cirad.fr/work/users/VincyaneBadouard/Lidar/ALS2023/HighAltitudeFlight/LAZ/P16_2023_4ha_HighAlt_buffer_intensitycormeth2.laz"
)

