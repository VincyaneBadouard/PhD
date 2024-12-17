# Prepare laz for AMAPvox

library(tidyverse)
library(lidR)
library(data.table)
library(terra)
library(raster)
library(tidyterra)
library(sf)

# Catalog
ST <- readLAScatalog( # # las catalog. filter only 1st returns
  folder = "//amap-data.cirad.fr/safe/lidar/ALS/Paracou/2023/LazClassifiedWithExtraByte", filter = "keep"
) 
ST <- st_set_crs(ST, 2972) # attribuer le dernier crs

# ROI
ROI <- vect("//amap-data.cirad.fr/work/users/VincyaneBadouard/Lidar/ALS2023/Plot16_25ha_buffer.shp") # interest zone + 100m buffer
ROI <- st_as_sf(ROI) # as sf object
ROI <- st_set_crs(ROI, 2972) # attribuer le dernier crs

# Clip the point cloud in region of interest
gc()
ST <- catalog_intersect(ST, ROI) # if laz catalog
PC <- lidR::clip_roi(las = ST, geometry = ROI) # très long

rm(ST);gc()

# Enlever le bruit
PC@data <- PC@data[Classification != 7,]

# Classifier les points sol
mntROI <- rast("//amap-data.cirad.fr/work/users/VincyaneBadouard/Lidar/ALS2023/LowAltitudeFlight/MNT/dtm2023_LowAlt_25 ha_buffer.asc")
## Normaliser la hauteur du sol
PC_norm <- lidR::normalize_height(PC, mntROI) # applati le relief

## Classifier les points sol
PC_norm@data[(Z>(-0.2) & Z< 0.5), Classification := 2] 

## Dénormaliser la hauteur du sol
PC <- lidR::unnormalize_height(PC_norm) 

# Compute Intensity
PC@data$initial_intensity <- PC@data$Intensity
# Intensité = réflectance apparente en ratio (albedo)
PC@data[, Intensity := as.integer((10^(Reflectance/10))*100)]
PC@data$initial_intensity <- NULL

# Rectifier les réflectances pour pouvoir prendre en compte les propriétés optiques des matériaux dans le calcul de la transmittance dans AMAPVox
means <- PC@data[NumberOfReturns == 1, .(mean=mean(Intensity)), by = Classification] # Reflectance mean by classification (veg, ground)

ratio <- means[Classification==2,]$mean/means[Classification==3,]$mean # ratio sol/veg reflectance 

PC@data <- PC@data %>% 
  mutate(Intensity = ifelse(Classification==2, as.integer(as.numeric(Intensity)/ratio), Intensity)) %>%  # Ground reflectance corrected by the ratio
  mutate(Classification = ifelse(Classification==2, 1L, Classification)) # for AMAPVox to give the soil without know it (?)

PC@data[, Intensity := ifelse(Intensity == 0L, 1L, Intensity)] # != 0 and integer

gc()
writeLAS(PC,
         "//amap-data.cirad.fr/work/users/VincyaneBadouard/Lidar/ALS2023/HighAltitudeFlight/LAZ/P16_2023_25ha_HighAlt_buffer_intensitycor.laz"
)

