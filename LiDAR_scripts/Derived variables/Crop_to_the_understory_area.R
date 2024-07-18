
# Crop to the understory area

path <- 
  # "Y:/users/VincyaneBadouard/Lidar/HovermapUAV2023/AMAPVox/LAZ/P16_2023_UAV_4ha_buffer_intensitycor_lastools.laz"
# "Y:/users/VincyaneBadouard/Lidar/ALS2023/HighAltitudeFlight/LAZ/P16_2023_4ha_HighAlt_buffer_intensitycor.laz"
"Y:/users/VincyaneBadouard/Lidar/ALS2023/LowAltitudeFlight/LAZ/P16_2023_4ha_LowAlt_buffer_intensitycor.laz"

ST <- readLAS(path)

zone <- vect("D:/UAV_Vincyane/Parcelles_Understory.shp")
zone <- st_as_sf(zone) # as sf object
zone <- st_set_crs(zone, 2972)
zone1 <- st_union(zone)

ST <- st_set_crs(ST, 2972)

PC <- lidR::clip_roi(las = ST, geometry = zone) 

path <- 
  # "Y:/users/VincyaneBadouard/Lidar/HovermapUAV2023/AMAPVox/LAZ/P16_2023_UAV_4ha_intensitycor_lastools.laz"
# "Y:/users/VincyaneBadouard/Lidar/ALS2023/HighAltitudeFlight/LAZ/P16_2023_4ha_HighAlt_intensitycor.laz"
"Y:/users/VincyaneBadouard/Lidar/ALS2023/LowAltitudeFlight/LAZ/P16_2023_4ha_LowAlt_intensitycor.laz"

writeLAS(PC, path)
         
