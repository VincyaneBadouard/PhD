library(lidR)


# Load LiDAR data
ST <- readLAS("//amap-data.cirad.fr/work/users/VincyaneBadouard/Lidar/ALS2023/HighAltitudeFlight/P16_2023_4ha_HighAlt_buffer.laz")

# readLAS(filter = "-help")
readLAS(select = "-help")
UAV_C14C15 <- readLAS("//amap-data.cirad.fr/work/users/VincyaneBadouard/Lidar/HovermapUAV2023/P16_C14C15/FullDens_cor/UAV_P16C14C15_cor.laz",
                      select = "xyzr")
UAV_C19C20 <- readLAS("//amap-data.cirad.fr/work/users/VincyaneBadouard/Lidar/HovermapUAV2023/P16_C19C20/FullDens_cor/UAV_P16C19C20_cor.laz",
                      select = "xyzr")


# Crop the ALS
library(sf)
library(terra)
library(tidyverse)

ROI <- st_as_sf(vect("//amap-data.cirad.fr/work/users/VincyaneBadouard/Lidar/ALS2022/Parcelles_Understory.shp")) # 4 carrés
ROI <- st_as_sf(ROI) # as sf object
ROI <- st_set_crs(ROI, 2972) 

ROI_C14C15 <- ROI %>% filter(SubPlot==14 | SubPlot==15)
ROI_C19C20 <- ROI %>% filter(SubPlot==19 | SubPlot==20)

ST_C14C15 <- lidR::clip_roi(las = ST, geometry = ROI_C14C15)
ST_C19C20 <- lidR::clip_roi(las = ST, geometry = ROI_C19C20)
rm(ST, ROI, ROI_C14C15, ROI_C19C20)
gc()

ST_C14C15 ; ST_C19C20

ST_C14C15 <- rbind(ST_C14C15[[1]], ST_C14C15[[2]])
ST_C19C20 <- rbind(ST_C19C20[[1]], ST_C19C20[[2]])
ST_C14C15 ; ST_C19C20

writeLAS(ST_C14C15, "//amap-data.cirad.fr/work/users/VincyaneBadouard/Lidar/ALS2023/HighAltitudeFlight/P16_2023_HighAlt_C14C15.laz")
writeLAS(ST_C19C20, "//amap-data.cirad.fr/work/users/VincyaneBadouard/Lidar/ALS2023/HighAltitudeFlight/P16_2023_HighAlt_C19C20.laz")


# DSM (Digital Surface Model)
ST_C14C15 <- readLAS("//amap-data.cirad.fr/work/users/VincyaneBadouard/Lidar/ALS2023/HighAltitudeFlight/P16_2023_HighAlt_C14C15.laz")
ST_C19C20 <- readLAS("//amap-data.cirad.fr/work/users/VincyaneBadouard/Lidar/ALS2023/HighAltitudeFlight/P16_2023_HighAlt_C19C20.laz")

DSM_ALS_C14C15 <- rasterize_canopy(ST_C14C15, res = 0.5, algorithm = dsmtin())
DSM_ALS_C19C20 <- rasterize_canopy(ST_C19C20, res = 0.5, algorithm = dsmtin())

DSM_UAV_C14C15 <- rasterize_canopy(UAV_C14C15, res = 0.5, algorithm = dsmtin())
rm(UAV_C14C15); gc()
DSM_UAV_C19C20 <- rasterize_canopy(UAV_C19C20, res = 0.5, algorithm = dsmtin())
rm(UAV_C19C20); gc()

# Write DSM
library(raster)

terra::writeRaster(raster(DSM_ALS_C14C15),"//amap-data.cirad.fr/work/users/VincyaneBadouard/Lidar/ALS2023/HighAltitudeFlight/DSM_P16_2023_HighAlt_C14C15.asc",
            format="ascii",  varname= "Z", overwrite=T)
writeRaster(raster(DSM_ALS_C19C20),"//amap-data.cirad.fr/work/users/VincyaneBadouard/Lidar/ALS2023/HighAltitudeFlight/DSM_P16_2023_HighAlt_C19C20.asc",
            format="ascii", overwrite=T)

writeRaster(raster(DSM_UAV_C14C15),"//amap-data.cirad.fr/work/users/VincyaneBadouard/Lidar/HovermapUAV2023/DSM_P16_2023_UAV_C14C15.asc",
            format="ascii", overwrite=T)
writeRaster(raster(DSM_UAV_C19C20),"//amap-data.cirad.fr/work/users/VincyaneBadouard/Lidar/HovermapUAV2023/DSM_P16_2023_UAV_C19C20.asc",
            format="ascii", overwrite=T)

# Plot
DSM_ALS_C14C15 <- rast("//amap-data.cirad.fr/work/users/VincyaneBadouard/Lidar/ALS2023/HighAltitudeFlight/DSM_P16_2023_HighAlt_C14C15.asc")
DSM_ALS_C19C20 <- rast("//amap-data.cirad.fr/work/users/VincyaneBadouard/Lidar/ALS2023/HighAltitudeFlight/DSM_P16_2023_HighAlt_C19C20.asc")
DSM_UAV_C14C15 <- rast("//amap-data.cirad.fr/work/users/VincyaneBadouard/Lidar/HovermapUAV2023/DSM_P16_2023_UAV_C14C15.asc")
DSM_UAV_C19C20 <- rast("//amap-data.cirad.fr/work/users/VincyaneBadouard/Lidar/HovermapUAV2023/DSM_P16_2023_UAV_C19C20.asc")

library(tidyterra)

for(D in c("DSM_ALS_C14C15", "DSM_ALS_C19C20")){ 
  print(
    ggplot() + 
      geom_spatraster(data = get(D), aes(fill = Z)) + 
      scale_fill_gradientn(name = "Canopy height (m)",
                           colors = height.colors(25), na.value="white") + 
      geom_sf(data = sf::st_cast(ROI, "LINESTRING")) +
      ggtitle("Paracou P16 4ha - DSM ALS 2023 - 0.5m res") + 
      theme_classic() +
      coord_sf()
  )
}

for(D in c("DSM_UAV_C14C15", "DSM_UAV_C19C20")){
  print(
    ggplot() +
      geom_spatraster(data = DSM_UAV_C19C20, aes(fill = DSM_P16_2023_UAV_C19C20)) +
      scale_fill_gradientn(name = "Canopy height (m)",
                           colors = height.colors(25), na.value="white") +
      ggtitle("Paracou P16 4ha - DSM UAV 2023 - 0.5m res") +
      theme_classic()
  )
}

DSM_ALS_C14C15$Z
DSM_UAV_C14C15$Z
DSM_UAV_C19C20$DSM_P16_2023_UAV_C19C20
