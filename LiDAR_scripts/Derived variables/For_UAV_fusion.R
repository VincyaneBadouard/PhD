library(lidR)


# Load LiDAR data
ST <- readLAS("//amap-data.cirad.fr/work/users/VincyaneBadouard/Lidar/ALS2023/HighAltitudeFlight/P16_2023_4ha_HighAlt_buffer.laz")

# readLAS(filter = "-help")
# readLAS(select = "-help")
UAV_C14C15 <- readLAS("//amap-data.cirad.fr/work/users/VincyaneBadouard/Lidar/HovermapUAV2023/P16_C14C15/Output-(2)_subsampled_laz1_4.laz",
                      select = "xyztr")
UAV_C19C20 <- readLAS("//amap-data.cirad.fr/work/users/VincyaneBadouard/Lidar/HovermapUAV2023/P16_C19C20/Output_subsampled_laz1_4.laz",
                      select = "xyztr")
# Message d'avis :
# Invalid data: 28460622 points with a return number equal to 0 found


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

# Crop UAV with the trajectory
library(data.table)
traj_C14C15 <- fread("//amap-data.cirad.fr/work/users/VincyaneBadouard/Lidar/HovermapUAV2023/P16_C14C15/C14C15_Output_traj.xyz")
traj_C19C20 <- fread("//amap-data.cirad.fr/work/users/VincyaneBadouard/Lidar/HovermapUAV2023/P16_C19C20/Output_traj.xyz")
rgl::plot3d(traj_C14C15[,3:5], aspect=F) # plot the trajectory (x,y,z)

# options(digits=22)
# range(traj$gpstime) # 1697736241.571922063828 1697737554.552948951721
# range(UAV_C14C15@data$gpstime) # 1697736245.296575307846 1697737554.548056840897
traj_crop <- traj_C14C15[y>=770 & x>(-250) & x<50,]
traj_crop <- traj_C19C20[y>=750 & x>(-500) & x<(-150),] 

rgl::plot3d(traj_crop[,3:5], aspect=F) # plot the trajectory (x,y,z)

UAV_C14C15_crop <- UAV_C14C15
UAV_C14C15_crop@data <- UAV_C14C15_crop@data[Y>=770 & Y<1075 & X>(-250) & X<50,]

UAV_C19C20_crop <- UAV_C19C20
UAV_C19C20_crop@data <- UAV_C19C20_crop@data[Y>=750 & Y<1050 & X>(-450) & X<(-200),]

writeLAS(UAV_C14C15_crop, "//amap-data.cirad.fr/work/users/VincyaneBadouard/Lidar/HovermapUAV2023/P16_C14C15/UAV_C14C15_subsampled_croptraj.laz")
writeLAS(UAV_C19C20_crop, "//amap-data.cirad.fr/work/users/VincyaneBadouard/Lidar/HovermapUAV2023/P16_C19C20/UAV_C19C20_subsampled_croptraj.laz")

rm(UAV_C14C15, traj_C14C15, traj_crop)
rm(UAV_C19C20, traj_C19C20, traj_crop)


# DSM (Digital Surface Model)
ST_C14C15 <- readLAS("//amap-data.cirad.fr/work/users/VincyaneBadouard/Lidar/ALS2023/HighAltitudeFlight/P16_2023_HighAlt_C14C15.laz")
ST_C19C20 <- readLAS("//amap-data.cirad.fr/work/users/VincyaneBadouard/Lidar/ALS2023/HighAltitudeFlight/P16_2023_HighAlt_C19C20.laz")

DSM_ALS_C14C15 <- rasterize_canopy(ST_C14C15, res = 0.5, algorithm = dsmtin())
DSM_ALS_C19C20 <- rasterize_canopy(ST_C19C20, res = 0.5, algorithm = dsmtin())

DSM_UAV_C14C15 <- rasterize_canopy(UAV_C14C15_crop, res = 0.5, algorithm = dsmtin())
rm(UAV_C14C15_crop); gc()
DSM_UAV_C19C20 <- rasterize_canopy(UAV_C19C20_crop, res = 0.5, algorithm = dsmtin())
rm(UAV_C19C20_crop); gc()

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

raster::plot()
terra::plot()

library(tidyterra)

for(D in c("DSM_ALS_C14C15", "DSM_ALS_C19C20")){ 
  print(
    ggplot() + 
      geom_spatraster(data = get(D), aes(fill = Z)) + 
      scale_fill_gradientn(name = "Canopy height (m)",
                           colors = height.colors(25), na.value="white") + 
      geom_sf(data = sf::st_cast(ROI, "LINESTRING")) +
      ggtitle(paste("Paracou P16 - ", D," - 0.5m res")) + 
      theme_classic() +
      coord_sf()
  )
}

for(D in c("DSM_UAV_C14C15", "DSM_UAV_C19C20")){
  print(
    ggplot() +
      geom_spatraster(data = get(D), aes(fill = Z)) +
      scale_fill_gradientn(name = "Canopy height (m)",
                           colors = height.colors(25), na.value="white") +
      ggtitle(paste("Paracou P16 - ", D," - 0.5m res")) +
      theme_classic()
  )
}

DSM_ALS_C14C15$Z
DSM_UAV_C14C15$Z
DSM_UAV_C19C20$DSM_P16_2023_UAV_C19C20
