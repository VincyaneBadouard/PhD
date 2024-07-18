
library(lidR)
library(data.table)

laz <- readLAS("Y:/users/VincyaneBadouard/Lidar/HovermapUAV2023/AMAPVox/LAZ/P16_2023_UAV_4ha_buffer_intensitycor_lastools.laz")
lascheck(laz)

d <- max(laz@data$Z)-2
laz@data <- laz@data[Z<d,] # enlever le drone
max(laz@data$Z)

writeLAS(laz, "Y:/users/VincyaneBadouard/Lidar/HovermapUAV2023/AMAPVox/LAZ/P16_2023_UAV_4ha_buffer_intensitycor_lastools.laz")

