library(lidR)
library(data.table)
library(terra)
library(raster)

options(digits = 22)

path <- 
  #C14C15
  # "//amap-data.cirad.fr/work/users/VincyaneBadouard/Lidar/ALS2023/HighAltitudeFlight/LAZ/P16_2023_HighAlt_C14C15.laz"
  # "//amap-data.cirad.fr/work/users/VincyaneBadouard/Lidar/ALS2023/LowAltitudeFlight/LAZ/P16_2023_LowAlt_C14C15.laz"
"//amap-data.cirad.fr/work/users/VincyaneBadouard/Lidar/HovermapUAV2023/AMAPVox/LAZ/P16_2023_UAV_C14C15_intensitycor.laz"
 -------------------------------------------------------------------------------
ST <- readLAS(path)
-------------------------------------------------------------------------------
# For UAV, same terrain model than ALS
mnt <- rast("//amap-data.cirad.fr/work/users/VincyaneBadouard/Lidar/HovermapUAV2023/AMAPVox/MNT/dtm2023_4ha_UAV_buffer.asc")
# Normaliser la hauteur du sol
ST <- lidR::normalize_height(ST, mnt) # applati le relief
# Classifier les points sol
ST@data[, Classification := 0]
ST@data[(Z>(-0.5) & Z< 0.642), Classification := 1] 
# Dénormaliser la hauteur du sol
ST <- lidR::unnormalize_height(ST)
-------------------------------------------------------------------------------
  

# Density ----------------------------------------------------------------------
mean(grid_density(ST, res=1)[]) 
STp <- retrieve_pulses(ST)
d <- rasterize_density(STp) # Creates a map of the point and pulses density
dim(STp@data[NumberOfReturns==ReturnNumber,]) 
plot(d)
pts=d[[1]] # points/m2
pls=d[[2]] # pulses/m2
mean(pts[][which(pts[]>0)]) # points/m2
mean(pls[][which(pls[]>0)]) # pulses/m2

# Penetration (proportion de points sol dans le dernier écho) ------------------
table(ST@data$NumberOfReturns) # ALS 2023 LowAlt: 14; HighAlt: 11; UAV : 3 echos

mean(ST@data[ReturnNumber==NumberOfReturns]$NumberOfReturns) # ALS 2023 HighAlt: 2.28 ; LowAlt: 3.22; UAV 1.48: Average number of echoes per shot

(nrow(ST@data[ReturnNumber==NumberOfReturns & Classification==1,])/nrow(ST@data[ReturnNumber==NumberOfReturns,]))*100
# 1 = sol # High: 9.6% ; Low: 18.9 % ; UAV : 3.2 % of ground points in the last echo

mean(ST@data[ReturnNumber==NumberOfReturns,Z]) # High: 31.2 ; Low: 26.1 ; UAV: 35.2 m average height of the last echo

# Footprint size ---------------------------------------------------------------
divergence <- 0.25 # mRAD divergence du lazer
h <- 500 # 1000 # 500 # fly heignt in m
tan(divergence*10^-3)*h # Footprint size in m
