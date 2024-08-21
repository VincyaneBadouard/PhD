library(lidR)
library(data.table)

options(digits = 22)

path <- 
  #C14C15
  # "//amap-data.cirad.fr/work/users/VincyaneBadouard/Lidar/ALS2023/HighAltitudeFlight/LAZ/P16_2023_HighAlt_C14C15.laz"
  # "//amap-data.cirad.fr/work/users/VincyaneBadouard/Lidar/ALS2023/LowAltitudeFlight/LAZ/P16_2023_LowAlt_C14C15.laz"
"//amap-data.cirad.fr/work/users/VincyaneBadouard/Lidar/HovermapUAV2023/AMAPVox/LAZ/P16_2023_UAV_C14C15_intensitycor.laz"

 -------------------------------------------------------------------------------
ST <- readLAS(path)

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

mean(ST@data$NumberOfReturns) # ALS 2023 HighAlt: 2.8 ; LowAlt: 4.0; UAV 1.70: Average number of echoes per shot

(nrow(ST@data[ReturnNumber==NumberOfReturns & Classification==1,])/nrow(ST@data[ReturnNumber==NumberOfReturns,]))*100
# 1 = sol # High: 9.6% ; Low: 18.9 % ; UAV :  1.24 % of ground points in the last echo

mean(ST@data[ReturnNumber==NumberOfReturns,Z]) # High: 31.2 ; Low: 26.1 ; UAV: 35.2 m average height of the last echo

# Footprint size ---------------------------------------------------------------
divergence <- 0.25 # mRAD divergence du lazer
h <- 500 # 1000 # 500 # fly heignt in m
tan(divergence*10^-3)*h # Footprint size in m
