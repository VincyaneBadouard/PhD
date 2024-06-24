# ça serait bien d'en faire des fonctions
library(lidR)
library(data.table)

local <- "//amap-data.cirad.fr/work/users/VincyaneBadouard/Lidar/"
# local <- "Y:/"

# laz_GroundClassif -> ST
ST <- readLAS(paste(local, "HovermapUAV2023/Processed/UAV_C19C20_translated_merged_CloudCompareClassif.las", sep = ""))
# ST <- readLAS(paste(local, "ALS2023/lowAltitudeFlight/LowFlight_alt4ha_buff100m_2023_RefAsInt.laz", sep = ""))
#"ALS2023/HighAltitudeFlight/HighFlight_alt4ha_buff100m_2023_RefAsInt.laz"
# "ALS2023/lowAltitudeFlight/LowFlight_alt4ha_buff100m_2023_RefAsInt.laz"
# "ALS2022/P16_2022_4ha_buffer.laz"

ST
summary(ST@data)
lidR::las_check(ST) # lidR checks

# UAV C19C20:
# ⚠ 36179 points are duplicated and share XYZ coordinates with other points
# ✗ 846416 pulses (points with the same gpstime) have points with identical ReturnNumber

# crs
crs(ST)

# Density
mean(grid_density(ST, res=1)[]) 
STp <- retrieve_pulses(ST)
d <- rasterize_density(STp) # Creates a map of the point and pulses density
dim(STp@data[NumberOfReturns==ReturnNumber,]) 
plot(d)
pts=d[[1]] # points/m2
pls=d[[2]] # pulses/m2
mean(pts[][which(pts[]>0)]) # points/m2
mean(pls[][which(pls[]>0)]) # pulses/m2

# Footprint size
divergence <- 0.25 # mRAD divergence du lazer
h <- 500 # fly heignt in m
tan(divergence*10^-3)*h # Footprint size in m

# Penetration (proportion de points sol dans le dernier écho)
table(ST@data$NumberOfReturns) # 14 echos ALS 2023 LowAlt; 12 HighAlt
min(ST@data$Z) # High : -2.34 ; Low : 4.06
(nrow(ST@data[ReturnNumber==NumberOfReturns & Classification==2,])/nrow(ST@data[ReturnNumber==NumberOfReturns,]))*100 # 2 = sol # High: 1.75% ; Low: 8.70 % of ground points in the last echo
alt = 1000 # 1000 ; 500
alt-mean(ST@data[ReturnNumber==NumberOfReturns & Classification==2,Z]) # High: 987 ; Low: 487 m distance

# Si l'intensité n'est pas déjà la réflectance apparente :
# ST@data$initial_intensity <- ST@data$Intensity
# ST@data[, Intensity := as.integer((10^(Reflectance/10))*100)] # Réflectance initialement en decibel (et selon une référence connue)

hist(ST@data[,Intensity])
range(ST@data[,Intensity]) # High: [1;625] ; Low: [0;29] of intensity
mean(ST@data[ReturnNumber==NumberOfReturns & Classification==2,Intensity]) # High: 8.49 ; Low:1.3 of intensity in average at the ground
mean(ST@data[Classification==2,Intensity]) # High: 8.6 ; Low: 1.3 of intensity in average at the ground
mean(ST@data[ReturnNumber==1,Intensity]) # High: 22.1 ; Low: 5.8 of intensity in average in the first echo
mean(ST@data[ReturnNumber==1,Intensity])-mean(ST@data[ReturnNumber==NumberOfReturns & Classification==2,Intensity]) # High: 13.6 ; Low: 4.5 of intensity lost
(mean(ST@data[ReturnNumber==NumberOfReturns & Classification==2,Intensity])/mean(ST@data[ReturnNumber==1,Intensity]))*100 # High: 38.5% ; Low: 22% of intensity lost

# gpstime
range(ST@data$gpstime) # 354557511 354559330

# Z
range(ST@data$Z)

# Classification
table(ST@data$Classification) # 7 = bruit, 2 = sol, veg = 3,4,5

# Echoes
table(ST@data$NumberOfReturns) # 14 echos ALS 2023 LowAlt; 12 HighAlt
# + d’écho veut dire que le signal a été intercepté plsrs fois sans s’éteindre car plus puissant.


all(ST@data[,ReturnNumber] != 0)
all(ST@data[,NumberOfReturns] != 0)

ST@data <- unique(setorder(ST@data, gpstime,ReturnNumber)) 

ST@data[1:1000][, .(test = all(ReturnNumber == c(1:NumberOfReturns))), by = .(gpstime)][test!=TRUE]
options(digits = 22)
# ST@data[gpstime== 354557510.8677127957344] # il maqnue les 1ers retours

ST@data <- unique(ST@data)

lidR::plot(ST)


# Trajectory
traj <- fread("//amap-data.cirad.fr/safe/lidar/ALS/Paracou/2022/trajecto/merged/trajectory.txt")
plot3d(traj[,2:4], aspect=F) # plot the trajectory (x,y,z)
summary(traj)
range(traj$time)  # 354555656 354559914

rm(traj)


