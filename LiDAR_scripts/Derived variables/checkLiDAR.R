library(lidR)
library(data.table)

ST <- readLAS("//amap-data.cirad.fr/work/users/VincyaneBadouard/Lidar/ALS2023/lowAltitudeFlight/LowFlight_alt4ha_buff100m_2023_RefAsInt.laz")
#"//amap-data.cirad.fr/work/users/VincyaneBadouard/Lidar/ALS2023/HighAltitudeFlight/HighFlight_alt4ha_buff100m_2023_RefAsInt.laz"
# "//amap-data.cirad.fr/work/users/VincyaneBadouard/Lidar/ALS2023/lowAltitudeFlight/LowFlight_alt4ha_buff100m_2023_RefAsInt.laz"
# "//amap-data.cirad.fr/work/users/VincyaneBadouard/Lidar/ALS2022/P16_2022_4ha_buffer.laz"

ST
summary(ST@data)
lidR::las_check(ST) # lidR checks

# crs
crs(ST)

# Density
mean(grid_density(ST, res=1)[])
ST <- retrieve_pulses(ST)
d <- rasterize_density(ST) # Creates a map of the point and pulses density
dim(ST@data[NumberOfReturns==ReturnNumber,])
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
table(ST@data[ReturnNumber==11 & NumberOfReturns==11, Classification]) # 2 = sol

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
