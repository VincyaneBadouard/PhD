library(lidR)

ST <- readLAS("//amap-data.cirad.fr/work/users/VincyaneBadouard/Lidar/ALS2022/P16_2022_4ha_buffer.laz")

ST
summary(ST@data)
lidR::las_check(ST)


crs(ST)
range(ST@data$gpstime)
range(ST@data$Z)
table(ST@data$Classification) # 7 = bruit, 2 = sol, veg = 3,4,5

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
range(traj$time)  

rm(traj)
