ST <- readLAS("//amap-data.cirad.fr/work/users/VincyaneBadouard/Lidar/ALS2022/P16_2022_4ha_buffer.laz")
traj <- fread("//amap-data.cirad.fr/safe/lidar/ALS/Paracou/2022/trajecto/merged/trajectory.txt")
STdata <- ST@data
lidR::plot(ST)
plot3d(traj[,2:4], aspect=F) # plot the trajectory (x,y,z)
range(STdata$gpstime)  
range(traj$time)  

all(STdata[,ReturnNumber] != 0)
all(STdata[,NumberOfReturns] != 0)

# by ring-gpstime, ReturnNumber = c(1:NumberOfReturns)
STdata <- unique(setorder(STdata, gpstime,ReturnNumber)) 

STdata[1:1000][, .(test = all(ReturnNumber == c(1:NumberOfReturns))), by = .(gpstime)][test!=TRUE]
options(digits = 22)
STdata[gpstime== 354557510.8677127957344] # il maqnue les 1ers retours

ST@data <- unique(ST@data)
lidR::las_check(ST)
