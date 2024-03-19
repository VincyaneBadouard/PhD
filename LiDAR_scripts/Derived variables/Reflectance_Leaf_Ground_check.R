library(data.table)
library(lidR)
low=readLAS("//amap-data.cirad.fr/work/users/VincyaneBadouard/Lidar/ALS2023/lowAltitudeFlight/LowFlight_alt4ha_buff100m_2023_RefAsInt.laz")
lowSO<-low
lowSO@data <- lowSO@data[NumberOfReturns==1,]
table(lowSO@data$Classification)
lowSO@data[,.(Int=mean(Intensity)), by=Classification]
boxplot(lowSO@data$Intensity~lowSO@data$Classification)
# ratio sol/veg ~0.63

readLAS(filter = "-help")
highSO<-readLAS("//amap-data.cirad.fr/work/users/VincyaneBadouard/Lidar/ALS2023/HighAltitudeFlight/HighFlight_alt4ha_buff100m_2023_RefAsInt.laz",
                  filter ="-keep_single")
highSO@data[,.(Int=mean(Intensity)), by=Classification]
#ratio sol/veg~0.69
  