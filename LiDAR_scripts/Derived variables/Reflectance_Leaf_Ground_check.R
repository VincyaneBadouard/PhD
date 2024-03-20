# Pour prendre en compte les propriétés optiques des matériaux dans le calcul de la transmittance dans AMAPVox

library(data.table)
library(lidR)
library(tidyverse)

lowSO=readLAS("//amap-data.cirad.fr/work/users/VincyaneBadouard/Lidar/ALS2023/lowAltitudeFlight/LowFlight_alt4ha_buff100m_2023_RefAsInt.laz")
table(lowSO@data$Classification)

means <- lowSO@data[NumberOfReturns == 1, .(mean=mean(Intensity)), by = Classification] # Reflectance mean by classification (veg, ground)

ratio <- means[Classification==2,]$mean/means[Classification==3,]$mean # ratio sol/veg reflectance 

lowSO@data <- lowSO@data %>% 
  mutate(Intensity = ifelse(Classification==2, as.integer(as.numeric(Intensity)/ratio), Intensity)) %>%  # Ground reflectance corrected by the ratio
  mutate(Classification = ifelse(Classification==2, 1L, Classification)) # for AMAPVox to give the soil without know it (?)
    


readLAS(filter = "-help")
highSO<-readLAS("//amap-data.cirad.fr/work/users/VincyaneBadouard/Lidar/ALS2023/HighAltitudeFlight/HighFlight_alt4ha_buff100m_2023_RefAsInt.laz",
                  filter ="-keep_single")
highSO@data <- highSO@data[NumberOfReturns==1,]
highSO@data[,.(Int=mean(Intensity)), by=Classification]
table(lowSO@data$Classification)
