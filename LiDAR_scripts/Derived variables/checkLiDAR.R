# ça serait bien d'en faire des fonctions
library(lidR)
library(data.table)

options(digits = 22)

# local <- "//amap-data.cirad.fr/work/users/VincyaneBadouard/Lidar/"
# # local <- "Y:/"
# 
# ST <- readLAS("//amap-data.cirad.fr/work/users/VincyaneBadouard/Lidar/HovermapUAV2023/P16_C19C20/Returns_and_gpstime_corrected.laz",
#               filter = "-keep_gps_time 1697733343 1697734000")
# ST <- readLAS("//amap-data.cirad.fr/work/users/VincyaneBadouard/Lidar/HovermapUAV2023/AMAPVox/LAZ/P16_2023_UAV_4ha_buffer_intensitycor_lastools.laz")


path <- 
  #C14C15
  # "//amap-data.cirad.fr/work/users/VincyaneBadouard/Lidar/ALS2023/HighAltitudeFlight/LAZ/P16_2023_HighAlt_C14C15.laz"
  # "//amap-data.cirad.fr/work/users/VincyaneBadouard/Lidar/ALS2023/LowAltitudeFlight/LAZ/P16_2023_LowAlt_C14C15.laz"
"//amap-data.cirad.fr/work/users/VincyaneBadouard/Lidar/HovermapUAV2023/AMAPVox/LAZ/P16_2023_UAV_C14C15_intensitycor.laz"

 -------------------------------------------------------------------------------
ST <- readLAS(path)

# ST <- readLAS(paste(local, "HovermapUAV2023/AMAPVox/LAZ/P16_2023_UAV_C19C20_buffer_intensitycor.laz", sep = "")) 
range(ST@data$gpstime) # C19C20: 1697733342.914306640625 1697734144.802246093750 ; C14C15: 1697736417.1210937500 1697737307.1318359375
names(ST@data) # 23 col dans les deux uAV
# [1] "X"                 "Y"                 "Z"                 "gpstime"           "Intensity"         "ReturnNumber"     
# [7] "NumberOfReturns"   "ScanDirectionFlag" "EdgeOfFlightline"  "Classification"    "ScannerChannel"    "Synthetic_flag"   
# [13] "Keypoint_flag"     "Withheld_flag"     "Overlap_flag"      "ScanAngle"         "UserData"          "PointSourceID"    
# [19] "R"                 "G"                 "B"                 "Ring"              "Range"            

ST@data$gpstime[1:10]
# "HovermapUAV2023/AMAPVox/LAZ/P16_2023_UAV_4ha_buffer_intensitycor_CloudCompare.laz" # trop lourd pour les tests
# paste(local, "ALS2023/lowAltitudeFlight/LowFlight_alt4ha_buff100m_2023_RefAsInt.laz", sep = ""))
#"ALS2023/HighAltitudeFlight/HighFlight_alt4ha_buff100m_2023_RefAsInt.laz"
# "ALS2023/lowAltitudeFlight/LowFlight_alt4ha_buff100m_2023_RefAsInt.laz"
# "ALS2022/P16_2022_4ha_buffer.laz"
gc()

ST
summary(ST@data)
lidR::las_check(ST) # lidR checks

# ALS High alt:
# ⚠ 42721 points are duplicated and share XYZ coordinates with other points

# ALS Low alt:
# ⚠ 985 points are duplicated and share XYZ coordinates with other points

# UAV processed :
# ⚠ 185 points are duplicated and share XYZ coordinates with other points
# ✗ 592218 pulses (points with the same gpstime) have points with identical ReturnNumber
# NumberOfReturns validity... ✓
# ReturnNumber vs. NumberOfReturns... ✓

# UAV C14C15:
# ⚠ 65539 points are duplicated and share XYZ coordinates with other points
# ✗ 458256 pulses (points with the same gpstime) have points with identical ReturnNumber

# UAV C19C20:
# ⚠ 36179 points are duplicated and share XYZ coordinates with other points
# ✗ 846416 pulses (points with the same gpstime) have points with identical ReturnNumber
# NumberOfReturns validity... ✓
# ReturnNumber vs. NumberOfReturns... ✓

# crs --------------------------------------------------------------------------
lidR::crs(ST)

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

# Footprint size ---------------------------------------------------------------
divergence <- 0.25 # mRAD divergence du lazer
h <- 500 # 1000 # 500 # fly heignt in m
tan(divergence*10^-3)*h # Footprint size in m

# Penetration (proportion de points sol dans le dernier écho) ------------------
table(ST@data$NumberOfReturns) # ALS 2023 LowAlt: 14; HighAlt: 11; UAV : 3 echos
mean(ST@data$NumberOfReturns) # ALS 2023 HighAlt: 2.8 ; LowAlt: 4.0; UAV 1.70: Average number of echoes per shot
min(ST@data$Z) # High : 6.24 ; Low : 6.23, UAV : 6.29
(nrow(ST@data[ReturnNumber==NumberOfReturns & Classification==1,])/nrow(ST@data[ReturnNumber==NumberOfReturns,]))*100
# 1 = sol # High: 9.6% ; Low: 18.9 % ; UAV :  1.24 % of ground points in the last echo
alt = 500  # 1000 ; 500 ; 85
alt-mean(ST@data[ReturnNumber==NumberOfReturns & Classification==1,Z]) # High: 987 ; Low: 487 ; UAV: 72 m distance
mean(ST@data[ReturnNumber==NumberOfReturns,Z]) # High: 31.2 ; Low: 26.1 ; UAV: 35.2 m average height of the last echo

# Si l'intensité n'est pas déjà la réflectance apparente :
# ST@data$initial_intensity <- ST@data$Intensity
# ST@data[, Intensity := as.integer((10^(Reflectance/10))*100)] # Réflectance initialement en decibel (et selon une référence connue)

hist(ST@data[,Intensity])
range(ST@data[,Intensity]) # High: [122;12078] ; Low: [4;2754] of intensity ; UAV: [0;338]
mean(ST@data[ReturnNumber==NumberOfReturns & Classification==1,Intensity]) # High: 894 ; Low:128 ; UAV: 10.8 of intensity in average at the ground
mean(ST@data[Classification==1,Intensity]) # High: 906 ; Low: 128 ; UAV: 10.8 of intensity in average at the ground
mean(ST@data[ReturnNumber==1,Intensity]) # High: 2149 ; Low: 562 ; UAV: 18.0 of intensity in average in the first echo
mean(ST@data[ReturnNumber==1,Intensity])-mean(ST@data[ReturnNumber==NumberOfReturns & Classification==1,Intensity]) # High: 1255 ; Low: 434 of intensity lost
(mean(ST@data[ReturnNumber==NumberOfReturns & Classification==1,Intensity])/mean(ST@data[ReturnNumber==1,Intensity]))*100
# High: 41.6% ; Low: 22.8% ; UAV: 7.2% of intensity lost

# gpstime ----------------------------------------------------------------------
range(ST@data$gpstime) # 354557511 354559330

# Z ----------------------------------------------------------------------------
range(ST@data$Z)
# UAV: [6.29;62.98]

# Classification ---------------------------------------------------------------
table(ST@data$Classification) # 7 = bruit, 2 = sol, veg = 3,4,5, Unclassified = 1

# Echoes -----------------------------------------------------------------------
table(ST@data$NumberOfReturns) # 14 echos ALS 2023 LowAlt; 12 HighAlt ; UAV: 4
# + d’écho veut dire que le signal a été intercepté plsrs fois sans s’éteindre car plus puissant.

ST@data <- unique(setorder(ST@data, gpstime,ReturnNumber)) 

options(digits = 22)

ST@data[1:1000][, .(test = all(ReturnNumber == c(1L:NumberOfReturns))), by = .(gpstime)][test!=TRUE]
# ST@data[gpstime== 1697733343.266845703125] # il maqnue les 1ers retours

test <- ST@data
ST@data[Ring==i,.(ReturnNumber, NumberOfReturns, Range), by = .(gpstime, Ring)][order(gpstime, Ring)]

# != 0
all(ST@data[,ReturnNumber] != 0)
all(ST@data[,NumberOfReturns] != 0)

max(ST@data$NumberOfReturns) == 3 # 3 echoes max in Hovermap
all(ST@data[,ReturnNumber <= NumberOfReturns])
View(ST@data[NumberOfReturns==4])

# by ring-gpstime, ReturnNumber = c(1:NumberOfReturns)
ST@data <- unique(setorder(ST@data, gpstime,Ring,ReturnNumber)) 

ST@data[, .(test = all(ReturnNumber == c(1L:NumberOfReturns))), by = .(gpstime, Ring)][test!=TRUE]
# ST@data[gpstime== 1697736323.571918487549 & Ring ==26]

# their range are in the increasing order
ST@data[,.(!is.unsorted(Range)), by = .(gpstime, Ring)][V1!=TRUE]
# ST@data[gpstime== 1697737530.000000715256 & Ring ==24]

ST@data <- unique(ST@data)

lidR::plot(ST)


# Trajectory -------------------------------------------------------------------
traj <- fread("//amap-data.cirad.fr/safe/lidar/ALS/Paracou/2022/trajecto/merged/trajectory.txt")
plot3d(traj[,2:4], aspect=F) # plot the trajectory (x,y,z)
summary(traj)
range(traj$time)  # 354555656 354559914

rm(traj)


