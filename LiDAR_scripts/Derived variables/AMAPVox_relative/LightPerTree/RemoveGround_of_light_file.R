
# Remove soil value of Light file

# Read Light file --------------------------------------------------------------
source("~/PhD/R_codes/PhD/LiDAR_scripts/Functions/ReadLightFile.R")
path <- "//amap-data.cirad.fr/work/users/VincyaneBadouard/Lidar/ALS2023/HighAltitudeFlight/ForTrees/P16C19_2023_1ha_HighAlt_Light_intensity2m_all2m.txt"
# "//amap-data.cirad.fr/work/users/VincyaneBadouard/Lidar/ALS2023/HighAltitudeFlight/ForTrees/P16_2023_25ha_buffer_HighAlt_Light_intensity1m_all1m.txt"

# P16_2023_1ha_buffer_HighAlt_Light_intensity1m_all1m.txt # 10201 obs
# P16_2023_25ha_buffer_HighAlt_Light_intensity1m_all1m.txt # 491 401 obs

Light <- ReadLightFile(path, skiplines=9)

setnames(Light, c("Period 1", "X", "Y", "Z"), c("Transmittance", "x", "y", "z")) # rename columns

# Compute UTM x,y coordinates --------------------------------------------------
source("~/PhD/R_codes/PhD/LiDAR_scripts/Functions/Relative2UTM.R", echo=TRUE)
VOP <- as.matrix(read.table("//amap-data.cirad.fr/work/users/VincyaneBadouard/Lidar/ALS2023/VOP/VOP_P16_25ha.txt"))

Light <- Relativ2UTM(Light, VOP) # Compute Xutm, Yutm
Light <- Light[, !"Zutm", with = FALSE] # remove this Zutm 

# Compute ground height --------------------------------------------------------
MNT <- terra::rast("//amap-data.cirad.fr/work/users/VincyaneBadouard/Lidar/ALS2023/HighAltitudeFlight/MNT/dtm2023_25ha_HighAlt_buffer.asc") 
crs(MNT) <- crs("EPSG:2972")

datasf <- st_as_sf(Light, coords = c('Xutm','Yutm'))
datasf <- st_set_crs(datasf, st_crs(MNT)) 

alt <- extract(MNT, datasf) %>% # extract z
  rename(Zutm = `MNT_0.5m_IRD_500m_Paracou_284000_579500`)

Light <- Light %>% cbind(alt) %>% setDT()

# Detect measurment in the ground
Light[, test := ifelse(z <= Zutm, T, F)]

Light <- Light[test==F,]

Light[, Z_above_ground := z-Zutm]
Light[, color := ifelse(Transmittance < 0.005, T, F)]

hist(Light$Transmittance)
hist(Light$z)
nrow(Light[Transmittance<0.01, ])/nrow(Light)*100 # 7.9% 
nrow(Light[Transmittance<0.001, ])/nrow(Light)*100 # 0.2% 
hist(Light[Transmittance<0.0015, Z_above_ground])
hist(Light[Transmittance<0.001, Z_above_ground])
hist(Light[Transmittance<0.002, Z_above_ground])
hist(Light[Transmittance<0.003, Z_above_ground])
hist(Light[Transmittance<0.004, Z_above_ground])
hist(Light[Transmittance<0.005, Z_above_ground])
hist(Light[Transmittance<0.01, Z_above_ground])
hist(Light[Transmittance<0.05, Z_above_ground])

ggplot(Light, aes(x= Transmittance, y= Z_above_ground)) +
geom_point()

plotly::plot_ly(Light, x = ~x, y = ~y, z = ~Z_above_ground,
                color=~color, size = 20)


