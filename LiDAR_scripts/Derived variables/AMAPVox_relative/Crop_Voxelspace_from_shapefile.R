# Crop Voxelspace from a shapefile

VX <- readVoxelSpace( # Voxelspace
  "//amap-data.cirad.fr/work/users/VincyaneBadouard/Lidar/ALS2023/HighAltitudeFlight/ForTrees/Vox/P16_2023_HighAlt_25ha_buffer_intensity1m_testnewEB.vox"
)

zone <- terra::vect("D:/Mes Donnees/PhD/SIG_data/Understory-ALT/Parcelles_Understory.shp") # shapefile
zone <- st_as_sf(zone) # as sf object
zone <- st_set_crs(zone, 2972)
zone <- st_union(zone)
zone_m <- as.matrix(zone[[1]])

# Lecture matrice de transformation
VOP <- as.matrix(read.table("//amap-data.cirad.fr/work/users/VincyaneBadouard/Lidar/ALS2023/VOP_P16_9ha.txt")) 

matrix <- cbind(zone_m, c = 0, d = 1)
local <- matrix %*% t(VOP) # same dimensions
local <- local[, -c(3,4)]

X <- range(local[,1]) ; Y <- range(local[,2])

# i, j, k = index not coordinates
VX@data[, c("x", "y", "z") := getPosition(VX)[, .(x, y, z)]]

# range(VX@data$x); range(VX@data$y)
I <- range(VX@data[x>X[1] & x<X[2] & y>Y[1] & y<Y[2], i]) 
J <- range(VX@data[x>X[1] & x<X[2] & y>Y[1] & y<Y[2], j])  
K <- range(VX@data[x>X[1] & x<X[2] & y>Y[1] & y<Y[2], k]) 

VXUTM_crop <- AMAPVox::crop(VX, imin = I[1], imax = I[2], jmin = J[1], jmax = J[2], kmin = K[1], kmax = K[2])

prod(VXUTM_crop@header$dim) == nrow(VXUTM_crop@data) # doit etre cohérent