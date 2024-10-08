# #' Crop voxel space with a shapefile
#'
#' @param VX VoxelSpace 
#' @param zone shapefile (sf) with a crs
#' @param VOP Rotation matrix
#'
#' @return VoxelSpace crop
#' @export
#' 
#' @import sf
#' @import data.table
#' @import AMAPVox
#' @importFrom zoo na.locf
#' 
#' @examples
#' zone <- terra::vect("D:/Mes Donnees/PhD/SIG_data/Understory-ALT/Parcelles_Understory.shp")
#'  zone <- st_as_sf(zone) # as sf object
#'  zone <- st_set_crs(zone, 2972)
#'  zone <- st_union(zone)
#' VOP <- as.matrix(read.table("//amap-data.cirad.fr/work/users/VincyaneBadouard/Lidar/ALS2023/VOP_P16_9ha.txt"))
#' VXUTM_crop <- Crop_vox_with_shapefile(vxsp_High, zone, VOP)
#' AMAPVox::plot(VXUTM_crop)
#' 
Crop_vox_with_shapefile <- function(VX, zone, VOP){
  
  # Shapefile prep ---------------------------------------------------------------
  zone_m <- as.matrix(zone[[1]])
  
  # Shapefile in the same coordinate system than the vxl space
  
  matrix <- cbind(zone_m, c = 0, d = 1)
  local <- matrix %*% t(VOP) # same dimensions
  local <- local[, -c(3,4)]
  
  X <- range(local[,1]) ; Y <- range(local[,2])
  
  # Crop the voxel space ---------------------------------------------------------
  # i, j, k = index not coordinates
  VX@data[, c("x", "y", "z") := getPosition(VX)[, .(x, y, z)]]
  
  # range(VX@data$x); range(VX@data$y)
  I <- range(VX@data[x>X[1] & x<X[2] & y>Y[1] & y<Y[2], i]) # 20 119
  J <- range(VX@data[x>X[1] & x<X[2] & y>Y[1] & y<Y[2], j])  # 51 150
  K <- range(VX@data[x>X[1] & x<X[2] & y>Y[1] & y<Y[2], k]) # 0 33
  
  VXUTM_crop <- AMAPVox::crop(VX, imin = I[1], imax = I[2], jmin = J[1], jmax = J[2], kmin = K[1], kmax = K[2])
  
  return(VXUTM_crop)
  
}

# Plot -------------------------------------------------------------------------
# prod(VXUTM_crop@header$dim) == nrow(VXUTM_crop@data) # test: doit etre cohérent
# 
# Relativ2UTM <- function(data, VOP){
#   # Apply inverse of VOP matrix to convert back to UTM
#   xyz <- tcrossprod(as.matrix(data@data[, .(x, y, z, c=1)]), solve(VOP))
#   data@data[, `:=`(Xutm = xyz[, 1], Yutm = xyz[, 2], Zutm = xyz[, 3])]
# 
#   data@data <- unique(data@data)
#   return(data)
# }
# 
# VXUTM_crop <- Relativ2UTM(VXUTM_crop, VOP)
# datasf <- st_as_sf(VXUTM_crop@data, coords = c('Xutm','Yutm'))
# datasf <- st_set_crs(datasf, st_crs(zone))
# gc()
# 
# ggplot() +
#   geom_sf(data = datasf, col="blue") +
#   theme_classic() +
#   geom_sf(data = sf::st_cast(zone, "LINESTRING"), col ="green")

