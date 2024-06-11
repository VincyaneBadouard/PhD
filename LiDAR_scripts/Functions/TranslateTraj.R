#' Translate LiDAR trajectory with a translation matrix
#'
#' @param traj Trajectory (data.table)
#' @param matrix Translation matrix ( 4 rows, 4 columns) (matrix)
#'               (can be computed on CloudCompare)
#'
#' @return Translated trajectory (data.table)
#' @export
#'
#' @import data.table
#'
#' @examples
#' traj <- fread("//amap-data.cirad.fr/work/users/VincyaneBadouard/Lidar/HovermapUAV2023/P16_C19C20/Output_traj.xyz", select = seq(1:5))
#' matrix <- as.matrix(read.table("//amap-data.cirad.fr/work/users/VincyaneBadouard/Lidar/HovermapUAV2023/Translation/Translation_matrix/Matrix_C19C20_cor.mat.txt", header=F))
#' Trans <- TranslateTraj(traj, matrix)
#' 
TranslateTraj <- function(traj, matrix){
  dat <- traj[,.(x,y,z)] # only coordinates
  dat$C <- 1 # 4 columns
  dat <- as.matrix(dat)
  
  Trans <- dat %*% t(matrix) # pour appliquer la matrice de translation

  # options(digits = 22)
  traj_cor <- cbind(traj[,.(time,gpstime)],Trans[,1:3])
  names(traj_cor) = c("time","gpstime", "x","y","z")
  
  return(traj_cor)
}

