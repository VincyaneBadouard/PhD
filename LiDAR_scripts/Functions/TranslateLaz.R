#' Translate LiDAR point cloud with a translation matrix
#'
#' @param las Point Cloud (laz)
#' @param matrix Translation matrix ( 4 rows, 4 columns) (matrix)
#'               (can be computed on CloudCompare)
#'               
#' @param suffix suffix for the previous coordinates (character)
#'
#' @return Translated point cloud (laz)
#' @export
#'
#' @import data.table
#'
#' @examples
#' library(lidR)
#' las <- readLAS("//amap-data.cirad.fr/work/users/VincyaneBadouard/Lidar/HovermapUAV2023/P16_C19C20/Split2/UAV_split_8295553.laz")
#' matrix <- as.matrix(read.table("//amap-data.cirad.fr/work/users/VincyaneBadouard/Lidar/HovermapUAV2023/Translation/Translation_matrix/Matrix_C19C20_cor.mat.txt", header=F))
#' Trans <- TranslateLaz(las, matrix)
#' 
TranslateLaz <- function(las, matrix, suffix = "_local"){
  
  las@data <- setDT(las@data)
  
  dat <- las@data[,.(X,Y,Z)] # only coordinates
  dat$C <- 1 # 4 columns
  dat <- as.matrix(dat)
  
  Trans <- dat %*% t(matrix) # pour appliquer la matrice de translation
  
  las@data <- setnames(las@data, c("X", "Y", "Z"), c(paste("x",suffix, sep = ""),
                                                     paste("y",suffix, sep = ""),
                                                     paste("z",suffix, sep = "")
  ))  # change names of local coordinates
  
  rm(dat); gc()
  # options(digits = 22)
  las@data <- cbind(Trans[,1:3], las@data)
  # View(las@data)
  
  las@data <- setnames(las@data, c("V1", "V2", "V3"), c("X", "Y", "Z"))  # change names of local coordinates
  
  
  return(las)
}

