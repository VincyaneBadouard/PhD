#' Relativ2UTM
#' 
#' @description
#' Translate the relative coordinates (x and y columns) into UTM coordinates
#' 
#'
#' @param data data.frame with x and y columns
#' @param VOP Translation matrix
#'
#' @return The input data.frame with Xutm and Yutm columns
#' @export
#'
#' @examples
#' VOP <- as.matrix(read.table("//amap-data.cirad.fr/work/users/VincyaneBadouard/Lidar/ALS2023/VOP_P16_9ha.txt"))
#' 
Relativ2UTM <- function(
    data,
    VOP
){
  
  # Apply inverse of VOP matrix to convert back to UTM
  data <- setDT(data)
  xyz <- tcrossprod(as.matrix(data[, .(x, y, z, c=1)]), solve(VOP))
  # xyz <- tcrossprod(as.matrix(data[, .(X, Y, Z, c=1)]), solve(VOP))
  data[, `:=`(Xutm = xyz[, 1], Yutm = xyz[, 2], Zutm = xyz[, 3])]
  
  data <- unique(data)
  
  return(data)
}