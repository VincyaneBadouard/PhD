#' ComputeScanAngle
#'
#' @description Compute missing scan angles of an aerial LiDAR acquisition.
#'
#'
#' @param las Point cloud with gpstime column (LAS)
#' @param traj LiDAR trajectory with gpstime column (data.frame)
#'
#' @return Point cloud (LAS) with *ScanAngle* column.
#' @export
#'
#' @import lidR
#' @import data.table
#' @importFrom zoo na.locf
#'
#' @examples
#' las <- ComputeScanAngle(las, traj)
#' las30 <- las[abs(las@data$ScanAngle)<=30 & !is.na(las@data$ScanAngleRank)]

ComputeScanAngle <- function (las, traj)
{
  
  #### Euclidean distance calculation #### -------------------------------------
  
  ## GPS time (trajectory) vectors
  TGPS <- matrix(data = NA, nrow = length(traj$gpstime), ncol = 3) # create an empty 3-column matrix
  # allocate GPS time (trajectory) to first column
  TGPS[,1] <- traj$gpstime
  # allocate 0s for GPS (trajectory) points to second column
  TGPS[,2] <- 0
  # allocate index to third column
  TGPS[,3] <- 1:length(traj$gpstime)
  
  ## LIDAR (target) time vectors
  TLIDAR <- matrix(data = NA,nrow = length(las@data$gpstime), ncol = 3) # create an empty 3-column matrix
  # allocate GPS time (LIDAR) to first column
  TLIDAR[,1] <- las@data$gpstime
  # allocate 1s for LIDAR points to second column
  TLIDAR[,2] <- 1
  # allocate index to third column
  TLIDAR[,3] <- 1:length(las@data$gpstime)
  
  # fusion GPS/LIDAR time vectors
  fusion <- rbind(TGPS,TLIDAR) # fusioned matrices
  
  # order fusion by time
  fusion_ordered <- fusion[order(fusion[,1]),] 
  
  # keep GPS (trajectory) points and write NAs for LIDAR points
  fusion_ordered[which(fusion_ordered[,2] == 1),1] <- NA # LiDAR time
  
  # fill empty spaces from left to right
  # LIDAR point takes the closest inferior GPS (trajectory) time available
  fusion_ordered[,1] <- zoo::na.locf(fusion_ordered[,1],na.rm = F) # Generic function for replacing each NA with the most recent non-NA prior to it.
  
  # keep only LIDAR points
  fusion_ordered <- fusion_ordered[which(fusion_ordered[,2] == 1),]
  
  # order by third column (index) (traj order), ID in tile450m12p
  fusion_ordered <- fusion_ordered[order(fusion_ordered[,3]),]
  
  # join to tile450m12p
  las@data$tgps <- fusion_ordered[,1] # laz <- traj time
  
  # fusion by time (Problme pcs la traj translated n'a pas de valeur après la virgule dans le gpstime)
  # https://campus.datacamp.com/courses/joining-data-with-datatable-in-r/diagnosing-and-fixing-common-join-problems?ex=8
  las@data <- merge(las@data,traj, by.x = "tgps", by.y = "gpstime", all.x = T) # fusion laz & traj by time
  
  # distance calculation
  # las@data$range <- mapply(dfdist, las@data$X, las@data$Y, las@data$Z,
  #                         las@data$x, las@data$y, las@data$z)
  
  
  # Shot angle calculation -----------------------------------------------------
  # un acos peut pas etre supérieur à 1
  las@data[, ScanAngle := round(((acos((z-Z)/Range))*180)/pi)] # Acos donne un angles en radians, le multiplier par 180 et divisé par pi le transforme en degrees
  # quand range < à z-Z -> NaN. Pq range < ?
  range(las@data$ScanAngle, na.rm =T) # 0 - 86
  las@data[, ScanAngle := ifelse(is.nan(ScanAngle), 180, ScanAngle)]
  
  # las@data$theta <-
  #   ((min(acos((las@data$z-las@data$Z)/las@data$Range), 1)/pi))*180 # compute angle (prbl produit des NaN)
  # view(las@data)
  
  # las@data$ScanAngle <- round(las@data$theta) # round angle
  
  return(las)
  
}

