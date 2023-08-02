#' trace_shots_with_echo
#' 
#'@description Compute the direction vectors (AB) between the emission (A) and the
#'  acquired points (B).
#'
#' @param ST (LAS) Points cloud
#' with:
#' - ReturnNumber
#' - Ring
#' - gpstime
#'
#' @param Traj (data.table) Trajectory file
#'
#' @param SampleTime (integer) Samples the dataset by the number of seconds given
#'   to quickly test the function
#'
#' @param OneRing (logical) TRUE: process only one ring ; FALSE (default):
#'   process all the rings
#'
#' @return A table (data.frame) with the vector coordinates of the shots with
#'   echo
#'   
#' @export
#' 
#' @importFrom lidR
#' @importFrom data.table
#' @importFrom interp
#'
#' @examples
#' library(lidR)
#' library(data.table)
#' 
#' ST <- readLAS("Z:/users/VincyaneBadouard/Lidar/Hovermap/Scans Hovermap ST-X/Escadrone180723_ss_filtre/local/out3_subsampled_laz1_4.laz")
#' ST_broc <- readLAS("Z:/users/VincyaneBadouard/Lidar/Hovermap/Scans Hovermap ST-X/Escadrone180723_ss_filtre/local/out3_decim2.5cm_range1.5.laz")
#' ST <- readLAS("Z:/users/VincyaneBadouard/Lidar/Hovermap/Data_test/out1_laz1_4.laz")
#' Traj <- fread("Z:/users/VincyaneBadouard/Lidar/Hovermap/Data_test/out1_traj.xyz")
#' 
#' ST <- ST_broc
#' 
#' vector_coord <- trace_shots_with_echo(ST, Traj, SampleTime = 3, OneRing = TRUE)
#' 
trace_shots_with_echo <- function(ST,
                                  Traj,
                                  SampleTime = NULL,
                                  OneRing = FALSE)
{
  
  options(digits=22) # to see all the digits
  
  # Take only the 1st echo
  Cloud <- ST@data[ReturnNumber == 0]
  
  if(!is.null(SampleTime)){ # sample only few seconds
    fstsec <- Cloud$gpstime[50]
    Cloud <- Cloud[gpstime >= fstsec & gpstime<= fstsec+SampleTime]
    Traj <- Traj[gpstime >= fstsec & gpstime<= fstsec+SampleTime]
  }
  
  if(OneRing){Rings <- 2} else {Rings <- min(Cloud[,Ring]):max(Cloud[,Ring])}
  
  for(R in Rings){
    # Separate data of each ring in different tables
    Ring <- Cloud[Ring == R]
    
    # Remove useless columns
    Ring <- Ring[,.(Ring, gpstime, X,Y,Z, Range)]
    Traj <- Traj[,.(gpstime, x,y,z)]
    
    # Acquired points coordinates (B)
    Xb <- Ring$X
    Yb <- Ring$Y
    Zb <- Ring$Z
    
    # Interpolate coordinates of the emission point (A) (in the traj) 
    # from coordinates before and after (= barycentric interpolation)
    # x : temps de la traj, xout : temps du cloud, $y pour avoir les coordonées x et non les temps
    Xa <- approx(x = Traj$gpstime, y = Traj$x , xout = Ring$gpstime)$y 
    Ya <- approx(Traj$gpstime, y = Traj$y , Ring$gpstime)$y
    Za <- approx(Traj$gpstime, y = Traj$z , Ring$gpstime)$y
    
    # Compute the coordinates of the AB vector between the emission and the acquired point
    vector_coord <- data.frame(x_dir = Xb-Xa,
                               y_dir = Yb-Ya,
                               z_dir = Zb-Za) # coordinates
    
    # Compute the norm of the direction vector (AB)
    vector_coord$Distance <- sqrt(
      (vector_coord$x_dir)^2 + (vector_coord$y_dir)^2 + (vector_coord$z_dir)^2
    ) 
    # Transform in direction vector of norm = 1
    vector_coord$x_dir <- vector_coord$x_dir / vector_coord$Distance
    vector_coord$y_dir <- vector_coord$y_dir / vector_coord$Distance
    vector_coord$z_dir <- vector_coord$z_dir / vector_coord$Distance
    
    # Add A (emission) coordinates in the table
    vector_coord$X0 <- Xa
    vector_coord$Y0 <- Ya
    vector_coord$Z0 <- Za
    
    # and associated time 
    vector_coord$gpstime <- Ring$gpstime 
    
    
    return(vector_coord)
    
  }
}