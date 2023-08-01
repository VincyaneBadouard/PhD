#' trace_shots_without_echo
#'
#' @description Retracer les tirs sans écho
#'
#' @param ST 
#' with:
#' - ReturnNumber
#' - Ring
#' - gpstime
#' 
#' @param Traj Trajectory file
#' 
#' @param vector_coord Table of the vector coordinates of the shots *with* echo
#'
#' @return A table with the vector coordinates of the shots without echo
#' @export
#'
#' @examples
#' ST <- readLAS("Z:/users/VincyaneBadouard/Lidar/Hovermap/Scans Hovermap ST-X/Escadrone180723_ss_filtre/local/out3_subsampled_laz1_4.laz")
#' ST_broc <- readLAS("Z:/users/VincyaneBadouard/Lidar/Hovermap/Scans Hovermap ST-X/Escadrone180723_ss_filtre/local/out3_decim2.5cm_range1.5.laz")
#' ST <- readLAS("Z:/users/VincyaneBadouard/Lidar/Hovermap/Data_test/out1_laz1_4.laz")
#' Traj <- fread("Z:/users/VincyaneBadouard/Lidar/Hovermap/Scans Hovermap ST-X/Escadrone180723_ss_filtre/local/out3_traj.xyz")
#' 
#' ST <- ST_broc
#' vector_coord <- trace_shots_with_echo(ST, traj)
#' Gaps_vect_coord <- trace_shots_without_echo(ST, traj)
#' 
trace_shots_without_echo <- function(ST, Traj, vector_coord){
  
  # Take only the 1st echo
  Cloud <- ST@data[ReturnNumber == 0]
  
  for(R in 1:max(Cloud[,Ring])){
    # Separate data of each ring in different tables
    Ring <- Cloud[Ring == R]

    # Find the temporal gaps in the regular time step --------------------------
    options(digits=22) # to see all the digits
    
    ## Compute time step 
    diff_time <- diff(sort(Ring$gpstime))
    # any(diff_time==0)
    reg_val <- round(median(diff_time),digits = 5) + 0.00001 # 0.00005 seems to be the most regular value
    dt_mean <- mean(diff_time[diff_time < reg_val]) # pas de temps de référence (dt) (moyenne des réguliers)
    gaps <- diff_time[diff_time > reg_val] # ceux qui ne sont pas réguliers
    # gaps
    ngaps <- round(gaps/dt_mean) # nombre de shots sans écho qui on pu se faire entre ceux avec écho
    # ngaps
    
    
    index <- which(diff_time > reg_val) # indice des shots pas réguliers
    
    all(Ring$gpstime[index+1]-Ring$gpstime[index] == gaps) # vérifier que le temps de gaps correspond au temps entre  entre 2 shots à écho
    
    # Calculer les gps time manqués ---------------------------------------------
    # chaque index a un ngaps
    # Ring$gpstime[index] + seq(ngaps) * dt_mean # temps de chaque tir manqué par index
    
    gpstime_gaps <- unlist(sapply(seq(ngaps), function(i)
      # temps annormal + (nbr)
      Ring$gpstime[index][i] + seq(ngaps[i]) * dt_mean # un index pour chaque tir sans écho
    ))
    # question : on devrait pas mettre -1 à index
    # all_gpstime <- c(Ring$gpstime, gpstime_gaps)
    # all(diff(all_gpstime[order(all_gpstime)]) < reg_val) # tous bien < 0.00006
    # length(gpstime_gaps) == sum(ngaps) # check qu'il y a autant de temps gps que de tirs sans écho
    
    
    # Retreive direction vectors -----------------------------------------------
    
    ## Interpolate coordinates of the emission point (A) (in the traj) for the gaps gpstime (shots without echo)
    Xa <- approx(x = Traj$gpstime, y = Traj$x, xout = gpstime_gaps)$y # x : temps de la traj, xout : temps des gaps, $y pour avoir les coordonées x et non les temps
    Ya <- approx(Traj$gpstime, y = Traj$y , gpstime_gaps)$y
    Za <- approx(Traj$gpstime, y = Traj$z , gpstime_gaps)$y
    
    ## Interpolate the direction vectors of this shots without echo (intercalate between vectors of the shots with echo)
    x_dir <- approx(x = vector_coord$gpstime, y = vector_coord$x_dir, xout = gpstime_gaps)$y
    y_dir <- approx(vector_coord$gpstime, y = vector_coord$y_dir, gpstime_gaps)$y
    z_dir <- approx(vector_coord$gpstime, y = vector_coord$z_dir, gpstime_gaps)$y
    
    
    ## In a table:
    gaps_vector_coord <- data.frame(x_dir = x_dir, # vector coordinates
                                    y_dir = y_dir,
                                    z_dir = z_dir,
                                    Distance = 200,# m. Il n'y a pas de distance puisqu'il n'y a pas d'echo
                                    gpstime = gpstime_gaps) 
    
    ## Compute the norm of the direction vector
    gaps_dir_norm <- sqrt(gaps_vector_coord$x_dir^2 + gaps_vector_coord$y_dir^2 + gaps_vector_coord$z_dir^2)# n'a pas conservé une norme =1
    
    ## Transform in unit vector (norm = 1)
    gaps_vector_coord$x_dir <- gaps_vector_coord$x_dir / gaps_dir_norm
    gaps_vector_coord$y_dir <- gaps_vector_coord$y_dir / gaps_dir_norm
    gaps_vector_coord$z_dir <- gaps_vector_coord$z_dir / gaps_dir_norm
    
    gaps_vector_coord$X0 <- Xa
    gaps_vector_coord$Y0 <- Ya
    gaps_vector_coord$Z0 <- Za
    
  } # end each Ring
  
  return(gaps_vector_coord)
}