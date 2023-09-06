#' trace_shots_without_echo
#'
#' @description Retracer les tirs sans écho
#'
#' @param ST (LAS) Points cloud
#' with:
#' - ReturnNumber
#' - Ring
#' - gpstime
#' 
#' @param Traj (data.table) Trajectory file
#'
#' @param vector_coord (data.frame) Table of the vector coordinates of the shots
#'   *with* echo
#'
#' @param frot (integer) Frequency of rotation in Hz
#'
#' @param SampleTime (integer) Samples the dataset by the number of seconds given
#'   to quickly test the function
#'
#' @param OneRing (logical) TRUE: process only one ring ; FALSE (default):
#'   process all the rings
#'
#' @return A table (data.frame) with the vector coordinates of the shots without
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
#' 
#' ST <- readLAS("Z:/users/VincyaneBadouard/Lidar/Hovermap/Scans Hovermap ST-X/Escadrone180723_ss_filtre/local/out3_laz1_4.laz")
#' ST_broc <- readLAS("Z:/users/VincyaneBadouard/Lidar/Hovermap/Scans Hovermap ST-X/Escadrone180723_ss_filtre/local/out3_decim2.5cm_range1.5.laz")
#' Traj <- fread("Z:/users/VincyaneBadouard/Lidar/Hovermap/Scans Hovermap ST-X/Escadrone180723_ss_filtre/local/out3_traj.xyz")
#' 
#' source("~/PhD/R_codes/PhD/LiDAR_scripts/Functions/trace_shots_with_echo.R")
#' vector_coord <- trace_shots_with_echo(ST, Traj, SampleTime = 3, OneRing = TRUE)
#' Gaps_vect_coord <- trace_shots_without_echo(ST, Traj, vector_coord, SampleTime = 3, OneRing = TRUE)
#' 
trace_shots_without_echo <- function(ST,
                                     Traj,
                                     vector_coord,
                                     frot = 20,
                                     SampleTime = NULL,
                                     OneRing = FALSE){
  
  options(digits=22) # to see all the digits
  
  # Take only the 1st echo
  Cloud <- ST@data[ReturnNumber == 0]
  
  if(!is.null(SampleTime)){ # sample only few seconds
    a <- Cloud[,.(Ring, gpstime)]
    a <- a[, .SD[1], by = .(Ring)] # 1er gpstime de chaque ring
    fstsec <- max(max(a$gpstime), min(Traj$gpstime)) # le 1er gpstime commun
    Traj <- Traj[gpstime >= fstsec & gpstime<= fstsec+SampleTime]
    Cloud <- Cloud[gpstime >= min(Traj$gpstime) & gpstime<= max(Traj$gpstime)]
    # la traj ne doit pas être inférieure au cloud en terme de temps sinon génère des NA
  }
  
  if(OneRing){Rings <- unique(Cloud[,Ring])[1]
  }else{ Rings <- unique(Cloud[,Ring]) }
  
  # R = Rings
  vector_coord_Rings <- c()
  for(R in Rings){
    # Separate data of each ring in different tables
    Ring <- Cloud[Ring == R]
    
    # Find the temporal gaps in the regular time step --------------------------
    
    ## Compute time step 
    diff_time <- diff(Ring$gpstime)
    vector_coord[Ring==R, diff_time := c(NA,diff_time)] # marche pas
    # any(diff_time==0)
    reg_val <- round(DescTools::Mode(diff_time)[1],digits = 5) + 0.00001 # 0.00005 seems to be the most regular value
    dt_mean <- mean(diff_time[diff_time < reg_val]) # pas de temps de référence (dt) (moyenne des réguliers)
    gaps <- diff_time[diff_time > reg_val] # ceux qui ne sont pas réguliers
    # gaps
    ngaps <- round(gaps/dt_mean)-1 # nombre de shots sans écho qui on pu se faire entre ceux avec écho
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
    gaps_vector_coord <- data.table(x_dir = x_dir, # vector coordinates
                                    y_dir = y_dir,
                                    z_dir = z_dir,
                                    Distance = 0,# on projette à 200m. Il n'y a pas de distance puisqu'il n'y a pas d'echo
                                    gpstime = gpstime_gaps,
                                    Ring = R) 
    
    ## Compute the norm of the direction vector
    gaps_dir_norm <- sqrt(gaps_vector_coord$x_dir^2 + gaps_vector_coord$y_dir^2 + gaps_vector_coord$z_dir^2)# n'a pas conservé une norme =1
    
    ## Transform in unit vector (norm = 1)
    gaps_vector_coord$x_dir <- gaps_vector_coord$x_dir / gaps_dir_norm
    gaps_vector_coord$y_dir <- gaps_vector_coord$y_dir / gaps_dir_norm
    gaps_vector_coord$z_dir <- gaps_vector_coord$z_dir / gaps_dir_norm
    
    gaps_vector_coord$X0 <- Xa
    gaps_vector_coord$Y0 <- Ya
    gaps_vector_coord$Z0 <- Za
    
    gaps_vector_coord$diff_time <- NA # time gaps between two emissions is unknown
    vectors <- rbind(vector_coord, gaps_vector_coord) # table for all the vectors (with and without echo)
    vectors <- vectors[order(gpstime),] # by ascending order of gpstime
    
    # A FAIRE
    # mettre le diff-time du bas sur les NA du desssus
    # pq on recalcule pas juste les diff-time  avec les gpstime ?
    
    # si diff_time > (1/frot)/2 -> on inverse le vecteur s'il a dépassé le demi-tour
    # si diff_time > (1/frot) -> on jette (distance = NULL) # tour complet
    
    # si plus de 180° entre 2 vecteurs avec echo, aucun vecteurs estimés n'est vrai car angle optu ou aigu est utilisé
    vectors[diff_time > (1/frot)/2, `:=`(x_dir = -x_dir , # s'il a dépassé le demi-tour
                                         y_dir = -y_dir,
                                         z_dir = -z_dir)]
    
    # plan B
    vectors[diff_time > (1/frot)/2, Distance := NULL]
    
    # virer tout les vecteurs du tour complet vide (gaps mais pas les bornes des gaps)
    vectors[diff_time > (1/frot), Distance := NULL] # tour complet (on le considère comme s'il n'y avait pas eu de tirs)
    
    # questions : 
    # pq on jette si tour complet ? pcq compliquer à corriger voir impossible. Et si pas de données sur un tour complet c'est que le scanner fonctionne pas (objet trop proche trop longtemps ou que du ciel)
    # il faudrait prendre en compte les inversions des vecteurs précédents dans les suivants 
# il faudrait à chaque vecteur inversé, recalculer ceux qui ont été estimés à partir de lui
    # il faut d'abord inversé les vecteur dans la fonction with écho
    
    # Cas des tirs ayant eu le temps de faire un demi tour ----------------------
    # Inverser les vecteurs qui on changé de plan de rotation par rapport aux
    # vecteurs dont ils sont interpolés : ils ont dépassé le demi-tour (n fois)
    # (1/40)/(5*(10^-5))
    # (1/frot)/2 = 1/40 : temps mis pour faire un demi tour
    # gaps>(1/40) -> vecteurs inversés
    demitour <- (1/frot)/2 # frot : frequency of rotation in Hz
    val <- ceiling(diff-time/demitour)-1 # ils ont dépassé le demi-tour
    # Les impairs c'est dans le plan oposé à ceux des vecteurs dont ils sont interpolés 
    # Les pairs ont eu le temps de revenir dans le même plan
    impair <- val%%2 != 0 
    
    gaps_vector_coord$x_dir[impair] <- gaps_vector_coord$x_dir[impair]*(-1)
    gaps_vector_coord$y_dir[impair] <- gaps_vector_coord$y_dir[impair]*(-1)
    gaps_vector_coord$y_dir[impair] <- gaps_vector_coord$z_dir[impair]*(-1)
    
    # Bind the vectors table of all the rings
    vector_coord_Rings <- rbind(vector_coord_Rings, gaps_vector_coord)
    
  } # end each Ring
  
  return(vector_coord_Rings)
}