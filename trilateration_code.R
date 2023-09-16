#' trilateration
#'
#' @description
#' Inspired by the script of Guillaume Salzet.
#' Nous cherchons à estimer les coordonées d'un point en fonction des distances
#' de ce point à 3 autres points connus. Il existe une source d'erreur dans la
#' mesure de ces distances.
#' L'objectif est donc de minimiser l'erreur entre ces 3 distances de manière à
#' trouver le point le plus probable.
#' L’estimation se fonde sur une minimisation de l’erreur de la somme des carrés
#' des distances.
#' Améliorations : Il serait pertinent de mettre un poids différent dans
#' l’estimation, car les mesures de faible distance sont plus fiables que les
#' longues distances.
#'
#' @param Target (data.frame) Table of points to estimate with the following
#'   variables: 
#'   - ID_measure
#'   - SubPlot
#'   - TreeNum1, TreeNum2, TreeNum3 : IDs of neighbouring trees
#'   - DTree1, DTree2, DTree3 : distances from the neighbouring trees to the
#'      point to estimate
#'   
#' @param RefTrees (data.frame) Table with the coordinates of known reference
#'   points with the following variables: SubPlot, TreeFieldNum, Xutm, Yutm.
#'
#' @return (data.frame) The input 'Target' table with the estimated coordinates:
#'   "Xutm_est", "Yutm_est".
#' 
#' @export
#' @import tidyverse
#'
#' @examples
#' trilateration(Target, RefTrees)
#' 
trilateration <- function(Target, RefTrees){  
  
  # Data prep (peut-être pas utile) -------------------------------------------
  
  # 3 pcq 3 arbres de ref !
  # Trees id (TreeFieldNum)
  DATA_trilateration_ID <- c(matrix(data = as.integer(unlist(
    Target %>% select(TreeNum1,TreeNum2,TreeNum3))),
    nrow = 3, byrow = TRUE))
  # Trees distance
  DATA_trilateration_Dist <- c(matrix(data = as.numeric(unlist(
    Target %>% select(DTree1,DTree2,DTree3))),
    nrow = 3,byrow = TRUE))
  
  # sous forme verticale (moi je l'ai en horizontale)
  DATA_trilateration <- tibble(
    ID_measure = rep(Target$ID_measure, each = 3), # sensor ID ?
    TreeFieldNum = as.integer(DATA_trilateration_ID),
    SubPlot = as.numeric(rep(Target$SubPlot, each = 3)),
    TreeFieldDist = as.numeric(DATA_trilateration_Dist)) %>%
    left_join(RefTrees, by = c("SubPlot","TreeFieldNum"))  %>%
    na.exclude()
  
  # Le travail en lui même ----------------------------------------------------
  # norm_vec <- function(x) sqrt(sum((x[1]-data_ref$x)^2+(x[2]-data_ref$x)^2))-sum(data_ref$r)
  # nlm(norm_vec,c(mean(data_ref$x),mean(data_ref$y)))
  
  # Estimation
  DATA_trilateration_est <- DATA_trilateration %>%
    group_by(ID_measure) %>%
    filter(n() >= 3) %>% # nbr de points de ref
    # minimisation de l’erreur (nlm)
    do(center = paste(nlm(function(x){
      # de la somme des carrés des distances :
      sqrt(sum((x[1]-.$Xutm)^2+(x[2]-.$Yutm)^2))-sum(.$TreeFieldDist)
    }, 
    c(mean(.$Xutm), mean(.$Yutm)))$estimate, collapse = ";")) %>%
    unnest(center) %>%
    separate(center, c("Xutm_est", "Yutm_est"), sep = ";") %>%
    ungroup()
  
  
  Target <- Target %>%
    left_join(DATA_trilateration_est, by = "ID_measure")
  
  # To see:
  # Target_sf <- sf::st_as_sf(na.omit(Target), coords = c("Xutm_est", "Yutm_est"),
  #                        crs = sf::st_crs(32622))
  # 
  # # Simple feature leaflet map that is coloured
  # simplevis::leaf_sf_col(Target_sf,
  #                        col_var = ID_measure) 
  # plot(Target_sf)
  
  
  return(Target)
  
}