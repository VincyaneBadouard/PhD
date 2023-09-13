# Inspired by the script of Guillaume Salzet Nous cherchons à estimer les
# coordonées d'un point en fonction des distances de ce point à 3 autres points
# connus. Il existe une source d'erreur dans la mesure de ces distances.
# L'objectif est donc de minimiser l'erreur entre ces 3 distances de manière à
# trouver le point le plus probable.
# l’estimation se fonde sur une minimisation de l’erreur de la somme des carrés
# des distances.
# Il serait pertinent de mettre un poids différent dans l’estimation, car les
# mesures de faible distance sont plus fiables que les longues distances.

library(tidyverse)
library(sf)
library(simplevis)

RefTrees <- Guyafor %>% filter(Forest == "Paracou" & Plot == "16") %>%
  select(SubPlot, TreeFieldNum, Xutm,Yutm) %>% distinct()
RefTrees <- Paracou16_2020 # Coordinates of known reference points with SubPlot, TreeFieldNum, Xutm, Yutm
Target <- coord_table # Table of points to estimate

# X : x subplot Y : y subplot
# ID_measure et ID_ref c'est l'identifiant unique de mon point à estimer ?
# T1C,T2C,T3C,T4C (les carrés dans le sens de understory de 10x10m).
# les arbres identifiés les plus proches des plantules peuvent se situer dans un
# carré voisin.
# _sf indique que l’objet est de la classe sf
# _est correspond à « estimate ».

---------------------------------------------------------------------------------
  
  trilateration_code <- function(RefTrees,
                                 Target
  )
  {  
    
    # Data prep (peut-être pas utile) -------------------------------------------
  
    # 4 pcq 4 arbres de ref !
    # Trees id (TreeFieldNum)
    DATA_trilateration_ID <- c(matrix(data = as.integer(unlist(
      Target %>% select(TreeNum1,TreeNum2,TreeNum3))),
      nrow = 3, byrow = TRUE))
    # Trees distance
    DATA_trilateration_Dist <- c(matrix(data = as.numeric(unlist(
      Target %>% select(DTree1,DTree2,DTree3))),
      nrow = 3,byrow = TRUE))
    # Trees location (SubPlot)
    # DATA_trilateration_C <- c(matrix(data = as.integer(unlist(
    #   Target %>% select(T1C,T2C,T3C,T4C))),
    #   nrow = 3,byrow = TRUE))
    
    # sous forme verticale (moi je l'ai en horizontale)
    DATA_trilateration_sf <- tibble(
      ID_measure = rep(Target$ID_measure, each = 3), # sensor ID ?
      TreeFieldNum = as.integer(DATA_trilateration_ID),
      SubPlot = as.numeric(rep(Target$SubPlot, each = 3)),
      TreeFieldDist = as.numeric(DATA_trilateration_Dist)) %>%
      left_join(RefTrees, by = c("SubPlot","TreeFieldNum"))  %>%
      na.exclude()
    # je n'ai pas assez d'arbres en commun (cirad) créer un faux jeux de données !
    
    # st_write(DATA_trilateration_sf, dsn = "DATA_trilateration.shp",append=FALSE)
    
    # Le travail en lui même ----------------------------------------------------
    # norm_vec <- function(x) sqrt(sum((x[1]-data_ref$x)^2+(x[2]-data_ref$x)^2))-sum(data_ref$r)
    # nlm(norm_vec,c(mean(data_ref$x),mean(data_ref$y)))
    
    # Estimation
    DATA_trilateration_est <- DATA_trilateration_sf %>%
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
    
    
    # st_write(DATA_trilateration_est , dsn = "DATA_trilateration_est.shp",append=FALSE)
    
    # DATA_trilateration_test <- DATA_trilateration_sf %>%
    #   st_as_sf(coords = c("Xutm","Yutm"), crs = st_crs(32622)) %>%
    #   group_by(ID_measure) %>%
    #   do(dist = all(st_is_within_distance(.,DATA_trilateration_est%>%
    #                                         st_as_sf(coords = c("Xutm_est","Yutm_est"), crs = st_crs(32622)) %>%
    #                                         filter(ID_measure == .$ID_measure),dist = 10,sparse = FALSE))) %>%
    #   ungroup()
    
    Target <- Target %>%
      left_join(DATA_trilateration_est, by = "ID_measure")
    
    
    
    # Simple feature leaflet map that is coloured
    simplevis::leaf_sf_col(Target,
                col_var = ID_measure) 
    
    
    return(Target_sf)
    
  }