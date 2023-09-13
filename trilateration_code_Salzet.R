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

# X : x subplot Y : y subplot
# T1C,T2C,T3C,T4C (les carrés dans le sens de understory de 10x10m).
# les arbres identifiés les plus proches des plantules peuvent se situer dans un
# carré voisin.
# _sf indique que l’objet est de la classe sf
# _est correspond à « estimate ».

---------------------------------------------------------------------------------
# RefTrees <- read.csv2("Paracou_P16_C1015_2020.csv") %>%
#   select(SubPlot, TreeFieldNum,Xutm,Yutm) %>% distinct()
# Target <- readxl::read_excel("Plantules r?pertori?es.xlsx")

# load("~/Work_in_progress/These/Data/Guyafor_Bota.RData")

# RefTrees <- Guyafor %>% filter(Forest == "Paracou"& Plot == "16"& (SubPlot == 10| SubPlot == 15)) %>%
#   select(SubPlot, TreeFieldNum,Xutm,Yutm) %>% distinct()

DATA_trilateration <- readxl::read_excel("Plantules r?pertori?es.xlsx") %>%
  select(Carre, X, Y, T1C,T1Id,T1D,T2C,T2Id,T2D,T3C,T3Id,T3D,T4C,T4Id,T4D) %>%
  distinct() %>%
  group_by(Carre,X,Y) %>%
  mutate(ID_ref = paste0("Subplot_",Carre,"_X_",X,"_Y_",Y,"_meas_",cur_group_id())) %>%
  ungroup()

# Trees ids
DATA_trilateration_ID <- c(matrix(data = as.integer(unlist(
  DATA_trilateration %>% select(T1Id,T2Id,T3Id,T4Id))),
  nrow = 4, byrow = TRUE))
# Trees distance
DATA_trilateration_Dist <- c(matrix(data = as.numeric(unlist(
  DATA_trilateration %>% select(T1D,T2D,T3D,T4D))),
  nrow = 4,byrow = TRUE))
# Trees subplot
DATA_trilateration_C <- c(matrix(data = as.integer(unlist(
  DATA_trilateration %>% select(T1C,T2C,T3C,T4C))),
  nrow = 4,byrow = TRUE))

DATA_trilateration_sf <- tibble(plot_P16 = rep(DATA_trilateration$Carre, each = 4),
                                X_subplot = rep(DATA_trilateration$X, each = 4),
                                Y_subplot = rep(DATA_trilateration$Y, each = 4),
                                ID_measure = rep(DATA_trilateration$ID_ref, each = 4),
                                TreeFieldID = as.integer(DATA_trilateration_ID),
                                TreeFieldC = as.numeric(DATA_trilateration_C),
                                TreeFieldDist = as.numeric(DATA_trilateration_Dist)) %>%
  left_join(RefTrees %>% select(SubPlot,TreeFieldNum, Xutm, Yutm),
            by = c("TreeFieldC" = "SubPlot","TreeFieldID" = "TreeFieldNum"))  %>%
  na.exclude()

# st_write(DATA_trilateration_sf, dsn = "DATA_trilateration.shp",append=FALSE)

DATA_trilateration_est <- DATA_trilateration_sf %>%
  group_by(ID_measure) %>%
  filter(n() >= 3) %>%
  do(center = paste(nlm(function(x){
    sqrt(sum((x[1]-.$Xutm)^2+(x[2]-.$Yutm)^2))-sum(.$TreeFieldDist)
  }, 
  c(mean(.$Xutm), mean(.$Yutm)))$estimate, collapse = ";")) %>%
  unnest(center) %>%
  separate(center, c("Xutm_ref", "Yutm_ref"), sep = ";") %>%
  ungroup()


st_write(DATA_trilateration_est , dsn = "DATA_trilateration_est.shp",append=FALSE)

# DATA_trilateration_test <- DATA_trilateration_sf %>%
#   st_as_sf(coords = c("Xutm","Yutm"), crs = st_crs(32622)) %>%
#   group_by(ID_measure) %>%
#   do(dist = all(st_is_within_distance(.,DATA_trilateration_est%>%
#                                         st_as_sf(coords = c("Xutm_ref","Yutm_ref"), crs = st_crs(32622)) %>%
#                                         filter(ID_measure == .$ID_measure),dist = 10,sparse = FALSE))) %>%
#   ungroup()

Target <- read_delim("Plantules r?pertori?es.csv",
                     delim = ";", escape_double = FALSE, trim_ws = TRUE) %>%
  mutate(specie = substr(Id,1,1)) %>%
  group_by(Carre,X,Y) %>%
  mutate(ID_measure = paste0("Subplot_",Carre,"_X_",X,"_Y_",Y,"_meas_",cur_group_id())) %>%
  ungroup() %>%
  left_join(DATA_trilateration_est, by = "ID_measure") %>%
  ungroup() %>%
  select(Carre,X,Y,specie,Angle,Distance,Xutm_ref,Yutm_ref) %>%
  mutate(Angle = as.numeric(Angle),
         Distance = as.numeric(Distance)) %>%
  na.exclude()

Target$Angle <- as.numeric(Target$Angle)
Target$Distance <- as.numeric(Target$Distance)
Target$Xutm_ref <- as.numeric(Target$Xutm_ref)
Target$Yutm_ref <- as.numeric(Target$Yutm_ref)

Target_sf  <- Target %>%
  mutate(Distance = if_else(Distance < sqrt(2)*10,
                            Distance, if_else(Distance < sqrt(2)*10,
                                              Distance, Distance/10000)),
         Angle = if_else(Angle <= 360 , Angle, Angle/100)) %>%
  mutate(Xutm = cos((as.numeric(Angle)/360)*2*pi)*as.numeric(Distance) + Xutm_ref,
         Yutm = sin((as.numeric(Angle)/360)*2*pi)*as.numeric(Distance) + Yutm_ref) %>%
  st_as_sf(coords = c("Xutm","Yutm"),crs = st_crs(32622))

st_write(Target_sf , dsn = "Target_est.shp",append=FALSE)

leaf_sf_col(Target_sf,
            col_var = specie)
