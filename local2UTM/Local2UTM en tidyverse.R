# Local2UTM en tidyverse

path <- # ton chemin vers la table de coordonnées UTM
tab <- read_delim(path,
                  delim = ";", escape_double = FALSE, trim_ws = TRUE) 

P=16 # ici tu mets ton numéro de parcelle
limx = 500 # (m de côté) à changer pour la parcelle concernée
limy = 500 # (m de côté) à changer pour la parcelle concernée

dataUTM <- data %>% 
  mutate(Xutm = (1-x/limx)*(1-y/limy)*tab[P,]$`so x`+(x/limx)*(1-y/limy)*tab[P,]$`se x`+(y/limy)*(1-x/limx)*tab[P,]$`no x`+((x*y)/(limx*limy))*tab[P,]$`ne x`) %>% 
  mutate(Yutm = (1-x/limx)*(1-y/limy)*tab[P,]$`so y`+(x/limx)*(1-y/limy)*tab[P,]$`se y`+(y/limy)*(1-x/limx)*tab[P,]$`no y`+((x*y)/(limx*limy))*tab[P,]$`ne y`)