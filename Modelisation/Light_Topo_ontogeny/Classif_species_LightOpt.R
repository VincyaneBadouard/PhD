# 1) At least 2 significantly different value?

# Posteriors overlap
# j'ai récupéré l'entièreté de chaque posterior, et pour chaque paire de catégories ordonnées de DBH,
# j'ai calculé les overlap de leur posterior avec la fonction bayestestR::overlap() de manière à prendre en compte
# pas seulement le range du posterior mais aussi la forme de sa distribution.
# J'ai fait ça pour chaque espèce. Ensuite pour chaque espèce
# j'ai sélectionné son overlap minimum enregistré et
# j'ai fait un histogramme pour décider de l'overlap minimum acceptable
# pour considérer deux valeurs de O comme différentes, similaires ou on ne sait pas.
# Je me suis aidée des biplot O-a par espèce pour visualiser ses recouvrements. 

# ------------------------------------------------------------------------------
library(bayestestR)
# S = "Eugenia_coffeifolia"
# 
# x <- fits[[S]][["1-3"]]$draws("O")
# y <- fits[[S]][["3-10"]]$draws("O")
# 
# overlap(x, y) # 24.5%
# plot(overlap(x, y))

# calcule Le pourcentage de la distribution partagée entre les deux postérieurs bayésien basé sur la fit de la densité sur les 4000 tirage du posterieur
# ------------------------------------------------------------------------------

d <- list()
for(S in names(fits)){
  # S = "Oxandra_asbeckii"
  # print(S)
  
  if(all(c("1-3", "3-10") %in% names(fits[[S]]))){
    `1-2` = round(as.numeric(overlap(fits[[S]][["1-3"]]$draws("O"), fits[[S]][["3-10"]]$draws("O")))*100,0)
  }else{`1-2` = NA}
  
  if(all(c("1-3", "10-25") %in% names(fits[[S]]))){
    `1-3` = round(as.numeric(overlap(fits[[S]][["1-3"]]$draws("O"), fits[[S]][["10-25"]]$draws("O")))*100,0)
  }else{`1-3` = NA}
  
  if(all(c("1-3", ">25") %in% names(fits[[S]]))){
    `1-4` = round(as.numeric(overlap(fits[[S]][["1-3"]]$draws("O"), fits[[S]][[">25"]]$draws("O")))*100,0)
  }else{`1-4` = NA}
  
  if(all(c("3-10", "10-25") %in% names(fits[[S]]))){
    `2-3` = round(as.numeric(overlap(fits[[S]][["3-10"]]$draws("O"), fits[[S]][["10-25"]]$draws("O")))*100,0)
  }else{`2-3` = NA}
  
  if(all(c("3-10", ">25") %in% names(fits[[S]]))){
    `2-4` = round(as.numeric(overlap(fits[[S]][["3-10"]]$draws("O"), fits[[S]][[">25"]]$draws("O")))*100,0)
  }else{`2-4` = NA}
  
  if(all(c("10-25", ">25") %in% names(fits[[S]]))){
    `3-4` = round(as.numeric(overlap(fits[[S]][["10-25"]]$draws("O"), fits[[S]][[">25"]]$draws("O")))*100,0)
  }else{`3-4` = NA}
  
  d[[S]] <- data.frame(Species = S) %>% 
    mutate(`1-2` = `1-2`, 
           `1-3` = `1-3`,
           `1-4` = `1-4`,
           `2-3` = `2-3`,
           `2-4` = `2-4`,
           `3-4` = `3-4`)
  
}
data <- list_rbind(list_flatten(d))

datah <- data %>% 
  pivot_longer(cols = names(data)[-1],
               names_to = "Pairs",
               values_to = "Cover%") %>% 
  group_by(Species) %>% 
  summarise(`MinCover%` = min(`Cover%`, na.rm = TRUE))

# Histo decision for the minimum posteriors coverage % -------------------------
ggplot(datah, aes(x=`MinCover%`)) +
  theme_minimal() +
  geom_histogram(fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  scale_x_continuous(trans="sqrt") +
  labs(x= 'Minimum posteriors coverage % per species', y= 'Species number')

# >= 80% de recouvrement : pas d'effet onto
# <= 20% de recouvrement : j'ai envie de dire qu'ils sont différents (20% c'est sûr)
# et entre on ne sait pas
# ------------------------------------------------------------------------------

datah <- datah %>% 
  mutate(Ontoeffect = case_when(
    `MinCover%` >= 80 ~ "Invariant",
    `MinCover%` <= 20 ~ "yes")) %>% 
  mutate(Ontoeffect = ifelse(is.na(Ontoeffect), "No significant pattern", Ontoeffect))

# O tree -----------------------------------------------------------------------
d <- list()
for(S in names(fits)){
  for(D in c("1-3","3-10","10-25",">25")){
    if(D %in% names(fits[[S]])){
      
      d[[S]][[D]] <- data.frame(
        O = fits[[S]][[D]]$summary("O", median)$median, 
        Species = S, 
        DBH = D)
      
    }}}
datam <- list_rbind(list_flatten(d))

datam <- datam %>% 
  pivot_wider(names_from = DBH,
              values_from = O) %>% 
  left_join(datah, by= 'Species')

test <- datam %>% 
  # Compute O modif between increasing DBH class
  mutate(`1-2` = `3-10`-`1-3`, 
         # `1-3` = `10-25`-`1-3`,
         # `1-4` = `>25`-`1-3`,
         `2-3` = `10-25`-`3-10`,
         # `2-4` = `>25`-`3-10`,
         `3-4` = `>25`-`10-25`) %>% 
  # 2) Plateaus?
  mutate(Plateaus = ifelse(Ontoeffect== "yes" &
                             ((abs(`1-2`)<.06 & !is.na(`1-2`)) | (abs(`2-3`)<.06 & !is.na(`2-3`)) | (abs(`3-4`)<.06 & !is.na(`3-4`))),
                           "plateau", "")) %>% 
  # 3) all in the same direction?
  mutate(SameDirection = case_when(
    Ontoeffect== "yes" & Plateaus=="" & ((`1-2`>0 | is.na(`1-2`)) & (`2-3`>0 | is.na(`2-3`)) & (`3-4`>0 | is.na(`3-4`))) ~ "Increasing order",
    Ontoeffect== "yes" & Plateaus=="" & ((`1-2`<0 | is.na(`1-2`)) & (`2-3`<0 | is.na(`2-3`)) & (`3-4`<0 | is.na(`3-4`))) ~ "Decreasing order",
    .default = "")) %>%  
  mutate(SameDirection = ifelse(Ontoeffect== "yes" &
                                  SameDirection=="" &
                                  ((`1-2`< -.06 | is.na(`1-2`)) | (`2-3`< -.06 | is.na(`2-3`)) | (`3-4`< -.06 | is.na(`3-4`))), # at least 1 decreasing which is not a plateau 
                                "not all in the same direction", SameDirection))

nrow(test %>% filter(Ontoeffect== "Invariant"))/70*100 
nrow(test %>% filter(Ontoeffect== "No significant pattern"))/70*100 
nrow(test %>% filter(Ontoeffect== "yes"))/70*100 
nrow(test %>% filter(Plateaus == "plateau"))/34*100 
nrow(test %>% filter(SameDirection == "Increasing order"))/34*100 
nrow(test %>% filter(SameDirection == "Decreasing order"))/34*100 
nrow(test %>% filter(SameDirection == "not all in the same direction"))/34*100 

# 8.6% no ontogenetic effect (6 sp) -> (coord cor + 2019) 2.9% (2 sp)
# 37.1% with no significant pattern (26 sp) -> 48.6% (34 sp)
# 54.3% ontogenetic effect (38 sp) -> 48.6% (34 sp)
# 10.5% with plateaus (4 sp) -> 5.9% (2 sp)
# 39.5% Increasing order (15 sp) -> 50% (17 sp)
# 2.6% Decreasing order (1 sp) -> 2.9% (1 sp)
# 50% not all in the same direction (19 sp) -> 47% (16 sp)

# Flat 1st stage ---------------------------------------------------------------
# flattable <- datam_a[,1:5] %>% 
#   pivot_longer(cols = names(datam_a[,2:5]),
#                values_to = "a") %>% 
#   mutate(Flat = ifelse(a> -0.02,"yes","no")) %>% 
#   pivot_wider(names_from = name,
#               values_from = c(a,Flat))

flattable <- datam_a[,1:2] %>% # Only first stage
  mutate(Flat_1st_stage = ifelse(`1-3`> -0.02,T,F)) %>% 
  select(Species, Flat_1st_stage)

see <- test %>% 
  left_join(flattable, by="Species") 

nrow(see %>% filter(Flat_1st_stage))/70*100 
nrow(see %>% filter(Flat_1st_stage & Ontoeffect== "yes"))/34*100 
nrow(see %>% filter(Flat_1st_stage & SameDirection == "Increasing order")) 
nrow(see %>% filter(Flat_1st_stage & SameDirection == "not all in the same direction")) 

# 11.4% with flat 1st stage among all sp (8 sp) ->  7.1% (5 sp)
# 7.9% with flat 1st stage among sp with onto effect (3 sp) ->  2.9% (1 sp)
# 2 sp with flat 1st stage among sp in increasing order ->  1 sp
# 1 sp with flat 1st stage among sp with non-linear order ->  0 sp

# Histo growth importance ------------------------------------------------------
test %>% 
  # filter(Ontoeffect== "yes") %>%
  filter(SameDirection == "Increasing order") %>%
  select(c(Species,`1-2`,`2-3`,`3-4`)) %>% 
  pivot_longer(cols = c(`1-2`,`2-3`,`3-4`),
               names_to = "Pairs",
               values_to = "Growth") %>% 
  # group_by(Species) %>% 
  # summarise(`MinCover%` = min(Growth, na.rm = TRUE))
  
  ggplot(aes(x=Growth)) +
  theme_minimal() +
  geom_histogram(fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  # scale_x_continuous(trans="sqrt", n.breaks = 15) +
  labs(x= 'O increase between increasing pairs of DBH classes (log(transmittance))')

# >= 2 : strong increase
# <= 1 : low increase
# et entre : intermediate increase
# ------------------------------------------------------------------------------

# test <- test %>%
#   mutate(SameDirection = case_when(
#     SameDirection == "Increasing order" &  ~ "Increasing order",
#     SameDirection == "Increasing order" &  ~ "Decreasing order")) %>% 


# Temperament ------------------------------------------------------------------
# exp(-2)*100 = 14
# exp(-3)*100 = 5

truc <- test %>% 
  mutate(Temperament = case_when(
    (`1-3`>= -2 | is.na(`1-3`)) &
      (`3-10`>= -2 | is.na(`3-10`)) &
      (`10-25`>= -2 | is.na(`10-25`)) &
      (`>25`>= -2 | is.na(`>25`)) ~ "Heliophile all life",
    (`1-3`<= -3 | is.na(`1-3`)) &
      (`3-10`<= -3 | is.na(`3-10`)) &
      (`10-25`<= -3 | is.na(`10-25`)) &
      (`>25`<= -3 | is.na(`>25`)) ~ "Sciaphile all life",
    
    (`1-3`> -3 & `1-3`< -2| is.na(`1-3`)) &
      (`3-10`> -3 & `3-10`< -2 | is.na(`3-10`)) &
      (`10-25`> -3 & `10-25`< -2 | is.na(`10-25`)) &
      (`>25`> -3 & `>25`< -2 | is.na(`>25`)) ~ "Intermediate all life",
    
    (`1-3`<= -3 | is.na(`1-3`)) &
      ((`3-10`>= -2 | is.na(`3-10`)) |
         (`10-25`>= -2 | is.na(`10-25`)) |
         (`>25`>= -2 | is.na(`>25`))) ~ "Shade then light",
    
    (`1-3`<= -3 | is.na(`1-3`)) &
      ((`3-10`> -3 & `3-10`< -2 | is.na(`3-10`)) |
         (`10-25`> -3 & `10-25`< -2 | is.na(`10-25`)) |
         (`>25`> -3 & `>25`< -2 | is.na(`>25`))) ~ "Shade then intermediate",
    
    (`1-3`> -3 & `1-3`< -2 | is.na(`1-3`)) &
      ((`3-10`>= -2 | is.na(`3-10`)) |
         (`10-25`>= -2 | is.na(`10-25`)) |
         (`>25`>= -2 | is.na(`>25`))) ~ "Intermediate then light",
    
    (`1-3`>= -2 | is.na(`1-3`)) &
      ((`3-10`<= -3 | is.na(`3-10`)) |
         (`10-25`<= -3 | is.na(`10-25`)) |
         (`>25`<= -3 | is.na(`>25`))) ~ "Light then shade",
    
    (`1-3`>= -2 | is.na(`1-3`)) &
      ((`3-10`> -3 & `3-10`< -2 | is.na(`3-10`)) |
         (`10-25`> -3 & `10-25`< -2 | is.na(`10-25`)) |
         (`>25`> -3 & `>25`< -2 | is.na(`>25`))) ~ "Light then intermediate"
    
  )) %>% 
  rowwise() %>% 
  mutate(Temperament = ifelse(`1-3`>= -2 & # light
                                any(`3-10`<= -3 & !is.na(`3-10`), `10-25`<= -3 & !is.na(`10-25`)) & # shade
                                any(`10-25`>= -2 & !is.na(`10-25`),`>25`>= -2 & !is.na(`>25`)), # light
                              "Light, shade then light", Temperament)) %>%
  rowwise() %>% 
  mutate(Temperament = ifelse(`1-3` <= -3 & # shade
                                any(`3-10`>= -2 & !is.na(`3-10`), `10-25`>= -2 & !is.na(`10-25`)) & # light
                                any(`10-25`<= -3 & !is.na(`10-25`),`>25`<= -3 & !is.na(`>25`)), # shade
                              "Shade, light then shade", Temperament)) %>%
  rowwise() %>% 
  mutate(Temperament = ifelse(`1-3` <= -3 & # shade
                                any(`3-10`> -3 & `3-10`< -2 & !is.na(`3-10`), `10-25`> -3 & `10-25`< -2 & !is.na(`10-25`)) & # intermediate
                                any(`10-25`<= -3 & !is.na(`10-25`),`>25`<= -3 & !is.na(`>25`)), # shade
                              "Shade, intermediate then shade", Temperament)) %>%
  rowwise() %>% 
  mutate(Temperament = ifelse(`1-3`>= -2 & # light
                                any(`3-10`> -3 & `3-10`< -2 & !is.na(`3-10`), `10-25`> -3 & `10-25`< -2 & !is.na(`10-25`)) & # intermediate
                                any(`10-25`>= -2 & !is.na(`10-25`),`>25`>= -2 & !is.na(`>25`)), # light
                              "Light, intermediate then light", Temperament)) %>%
  rowwise() %>% 
  mutate(Temperament = ifelse(`1-3`> -3 & `1-3`< -2 & # intermediate
                                any(`3-10`>= -2 & !is.na(`3-10`), `10-25`>= -2 & !is.na(`10-25`)) & # light
                                any(`10-25`> -3 & `10-25`< -2 & !is.na(`10-25`),`>25`> -3 & `>25`< -2 & !is.na(`>25`)), # intermediate
                              "Intermediate, light then intermediate", Temperament)) %>%
  ungroup()

# Without intermediate class
nrow(truc %>% filter(Temperament== "Heliophile all life"))/70*100 # 3.3% Heliophile all life (3 sp)
nrow(truc %>% filter(Temperament== "Sciaphile all life"))/70*100 # 47.1% Sciaphile all life (33 sp)
nrow(truc %>% filter(Temperament== "Shade then light"))/70*100 # 40% Shade then light (28 sp)
nrow(truc %>% filter(Temperament== "Light then shade"))/70*100 # 1.4% Light then shade (1 sp)
nrow(truc %>% filter(Temperament== "Light, shade then light"))/70*100 # 2.9% Light, shade then light (2 sp)
nrow(truc %>% filter(Temperament== "Shade, light then shade"))/70*100 # 4.3% Shade, light then shade (3 sp)

# With intermediate class
nrow(truc %>% filter(Temperament== "Heliophile all life"))/70*100 
nrow(truc %>% filter(Temperament== "Sciaphile all life"))/70*100 
nrow(truc %>% filter(Temperament== "Intermediate all life"))/70*100 
# 4.3% Heliophile all life (3 sp) -> 2.9% (2 sp)
# 30% Sciaphile all life (21 sp) -> 32.9% (23 sp)
# 0% Intermediate all life (0 sp) -> 0% (0 sp)

nrow(truc %>% filter(Temperament== "Shade then intermediate"))/70*100 
nrow(truc %>% filter(Temperament== "Light then intermediate"))/70*100 
nrow(truc %>% filter(Temperament== "Shade then light"))/70*100 
nrow(truc %>% filter(Temperament== "Intermediate then light"))/70*100 
# 8.6% Shade then intermediate (6 sp) -> 7.1% (5 sp)
# 0% Light then intermediate (0 sp) -> 0% (0 sp)
# 38.6% Shade then light (27 sp) -> 37.1% (26 sp)
# 11.4% Intermediate then light (8 sp) -> 7.1% (5 sp)
# 

nrow(truc %>% filter(Temperament== "Light then shade"))/70*100 
nrow(truc %>% filter(Temperament== "Light, shade then light"))/70*100 
nrow(truc %>% filter(Temperament== "Shade, intermediate then shade"))/70*100 
nrow(truc %>% filter(Temperament== "Light, intermediate then light"))/70*100 
nrow(truc %>% filter(Temperament== "Shade, light then shade"))/70*100 
# 1.4% Light then shade (1 sp) -> 2.9% (2 sp)
# 1.4% Light, shade then light (1 sp) -> 2.9% (2 sp)
# 2.9% Shade, intermediate then shade (2 sp) -> 1.4% (1 sp)
# 1.4% Light, intermediate then light (1 sp) -> 2.9% (2 sp)
# 0% Shade, light then shade (0 sp) -> 0% (0 sp)

# LogT : -6, -4, -2, 0
# %T : 0, 1.83, 14, 100

con3 <- truc %>% 
  group_by(Temperament) %>% 
  count()
