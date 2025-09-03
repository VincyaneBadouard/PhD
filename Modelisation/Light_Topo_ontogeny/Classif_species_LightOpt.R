# 1) At least 2 significantly different value?

# Posteriors overlap
# j'ai récupéré l'entièreté de chaque posterior, et **pour chaque paire de catégories de DBH**,
# j'ai calculé les overlap de leur posterior avec la fonction bayestestR::overlap() de manière à prendre en compte
# pas seulement le range du posterior mais aussi la forme de sa distribution.
# J'ai fait ça pour chaque espèce. Ensuite pour chaque espèce
# j'ai sélectionné son overlap minimum enregistré et
# j'ai fait un histogramme pour décider de l'overlap minimum acceptable
# pour considérer deux valeurs de O comme différentes, similaires ou on ne sait pas.
# Je me suis aidée des biplot O-a par espèce pour visualiser ses recouvrements. 

PATH <- "//amap-data.cirad.fr/work/users/VincyaneBadouard/Modelisation/Plots/Light_topo_onto/Autocor"
fits <- readRDS(paste(PATH, "/fits.rds", sep=''))

# ------------------------------------------------------------------------------
library(bayestestR)
library(tidyverse)
library(cmdstanr)
library(bayesplot)

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

# datah <- data %>% 
#   pivot_longer(cols = names(data)[-1],
#                names_to = "Pairs",
#                values_to = "Cover%") %>% 
#   group_by(Species) %>% 
#   summarise(`MinCover%` = min(`Cover%`, na.rm = TRUE))

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

# datah <- datah %>% 
#   mutate(Ontoeffect = case_when(
#     `MinCover%` >= 80 ~ "Invariant",
#     `MinCover%` <= 20 ~ "yes")) %>% 
#   mutate(Ontoeffect = ifelse(is.na(Ontoeffect), "No significant pattern", Ontoeffect))

# O tree -----------------------------------------------------------------------
d <- list()
for(S in names(fits)){
  for(D in c("1-3","3-10","10-25",">25")){
    if(D %in% names(fits[[S]])){
      
      # d[[S]][[D]] <- data.frame(
      #   O = fits[[S]][[D]]$summary("O", median)$median, 
      #   Species = S, 
      #   DBH = D)
      
      d[[S]][[D]] <- fits[[S]][[D]]$summary(c("O","a"), median) %>% 
        mutate(Species = S) %>% 
        mutate(DBH = D)
    }}}
# Detect flat niche -> don't consider O for a flat niche
datam <- list_rbind(list_flatten(d)) %>% 
  pivot_wider(names_from = variable,
              values_from = median) %>% 
  mutate(O = ifelse(a > -0.02, 'flat', O))


datam <- datam %>% 
  select(-a) %>% 
  pivot_wider(names_from = DBH,
              values_from = O) %>% 
  left_join(data, by= 'Species') # datah

datam <- datam %>% 
  mutate_at(vars(c("1-2", "1-3.y", "1-4")), ~ ifelse(`1-3.x`=='flat', NA, .)) %>% # 1
  mutate_at(vars(c("1-2", "2-3", "2-4")), ~ ifelse(`3-10`=='flat', NA, .)) %>% # 2
  mutate_at(vars(c("1-3.y","2-3", "3-4")), ~ ifelse(`10-25`=='flat', NA, .)) %>% # 3
  mutate_at(vars(c("1-4", "2-4", "3-4")), ~ ifelse(`>25`=='flat', NA, .)) # 4

datam <- datam %>% 
  rowwise() %>% 
  mutate(Ontoeffect = case_when(
    min(`1-2`, `1-3.y`, `1-4`, `2-3`, `2-4`, `3-4`, na.rm=T) >= 80 ~ "Invariant",
    min(`1-2`, `1-3.y`, `1-4`, `2-3`, `2-4`, `3-4`, na.rm=T) <= 20 ~ "yes")) %>% 
  mutate(Ontoeffect = ifelse(is.na(Ontoeffect), "No significant pattern", Ontoeffect)) %>% 
  rowwise() %>% 
  # flat then spé -> onto effect ('yes')
  mutate(Ontoeffect = ifelse(Ontoeffect=="Invariant" &
                               any(na.omit(c(`1-3.x`=='flat',
                                             `3-10`=='flat',
                                             `10-25`=='flat',
                                             `>25`=='flat'))) & any(na.omit(c(`1-3.x`!='flat',
                                                                              `3-10`!='flat',
                                                                              `10-25`!='flat',
                                                                              `>25`!='flat'))), 'flatspe', Ontoeffect)) %>% 
  ungroup()

test <- datam %>% 
  select(-c(`1-2`, `1-3.y`, `1-4`, `2-3`, `2-4`, `3-4`)) %>% 
  rename(`1-3`=`1-3.x`) %>% 
  mutate_at(c("1-3","3-10","10-25",">25"), ~ recode(., "flat" = NA_character_, .default =.)) %>% # flat -> NA
  mutate_at(c("1-3","3-10","10-25",">25"), ~ as.numeric(.)) %>% 
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
    Ontoeffect== "yes" & ((`1-2`>0 | is.na(`1-2`)) & (`2-3`>0 | is.na(`2-3`)) & (`3-4`>0 | is.na(`3-4`))) ~ "Increasing order",
    Ontoeffect== "yes" & ((`1-2`<0 | is.na(`1-2`)) & (`2-3`<0 | is.na(`2-3`)) & (`3-4`<0 | is.na(`3-4`))) ~ "Decreasing order",
    .default = "")) %>%  
  mutate(SameDirection = ifelse(Ontoeffect== "yes" &
                                  SameDirection=="" &
                                  ((`1-2`< -.06 | is.na(`1-2`)) | (`2-3`< -.06 | is.na(`2-3`)) | (`3-4`< -.06 | is.na(`3-4`))), # at least 1 decreasing which is not a plateau 
                                "not all in the same direction", SameDirection)) %>% 
  mutate(SameDirection = ifelse(Ontoeffect=="flatspe", "Last stage specialisation", SameDirection)) %>% 
  mutate(Ontoeffect = ifelse(Ontoeffect=="flatspe", "yes", Ontoeffect))

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

# flattable <- datam_a[,1:2] %>% # Only first stage
#   mutate(Flat_1st_stage = ifelse(`1-3`> -0.02,T,F)) %>% 
#   select(Species, Flat_1st_stage)
# 
# see <- test %>% 
#   left_join(flattable, by="Species") 

# nrow(see %>% filter(Flat_1st_stage))/70*100 
# nrow(see %>% filter(Flat_1st_stage & Ontoeffect== "yes"))/34*100 
# nrow(see %>% filter(Flat_1st_stage & SameDirection == "Increasing order")) 
# nrow(see %>% filter(Flat_1st_stage & SameDirection == "not all in the same direction")) 

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
  filter(Ontoeffect != "No significant pattern") %>% 
  pivot_longer(cols = c("1-3","3-10","10-25",">25"),
               values_to = "O") %>% 
  mutate(Temp = case_when(
    O >= -2.3 ~ "light", #  -2.3 -> 10%T ; -2 -> 14%T
    O <= -3.5 ~ "shade", # -3.5 -> 3%T ; -3 -> 5%T
    O > -3.5 & O < -2.3 ~ "intermediate")) # 148 rows

truc <- datam %>% # with the flat info
  filter(Ontoeffect != "No significant pattern") %>% 
  select(-c(`1-2`, `1-3.y`, `1-4`, `2-3`, `2-4`, `3-4`, Ontoeffect)) %>% 
  rename(`1-3`=`1-3.x`) %>% 
  pivot_longer(cols = c("1-3","3-10","10-25",">25"),
               values_to = "O") %>% 
  left_join(truc %>% select(-O), by = c("Species", "name")) %>% 
  mutate(Temp = ifelse(Ontoeffect != "No significant pattern" & O=='flat', "generalist", Temp)) 

muche <- truc %>%
  pivot_wider(names_from = name,
              values_from = c(O, Temp)) %>% 
  unite(starts_with("Temp_"), col = "raw", sep = " ", remove = F, na.rm = T) %>% 
  rowwise() %>% 
  # remove succesive duplicate pattern
  mutate(Temperament = gsub("\\b(\\w+)(\\s+\\1)+\\b", "\\1", raw)) %>%
  mutate(Temperament = str_replace(Temperament, " ", "-")) %>% 
  mutate(Temperament = gsub("\\s+", "-", Temperament)) 


unique(muche$Temperament) # 5%: 15 (3 quadriclasses) ; 3%: 17 (3 quadriclasses)

# pour shade = <=5%
# "shade intermediate shade light" (1)
# exp(c(-5, -2.8, -3.3))*100 # 0.6 - 6 - 3.7 % -> 3.7 peut devenir intermédiaire
# "intermediate shade light intermediate" (1)
# exp(c(-2.3, -3, -2.6))*100 # 10 - 5 - 7 % -> 5 peut devenir intermédiaire
# "light shade intermediate shade" (1)
# exp(c(- 4.2, -2, -4.3))*100 # 1.5 - 13.5 - 1.3 %


# Mono level
nrow(muche %>% filter(Temperament== "shade"))/37*100 # 13.5% Sciaphile all life (5 sp)
nrow(muche %>% filter(Temperament== "light"))/37*100 # 2.7% Heliophile all life (1 sp)
nrow(muche %>% filter(Temperament== "generalist"))/37*100 # 2.7% generalist all life (1 sp)

# Bi level
nrow(muche %>% filter(Temperament== "shade-light"))/37*100 # 29.7% shade-light (11 sp)
nrow(muche %>% filter(Temperament== "shade-intermediate"))/37*100 # 8.1% shade-intermediate (3 sp)
nrow(muche %>% filter(Temperament== "generalist-light"))/37*100 # 2.7% generalist-light (1 sp)
nrow(muche %>% filter(Temperament== "intermediate-light"))/37*100 # 2.7% intermediate-light (1 sp)
nrow(muche %>% filter(Temperament== "generalist-intermediate"))/37*100 # 2.7% generalist-intermediate (1 sp)
nrow(muche %>% filter(Temperament== "light-shade"))/37*100 # 2.7% light-shade (1 sp)

# Tri-level
nrow(muche %>% filter(Temperament== "shade-intermediate-light"))/37*100 # 16.2% shade-intermediate-light (6 sp)
nrow(muche %>% filter(Temperament== "intermediate-light-intermediate"))/37*100 # 2.7% intermediate-light-intermediate (1 sp)
nrow(muche %>% filter(Temperament== "light-shade-light"))/37*100 # 2.7% light-shade-light (1 sp)
nrow(muche %>% filter(Temperament== "light-intermediate-light"))/37*100 # 2.7% light-intermediate-light (1 sp)
nrow(muche %>% filter(Temperament== "intermediate-shade-light"))/37*100 # 2.7% intermediate-shade-light (1 sp)
nrow(muche %>% filter(Temperament== "shade-generalist-light"))/37*100 # 2.7% shade-generalist-light  (1 sp)

# Quadri level
nrow(muche %>% filter(Temperament== "light-shade-intermediate-shade"))/37*100 # 2.7% light-shade-intermediate-shade (1 sp)

badchains <- muche %>% 
  filter(Species %in% c('Hymenopus_heteromorphus','Duroia_longiflora',
                        'Eschweilera_congestiflora','Couma_guianensis',
                        'Chaetocarpus_schomburgkianus','Bocoa_prouacensis',
                        'Eugenia_coffeifolia','Catostemma_fragrans','Pradosia_cochlearia',
                        'Paypayrola_hulkiana'))

badchains$Species # "Bocoa_prouacensis" "Paypayrola_hulkiana" "Pradosia_cochlearia" "Chaetocarpus_schomburgkianus"
unique(badchains$Temperament) # "shade-light" "shade"


# LogT : -6, -4, -2, 0
# %T : 0, 1.83, 14, 100

con3 <- truc %>% 
  group_by(Temperament) %>% 
  count()
