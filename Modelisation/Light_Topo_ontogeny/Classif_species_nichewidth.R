# 1) At least 2 significantly different value?

# Posteriors overlap
# j'ai récupéré l'entièreté de chaque posterior, et pour chaque paire de catégories ordonnées de DBH,
# j'ai calculé les overlap de leur posterior avec la fonction bayestestR::overlap() de manière à prendre en compte
# pas seulement le range du posterior mais aussi la forme de sa distribution.
# J'ai fait ça pour chaque espèce. Ensuite pour chaque espèce
# j'ai sélectionné son overlap minimum enregistré et
# j'ai fait un histogramme pour décider de l'overlap minimum acceptable
# pour considérer deux valeurs de a comme différentes, similaires ou on ne sait pas.
# Je me suis aidée des biplot O-a par espèce pour visualiser ses recouvrements. 

# ------------------------------------------------------------------------------
library(bayestestR)
# S = "Guarea_costata"
# 
# x <- fits[[S]][["1-3"]]$draws("a")
# y <- fits[[S]][["3-10"]]$draws("a")
# 
# overlap(x, y) # 41.5%
# plot(overlap(x, y))

# calcule Le pourcentage de la distribution partagée entre les deux postérieurs bayésien basé sur la fit de la densité sur les 4000 tirage du posterieur
# ------------------------------------------------------------------------------

d_a <- list()
for(S in names(fits)){
  # S = "Oxandra_asbeckii"
  # print(S)
  
  if(all(c("1-3", "3-10") %in% names(fits[[S]]))){
    `1-2` = round(as.numeric(overlap(fits[[S]][["1-3"]]$draws("a"), fits[[S]][["3-10"]]$draws("a")))*100,0)
  }else{`1-2` = NA}
  
  if(all(c("1-3", "10-25") %in% names(fits[[S]]))){
    `1-3` = round(as.numeric(overlap(fits[[S]][["1-3"]]$draws("a"), fits[[S]][["10-25"]]$draws("a")))*100,0)
  }else{`1-3` = NA}
  
  if(all(c("1-3", ">25") %in% names(fits[[S]]))){
    `1-4` = round(as.numeric(overlap(fits[[S]][["1-3"]]$draws("a"), fits[[S]][[">25"]]$draws("a")))*100,0)
  }else{`1-4` = NA}
  
  if(all(c("3-10", "10-25") %in% names(fits[[S]]))){
    `2-3` = round(as.numeric(overlap(fits[[S]][["3-10"]]$draws("a"), fits[[S]][["10-25"]]$draws("a")))*100,0)
  }else{`2-3` = NA}
  
  if(all(c("3-10", ">25") %in% names(fits[[S]]))){
    `2-4` = round(as.numeric(overlap(fits[[S]][["3-10"]]$draws("a"), fits[[S]][[">25"]]$draws("a")))*100,0)
  }else{`2-4` = NA}
  
  if(all(c("10-25", ">25") %in% names(fits[[S]]))){
    `3-4` = round(as.numeric(overlap(fits[[S]][["10-25"]]$draws("a"), fits[[S]][[">25"]]$draws("a")))*100,0)
  }else{`3-4` = NA}
  
  d_a[[S]] <- data.frame(Species = S) %>% 
    mutate(`1-2` = `1-2`, 
           `1-3` = `1-3`,
           `1-4` = `1-4`,
           `2-3` = `2-3`,
           `2-4` = `2-4`,
           `3-4` = `3-4`)
  
}
data_a <- list_rbind(list_flatten(d_a))

datah_a <- data_a %>% 
  pivot_longer(cols = names(data_a)[-1],
               names_to = "Pairs",
               values_to = "Cover%") %>% 
  group_by(Species) %>% 
  summarise(`MinCover%` = min(`Cover%`, na.rm = TRUE))

# Histo decision for the minimum posteriors coverage % -------------------------
ggplot(datah_a, aes(x=`MinCover%`)) +
  theme_minimal() +
  geom_histogram(fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  scale_x_continuous(trans="sqrt",n.breaks = 15) +
  labs(x= 'Minimum `a` posteriors coverage % per species', y= 'Species number')

# >= 80% de recouvrement : pas d'effet onto
# <= 20% de recouvrement : j'ai envie de dire qu'ils sont différents (20% c'est sûr)
# et entre on ne sait pas
# ------------------------------------------------------------------------------

datah_a <- datah_a %>% 
  mutate(NicheWidthDiff = case_when(
    `MinCover%` >= 80 ~ "Invariant",
    `MinCover%` <= 20 ~ "yes")) %>% 
  mutate(NicheWidthDiff = ifelse(is.na(NicheWidthDiff), "No signifiant pattern", NicheWidthDiff))

d_a <- list()
for(S in names(fits)){
  for(D in c("1-3","3-10","10-25",">25")){
    if(D %in% names(fits[[S]])){
      
      d_a[[S]][[D]] <- data.frame(
        a = fits[[S]][[D]]$summary("a", median)$median, 
        Species = S, 
        DBH = D)
      
    }}}
datam_a <- list_rbind(list_flatten(d_a))

datam_a <- datam_a %>% 
  pivot_wider(names_from = DBH,
              values_from = a) %>% 
  left_join(datah_a, by= 'Species')

# 2) Niche size for invariants
# Histo niches size -------------------------
datam_a %>% 
  select(c(Species,`1-3`,`3-10`,`10-25`,`>25`)) %>% 
  pivot_longer(cols = c(`1-3`,`3-10`,`10-25`,`>25`),
               names_to = "Pairs",
               values_to = "NicheWidth") %>% 
  # group_by(Species) %>% 
  # summarise(`MinCover%` = min(Growth, na.rm = TRUE))
  
  ggplot(aes(x=-NicheWidth)) +
  theme_minimal() +
  geom_histogram(fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  scale_x_continuous(n.breaks = 10) +
  # scale_x_continuous(trans="log", n.breaks = 10, labels = ~round(.,2)) +
  labs(x= '-a')

# >= -0.25 : Wide niche
# <= -0.75 : Narow niche
# <= -0.02 : Flat niche
# et entre : intermediate increase
# ------------------------------------------------------------------------------
datam_a <- datam_a %>% 
  mutate(NicheSize = case_when(
    NicheWidthDiff == "Invariant" &
      ((`1-3`> -.25 | is.na(`1-3`)) &
         (`3-10`> -.25 | is.na(`3-10`)) &
         (`10-25`> -.25 | is.na(`10-25`)) &
         (`>25`> -.25 | is.na(`>25`))) ~ "Wide",
    NicheWidthDiff == "Invariant" &
      ((`1-3`< -.75 | is.na(`1-3`)) &
         (`3-10`< -.75 | is.na(`3-10`)) &
         (`10-25`< -.75 | is.na(`10-25`)) &
         (`>25`< -.75 | is.na(`>25`))) ~ "Narrow"
  )) %>% 
  mutate(NicheSize = ifelse((`1-3`> -.02 | is.na(`1-3`)) &
                  (`3-10`> -.02 | is.na(`3-10`)) &
                  (`10-25`> -.02 | is.na(`10-25`)) &
                  (`>25`> -.02 | is.na(`>25`)), "Flat", NicheSize))

test_a <- datam_a %>% 
  # Compute a modif between increasing DBH class
  mutate(`1-2` = `3-10`-`1-3`, 
         # `1-3` = `10-25`-`1-3`,
         # `1-4` = `>25`-`1-3`,
         `2-3` = `10-25`-`3-10`,
         # `2-4` = `>25`-`3-10`,
         `3-4` = `>25`-`10-25`) %>% 
  # 2) Plateaus?
  mutate(Plateaus = ifelse(NicheWidthDiff== "yes" &
                             ((abs(`1-2`)<.005 & !is.na(`1-2`)) | (abs(`2-3`)<.005 & !is.na(`2-3`)) | (abs(`3-4`)<.005 & !is.na(`3-4`))),
                           "plateau", "")) %>% 
  # 3) all in the same direction?
  mutate(SameDirection = case_when(
    NicheWidthDiff== "yes" & Plateaus=="" & ((`1-2`>0 | is.na(`1-2`)) & (`2-3`>0 | is.na(`2-3`)) & (`3-4`>0 | is.na(`3-4`))) ~ "Increasing order",
    NicheWidthDiff== "yes" & Plateaus=="" & ((`1-2`<0 | is.na(`1-2`)) & (`2-3`<0 | is.na(`2-3`)) & (`3-4`<0 | is.na(`3-4`))) ~ "Decreasing order",
    .default = "")) %>% 
  mutate(SameDirection = ifelse(NicheWidthDiff== "yes" &
                                  SameDirection=="" &
                                  ((`1-2`>.005 | is.na(`1-2`)) | (`2-3`>.005 | is.na(`2-3`)) | (`3-4`>.005 | is.na(`3-4`))),
                                "not all in the same direction", SameDirection))

nrow(test_a %>% filter(NicheWidthDiff== "Invariant"))/70*100 # 7.1% no Niche width difference (5 sp)
nrow(test_a %>% filter(NicheWidthDiff== "yes"))/70*100 # 38.6% Niche width difference (27 sp)
nrow(test_a %>% filter(Plateaus == "plateau"))/27*100 # 14.8% with plateaus (4 sp)
nrow(test_a %>% filter(SameDirection == "Increasing order"))/27*100 # 0% Increasing order (0 sp)
nrow(test_a %>% filter(SameDirection == "Decreasing order"))/27*100 # 44.4% Decreasing order (12 sp)
nrow(test_a %>% filter(SameDirection == "not all in the same direction"))/27*100 # 51.9% not all in the same direction (14 sp)

# Histo growth importance -------------------------
test_a %>% 
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
  scale_x_continuous(trans="sqrt", n.breaks = 15) +
  labs(x= 'a increase between increasing pairs of DBH classes (log(transmittance))', y= 'Species number')

# >= 2 : strong increase
# <= 1 : low increase
# et entre : intermediate increase
# ------------------------------------------------------------------------------

# test_a <- test_a %>%
#   mutate(SameDirection = case_when(
#     SameDirection == "Increasing order" &  ~ "Increasing order",
#     SameDirection == "Increasing order" &  ~ "Decreasing order")) %>% 

# Contingency O-a table

Pattern_a <- test_a %>%
  mutate(a_pattern =  case_when(
    NicheWidthDiff== "yes" ~ paste(SameDirection, Plateaus),
    NicheWidthDiff!= "yes" ~ NicheWidthDiff)) %>% 
  select(Species, a_pattern)

Pattern_O <- test %>%
  mutate(O_pattern =  case_when(
    Ontoeffect== "yes" ~ paste(SameDirection, Plateaus),
    Ontoeffect!= "yes" ~ Ontoeffect)) %>% 
  select(Species, O_pattern) %>% 
  left_join(Pattern_a, by="Species")

con1 <-table(Pattern_O$O_pattern, Pattern_O$a_pattern)
con1_df <- as.data.frame(con1) %>% 
  filter(Freq>0) %>% 
  rename(O_pattern = Var1, a_pattern = Var2) %>% 
  mutate(Percent= round((Freq/70)*100, 0))
sum(con1_df$Freq) # 70


write_csv(con1_df, paste(PATH, "/Contigency_table.csv", sep=""))

mosaicplot(con1, xlab ="a", ylab = "O", cex = .5, color = T)

con2 <-table(Pattern_O$O_pattern)

# 2) Calculate percentages %
tab2 = prop.table(con1)
percent <- round(tab2*100,1)

# 3a) Create labels for each pie in the chart
pielabels <- paste(percent, "%", sep="")


# 3b) Generate the Pie Chart
pie(tab2, 
    col = c("gray","black"), 
    labels = pielabels,
    main = '% of cars by Transmission (am)', 
    cex = .6)

# 3c) Legend for the pie chart
legend("topright", 
       c("0","1"), 
       cex=0.8, 
       fill=c("gray","black"))



# 3) (Optional) Display the percentages on the Bar Plot
text(bp, 0, tab3, pos=3)
