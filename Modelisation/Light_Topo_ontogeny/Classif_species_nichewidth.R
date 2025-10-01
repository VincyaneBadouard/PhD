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

PATH <- "//amap-data.cirad.fr/work/users/VincyaneBadouard/Modelisation/Plots/Light_topo_onto/Autocor"
fits <- readRDS(paste(PATH, "/fits.rds", sep=''))
# ------------------------------------------------------------------------------
library(bayestestR)
library(tidyverse)
library(cmdstanr)
library(bayesplot)
library(posterior)
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
  mutate(NicheWidthDiff = ifelse(is.na(NicheWidthDiff), "No significant pattern", NicheWidthDiff))

Summary <- datah_a %>% 
  group_by(NicheWidthDiff) %>% 
  summarise(N = n(),
            `%`= round(n()/nrow(.)*100, 1)) %>% 
  arrange(desc(`%`))

write_csv(Summary, paste(PATH,'/Width_change_sg_summary.csv',sep=''))

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
range(datam_a$a) # -3.46919000 -0.01220235
(max(datam_a$a)-min(datam_a$a))/100 # 1% de a = 0.03

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
         `2-3` = `10-25`-`3-10`,
         `3-4` = `>25`-`10-25`) %>% 
  # 2) Plateaus?
  mutate(Plateaus = ifelse(NicheWidthDiff== "yes" &
                             ((abs(`1-2`)<.03 & !is.na(`1-2`)) | (abs(`2-3`)<.03 & !is.na(`2-3`)) | (abs(`3-4`)<.03 & !is.na(`3-4`))),
                           "plateau", "")) %>% 
  # 3) all in the same direction?
  mutate(SameDirection = case_when(
    NicheWidthDiff== "yes" & ((`1-2`>0 | abs(`1-2`)<.03 | is.na(`1-2`)) & (`2-3`>0 | abs(`2-3`)<.03 | is.na(`2-3`)) & (`3-4`>0 | abs(`3-4`)<.03 | is.na(`3-4`))) ~ "Increasing order",
    NicheWidthDiff== "yes" & ((`1-2`<0 | abs(`1-2`)<.03 | is.na(`1-2`)) & (`2-3`<0 | abs(`2-3`)<.03 | is.na(`2-3`)) & (`3-4`<0 | abs(`3-4`)<.03 | is.na(`3-4`))) ~ "Decreasing order",
    .default = "")) %>% 
  mutate(SameDirection = ifelse(NicheWidthDiff== "yes" &
                                  SameDirection=="" &
                                  ((`1-2`>.03 | is.na(`1-2`)) | (`2-3`>.03 | is.na(`2-3`)) | (`3-4`>.03 | is.na(`3-4`))), # at least 1 increasing which is not a plateau 
                                "not all in the same direction", SameDirection))

Summary <- test_a %>% 
  filter(NicheWidthDiff== "yes") %>% 
  group_by(SameDirection) %>% 
  summarise(N = n(),
         `%`= round(n()/nrow(.)*100, 1)) %>% 
  arrange(desc(`%`))

write_csv(Summary, paste(PATH,'/Width_change_summary.csv',sep=''))

nrow(test_a %>% filter(NicheWidthDiff== "Invariant"))/70*100 
nrow(test_a %>% filter(NicheWidthDiff== "No significant pattern"))/70*100 
nrow(test_a %>% filter(NicheWidthDiff== "yes"))/70*100 
nrow(test_a %>% filter(Plateaus == "plateau"))/23*100 
nrow(test_a %>% filter(SameDirection == "Increasing order"))/23*100 
nrow(test_a %>% filter(SameDirection == "Decreasing order"))/23*100 
nrow(test_a %>% filter(SameDirection == "not all in the same direction"))/23*100 

# 7.1% no Niche width difference (5 sp) -> (coord cor + 2019) 2.9% (2 sp)
# 54.3% no significant niche width difference (38 sp) ->  64.3% (45 sp)
# 38.6% Niche width difference (27 sp) ->  32.9% (23 sp)
# 14.8% with plateaus (4 sp) ->  34.8% (8 sp)
# 0% Increasing order (0 sp) ->  4.3% (1 sp)
# 44.4% Decreasing order (12 sp) ->  17.4% (4 sp)
# 51.9% not all in the same direction (14 sp) ->  56.5% (13 sp)



# Histo growth importance -------------------------
test_a %>% 
  filter(NicheWidthDiff== "yes") %>%
  # filter(SameDirection == "Decreasing order") %>%
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
  labs(x= 'a decrease between increasing pairs of DBH classes', y= 'Species number')

# >=  : strong increase
# <=  : low increase
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

con2df <- as.data.frame(con1) %>% # real contingency table
  rename(O_pattern = Var1, a_pattern = Var2) %>% 
  pivot_wider(names_from = a_pattern,
              values_from = Freq) %>% 
  mutate(O_pattern = factor(O_pattern,
                            levels = c("Invariant", "Increasing order ", "Decreasing order ","not all in the same direction ","not all in the same direction plateau"," plateau","No significant pattern"))) %>% 
  arrange(O_pattern) %>% 
  select("O_pattern","Invariant","Decreasing order ","not all in the same direction ","not all in the same direction plateau"," plateau","No significant pattern")

write_csv(con2df, paste(PATH, "/Contigency_table.csv", sep=""))

# mosaicplot(con1, xlab ="a", ylab = "O", cex = .5, color = T)
# 
# con2 <-table(Pattern_O$O_pattern)
# 
# # 2) Calculate percentages %
# tab2 = prop.table(con1)
# percent <- round(tab2*100,1)
# 
# # 3a) Create labels for each pie in the chart
# pielabels <- paste(percent, "%", sep="")
# 
# 
# # 3b) Generate the Pie Chart
# pie(tab2, 
#     col = c("gray","black"), 
#     labels = pielabels,
#     main = '% of cars by Transmission (am)', 
#     cex = .6)
# 
# # 3c) Legend for the pie chart
# legend("topright", 
#        c("0","1"), 
#        cex=0.8, 
#        fill=c("gray","black"))
# 
# 
# 
# # 3) (Optional) Display the percentages on the Bar Plot
# text(bp, 0, tab3, pos=3)
