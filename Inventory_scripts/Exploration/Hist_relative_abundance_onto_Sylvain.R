
# Hist relative abundance + onto de Sylvain
library(tidyverse)
load("~/Téléchargements/Data_for_Hist_relative_abundances.RData")
step <- 0.05 # to be played with depending on counts per class and resolution you want for x
data <- data %>%
  mutate(Transmittance_class = cut(Transmittance, 
                                   breaks = c(seq(0, 1, by = step)), 
                                   labels = c(seq(0, 1-step, by = step)+step/2))) %>% 
  mutate(Transmittance_class = as.numeric(as.character(Transmittance_class))) %>% 
  group_by(Size, Transmittance_class) %>% 
  mutate(ComAbundSize = n()) %>% 
  ungroup()
sp <- unique(data$ScientificName)[1]
dataS <- data %>% 
  filter(ScientificName == sp) %>% 
  group_by(Size, Transmittance_class) %>% 
  summarise(RelativeAbund = n()/ComAbundSize)
ggplot(dataS, aes(Transmittance_class, RelativeAbund)) +
  geom_col() +
  facet_wrap(~ Size, scales = "free_y", ncol=1) +
  theme_bw()