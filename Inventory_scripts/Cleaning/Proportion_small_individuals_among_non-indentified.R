# Proportion of small individuals among non-indentified trees

data <- read_csv("D:/Mes Donnees/PhD/Inventories/Data/Adults_Understory/Paracou/P16_2019_Paracou_InvandEnv.csv")

# data <- UnderstoryP
"parmi les non-identifiés combien sont <20 cm DBH:"
undind <- data %>%
  filter(is.na(ScientificName) | # is.na(Genus) | is.na(Species) |
           grepl("indet", ScientificName) | grepl("Indet", ScientificName) | grepl("aceae", ScientificName)|
           (grepl("_sp", ScientificName) & grepl("\\.", ScientificName)))

small_undind <- undind %>% 
  filter(DBHcor<=20)

big_undind <- undind %>% 
  filter(DBHcor>20)

unique(undind$ScientificName)

nrow(small_undind)/nrow(undind)*100 # 95% of unidentified trees are <= 20 cm DBH


"parmi les <20 cm DBH combien sont non-identifiés:"
small <- data %>%
  filter(DBHcor<=20)

big <- data %>% 
  filter(DBHcor>20)

undind_small <- small %>% 
  filter(is.na(ScientificName) | # is.na(Genus) | is.na(Species) |
           grepl("indet", ScientificName) | grepl("Indet", ScientificName) | grepl("aceae", ScientificName)|
           (grepl("_sp", ScientificName) & grepl("\\.", ScientificName)))

undind_big <- big %>% 
  filter(is.na(ScientificName) | # is.na(Genus) | is.na(Species) |
           grepl("indet", ScientificName) | grepl("Indet", ScientificName) | grepl("aceae", ScientificName)|
           (grepl("_sp", ScientificName) & grepl("\\.", ScientificName)))

nrow(undind_small)/nrow(small)*100 # 14% of <= 20 cm DBH trees are unidentified
nrow(undind_big)/nrow(big)*100 # 7% of > 20 cm DBH trees are unidentified

