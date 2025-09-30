# Proportion of small individuals among non-indentified trees

data <- read_csv("D:/Mes Donnees/PhD/Inventories/Data/Adults_Understory/Paracou/P16_2019_Paracou_InvandEnv.csv")

# data <- UnderstoryP
undind <- data %>%
  filter(is.na(ScientificName) | # is.na(Genus) | is.na(Species) |
           grepl("indet", ScientificName) | grepl("Indet", ScientificName) | grepl("aceae", ScientificName)|
           (grepl("_sp", ScientificName) & grepl("\\.", ScientificName)))

small_undind <- undind %>% 
  filter(DBHcor<=20)

big_undind <- undind %>% 
  filter(DBHcor>20)

unique(undind$ScientificName)

nrow(small_undind)/nrow(undind)*100 # 95%
