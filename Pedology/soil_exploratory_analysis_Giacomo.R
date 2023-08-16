# load packages
library(readr)

#load data
Labo_data_binded <- read_csv("data/Labo_data_binded.csv")
View(Labo_data_binded)

#create tripartite texture
Labo_data_binded$`Sand %` <- Labo_data_binded$`Sables fins %` + Labo_data_binded$`Sables grossiers %`
Labo_data_binded$`Silt %` <- Labo_data_binded$`Limons fins %` + Labo_data_binded$`Limons grossiers %`
Labo_data_binded$`Clay %` <- Labo_data_binded$`Argiles %`

#select only bas-fond and plateaux samples.
#I hide the cations and anions because they're redundant with CEC
ACP_Data <- subset (Labo_data_binded, grepl("CH-BF", Labo_data_binded$'Sample ID') |
                      grepl("CH-Pl", Labo_data_binded$'Sample ID'), 
                    select =c ("Sample ID", "MO %", "Corg %", "N ‰", "Corg/N\r\n", #"Ca me/100g",
                               #"Mg me/100g","K me/100g", "Na me/100g", "Al me/100g", "Mn me/100g",
                               #"H me/100g", "Σ_CationsEch me/100g", 
                               "CEC me/100g" , "TS %",
                               "pHCo\r\n", "Phosphore assimilable - Ext Olsen P-PO4 mg/kg",
                               "Phosphore assimilable - Ext Bray I P-PO4 mg/kg", "pH eau\r\n",
                               "P total mg/kg", "Clay %", "Sand %", "Site", "Topgraphy" ))

#do the scaled pca
res.pca <- prcomp(ACP_Data[2:14], scale = TRUE)
groups <- as.factor(ACP_Data$Site)

fviz_pca_biplot(res.pca, col.ind = groups, # color by groups
                palette = c("#00AFBB",  "#FC4E07"),
                legend.title = "Groups",
                repel = TRUE
)

#boxplots
ACP_Data_long <- melt(ACP_Data[2:15], id = "Site")  

ggplot(ACP_Data_long, aes(x = variable, y = value, color = Site)) +  # ggplot function
  geom_boxplot()+ facet_wrap(facets = ~variable, scales = 'free')
