# #Authors : Giacomo Sellan
# #Date : 07/11/2022
# 
# #this code verifies that all the rows with empty species identifications in the database (T0)
# #correspond to one herbarium samples. Any empty species identification should be 
# #noted as "prelevee" = TRUE. Nonetheless, it can happen that when we collect a sample,
# #we forget to note it as "prelevee".  
# #Julie Carvalho, VSCherbier, notes the number of any herbarium sample that we take. 
# #This list is here called "list_herbarium_samplesC18".

# packages :
library(readr)
library(tidyverse)
library(ggplot2)

# import datasets
list_herbarium_samplesC18 <- read_csv("data/list_herbarium_samplesC18.csv", 
                                      col_names = FALSE, col_types = cols(X1 = col_number()))
View(list_herbarium_samplesC18)

T0 <- read_csv("T0.csv")
View(T0)

# In Carree 18 of P16, we have collected 
nrow(list_herbarium_samplesC18)
#herbarium samples. From the database, in total there are 
nrow(T0)
# trees, we identified 
sum(!is.na(T0$species))
# trees on the field. There are 
sum(!is.na(T0$CIRAD.nb))
# CIRAD trees (3311 + 479 = 3790) and I still need to put the CIRAD data together
#with our one. Nonetheless, there are 803 trees which have no name (after having cleaned for dead trees, multiple stems not
# identified and lianas). This would result in 78 individuals that would have not been collected
# and not identified in the field. 

list_herbarium_samplesC18$X1 <- (paste0(list_herbarium_samplesC18$X1, "_1")) #create a new column to merge with a unique stem number
list_herbarium_samplesC18 <- as.data.frame(list_herbarium_samplesC18)
list_herbarium_samplesC18$Herborized <- "TRUE" #create the column to indicate we have and know the sample

T18_1 <- merge(T0, list_herbarium_samplesC18, by.x = "tree.unique", by.y = "X1", all.x = TRUE)
T18_1 <- subset(T18_1, !duplicated(T18_1$tree.unique)) #clean the duplicates that merge does (why merge does duplicates?)

#create the column specifying which tree has to be revisited in C18
T18_1$revisit <- NA
T18_1 <- T18_1 %>% 
  mutate(
    revisit = ifelse (is.na(T0$species) & # the samples we haven't any info on 
                        is.na(T0$CIRAD.nb)& 
                        !grepl("true", T0$liane, ignore.case = TRUE) & 
                        !grepl("vrai", T0$liane, ignore.case = TRUE) &
                        !grepl("true", T0$mort, ignore.case = TRUE) &
                        !grepl("vrai", T0$mort, ignore.case = TRUE) & 
                        !grepl("true", T0$tres.haut, ignore.case = TRUE) &
                        !grepl("vrai", T0$tres.haut, ignore.case = TRUE) &
                        T0$stem.nb == "1" &
                        is.na(Herborized), "Collect_me", revisit),
    revisit = ifelse (tres.haut == "TRUE" & is.na(species) & is.na(Herborized), "Big_SHot", # too big trees to be taken with the bigshot
                      revisit)
    ,
    revisit = ifelse (T18_1$Herborized == "TRUE" & !is.na(T18_1$species), "IDed_But_Herbier?", # too big trees to be taken with the bigshot
                  revisit)
  )

ggplot(T18_1, aes ( x = glob.pos.X, y = glob.pos.Y)) + geom_point(aes(color = revisit))+ coord_equal()

# There are 81 trees that are signed as "prelevee" on the dataset, but are not herborised.
# Totally we shall visit 285 trees, 
# We need to collect 69 trees with the bigshot too

write.csv(T18_1, "T18_1Appurement.csv")
