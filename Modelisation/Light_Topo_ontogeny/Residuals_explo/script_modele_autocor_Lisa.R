###### Script modele avec autocorrelation spatiale
# Bauman et al., 2018
# GD - 04/07/2022 à discuter avec Lisa
# ce script créée une fonction pour l'ensemble du workflow de modelisation
# pour une espèce
# et montre comment l'utiliser

library(adespatial)
library(MASS)
library(ggplot2)


###############################################################################################
#### Importe et prépare les données
###############################################################################################
#DataQuadrat <- read.csv(file="modele/Data_Arbres_light5_cent.csv", sep=";", dec=",", row.names = 1)
DataQuadrat <- read.csv(file="modele/Data_all_variables.csv", sep=";", dec=",", row.names = 1)
str(DataQuadrat)
DataQuadrat$quadratID <- as.factor(DataQuadrat$quadratID)
head(DataQuadrat)
summary(DataQuadrat)

# préparer les données xy 
# A FAIRE
CoordXY <- DataQuadrat[,c("Xutm", "Yutm")] # dataframe avec 2 colonnes les x et les y
plot(CoordXY)
# !!! les cooordonnées des quadrats doivent bien sur être dans le même ordre que les quadrats

# # A VIRER, juste en attendant d'avoir les données
# grid <- expand.grid(x = seq(1, 25, 1), y = seq(1, 25, 1)) 
# plot(grid)
# sample <- sample(c(1:nrow(grid)), 143, replace = FALSE) 
# xy <- grid[sample, ] 
# xy[, 1] <- sample(c(1:143), 143, replace = FALSE)
# xy[, 2] <- sample(c(1:143), 143, replace = FALSE)
# CoordXY <- xy
# plot(CoordXY)



###############################################################################################
### Préparer les matrices SWM qu'on veut tester
###############################################################################################

# on peut faire ça en dehors de la fonction car ça sera les mêmes candidates pour tous les modèles
# avec la fonction listw.candidates (tuto papier Bauman annexe 3, p.7)

# on teste 2 façons de faire la matrice de connectivité B
  # Gabriel's graph (gab)
  # Minimul spanning tree (mst)
# et deux pondérations A
  # sans pondération (binary)
  # pondération linéaire (flin)

SWM_candidate <- listw.candidates(coord = CoordXY, nb = c("gab", "mst"), 
                                  weights = c("binary", "flin"))
str(SWM_candidate, max.level = 1)
# SWM_candidate$Gabriel_Binary

# # ici avec mon ex bidon, j'ai un cas (MST_Linear) avec le message d'erreur suivant
# # Warning message:
# #   In nb2listw(nb.object, style = style, glist = lapply(nb.dist, f1,  :
# #                                                          zero sum general weights
# listw.candidates(coord = CoordXY, nb = c("gab"), 
#                  weights = c("binary"))
# listw.candidates(coord = CoordXY, nb = c("gab"), 
#                  weights = c("flin"))
# listw.candidates(coord = CoordXY, nb = c("mst"), 
#                  weights = c("binary"))
# listw.candidates(coord = CoordXY, nb = c("mst"), 
#                  weights = c("flin"))
# 
# # je pense que c'est quand il n'y a pas de voisins dans le thereshold de distance
# #https://stat.ethz.ch/pipermail/r-sig-geo/2010-February/007698.html
# #SWM_candidate$MST_Linear$weights
# #SWM_candidate$MST_Linear$weights[[31]]
# # a voir avec David si on a le cas avec les vai données, mais je ne pense pas
# 
# ggplot(data=CoordXY, aes(x=x, y=y)) + geom_point() + 
#   geom_point(data=CoordXY[31,], aes(x=x, y=y), col="red")
# # c'est bien ça


###############################################################################################
#### Fait une fonction pour l'ensemble du workflow
###############################################################################################


FuncMod <- function(Data, SpClass, Enviro, SWM_candidate) {

## 1-Selectione les donnees
Databund <- Data[, which(colnames(Data) == SpClass)] # vecteur d'abondance de l'espèce et classe considérée
Dataenviro <- as.data.frame(Data[, which(colnames(Data) %in% Enviro)]) # dataframe avec une colonne pour chaque variables enviro
colnames(Dataenviro) <- Enviro
# A FAIRE: vérifier que bien dans l'ordre pour plusieurs variables

## 2-Fit un glm Poisson avec un lien log (par defaut)
M_Pois <-  glm(Databund ~ ., family = poisson, data = Dataenviro)
# summary(M_Pois)
# récupère les résidus
res_Pois <- residuals(M_Pois)


## 3-Voir si autocorrelation spatiale des résidus
# selectionne la meilleure matrice SWM parmis les candidates 
# et selectionne les meilleurs vecteurs de la MEM
Wsel_Pois <- listw.select(res_Pois, SWM_candidate, 
                          MEM.autocor = "positive", 
                          p.adjust = TRUE,
                          method = "MIR")
# str(Wsel_Pois, max.levels=1)
# Wsel_Pois$candidates #pour chaque SWM l'indice I de Moran et la p-value, ainsi que le nombre de vecteurs propres à garder. Quand on a des NA, c'est bien quand il n'y a pas de corrélation spatiale au vue de l'indice I ? 
# Wsel_Pois$best.id # donne le nom de la meilleure SWM (NULL si aucune matrice ne montre de correlation spatiale des résidus)
# Wsel_Pois$best$MEM.select # donne les vecteurs propres sélectionnés pour la meilleure SWM (NULL si aucune matrice ne montre de correlation spatiale des résidus)


## 4- si autocorrelation spatiale, integre les vecteurs propres selectionnés dans le modèle
if (!(is.null(Wsel_Pois$best.id))) { # s'il y a autocorrelation spatiale
  # ajoute les vecteurs propres sélectionnés dans les variables enviro
  Dataenviro_PoisSpa <- cbind(Dataenviro, Wsel_Pois$best$MEM.select)
  M_PoisSpa <-  glm(Databund ~ ., family = poisson, data = Dataenviro_PoisSpa)
  # summary(M_PoisSpa)
  M_PoisBest <- M_PoisSpa
  PoisBest <- "Poisson avec correlation spatiale"
} else { # si pas d'autocorrelation spatiale
  M_PoisSpa <- NULL
  M_PoisBest <- M_Pois
  PoisBest <- "Poisson sans correlation spatiale"
}

## 5 - test si overdispersion (voir livre Zuur p. 224)
Overdisp <- M_PoisBest$deviance/M_PoisBest$df.residual
# si > 1.1 on considère qu'il y a overdispersion


## 6 - si overdispersion, recommence tout avec un negative-binomial GLM (voir livre Zuur p. 233)
if (Overdisp > 1.1) {
  # on fait un glm negative binomial avec un lien log avec le package MASS (etape 2)
  M_NB <-  glm.nb(Databund ~ ., link = "log", data = Dataenviro)
  # summary(M_NB)
  # récupère les résidus
  res_NB <- residuals(M_NB)
  
  # autocorrelation spatiale des résidus (etape 3)
  Wsel_NB <- listw.select(res_NB, SWM_candidate, 
                            MEM.autocor = "positive", 
                            p.adjust = TRUE,
                            method = "MIR")
  
  # si autocorrelation spatiale, integre les vecteurs propres selectionnés dans le modèle (étape 4)
  if (!(is.null(Wsel_NB$best.id))) { # s'il y a autocorrelation spatiale
    # ajoute les vecteurs propres sélectionnés dans les variables enviro
    Dataenviro_NBSpa <- cbind(Dataenviro, Wsel_NB$best$MEM.select)
    M_NBSpa <-  glm.nb(Databund ~ ., link = "log", data = Dataenviro_NBSpa)
    # summary(M_PoisSpa)
    M_NBBest <- M_NBSpa
    NBBest <- "Negative Binomial avec correlation spatiale"
    } else { # si pas d'autocorrelation spatiale
      M_NBSpa <- NULL
      M_NBBest <- M_NB
      NBBest <- "Negative Binomial sans correlation spatiale"
    }
    M_Best <- M_NBBest 
    Best <- NBBest
  } else { # si pas d'overdispersion
    M_NB <- NULL
    Wsel_NB <- NULL
    M_NBSpa <- NULL
    M_Best <- M_PoisBest
    Best <- PoisBest
  }


## 7 - mets tous les résultats d'interêt dans une liste et la renvoie
Result <- list(SpClass, # rappel de l'espèce et classe testées
               Enviro,  # rappel des variables enviro testées
               M_Pois, # Poisson GLM sans prise en compte de la correlation spatiale
               Wsel_Pois, # selection de SWM et MEM vectors pour Poisson
               M_PoisSpa, # Poisson GLM avec prise en compte de la correlation spatiale
               Overdisp, # overdispersion? 
               M_NB, # Negative Binomial GLM sans prise en compte de la correlation spatiale
               Wsel_NB, # selection de SWM et MEM vectors pour Negative Binomial
               M_NBSpa, # Negative Bionomiale GLM avec prise en compte de la correlation spatiale
               M_Best, # Meilleur modèle
               Best) # le nom du meilleur modele

# nomme les élements de la liste
names(Result) <- c("SpClass",
                   "Enviro",
                   "M_Pois",
                   "Wsel_Pois",
                   "M_PoisSpa",
                   "Overdisp",
                   "M_NB",
                   "Wsel_NB",
                   "M_NBSpa",
                   "M_Best", 
                   "Best")

return(Result) # ce qui est renvoyé par la fonction
}



###############################################################################################
#### Utiliser la fonction et examiner les outputs
###############################################################################################

# Crée des data frame pour stocker les résultats de toutes les espèces
# !!!! a ne faire qu'une seule fois pour toutes les espèces
# on va les remplir au fur et à mesure
BestModel <- data.frame(SpClass = character(),
                           Enviro = character(),
                           BestModel = character())
ResBestModel <-  data.frame(SpClass = character(),
                         Enviro = character(),
                         NameCoeff = character(),
                         Coeff = numeric(),
                         Std.Error = numeric(),
                         Pvalue = numeric()) 


# fait tourner la fonction (à faire pour chaque espèce)
########################################################################
ResultModel <- FuncMod(Data =  DataQuadrat, # les données par quadrat
                       SpClass = "Oxa_asb1", # le nom de la colonnes 
                       # contenant les données d'abondance qu'on veut modéliser
                       Enviro = c("light_mean", "twi_extr"), # un vecteur avec les noms 
                       # de colonnes contenant les variables enviro  à inclure dans le modèle
                       SWM_candidate = SWM_candidate)# matrice SWM à tester qu'on a crée plus haut

# regarde ce qu'elle renvoie (à faire pour chaque espèce)
str(ResultModel, max.level = 1)
# une liste avec 11 éléments
  # rappel de l'espèce et classe testées
    ResultModel$SpClass
  # rappel des variables enviro testées
    ResultModel$Enviro
  # Poisson GLM sans prise en compte de la correlation spatiale
    summary(ResultModel$M_Pois)
  # selection de SWM et MEM vectors pour Poisson  
    ResultModel$Wsel_Pois$candidates # les matrices SWM testées avec leur résultat 
      #(p non signif et NA si pas de correlation spatiale)
    ResultModel$Wsel_Pois$best.id # la meilleure matrice SWM (NULL si pas de correlation spatiale)
    ResultModel$Wsel_Pois$best$MEM.select # les vecteurs propres sélectionnés pour la meilleure SWM
      #(NULL si pas de correlation spatiale)
  # Poisson GLM avec prise en compte de la correlation spatiale
    summary(ResultModel$M_PoisSpa) # (NULL si pas de correlation spatiale)
  # overdispersion parameter (si > 1.1 overdispersion)
    ResultModel$Overdisp
  # Negative Binomial GLM sans prise en compte de la correlation spatiale  
    summary(ResultModel$M_NB) # (NULL si pas d'overdispersion)
  # selection de SWM et MEM vectors pour Negative Binomial  
    ResultModel$Wsel_NB$candidates # les matrices SWM testées avec leur résultat 
    #(p non signif et NA si pas de correlation spatiale) (NULL si pas d'overdispersion)
    ResultModel$Wsel_NB$best.id # la meilleure matrice SWM 
      # (NULL si pas d'overdispersion ou pas de correlation spatiale)
    ResultModel$Wsel_NB$best$MEM.select # les vecteurs propres sélectionnés pour la meilleure SWM
      # (NULL si pas d'overdispersion ou pas de correlation spatiale)   
  # Negative Bionomiale GLM avec prise en compte de la correlation spatiale   
    summary(ResultModel$M_NBSpa) # (NULL si pas d'overdispersion ou pas de correlation spatiale)
  # Résultat du meilleur modèle
    summary(ResultModel$M_Best) 
  # Nom du meilleur modèle
    ResultModel$Best

# sauve les résultats (à faire pour chaque espèce)
  # enregistre le fichier avec un nom qui va bien
      # A FAIRE => LISA : mettre le ResultModel dans un RData à sauvegarder
    save(ResultModel, file = "modele/ResultModel_Oxa_asb1_lightTWI.RData")
  # rempli ResBestModel
    bestall <- data.frame(SpClass = ResultModel$SpClass,
                          Enviro = paste(ResultModel$Enviro,collapse=" & "),
                          BestModel = ResultModel$Best)
    BestModel <- rbind(BestModel, bestall)
  # récupère les résultats du meilleur modèle pour remplir BestModel
    resbest <- data.frame(SpClass = ResultModel$SpClass,
                          Enviro = paste(ResultModel$Enviro,collapse=" & "),
                          NameCoeff = names(ResultModel$M_Best$coefficients),
                          Coeff = ResultModel$M_Best$coefficients,
                          Std.Error = summary(ResultModel$M_Best)$coefficients[,2],
                          Pvalue = summary(ResultModel$M_Best)$coefficients[,4])
    rownames(resbest) <- c()
    ResBestModel <- rbind(ResBestModel, resbest)
    
