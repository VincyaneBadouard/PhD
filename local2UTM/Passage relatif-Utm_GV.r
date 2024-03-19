

tab = read.csv(
  "C:/Users/Greg/Documents/GVincent/Rapports et notes/Articles/DendroLidar/codeR/CoordCoinsP1_15.csv",
  sep=";")

coordUTM <- function (x,y,P)
  {
  limx = 250 # à changer pour la P16
  limy = 250 # à changer pour la P16
  #nox... soy = coordonnées UTM des coins de la parcelle  
  XUTM = (1-x/limx)*(1-y/limy)*tab[P,"so.x"]+(x/limx)*(1-y/limy)*tab[P,"se.x"]+(y/limy)*(1-x/limx)*tab[P,"no.x"]+((x*y)/(limx*limy))*tab[P,"ne.x"] # formule tirée du fichier Excel sur SigParacou
  YUTM = (1-x/limx)*(1-y/limy)*tab[P,"so.y"]+(x/limx)*(1-y/limy)*tab[P,"se.y"]+(y/limy)*(1-x/limx)*tab[P,"no.y"]+((x*y)/(limx*limy))*tab[P,"ne.y"]

  return (c(XUTM,YUTM))
  }

