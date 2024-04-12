# zone <- st_as_sf(vect("//amap-data.cirad.fr/work/users/VincyaneBadouard/Lidar/ALS2022/Parcelles_Understory.shp")) # 4 carrés

Crop <- function(
    data,
    zone
){
  
  datasf <- st_as_sf(data, coords = c('Xutm','Yutm'))
  datasf <- st_set_crs(datasf, crs(zone))
  
  datacrop <- st_intersection(datasf, zone)
  
  XY <- st_coordinates(datacrop) # stock coordinates
  colnames(XY) <- c("Xutm", "Yutm") 
  st_geometry(datacrop) <- NULL # no more sf object
  datacrop <- cbind(datacrop, XY) # bind XY coord
  
  return(datacrop)
  
}