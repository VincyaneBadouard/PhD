
#' CreateTreePolygon
#'
#' @param data data.frame with:
#'  - Xutm, Yutm,
#'  - DBH,
#'  - TrunkHeight,
#'  - CrownHeight, CrownDiameter
#'
#' @return
#' @export
#'
#' @examples
#' data <-
#' rslt <- CreateTreePolygon(data)
#' 
#' library(ggplot2)
#' ggplot() +
#'   geom_sf(data = sf::st_set_crs(sf::st_sfc(rslt$Foot),
#'                                 sf::st_crs(SecondaryTrails$MainTrailsAccess))) +
#'   geom_sf(data = sf::st_set_crs(sf::st_as_sf(rslt$Trail),
#'                                 sf::st_crs(SecondaryTrails$MainTrailsAccess))) +
#'   geom_sf(data = rslt$NearestPoints) +
#'   geom_sf(data = sf::st_set_crs(sf::st_sfc(rslt$TrailPt),
#'                                 sf::st_crs(SecondaryTrails$MainTrailsAccess))) +
#'   geom_sf(data = sf::st_set_crs(rslt$FallenTree,
#'                                 sf::st_crs(SecondaryTrails$MainTrailsAccess))) +
#'   geom_sf(data = sf::st_set_crs(FutureReserveCrowns,
#'                                 sf::st_crs(SecondaryTrails$MainTrailsAccess))) # set a crs
#'                                 

CreateTreePolygon <- function(data){ 
  
  #### Individual geometry ####
  # The foot
  Foot <- st_point(c(data$Xutm,data$Yutm))
  Foot_crs <- st_sfc(Foot) # as sfc
  Foot_crs <- st_set_crs(Foot_crs, st_crs(P16)) # set a crs
  
  # The crown
  Crown <- data %>%
    mutate(xCrown = Xutm,
           yCrown = Yutm + TrunkHeight + CrownHeight/2,
           exCrown = CrownDiameter/2,
           eyCrown = CrownHeight/2) %>%
    st_as_sf(coords = c("xCrown", "yCrown")) # ellipse centroid coordinates
  Crown <- st_ellipse(Crown, Crown$exCrown, Crown$eyCrown) # create the ellipse
  
  # The trunk
  Trunk <- with(data,
                st_polygon(list(matrix(c(Xutm-(DBH/100)/2, Yutm,
                                         Xutm-(DBH/100)/2, Yutm + TrunkHeight,
                                         Xutm+(DBH/100)/2, Yutm + TrunkHeight,
                                         Xutm+(DBH/100)/2, Yutm,
                                         Xutm-(DBH/100)/2, Yutm) # the return
                                       ,ncol=2, byrow=TRUE))))
  
  Trunksf <- st_as_sf(Trunk)
  Crownsf <- st_as_sf(Crown)
  
  Trunk_Crownsf <- Trunksf %>% rbind(Crownsf)
  
  # ou Trunk_Crownsf <- st_difference(st_union(Trunk, Crown)
  
  return(Trunk_Crownsf)
  
}