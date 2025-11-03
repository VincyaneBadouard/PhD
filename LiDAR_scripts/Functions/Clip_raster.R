#' Clip_raster
#'
#' @param file_to_clip (character) file to clip path
#' @param mask (sf) mask
#' @param outpout (character) path to save the result
#'
#' @return The raster clipped and save the raster clipped in the *oupout* path
#' @export
#' @import sf
#' @import terra, raster
#'
#' @examples
#' Rslt <- Clip_raster(file_to_clip = "test/Paracou_MNT_cor.tif",
#'                     mask = "D:/Mes Donnees/PhD/R_codes/PhD/test/Plot16.shp", 
#'                     outpout = "test/Paracou_P16_MNT.tif")
#'   
#'   
# Rslt <- Clip_raster(file_to_clip = "//amap-data.cirad.fr/safe/lidar/ALS/Paracou/2019/rasters/CHM_Par2019_1m_crs2972.tif",
#                     mask = "D:/Mes Donnees/PhD/R_codes/PhD/test/ALT.shp",
#                     outpout = "test/Paracou_P16_ALT_CHM_2019.tif")

Clip_raster <- function(
    file_to_clip,
    mask,
    outpout
){
  
  raster <- terra::rast(file_to_clip)
  mask <- vect(mask)
  
  raster::crs(raster) <- terra::crs("EPSG:2972") # attribuer le crs
  mask <- st_set_crs(sf::st_as_sf(mask), st_crs(raster)) # attribuer le crs
 
  
  cro <- terra::crop(raster, mask)
  Rslt <- terra::mask(cro, mask)
  
  terra::writeRaster(Rslt, outpout, overwrite=TRUE)
  
  return(Rslt)
  
}