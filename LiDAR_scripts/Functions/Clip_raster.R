#' Clip_raster
#'
#' @param file_to_clip (character) file to clip path
#' @param mask (sf) mask
#' @param outpout (character) path to save the result
#'
#' @return The raster clipped and save the raster clipped in the *oupout* path
#' @export
#' @import sf
#' @import terra
#'
#' @examples
#' Rslt <- Clip_raster(file_to_clip = "test/Paracou_MNT_cor.tif",
#'                     mask = Plot16, 
#'                     outpout = "test/Paracou_P16_MNT.tif")
#'   
Clip_raster <- function(
    file_to_clip,
    mask,
    outpout
){
  
  raster <- terra::rast(file_to_clip)
  
  mask <- st_set_crs(mask, 2972) # attribuer le crs
  raster::crs(raster) <- st_set_crs(mask) # attribuer le crs
  
  cro <- terra::crop(raster, mask)
  Rslt <- terra::mask(cro, mask)
  
  terra::writeRaster(Rslt, outpout, overwrite=TRUE)
  
  return(Rslt)
  
}