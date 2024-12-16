#' ReadLightFile
#'
#' @param path (character) File path
#' @param skiplines (numeric) number of rows to skip
#'
#' @return Light data.table
#' @import data.table
#' @importFrom readr read_delim
#' @export
#'
#' @examples
#' path_tree <- "//amap-data.cirad.fr/work/users/VincyaneBadouard/Lidar/ALS2023/HighAltitudeFlight/Intensity_1m/ByTree/Tree_Light/P16_2023_25ha_HighAlt_Light_intensity1m_template.txt"
#' Light_Tree <- ReadLightFile(path_tree, skiplines=9)
#' 
ReadLightFile <- function(path, skiplines=10){
  # Light <- fread(path, skip=skiplines) # skip the 10 1st lines of header
  
  Light <- setDT(read_delim(path, 
                            delim = "\t", escape_double = FALSE, 
                            trim_ws = TRUE, skip = skiplines))
  
  Light <- Light[Y != "MEAN"] # enlever les 2 lignes mean
  Light <- janitor::remove_empty(Light,"cols")
  
  Light[, `:=`(X = as.numeric(X) , Y = as.numeric(Y))] # all as numeric
  Light <- Light[!is.na(X)] 
  
  return(Light)
  
}