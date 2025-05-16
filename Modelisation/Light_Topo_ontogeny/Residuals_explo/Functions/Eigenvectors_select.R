#' Select eigenvectors
#'
#' @param sp (character) species name
#'
#' @return Write a csv with the eigenvectors for the species
#' @import adespatial, tidyverse
#' @export
#'
#' @examples
#' Eigenvectors_select(sp = "Dicorynia_guianensis")
#' 
Eigenvectors_select <- function(sp){
  
  library(adespatial); library(tidyverse)
  
  DATA <- read_csv(
    paste0("D:/Mes Donnees/PhD/R_codes/PhD/Modelisation/Light_Topo_ontogeny/Residuals_explo/Spatial_bin_res/", sp, ".csv")) 
  
  # Coords ---------------------------------------------------------------------
  CoordXY <- DATA %>% select(x_m, y_m) %>% unique() # dataframe with x and y coordinates in columns

  # Residuals ------------------------------------------------------------------
  Residuals <- DATA %>% select(x_m, y_m, e) %>% unique() %>% select(e)
  
  # List SWMs ------------------------------------------------------------------
  SWM_candidate <- listw.candidates(coord = CoordXY, # point coordinates
                                    nb = c("mst", "gab", "rel"), # c("gab", "mst") 
                                    weights = c("binary", "flin", "fup", "fdown") ) # c("binary", "flin")
  
  # Select SWM and eigenvectors ------------------------------------------------
  SWM_selec <- listw.select(Residuals$e, # model residuals
                            SWM_candidate, 
                            MEM.autocor = "positive", # Sign of the spatial eigenvectors
                            p.adjust = TRUE, # p-value corrected for multiple tests 
                            method = "MIR") # select eigenvectors to minimize the spatial autocorrelation in the residuals
  
  SWM <- names(SWM_selec$best.id) # the best SWM
  print(paste(sp,"best SWM:",SWM))
  
  # if (!(is.null(SWM_selec$best.id))) {
  # The selected eignevalues for the best SWM (NULL if no autocor)
  eigenval <- cbind(CoordXY, SWM, SWM_selec$best$MEM.select) # n rows = n residuals, n col = n eigenvectors
  
  eigenval <- DATA %>%
    select(Xutm, Yutm, x_m, y_m) %>% 
    left_join(eigenval, by= c("x_m", "y_m"))
  
  write_csv(eigenval, paste0("D:/Mes Donnees/PhD/Inventories/Data/Agregation/Eigenvectors/", sp, ".csv"))
  # }
  
}