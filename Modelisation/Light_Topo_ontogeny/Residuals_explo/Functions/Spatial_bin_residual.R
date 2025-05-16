#' Compute spatial binned residuals
#'
#' @param sp (character) Species name
#'
#' @return Write a csv with the spatial binned residuals for the species
#' @importFrom cmdstanr as_cmdstan_fit
#' @import tidyverse
#' @export
#'
#' @examples
#' Spatial_bin_residual(sp = "Dicorynia_guianensis")
Spatial_bin_residual <- function(sp){
  
  library(cmdstanr)
  library(tidyverse)
  
  path <- "D:/Mes Donnees/PhD/R_codes/PhD/Modelisation/"
  load(paste(path, "Realdata/Realsp_25ha.Rdata", sep=''))
  
  datalist <- datalist[[sp]] # only species in sp
  
  chain_path <- paste("D:/Mes Donnees/PhD/R_codes/PhD/Modelisation/Light_Topo_ontogeny/Residuals_explo/Chains/Hybrid_allpred/", sp, sep="") # ".." for the he directory above the current one
  fit <- as_cmdstan_fit(list.files(chain_path, full.names = TRUE)[1]) # only 1 chain
  
  DATA <- fit$summary("p", "median") # p posterior
  
  # e_complete <- list()
  # for(bin in c("2", "5", "10", "15", "20", "50")){
  # e_complete[[bin]]
  bin <- 20
  e_complete <- data.frame(y = datalist$Presence, # Observed values (y = 0 or 1)
                           p_hat = DATA$median) %>% # predicted probability from model
    bind_cols(datalist %>% select(Xutm,Yutm, logDBH, logTransmittance, logTWI)) %>% 
    
    mutate(x_bin = cut(Xutm, seq(min(Xutm), max(Xutm), as.numeric(bin))), # to categories
           y_bin = cut(Yutm, seq(min(Yutm), max(Yutm), as.numeric(bin)))) %>% 
    group_by(x_bin, y_bin) %>% 
    mutate(obs_m = mean(y), p_m = mean(p_hat), x_m = mean(Xutm), y_m = mean(Yutm), N = n()) %>% 
    ungroup() %>% 
    mutate(e = obs_m - p_m) %>% 
    select(x_bin, y_bin, Xutm, Yutm, obs_m, p_m, x_m, y_m, N, e)
  # }
  
  write_csv(e_complete,
            paste0("D:/Mes Donnees/PhD/R_codes/PhD/Modelisation/Light_Topo_ontogeny/Residuals_explo/Spatial_bin_res/", sp, ".csv")) # e_complete[["20"]]
  
}
