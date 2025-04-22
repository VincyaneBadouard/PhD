#' ResidualExplo
#' 
#' @description Compute models residuals (raw, Person, deviance)
#'
#' @param sp (character) Species vector of interest
#' @param datalist (list) list of models observations data (data.frames), one per species
#' @param fitspath (character) Path for the models fits (end by "/")
#' @return
#' @import tidyverse cmdstanr
#' @export
#'
#' @examples
#' sp <- c("Anaxagorea_dolichocarpa", "Tabernaemontana_macrocalyx",
#'         "Eperua_falcata", "Dicorynia_guianensis", "Paypayrola_hulkiana")
#'         
#' path <- "D:/Mes Donnees/PhD/R_codes/PhD/Modelisation/"
#' load(paste(path, "Realdata/Realsp_25ha.Rdata", sep=''))
#' # View(datalist[[1]])
#'         
#' ResidualExplo(sp, datalist, "../Chains/Hybrid_allpred/")
#' 
ResidualExplo <- function(sp, datalist, fitspath){
  
  # Filter data for for interest species ---------------------------------------
  datalist <- datalist[names(datalist) %in% sp] # only species in sp
  # View(datalist[["Iryanthera_hostmannii"]])
  
  # Load models fits for interest species --------------------------------------
  fits <- list()
  for(S in sp){
    tryCatch({
      chain_path <- paste(fitspath, S) # ".." for the he directory above the current one
      fits[[S]] <- as_cmdstan_fit(list.files(,
                                             full.names = TRUE))},
      error=function(e){cat("ERROR :",S, conditionMessage(e), "\n")}
    )
    
    # Compute raw residuals ----------------------------------------------------
    # get the posterior residuals for each observations, and I take the median across iterations.
    DATA <- list()
    DATA[[S]] <- fits[[S]]$summary("p") # p posterior
    
    # rm(fits)
    
    Residuals <- list()
    Residuals[[S]] <- data.frame(y = datalist[[S]]$Presence, # Observed values (y = 0 or 1)
                                 p_hat = DATA[[S]]$mean) %>% # predicted probability from model
      mutate(raw_e = y - p_hat, # raw residuals,
             Pearson_e = raw_e / sqrt(p_hat * (1 - p_hat)),
             Deviance_e = sign(raw_e)*sqrt(-2*(y*log(p_hat) + (1 - y)*log(1 - p_hat)))
      ) %>% 
      bind_cols(datalist[[S]] %>% select(Xutm,Yutm, DBHcor))
    
    # rm(DATA)
    
    # Moran's I ----------------------------------------------------------------
    n <- 10^3 # ICI prendre plutot ttes les présences et autant d'absences
    samp <- sample_n(Residuals[[S]], n)
    
    # Computes Moran's coefficients on distance classes
    Moran <- list()
    Moran[[S]] <- pgirmess::correlog(coords = data.frame(Residuals[[S]]$Xutm, Residuals[[S]]$Yutm), # long
                              Residuals[[S]]$raw_e,
                              method = "Moran", nbclass = 30) %>% 
      as.data.frame()
    
  }
  list(Residuals, Moran)
  return()
  
}

