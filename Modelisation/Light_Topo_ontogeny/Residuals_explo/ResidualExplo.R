#' ResidualExplo
#' 
#' @description Compute models residuals (raw, Person, deviance)
#'
#' @param sp (character) Species vector of interest
#' @param datalist (list) list of models observations data (data.frames), one per species
#' @param fitspath (character) Path for the models fits (end by "/")
#' @return
#' @import tidyverse cmdstanr
#' @importFrom pgirmess correlog
#' @export
#'
#' @examples
#' s <- "Eperua_falcata"
#'         
#' path <- "D:/Mes Donnees/PhD/R_codes/PhD/Modelisation/"
#' load(paste(path, "Realdata/Realsp_25ha.Rdata", sep=''))
#' # View(datalist[[1]])
#' setwd("D:/Mes Donnees/PhD/R_codes/PhD/Modelisation/Light_Topo_ontogeny/Residuals_explo/")        
#' ResidualExplo(s, datalist, fitspath = "./Chains/Hybrid_allpred/")
#' 
ResidualExplo <- function(s, datalist, fitspath ){
  
  print(paste("run for sp", s))
  
    # Filter data for for interest species ---------------------------------------
  datalist <- datalist[names(datalist) %in% s] # only species in sp
  # View(datalist[["Iryanthera_hostmannii"]])
  
  # Load models fits for interest species --------------------------------------
  tryCatch({
    chain_path <- paste(fitspath, s, sep="") # ".." for the he directory above the current one
    print(chain_path)
    fits <- as_cmdstan_fit(list.files(chain_path,
                                      full.names = TRUE))},
    error=function(e){cat("ERROR :",s, conditionMessage(e), "\n")}
  )
  
  # Compute raw residuals ----------------------------------------------------
  # get the posterior residuals for each observations, and I take the median across iterations.
  DATA <- fits$summary("p") # p posterior
  
  rm(fits)
  
  Residuals <- data.frame(y = datalist[[s]]$Presence, # Observed values (y = 0 or 1)
                          p_hat = DATA$mean) %>% # predicted probability from model
    mutate(raw_e = y - p_hat, # raw residuals,
           Pearson_e = raw_e / sqrt(p_hat * (1 - p_hat)),
           Deviance_e = sign(raw_e)*sqrt(-2*(y*log(p_hat) + (1 - y)*log(1 - p_hat)))
    ) %>% 
    bind_cols(datalist[[s]] %>% select(Xutm,Yutm, DBHcor))
  
  rm(DATA)
  
  print("Residuals computed")
  
  # Moran's I ----------------------------------------------------------------
  # n <- 10^3
  # Take all the presences and the same nbr of absences
  pres <- Residuals %>% filter(y==1)
  abs <- Residuals %>% 
    filter(y==0) %>% 
    sample_n(nrow(pres))
  samp <- bind_rows(pres, abs)
  
  
  # Computes Moran's coefficients on distance classes
  Moran_raw <- pgirmess::correlog(coords = data.frame(samp$Xutm, samp$Yutm),
                                  samp$raw_e,
                                  method = "Moran", nbclass = NULL) %>% 
    as.data.frame()
  Moran_Pearson <- pgirmess::correlog(coords = data.frame(samp$Xutm, samp$Yutm),
                                      samp$Pearson_e,
                                      method = "Moran", nbclass = NULL) %>% 
    as.data.frame()
  
  Moran_Deviance <- pgirmess::correlog(coords = data.frame(samp$Xutm, samp$Yutm),
                                       samp$Deviance_e,
                                       method = "Moran", nbclass = NULL) %>% 
    as.data.frame()
  
  print("Moran's I computed")
  
  Results <- list(Residuals = Residuals,
                  Moran_raw = Moran_raw,
                  Moran_Pearson = Moran_Pearson,
                  Moran_Deviance = Moran_Deviance)
  
  # Savec the outpout
  if(!file.exists("Residuals_Moran"))
    dir.create("Residuals_Moran")
  saveRDS(Results, paste("./Residuals_Moran/Residuals_Moran_", s,".rds", sep=""))
  
  print(paste(s, "DONE"))
  
}

