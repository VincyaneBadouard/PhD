# ResidualExplo
# 
# description Compute models residuals (raw, Person, deviance) and Moran's I.
library(tidyverse)
library(cmdstanr)

# Args
arg <- commandArgs(trailingOnly = TRUE)
ID <- as.integer(arg[1])

# Species of interest
# sp <- (read.csv("../../Data/InterestSpecies.csv")[,2]) # 75
sp <- c("Anaxagorea_dolichocarpa", "Tabernaemontana_macrocalyx", # 5 agreg sp
        "Eperua_falcata", "Dicorynia_guianensis", "Paypayrola_hulkiana")

print(paste("ID:", ID))
s <- sp[ID]
# s <- "Paypayrola_hulkiana" 

print(paste("run for sp", s))


# Model data
load("/lustre/badouardv/PhD_cluster/Data/Realsp_25ha.Rdata")
# View(datalist[[1]])

# Filter data for for interest species ---------------------------------------
datalist <- datalist[names(datalist) %in% s] # only species in sp
# View(datalist[["Iryanthera_hostmannii"]])

# Load models fits for interest species --------------------------------------
tryCatch({
  chain_path <- paste("/lustre/badouardv/PhD_cluster/Topo_Light_Onto/Autocor_explo/Chains/Hybrid_allpred/", s, sep="") # ".." for the he directory above the current one
  print(chain_path)
  fits <- as_cmdstan_fit(list.files(chain_path,
                                    full.names = TRUE))},
  error=function(e){cat("ERROR :",s, conditionMessage(e), "\n")}
)

# Compute raw residuals ----------------------------------------------------
# get the posterior residuals for each observations, and I take the median across iterations.
DATA <- list()
DATA <- fits$summary("p") # p posterior

rm(fits)

Residuals <- data.frame(y = datalist[[1]]$Presence, # Observed values (y = 0 or 1)
                        p_hat = DATA$mean) %>% # predicted probability from model
  mutate(raw_e = y - p_hat, # raw residuals,
         Pearson_e = raw_e / sqrt(p_hat * (1 - p_hat)),
         Deviance_e = sign(raw_e)*sqrt(-2*(y*log(p_hat) + (1 - y)*log(1 - p_hat)))
  ) %>% 
  bind_cols(datalist[[1]] %>% select(Xutm,Yutm, DBHcor))

rm(DATA)

print("Residuals computed")

# Moran's I ----------------------------------------------------------------
# n <- 10^3
# prendre ttes les présences et autant d'absences
pres <- Residuals %>% filter(y==1)
abs <- Residuals %>% 
  filter(y==0) %>% 
  sample_n(nrow(pres))
samp <- bind_rows(pres, abs)


# Computes Moran's coefficients on distance classes
Moran_raw <- pgirmess::correlog(coords = data.frame(samp$Xutm, samp$Yutm),
                            samp$raw_e,
                            method = "Moran", nbclass = 30) %>% 
  as.data.frame()
Moran_Pearson <- pgirmess::correlog(coords = data.frame(samp$Xutm, samp$Yutm),
                                samp$Pearson_e,
                                method = "Moran", nbclass = 30) %>% 
  as.data.frame()

Moran_Deviance <- pgirmess::correlog(coords = data.frame(samp$Xutm, samp$Yutm),
                                    samp$Pearson_e,
                                    method = "Moran", nbclass = 30) %>% 
  as.data.frame()

print("Moran's I computed")

Results <- list(Residuals = Residuals,
                Moran_raw= Moran_raw,
                Moran_Pearson = Moran_Pearson,
                Moran_Deviance = Moran_Deviance)

# Savec the outpout
getwd()
setwd("D:/Mes Donnees/PhD/R_codes/PhD/Modelisation/Light_Topo_ontogeny/Residuals_explo/")
if(!file.exists("Residuals_Moran"))
  dir.create("Residuals_Moran")

saveRDS(Results,
     file=paste("D:/Mes Donnees/PhD/R_codes/PhD/Modelisation/Light_Topo_ontogeny/Residuals_explo/Residuals_Moran/Residuals_Moran_", sp,".rds", sep=""))

print(paste(s, "DONE"))
