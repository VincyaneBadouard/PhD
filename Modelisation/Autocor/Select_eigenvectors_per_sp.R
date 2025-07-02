# Select_eigenvectors_per_sp

# Glmnet fits generalized linear and similar models via **penalized maximum likelihood**.
# It fits linear, **logistic** and multinomial, poisson, and Cox regression models.  
# 
# The function glmnet returns a sequence of models for the users to choose from. Cross-validation is perhaps the simplest and most widely used method for that task. **cv.glmnet** is the main function to do cross-validation here.  
# 
# lambda.min = λ that gives minimum mean cross-validated error.  


library(readr)
library(tidyverse)
library(glmnet)
library(foreach)
library(parallel)

species <- (read.csv("D:/Mes Donnees/PhD/Inventories/Data/Understory/Paracou/InterestSpecies.csv"))[,3]

# Load eigenvectors
eigenval <- read_csv("D:/Mes Donnees/PhD/Inventories/Data/Agregation/Eigenvectors/All_obs_Eigenvectors.csv")

# Load species data
path <- "D:/Mes Donnees/PhD/R_codes/PhD/Modelisation/"
load(paste(path, "Realdata/Realsp_25ha.Rdata", sep=''))

# -----------------------------------------------------------------------------
cores = 5 # nbr of cores to use
# parallel::detectCores() # 8
i <- NULL
j = length(species)

# L'enregistrement des clusters
cl <- parallel::makeCluster(cores)
doSNOW::registerDoSNOW(cl)

# Progress bar:
pb <- txtProgressBar(min = 0, max = j, style = 3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)


# ------------------------------------------------------------------------------
foreach::foreach(
  i=1:j,
  .packages = c("tidyverse", "glmnet"), # necessary packages
  .options.snow = opts # ProgressBar
) %dopar% {
  print(i) # to check
  # the function to parallelise:
  Select_eigenvectors_per_sp(sp = species[i], datalist = datalist, eigenval = eigenval)
}

# close progressbar and cluster
close(pb)
stopCluster(cl)

# Function
Select_eigenvectors_per_sp <- function(sp, datalist = datalist, eigenval = eigenval){
  print(sp)
  
  DATA <- datalist[[sp]] %>% # only species in sp
    select(Xutm, Yutm, Presence, logTransmittance,logTWI,logDBH)
  
  y <- DATA$Presence # response vector
  
  
  # Bind
  dataall <- DATA %>% 
    bind_cols(eigenval)
  
  # Model with eigenvectors
  # Formula
  formula_full <- as.formula(paste("~ logTransmittance + I(logTransmittance^2) + logTWI",
                                   paste(paste0("V",1:200), collapse="+"), sep="+"))
  
  # Matrix of dimension nobs x nvars
  matrix_full <- model.matrix(formula_full, data=dataall)[,-1] 
  
  # Cross-validation
  Sys.time() 
  cv_fit <- cv.glmnet(matrix_full, y, family = "binomial", type.measure = "auc") # according to ROC curve
  Sys.time() # 6 min for 25ha
  
  coef_glmnet <- coef(cv_fit, s = "lambda.min") #  model coefficients at that value of λ
  coef_glmnet 
  
  
  # Selection of max 5 eigenvectors
  vars <- data.frame(Vars = c("Intercept","logTransmittance","I(logTransmittance^2)","logTWI", paste0("V",1:200))) %>%
    tibble::rownames_to_column("i") %>% 
    mutate(i=as.numeric(i))
  
  selection <- as.data.frame(summary(coef_glmnet)) %>% 
    select(-j) %>% 
    left_join(vars, by="i") %>% 
    arrange(desc(abs(x))) %>% 
    filter(!Vars %in% c("Intercept","logTransmittance","I(logTransmittance^2)","logTWI")) %>% 
    select(-i) %>% 
    slice(1:5)
  
  eigenval_select <- eigenval %>% 
    select(selection$Vars)
  
  write_csv(eigenval_select,
            paste0("D:/Mes Donnees/PhD/Inventories/Data/Agregation/Eigenvectors/Per_species/", sp, ".csv"))
}
