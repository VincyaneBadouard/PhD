
sp <- (read.csv("D:/Mes Donnees/PhD/Inventories/Data/Understory/Paracou/InterestSpecies.csv")[,3]) # 75

# sp <- c("Anaxagorea_dolichocarpa", "Tabernaemontana_macrocalyx",
#         "Eperua_falcata", "Dicorynia_guianensis", "Paypayrola_hulkiana") # 5 agreg sp

# sp <- "Anaxagorea_dolichocarpa"

setwd("D:/Mes Donnees/PhD/R_codes/PhD/Modelisation/Light_Topo_ontogeny/Residuals_explo/") 

qmd <- "DHARMA_test" # "Residuals_explo_plots"

if(!file.exists("Diagnose_species"))
  dir.create("Diagnose_species")

for(s in sp){
  print(s)
  file_name <- paste0(qmd, "_", s, ".html")
  if(!file.exists(file.path("Diagnose_species/", file_name))){
    quarto::quarto_render( # create a html per species
      input = paste(qmd, ".qmd", sep=""),
      output_file = file_name,
      execute_params = list(species = s)
    )
    file.rename(
      from = file_name,
      to = file.path("Diagnose_species/", file_name)
    )
  }
}

# library(foreach) # parallisation
# library(parallel)
# gc()
# Sys.time() # h
# j <- length(sp)
# cores = min(j, 5) # nbr of cores to use
# s <- NULL
# 
# # for(s in sp) {
# # S=sp[1]
# 
# # L'enregistrement des clusters
# cl <- parallel::makeCluster(cores, outfile = "")
# doSNOW::registerDoSNOW(cl)
# 
# # Progress bar:
# pb <- txtProgressBar(max=j)
# progress <- function(n) cat(sprintf("Run %d is complete\n", n))
# opts <- list(progress = progress)
# 
# foreach::foreach(
#   s=1:j, # j
#   .options.snow = opts # ProgressBar
# ) %dopar% {
#   
#   # the function to parallelise:
#   # print(s)
#   file_name <- paste0("Diagnose_", sp[s], ".html") 
#   if(!file.exists(file.path("Diagnose_species/", file_name))){
#     quarto::quarto_render( # create a html per species
#       input = "Diagnose_species.qmd", 
#       output_file = file_name,
#       execute_params = list(species = sp[s])
#     )
#     file.rename(
#       from = file_name,
#       to = file.path("Diagnose_species/", file_name)
#     ) 
#   }
# }
# # close progressbar and cluster
# close(pb)
# stopCluster(cl)
# # }
# 
# Sys.time() # h
# 
# 
