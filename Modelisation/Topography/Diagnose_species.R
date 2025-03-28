
# sp <- (read.csv("D:/Mes Donnees/PhD/Inventories/Data/Understory/Paracou/InterestSpecies.csv")[,2]) # 75


# sp <- c("Licania_membranacea" , "Lecythis_persistens", "Piparea_multiflora",
#         "Iryanthera_hostmannii", "Tabernaemontana_macrocalyx") # 4 forms
sp <- "Symphonia_sp.1"


for(s in sp){
  print(s)
  file_name <- paste0("Diagnose_", s, ".html") 
  if(!file.exists(file.path("Diagnose_species/", file_name))){
    quarto::quarto_render( # create a html per species
      input = "Diagnose_species.qmd", 
      output_file = file_name,
      execute_params = list(species = s)
    )
    file.rename(
      from = file_name,
      to = file.path("Diagnose_species/", file_name)
    ) 
  }
}
