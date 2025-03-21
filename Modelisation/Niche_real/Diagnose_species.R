sp <- c("Licania_membranacea" , "Lecythis_persistens", "Piparea_multiflora",
        "Iryanthera_hostmannii", "Tabernaemontana_macrocalyx") # 4
for(s in sp){
  print(s)
  file_name <- paste0("Diagnose_", s, ".html")
  if(!file.exists(file.path("Diagnose_species/", file_name))){
    quarto::quarto_render(
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
