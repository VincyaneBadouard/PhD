

TopoLevels <- st_read("D:/Mes Donnees/PhD/SIG_data/Topo_P16_4classes/Topo4Levels.shp") %>% 
  filter(Plot==16) %>% mutate(TopoTypeEn = ifelse(idTopo ==38, "Lower slope", TopoTypeEn))

Multistem_plot <- data %>% 
  filter(ScientificName %in% Sp$ScientificName) %>% 
  filter(Stem.nb ==1) %>% 
  filter(!is.na(Xutm)) %>% 
  filter(!is.na(Yutm)) %>% 
  st_as_sf(coords = c("Xutm", "Yutm")) %>% 
  st_set_crs(st_crs(TopoLevels))


plist <- vector('list', length(unique(Multistem_plot$ScientificName)))

for(p in 1:length(unique(Multistem_plot$ScientificName))){
  plist[[p]] <- local({
    dataS <- Multistem_plot %>% 
      filter(ScientificName==unique(Multistem_plot$ScientificName)[p]) 
    
    ggplot() +
      geom_sf(data = TopoLevels, aes(fill = TopoTypeEn), alpha = 0.3) +
      geom_sf(data = dataS, size =2, aes(col= Multistem)) + # , aes(col= Genus)
      scale_fill_manual(values = c("Plateau" = "#009E73",
                                   "Slope"= "#E7B800",
                                   "Lower slope"= "#bfe6eb",
                                   "Bottomland" = "#1f78b4")) +
      theme_void() +
      guides(fill = FALSE) +
      labs(title = unique(dataS$ScientificName))
           
  })
}

plots <- gridExtra::marrangeGrob(plist, nrow = 2, ncol = 3)

ggsave("Multistems_interest_species.pdf",
       path = "D:/Mes Donnees/PhD/R_codes/PhD/Inventory_scripts/Exploration/pdf",
       plots, width = 15, height = 10)
