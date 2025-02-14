

# Check simulate data for pseudo-absences tests

library(tidyverse)

load("D:/Mes Donnees/PhD/R_codes/PhD/Modelisation/Simdata/simulatedata_PA.rds")

# plot initial total and subsampled probabilites, and presences as density plots

data <- bind_rows(plist, .id = "Simulation")#  %>% 
  # rename(TotProbability= Probability) %>% 
  # mutate(Probability = ifelse(!is.na(Presence), TotProbability, NA))

gc()
# Plot initial total and subsampled probabilites  ------------------------------
plist <- vector('list', length(unique(data$Simulation)))

for(p in 1:length(unique(data$Simulation))){
  # message(s)
  plist[[p]] <- local({
    
    dataS <- data %>% 
      filter(Simulation==unique(data$Simulation)[p])
    
    ggplot(dataS, aes(x= logTransmittance, y= TotPresence)) +
      # Total
      geom_point(col = "grey") +
      geom_point(aes(y = TotProbability), col = "grey", size =1) + 
      
      ggridges::geom_density_ridges(data = (dataS %>% filter(TotPresence==1)), # Presence density
                                    scale = 0.6,
                                    jittered_points = T,
                                    col = "grey",
                                    alpha=0.05, point_alpha  = 0.7) +
      
      
      # Subsampled
      geom_point(aes(y= Presence), col = "red") +
      geom_point(aes(y = Probability), col = "red", size =1) + 
      
      ggridges::geom_density_ridges(data = (dataS %>% filter(!is.na(Presence) & Presence==1)), # Presence density
                                    scale = 0.6,
                                    jittered_points = T,
                                    col = "red",
                                    alpha=0.05, point_alpha  = 0.7) +
      
      
      labs(title = unique(dataS$Simulation), y="Presence") +
      
      theme_minimal()
    
  })
}

plots <- gridExtra::marrangeGrob(plist, nrow = 3, ncol = 3)
# plots

ggsave("SimulatedData_PA.pdf",
       path = "D:/Mes Donnees/PhD/R_codes/PhD/Modelisation/Plots",
       plots, width = 15, height = 10)

gc()
# Plot presence as density + points --------------------------------------------
plist <- vector('list', length(unique(data$Simulation)))

for(p in 1:length(unique(data$Simulation))){
  # message(s)
  plist[[p]] <- local({
    
    dataS <- data %>%
      select(Simulation, Presence, logTransmittance) %>% 
      filter(Simulation==unique(data$Simulation)[p]) %>% 
      filter(Presence==1)
    
    
    ggplot(dataS, aes(x= logTransmittance, y= Simulation)) +
      ggridges::geom_density_ridges(scale = 0.6,
                                    jittered_points = T,
                                    alpha=0, point_alpha  = 0.7) +
      
      theme_classic() +
      theme(axis.text.y = element_blank()) +
      guides(fill = FALSE) +
      labs(title = unique(dataS$Simulation),
           x= "logTransmittance", y="Density") 
    
  })
}

plots <- gridExtra::marrangeGrob(plist, nrow = 2, ncol = 3)
# plots

ggsave("SimulatedData_PA_density_points.pdf",
       path = "D:/Mes Donnees/PhD/R_codes/PhD/Modelisation/Plots",
       plots, width = 15, height = 10)

