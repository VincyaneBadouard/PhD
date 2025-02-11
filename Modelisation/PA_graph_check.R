

# Check pseudo-absences tests graphicaly
# Check presence: Presence density plot - obs vs est
# Check niche form: Proba facet - obs vs est
# Check extremum : Extremum plot

library(tidyverse)

# plot initial total and subsampled probabilites, and presences as density plots

# Simulate data ----------------------------------------------------------------
plist <- readRDS("~/PhD/R_codes/PhD/Modelisation/Simdata/simulatedata_PA.rds")[1:18] # only concave

# dataS <- bind_rows(plist, .id = "Scenario")

# Estimations ------------------------------------------------------------------
fits <- readRDS("D:/Mes Donnees/PhD/R_codes/PhD/Modelisation/Fits/Pseudo_abs_tests_concaves.rds")
gc()

Sys.time() #  1 min

data <- lapply(as.list(names(plist)), function(scenario)
  cbind(Scenario = scenario, plist[[scenario]],
        mu = apply(as.matrix(fits[[scenario]], pars = "p"), 2, median),
        t(apply(as.matrix(fits[[scenario]], pars = "p"), 2,
                quantile, probs = c(0.05, 0.95))) # ,
        # Opt = apply(as.matrix(fits[[scenario]], pars = "o"), 2, median),
        # Opt = t(apply(as.matrix(fits[[scenario]], pars = "o"), 2,
        #               quantile, probs = c(0.05, 0.95)))
  )) %>%
  bind_rows()

Sys.time() 
gc()
save(data, file = "D:/Mes Donnees/PhD/R_codes/PhD/Modelisation/Fits/Pseudo_abs_tests_concaves_mediandata.Rdata")
gc()
load("D:/Mes Donnees/PhD/R_codes/PhD/Modelisation/Fits/Pseudo_abs_tests_concaves_mediandata.Rdata")

data <- data %>% 
  rowwise() %>%
  # Estimated presence from estimated proba
  mutate(EstPresence = rbinom(1, size = 1, # Bernoulli = 1 trial
                              prob = mu)) %>%
  ungroup()

gc()
# Plot initial total, subsampled and estimated probabilites  ------------------------------
plist <- vector('list', length(unique(data$Scenario)))

for(p in 1:length(unique(data$Scenario))){
  # message(s)
  plist[[p]] <- local({
    
    dataS <- data %>% 
      filter(Scenario==unique(data$Scenario)[p])
    
    ggplot(dataS, aes(x= logTransmittance, y= TotPresence)) +
      xlim(min(data$logTransmittance), 0) +
      # Total
      # geom_point(col = "grey") +
      # geom_point(aes(y = TotProbability), col = "grey", size =1.5) + 
      # 
      # ggridges::geom_density_ridges(data = (dataS %>% filter(TotPresence==1)), # Presence density
      #                               scale = 0.6,
      #                               jittered_points = T,
      #                               col = "grey",
      #                               alpha=0.05, point_alpha  = 0.7) +
      
      
      # Subsampled
      # geom_point(aes(y= Presence), col = "darkgreen") +
      geom_point(aes(y = Probability), col = "darkgreen", size =1) +
      
      ggridges::geom_density_ridges(data = (dataS %>% filter(!is.na(Presence) & Presence==1)), # Presence density
                                    scale = 0.6,
                                    jittered_points = T,
                                    col = "darkgreen",
                                    alpha=0.05, point_alpha  = 0.7) +
      
      # Probas estimated
      geom_point(aes(y = mu), size=0.7) +
      geom_ribbon(aes(ymin = `5%`, ymax = `95%`), color = 'red', alpha = 0.2) +
      
      ggridges::geom_density_ridges(data = (dataS %>% filter(EstPresence==1)), # Presence density
                                    aes(y= EstPresence), 
                                    scale = 0.6,
                                    jittered_points = T,
                                    col = "black",
                                    alpha=0.05, point_alpha  = 0.7) +
      
      
      labs(title = unique(dataS$Scenario), y="Presence") +
      
      theme_minimal()
    
  })
}

plots <- gridExtra::marrangeGrob(plist, nrow = 3, ncol = 3)
# plots

ggsave("PA_graph_check_concaves.pdf",
       path = "D:/Mes Donnees/PhD/R_codes/PhD/Modelisation/Plots",
       plots, width = 15, height = 10)

gc()
