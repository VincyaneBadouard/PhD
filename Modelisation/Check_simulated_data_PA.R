
# Check simulate data for pseudo-absences tests

load("./Simdata/simulatedata_PA.Rdata")

# plot initial total and subsampled probabilites, and presences as density plots

test <- bind_rows(plist, .id = "Simulation") %>% 
  rename(TotProbability= Probability) %>% 
  mutate(Probability = ifelse(!is.na(Presence), TotProbability, NA))

test$TotPresence ==test$Presence

test %>% 
  filter(Simulation == "concavebell-30-30" | Simulation == "concavebell-30-1000") %>% 
  ggplot(aes(x= logTransmittance, y= TotPresence)) +
  # Total
  # geom_point() +
  # geom_line(aes(y = TotProbability), linewidth =1) + # total proba
  
  # Subsampled
  geom_point(aes(y= Presence), col = "red") +
  geom_line(aes(y = Probability), col = "red", linewidth =1) + 
  
  
  theme_minimal() +
  facet_wrap(~ Simulation, scales = "free")


# labs(title=paste(Simulation,
#                  "; a=",round(a,1), "b=",b, "c=",c,"N1=",N1,"N2=",N2),
#      y="Presence (1) / Absence (0)", x="Explicative variable (X)")
