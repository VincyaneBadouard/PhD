# //amap-data.cirad.fr/work <- y:

# compare Planophile and spherical leaf angle distribution in LAI2200 
library(AMAPVox) # v 2.3.1
#crate voxelspave assumin either planophile or plagiophile distribution of leaf angles
library(lidR)
las <- readLAS("//amap-data.cirad.fr/work/users/VincyaneBadouard/Lidar/ALS2023/LowAltitudeFlight/LAZ/P16_2023_4ha_buffer.laz")
hist(abs(las@data$ScanAngle), breaks=60)
mean(abs(las@data$ScanAngle)) #13.5??
theta = 13.5/90*pi/2
computeG(theta, pdf = "planophile") #0.82
computeG(theta, pdf = "plagiophile") #0.66

# PadBVTotal -------------------------------------------------------------------
#Hence assuming spherical LIDF while planophile should apply we would overestiamte PAD by 0.82/0.5= 1.64
#Hence assuming spherical LIDF while plagiophile should apply we would overestiamte PAD by 0.66/0.5= 1.32
# On Low altitude ALS
vsp <- readVoxelSpace("//amap-data.cirad.fr/work/users/VincyaneBadouard/Lidar/ALS2023/LowAltitudeFlight/Intensity_1m/P16_2023_4ha_buffer_LowAlt_PadHLE_intensity1m.vox")

vsp@data$PadPlano <- vsp@data$PadBVTotal/1.64
vsp@header$columnNames <- c("i", "j", "k", "ground_distance", "nbEchos", "nbSampling", 
                            "lgTotal", "lMeanTotal", "angleMean", "bsPotential", "bsEntering", 
                            "bsIntercepted", "attenuation_FPL_biasedMLE", "attenuation_FPL_biasCorrection", 
                            "attenuation_FPL_unbiasedMLE", "weightedEffectiveFreepathLength", 
                            "weightedFreepathLength", "attenuation_PPL_MLE", "distLaser", 
                            "HLE", "PadBVTotal","PadPlano")
writeVoxelSpace(vsp,"//amap-data.cirad.fr/work/users/VincyaneBadouard/Anisotropy_test/P16_2023_4ha_buffer_LowAlt_PadHLE_intensity1m_plano.vox")


vsp@data$PadPlagio <- vsp@data$PadBVTotal/1.32
vsp@header$columnNames <- c("i", "j", "k", "ground_distance", "nbEchos", "nbSampling", 
                            "lgTotal", "lMeanTotal", "angleMean", "bsPotential", "bsEntering", 
                            "bsIntercepted", "attenuation_FPL_biasedMLE", "attenuation_FPL_biasCorrection", 
                            "attenuation_FPL_unbiasedMLE", "weightedEffectiveFreepathLength", 
                            "weightedFreepathLength", "attenuation_PPL_MLE", "distLaser", 
                            "HLE", "PadBVTotal","PadPlagio")

writeVoxelSpace(vsp,"//amap-data.cirad.fr/work/users/VincyaneBadouard/Anisotropy_test/P16_2023_4ha_buffer_LowAlt_PadHLE_intensity1m_plagio.vox")

# AMAPVox ----------------------------------------------------------------------
AMAPVox::run()
#[compute LAI2200 for both options]#

library(data.table)
sphere <- fread("//amap-data.cirad.fr/work/users/VincyaneBadouard/Anisotropy_test/test_spherical")
plano <- fread("//amap-data.cirad.fr/work/users/VincyaneBadouard/Anisotropy_test/test_plano")
plagio <- fread("//amap-data.cirad.fr/work/users/VincyaneBadouard/Anisotropy_test/test_plagio")

sphere=cbind(sphere, "Spherical")
plano=cbind(plano, "Planophile")
plagio=cbind(plagio,"Plagiophile")
all=rbind(plagio,sphere,plano)
all_mean<-all[,.(r1=mean(`GAP[1]`),
                 r2=mean(`GAP[2]`),
                 r3=mean(`GAP[3]`),
                 r4=mean(`GAP[4]`),
                 r5=mean(`GAP[5]`)), by='V2']
long <- reshape(all_mean, direction="long", varying=list(2:6)) %>% select(-id)
names(long)=c("LIDF", "Ring","Transmittance")

# LAI2200 ----------------------------------------------------------------------
LAI2200_ring <- LAI2200 %>%
  group_by(Ring) %>% # variables selon lesquelles grouper
  summarise(across(MeanTransmittance_LAI2200, # Variables à analyser
                   list(mean = ~mean(.), # les stats
                        sd = ~sd(.),
                        se = ~plotrix::std.error(.)),
                   .names = "{.col}_{.fn}")) %>% 
  # Correction des rings 4 et 5 d'après la transmittance estimée de la DZ au LiDAR
  mutate(MeanTransmittance_LAI2200_mean = ifelse(Ring==4, MeanTransmittance_LAI2200_mean*0.96,
                                                 MeanTransmittance_LAI2200_mean)) %>% 
  mutate(MeanTransmittance_LAI2200_mean = ifelse(Ring==5, MeanTransmittance_LAI2200_mean*0.23,
                                                 MeanTransmittance_LAI2200_mean)) %>% 
  select(Ring, MeanTransmittance_LAI2200_mean)

# Error ------------------------------------------------------------------------
# erreur moyenne relative : mae / moyenne des transmittances mesurées sur l'ensemble des rings
# biais relatifs  : moyene des écarts  / moyenne des transmittances mesurées sur l'ensemble des rings
# tout sur le plot

measuredTranmittance_mean <- mean(LAI2200_ring$MeanTransmittance_LAI2200_mean) # 0.032


# Root Mean Square Error of Prediction
RMSEP_sph <- chillR::RMSEP(long[long$LIDF=="Spherical",]$Transmittance, LAI2200_ring$MeanTransmittance_LAI2200_mean)
RMSEP_plan <- chillR::RMSEP(long[long$LIDF=="Planophile",]$Transmittance, LAI2200_ring$MeanTransmittance_LAI2200_mean)
RMSEP_plag <- chillR::RMSEP(long[long$LIDF=="Plagiophile",]$Transmittance, LAI2200_ring$MeanTransmittance_LAI2200_mean)

long <- long %>% 
  left_join(LAI2200_ring, by = "Ring") %>% 
  group_by(LIDF) %>% 
  # Relative Mean Error: mae/mean(mesuré)
  mutate(ME = Metrics::mae(MeanTransmittance_LAI2200_mean, Transmittance)/measuredTranmittance_mean) %>% 
  # Relative biases: mean(estimé-mesuré)/mean(mesuré)
  mutate(`Relative biases` = mean(Transmittance-MeanTransmittance_LAI2200_mean)/measuredTranmittance_mean) %>% 
  ungroup()

label <- data.frame(
  Ring = 4.5, 
  # Transmittance = c(0.08, 0.075, 0.07, 0.065, 0.06, 0.055), 
  Transmittance = c(0.078, 0.075, 0.07, 0.068, 0.065, 0.06), 
  LIDF = unique(long$LIDF),
  error = c(paste("ME = ",round(unique(long$ME),2)), paste("RE = ",round(unique(long$`Relative biases`),3)))
) %>% filter(LIDF != "Planophile") 

library(ggplot2)
long %>% 
  filter(LIDF != "Planophile") %>% 
  mutate(LIDF = as.factor(LIDF)) %>% 
  ggplot(aes(x=Ring, y=Transmittance, color = LIDF)) +
  geom_point(size = 3) +
  geom_line() +
  geom_point(data=LAI2200_ring, aes(y = MeanTransmittance_LAI2200_mean, colour = "LAI2200"),size = 3) +
  geom_line(data=LAI2200_ring,aes(y = MeanTransmittance_LAI2200_mean, colour = "LAI2200")) +
  
  scale_colour_manual(values = c("Spherical" = "#56B4E9",
                                 # "Planophile" = "#CC79A7",
                                 "Plagiophile" = "#009E73",
                                 "LAI2200" = "#D55E00"),
                      breaks=c("Spherical",
                               # "Planophile",
                               "Plagiophile",
                               "LAI2200")) +
  geom_text(data = label, aes(label = error), size = 5, show.legend = F) +

  theme_minimal() +
  theme(title= element_text(size=16), axis.title= element_text(size=16), axis.text= element_text(size=16),
        legend.title= element_text(size=16), legend.text= element_text(size=16)) +
  labs(title = "P16 LowAlt Intensity1m", y = "Average transmittance")

ggsave(paste("Long_profile_The2_transects_Transmittance_byRing_Intensity1m_anisotropytest.png", sep=""),
       path = "D:/Mes Donnees/PhD/Figures/lidar/Anisotropy_test",
       width = 20, height = 20, units = "cm", dpi=800, bg="white")

# Compute Relative differences
label <- long %>%
  left_join(LAI2200_ring, by = "Ring") %>% 
  mutate(RD = round((Transmittance - MeanTransmittance_LAI2200_mean)/MeanTransmittance_LAI2200_mean,2)) %>%  # (estimé-mesuré)/mesuré
  select(Ring, LIDF, RD) %>% 
  pivot_wider(names_from = LIDF,
              values_from = RD)

write.csv(label, "D:/Mes Donnees/PhD/Figures/lidar/Anisotropy_test/Relative_differences_byRing_anisotropytest.csv")



