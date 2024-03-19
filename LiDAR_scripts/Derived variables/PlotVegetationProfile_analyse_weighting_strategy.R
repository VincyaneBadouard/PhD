# install.packages("remotes")
# remotes::install_github('umr-amap/AMAPVox')
# remove.packages("rgl")
# install.packages('rgl', dependencies = TRUE)
# install.packages("ggplot2")
# install.packages("viridis")
# install.packages("viridisLite")
library(viridis)
library(viridisLite)
library(rgl)
library(AMAPVox)
library(lattice)
library(data.table)
library(MASS)
library(ggplot2)
library(dplyr)
library(scales)
library(grid)
rm(list=ls())
setwd("/home/yuchen/Documents/PhD/data_for_project/23-11-08_simu_data_for_second_paper")

#read voxels space and plot nbS
read_AMAPVox_output <- function(vsp_path){
  vsp <- readVoxelSpace(vsp_path)
  vsp@data<-cbind(vsp@data,getPosition(vsp))
  return(vsp)
}

# read leaf&wood, ... output using div&strongest echo
leafOnly=read_AMAPVox_output("only_leaf/only_leaf_10pct_rel.vox")
WoodLeaf=read_AMAPVox_output("leaf_and_wood/leaf_and_wood_rel.vox")
WoodOnly=read_AMAPVox_output("only_wood/only_wood_10pct_stg.vox")
LeafOnlyRot=read_AMAPVox_output("only_leaf_rotated/only_leaf_rotated_10pct_rel.vox")
LeafOnlyShif=read_AMAPVox_output("only_leaf_shifted/only_leaf_shifted_10pct_rel.vox")
LeafOnlyRotShif=read_AMAPVox_output("only_leaf_rotated_plus_shifted/res_mecho_div_relative_echo_w/only_leaf_r_and_s_div.vox")
LeafOnlyShifShrink = read_AMAPVox_output("/home/yuchen/Documents/PhD/data_for_project/23-11-08_simu_data_for_second_paper/only_leaf_shifted_shrink/only_leaf_shifted_shrink.vox")
LeafOnlyRotShifShrink = read_AMAPVox_output("/home/yuchen/Documents/PhD/data_for_project/23-11-08_simu_data_for_second_paper/only_leaf_rotated_plus_shifted_shrink/only_leaf_r_and_s_shrink.vox")
LeafOnlyRotShif2M=read_AMAPVox_output("/home/yuchen/Documents/PhD/data_for_project/23-11-08_simu_data_for_second_paper/only_leaf_rotated_plus_shifted_2M/only_leaf_r_and_s_2M.vox")

# rank echo
LeafOnlyRotShif_MulEcho_Div_RanKCust <- read_AMAPVox_output("only_leaf_rotated_plus_shifted/res_mecho_div_rankCustomeEcho_weight/only_leaf_r_and_s_0403_10pct.vox")
LeafOnlyRotShif_MulEcho_Div_RanKCust_3000 <- read_AMAPVox_output("only_leaf_rotated_plus_shifted/res_mecho_div_rankCustomeEcho_weight/only_leaf_r_and_s_0403.vox")

# relative echo
LeafOnlyRotShif_3000 <- readVoxelSpace("only_leaf_rotated_plus_shifted/res_mecho_div_relative_echo_w/only_leaf_r_and_s_div.vox")

# wood mask
LeafWood_RelEcho_filter_w_50 <- readVoxelSpace("leaf_and_wood/leaf_and_wood_10pct_rel_filter_wood.vox")
LeafWood_RelEcho_filter_w_SOUL <- readVoxelSpace("leaf_and_wood/leaf_and_wood_10pct_rel_filter_wood_SOUL.vox")

#### Order voxels with their position and store in list
voxlist=list()

WoodLeaf@data = setorderv(WoodLeaf@data, c('i','j','k'))
voxlist[[1]]<-WoodLeaf
names(voxlist)[1]<-"WoodLeaf"

leafOnly@data = setorderv(leafOnly@data, c('i','j','k'))
voxlist[[2]]<-leafOnly
names(voxlist)[2]<-"LeafOnly"

WoodOnly@data = setorderv(WoodOnly@data, c('i','j','k'))
voxlist[[3]]<-WoodOnly
names(voxlist)[3]<-"WoodOnly"

LeafOnlyRotShif_3000@data = setorderv(LeafOnlyRotShif_3000@data, c('i','j','k'))
voxlist[[4]]<-LeafOnlyRotShif_3000
names(voxlist)[4]<-"LeafOnlyRotShif"

LeafOnlyRotShif_MulEcho_Div_RanKCust_3000@data = setorderv(LeafOnlyRotShif_MulEcho_Div_RanKCust_3000@data, c('i','j','k'))
voxlist[[5]]<-LeafOnlyRotShif_MulEcho_Div_RanKCust_3000
names(voxlist)[5]<-"LeafOnlyRotShif_MulEcho_Div_RanKCust"

LeafWood_RelEcho_filter_w_50@data = setorderv(LeafWood_RelEcho_filter_w_50@data, c('i','j','k'))
voxlist[[6]]<-LeafWood_RelEcho_filter_w_50
names(voxlist)[6]<-"LeafWood_RelEcho_filter_w_50"

LeafWood_RelEcho_filter_w_SOUL@data = setorderv(LeafWood_RelEcho_filter_w_SOUL@data, c('i','j','k'))
voxlist[[7]]<-LeafWood_RelEcho_filter_w_SOUL
names(voxlist)[7]<-"LeafWood_RelEcho_filter_w_SOUL"

LeafOnlyShif@data = setorderv(LeafOnlyShif@data, c('i','j','k'))
voxlist[[8]]<-LeafOnlyShif
names(voxlist)[8]<-"LeafOnlyShif"

LeafOnlyRot@data = setorderv(LeafOnlyRot@data, c('i','j','k'))
voxlist[[9]]<-LeafOnlyRot
names(voxlist)[9]<-"LeafOnlyRot"

#LeafOnlyShifShrink = setorderv(LeafOnlyShifShrink@data, c('i','j','k'))
voxlist[[10]]<-LeafOnlyShifShrink
names(voxlist)[10]<-"LeafOnlyShifShrink"

#LeafOnlyRotShifShrink = setorderv(LeafOnlyRotShifShrink@data, c('i','j','k'))
voxlist[[11]]<-LeafOnlyRotShifShrink
names(voxlist)[11]<-"LeafOnlyRotShifShrink"

#LeafOnlyRotShif2M = setorderv(LeafOnlyRotShif2M@data, c('i','j','k'))
voxlist[[12]]<-LeafOnlyRotShif2M
names(voxlist)[12]<-"LeafOnlyRotShif2M"

# sum(LeafOnlyRotShif_IntCor$nbEchos[which(LeafOnlyRotShif_IntCor$nbEchos==0)])
# summary(LeafOnlyRotShif_IntCor@data)


ldf=c() #long data frame
for (i in 1:length(voxlist))
{
       df=voxlist[[i]]@data[,.(i,j,k, ground_distance, attenuation_PPL_MLE, attenuation_FPL_unbiasedMLE, attenuation_FPL_biasedMLE, attenuation_FPL_biasCorrection, transmittance, distLaser, nbSampling,angleMean)]
       df$flight=names(voxlist)[i]
       ldf=rbind(ldf,df)
}

# we set threshold here
ldf<-as.data.table(ldf)
ldf$att_from_tr=-log(ldf$transmittance)
ldf$att_from_tr[ldf$att_from_tr>5] <- 5
ldf$attenuation_PPL_MLE[ldf$attenuation_PPL_MLE>5] <- 5
ldf$attenuation_FPL_biasedMLE[ldf$attenuation_FPL_biasedMLE>5] <- 5
#ldf$attenuation_FPL_unbiasedMLE[ldf$attenuation_FPL_unbiasedMLE>5] <- 5
#ldf$attenuation_FPL_biasCorrection[ldf$attenuation_FPL_biasCorrection>5] <- 5
#summary(ldf$attenuation_PPL_MLE)
ldf$flight['LeafOnlyShif']

ldf$ground_distance[which(ldf$ground_distance<0)]<- -1
summary(ldf)
pdf=c() #profile data frame
nona=function(vec) {length(na.omit(vec))}
pos=function(vec) {sum(na.omit(vec>0))}
res=1


# round is round, 0.4999 -> 0, 0.50001 ->1
for (f in 1:length(names(voxlist)))
{
        sel=ldf[flight==names(voxlist)[f] & ground_distance>1 ,]
        df=data.frame(ground_distance=sort(unique(as.integer(sel$ground_distance/res)*res)),
                     att=tapply(sel$attenuation_FPL_unbiasedMLE, as.integer(sel$ground_distance/res)*res, sum, na.rm=T), 
                     att_tr=tapply(sel$att_from_tr,as.integer(sel$ground_distance/res)*res, sum, na.rm=T),
                     angle=tapply(sel$angle, as.integer(sel$ground_distance/res)*res, mean, na.rm=T),
                     nbs=tapply(sel$nbSampling, as.integer(sel$ground_distance/res)*res, mean, na.rm=T),
                     comp=tapply(sel$nbSampling, as.integer(sel$ground_distance/res)*res,pos)/dim(unique(sel[,.(i,j)]))[1],
                     dl=tapply(sel$distLaser, as.integer(sel$ground_distance/res)*res,mean, na.rm=T),
                     flight=unique(sel$flight)
                     )
        pdf=rbind(pdf,df)
}

summary(pdf)
#pdf$att=-log(pdf$trans)
pdf$PAD=2*pdf$att
#tapply(pdf$PAD, pdf$flight,sum)*res

#### plot attenuation profiles prior to adjustment
#any voxel contributing to interception at a given height should be used for profile 

c(pdf$z)
df_att <- data.frame(
  ground_distance = c(pdf$ground_distance),
  #att = c(pdf$att),
  pad = c(pdf$PAD),
  flight = c(pdf$flight)
)

read_LAI_file <- function(lai, flight_name, x_min, x_max, y_min, y_max, res=1.0){
  true_lai <- setorderv(lai, c('x','y','z'))
  true_lai_crop <- true_lai[true_lai$x>=x_min & true_lai$x<x_max & true_lai$y>=y_min & true_lai$y<y_max, ]
  new_lai <- aggregate(pad ~ groundDist, data = true_lai_crop, FUN = sum)
  df <- data.frame(
    z = c(new_lai$groundDist),
    att = c(new_lai$pad/3600),
    pad = c(new_lai$pad/3600),
    flight = c(rep(flight_name,length(new_lai$groundDist)))
  )
  return(df)
}

# after correction
true_lai <- read.csv("dataframe_pad/LAI_2024_03_04_17_13_all_leaves.csv")
true_lai_crop <- true_lai[true_lai$x>=20 & true_lai$x<80 & true_lai$y>=0 & true_lai$y<60, ]
df_LAI <- true_lai_crop %>% 
  group_by(x,y,z) %>% 
  summarise(
    pad=sum(pad),
    dtg = mean(dtg, na.rm = TRUE)
  )
# df_LAI$x <- df_LAI$x - 20
# df_LAI$z <- df_LAI$z - 5
df_LAI <- setorderv(df_LAI, c('x','y','z'))
#df_LAI <- df_LAI[df_LAI$dtg > 0.5,]
summary(df_LAI)

LeafOnlyRotShif_s <- LeafOnlyRotShif@data[, c('x', 'y', 'z', 'attenuation_FPL_unbiasedMLE', 'ground_distance', 'nbEchos', 'nbSampling', 'attenuation_FPL_biasCorrection' ,'explorationRate', "attenuation_PPL_MLE", 'transmittance')]
df_LAI_merged <- merge(df_LAI, LeafOnlyRotShif_s, by = c('x', 'y', 'z'), all.x = TRUE, all.y=T)
df_LAI_merged$pad <- ifelse(is.na(df_LAI_merged$pad), 0, df_LAI_merged$pad)
df_LAI_merged <- df_LAI_merged[df_LAI_merged$ground_distance > 2, ]

df_LAI_merged$ground_distance <-as.integer(df_LAI_merged$ground_distance) 
df_LAI_true <- aggregate(pad ~ ground_distance, data = df_LAI_merged, FUN = sum)
df_LAI_true$flight <- "GroundTruth"

df_att_all <- rbind(df_att, df_LAI_true)

df_att_all$flight

unique(df_att_all$flight)
sum(df_att_all[df_att_all['flight']=='LeafOnly',]$pad)
sum(df_att_all[df_att_all['flight']=='LeafOnlyRotShif',]$pad)/3600
sum(df_att_all[df_att_all['flight']=='LeafOnlyShifShrink',]$pad)/3600
sum(df_att_all[df_att_all['flight']=='LeafOnlyRotShifShrink',]$pad)/3600
sum(df_att_all[df_att_all['flight']=='LeafOnlyRotShif2M',]$pad)/900


# simple version 
#flight_list <- c( 'GroundTruth', 'LeafOnlyRot', 'LeafOnlyRotShif')
flight_list <- c('LeafOnly',
                 'WoodLeaf',
                 'LeafWood_RelEcho_filter_w_50',
                 'LeafWood_RelEcho_filter_w_SOUL',
                 'LeafOnlyShif',
                 'TrueLAI')
df_att_selected <- df_att_all[df_att_all$flight %in% flight_list, ]
df_att_selected

ggplot(df_att_selected, aes(x=ground_distance, y=pad/3600)) +
  geom_point(aes(color=factor(flight))) + 
  geom_line(aes(color=factor(flight))) +
  #scale_color_viridis_d() +
  xlim(0,30) +
  ylim(0,0.7) + 
  ggtitle("Weighting strategy FPL unbaisedMLE") +
  theme(plot.title = element_text(hjust=0.5)) + 
  labs(y="PAD", x="Absolute Tree Height") +
  coord_flip()

# complete version

# flight_list <- c('LeafOnlyRotShif',
#                  'LeafOnlyRotShif_MulEcho_Div_RanKCust',
#                  'LeafWood_RelEcho_filter_w_50',
#                  'LeafWood_RelEcho_filter_w_SOUL',
#                  'TrueLAI')
flight_list <- c( 'GroundTruth', 'LeafOnlyRot', 'LeafOnlyRotShif', 'LeafOnlyShif', 'LeafOnly'
                 )
df_att_selected <- df_att_all[df_att_all$flight %in% flight_list, ]
df_att_selected$LAI <- ave(df_att_selected$pad, df_att_selected$flight, FUN = sum)
#df_att_selected$LAI <- df_att_selected$LAI/3600

df_att_selected<-as.data.table(df_att_selected)

LAI_calculated <- aggregate(df_att_selected$pad, by=list(LAI=df_att_selected$flight), FUN = sum)
LAI_calculated

LAI_LeafOnlyRot <- as.numeric(LAI_calculated[LAI_calculated['LAI']=='LeafOnlyRot'][2])/3600
LAI_LeafOnlyRotShif <- as.numeric(LAI_calculated[LAI_calculated['LAI']=='LeafOnlyRotShif'][2])/3600
LAI_LeafOnlyShif <- as.numeric(LAI_calculated[LAI_calculated['LAI']=='LeafOnlyShif'][2])/3600
LAI_True <- as.numeric(LAI_calculated[LAI_calculated['LAI']=='GroundTruth'][2])/3600
LAI_LeafOnly <- as.numeric(LAI_calculated[LAI_calculated['LAI']=='LeafOnly'][2])/3600
LAI_calculated
LAI_calculated
LAI_LeafOnlyShif

# create text
grob_fontsize <- 10
grob_x_pos <- 0.48
grob3 <- grobTree(textGrob(paste("LAI_GroundTruth =", round(LAI_True,2)), x=grob_x_pos,  y=0.93, hjust=0,
                           gp=gpar(col="#F8766D", fontsize=grob_fontsize, fontface="bold")))
grob5 <- grobTree(textGrob(paste("LAI_LeafOnly =", round(LAI_LeafOnly,2)), x=grob_x_pos,  y=0.90, hjust=0,
                           gp=gpar(col="#B79F00", fontsize=grob_fontsize, fontface="bold")))
grob1 <- grobTree(textGrob(paste("LAI_LeafOnlyRot =", round(LAI_LeafOnlyRot,2)), x=grob_x_pos,  y=0.87, hjust=0,
                          gp=gpar(col="#00BA38", fontsize=grob_fontsize, fontface="bold")))
grob2 <- grobTree(textGrob(paste("LAI_LeafOnlyRotShif =", round(LAI_LeafOnlyRotShif,2)), x=grob_x_pos,  y=0.84, hjust=0,
                           gp=gpar(col="#619CFF", fontsize=grob_fontsize, fontface="bold")))
grob4 <- grobTree(textGrob(paste("LAI_LeafOnlyShif =", round(LAI_LeafOnlyShif,2)), x=grob_x_pos,  y=0.81, hjust=0,
                           gp=gpar(col="#F564E3", fontsize=grob_fontsize, fontface="bold")))


ggplot(df_att_selected, aes(x=ground_distance, y=pad/3600)) +
  geom_point(aes(color=factor(flight))) + 
  geom_line(aes(color=factor(flight))) +
  labs(color="Modalities") + 
  theme(legend.position = c(0.74, 0.25)) + 
  #scale_color_viridis_d() +
  xlim(0,30) +
  ylim(0,0.5) + 
  #ggtitle("Clumping Effect") +
  theme(plot.title = element_text(hjust=0.5 ,size=20), , legend.text=element_text(size=10), legend.title =element_text(size=12)) + 
  labs(y="LAD", x="Height Above Ground") +
  annotation_custom(grob1) +
  annotation_custom(grob2) +
  annotation_custom(grob3) +
  annotation_custom(grob4) +
  annotation_custom(grob5) +
  coord_flip()

# check colors
show_col(hue_pal()(6))
#grid()

# for meeting 19/03
flight_list <- c( 'GroundTruth', 'LeafOnlyRotShif', 'LeafOnlyRotShifShrink'
)
df_att_selected <- df_att_all[df_att_all$flight %in% flight_list, ]
df_att_selected$LAI <- ave(df_att_selected$pad, df_att_selected$flight, FUN = sum)
#df_att_selected$LAI <- df_att_selected$LAI/3600

df_att_selected<-as.data.table(df_att_selected)

LAI_calculated <- aggregate(df_att_selected$pad, by=list(LAI=df_att_selected$flight), FUN = sum)

LAI_True <- as.numeric(LAI_calculated[LAI_calculated['LAI']=='GroundTruth'][2])/3600
LAI_LeafOnlyRotShif <- as.numeric(LAI_calculated[LAI_calculated['LAI']=='LeafOnlyRotShif'][2])/3600
LAI_LeafOnlyRotShifShrink <- as.numeric(LAI_calculated[LAI_calculated['LAI']=='LeafOnlyRotShifShrink'][2])/3600
LAI_LeafOnlyRotShifShrink

LAI_calculated[LAI_calculated['LAI']=='LeafOnlyRotShifShrink', ]$flight
# create text
grob_fontsize <- 10
grob_x_pos <- 0.44
grob1 <- grobTree(textGrob(paste("LAI_GroundTruth =", round(LAI_True,2)), x=grob_x_pos,  y=0.93, hjust=0,
                           gp=gpar(col="#F8766D", fontsize=grob_fontsize, fontface="bold")))
grob2 <- grobTree(textGrob(paste("LAI_LeafOnlyRotShif =", round(LAI_LeafOnlyRotShif,2)), x=grob_x_pos,  y=0.90, hjust=0,
                           gp=gpar(col="#00BA38", fontsize=grob_fontsize, fontface="bold")))
grob3 <- grobTree(textGrob(paste("LAI_OnlyRotShifShrink =", round(LAI_LeafOnlyRotShifShrink,2)), x=grob_x_pos,  y=0.87, hjust=0,
                           gp=gpar(col="#619CFF", fontsize=grob_fontsize, fontface="bold")))

ggplot(df_att_selected, aes(x=ground_distance, y=pad/3600)) +
  geom_point(aes(color=factor(flight))) + 
  geom_line(aes(color=factor(flight))) +
  labs(color="Modalities") + 
  theme(legend.position = c(0.74, 0.25)) + 
  #scale_color_viridis_d() +
  xlim(0,30) +
  ylim(0,0.5) + 
  #ggtitle("Clumping Effect") +
  theme(plot.title = element_text(hjust=0.5 ,size=20), , legend.text=element_text(size=10), legend.title =element_text(size=12)) + 
  labs(y="LAD", x="Height Above Ground") +
  annotation_custom(grob1) +
  annotation_custom(grob2) +
  annotation_custom(grob3) +
  coord_flip()

# for meeting 19/03, shift
flight_list <- c( 'GroundTruth', 'LeafOnlyShif', 'LeafOnlyShifShrink'
)
df_att_selected <- df_att_all[df_att_all$flight %in% flight_list, ]
df_att_selected$LAI <- ave(df_att_selected$pad, df_att_selected$flight, FUN = sum)
#df_att_selected$LAI <- df_att_selected$LAI/3600

df_att_selected<-as.data.table(df_att_selected)

LAI_calculated <- aggregate(df_att_selected$pad, by=list(LAI=df_att_selected$flight), FUN = sum)

LAI_True <- as.numeric(LAI_calculated[LAI_calculated['LAI']=='GroundTruth'][2])/3600
LAI_LeafOnlyShif <- as.numeric(LAI_calculated[LAI_calculated['LAI']=='LeafOnlyShif'][2])/3600
LAI_LeafOnlyShifShrink <- as.numeric(LAI_calculated[LAI_calculated['LAI']=='LeafOnlyShifShrink'][2])/3600

# create text
grob_fontsize <- 10
grob_x_pos <- 0.44
grob1 <- grobTree(textGrob(paste("LAI_GroundTruth =", round(LAI_True,2)), x=grob_x_pos,  y=0.93, hjust=0,
                           gp=gpar(col="#F8766D", fontsize=grob_fontsize, fontface="bold")))
grob2 <- grobTree(textGrob(paste("LAI_LeafOnlyShif =", round(LAI_LeafOnlyShif,2)), x=grob_x_pos,  y=0.90, hjust=0,
                           gp=gpar(col="#00BA38", fontsize=grob_fontsize, fontface="bold")))
grob3 <- grobTree(textGrob(paste("LAI_LeafOnlyShifShrink =", round(LAI_LeafOnlyShifShrink,2)), x=grob_x_pos,  y=0.87, hjust=0,
                           gp=gpar(col="#619CFF", fontsize=grob_fontsize, fontface="bold")))

ggplot(df_att_selected, aes(x=ground_distance, y=pad/3600)) +
  geom_point(aes(color=factor(flight))) + 
  geom_line(aes(color=factor(flight))) +
  labs(color="Modalities") + 
  theme(legend.position = c(0.74, 0.25)) + 
  #scale_color_viridis_d() +
  xlim(0,30) +
  ylim(0,0.5) + 
  #ggtitle("Clumping Effect") +
  theme(plot.title = element_text(hjust=0.5 ,size=20), , legend.text=element_text(size=10), legend.title =element_text(size=12)) + 
  labs(y="LAD", x="Height Above Ground") +
  annotation_custom(grob1) +
  annotation_custom(grob2) +
  annotation_custom(grob3) +
  coord_flip()

# for meeting 19/03, 2M 
flight_list <- c( 'GroundTruth', 'LeafOnlyRotShif', 'LeafOnlyRotShif2M'
)
df_att_selected <- df_att_all[df_att_all$flight %in% flight_list, ]
df_att_selected$LAI <- ave(df_att_selected$pad, df_att_selected$flight, FUN = sum)
#df_att_selected$LAI <- df_att_selected$LAI/3600

df_att_selected<-as.data.table(df_att_selected)

LAI_calculated <- aggregate(df_att_selected$pad, by=list(LAI=df_att_selected$flight), FUN = sum)

LAI_True <- as.numeric(LAI_calculated[LAI_calculated['LAI']=='GroundTruth'][2])/3600
LAI_LeafOnlyRotShif <- as.numeric(LAI_calculated[LAI_calculated['LAI']=='LeafOnlyRotShif'][2])/3600
LAI_LeafOnlyRotShif2M <- as.numeric(LAI_calculated[LAI_calculated['LAI']=='LeafOnlyRotShif2M'][2])/900

LAI_calculated$x

# create text
grob_fontsize <- 10
grob_x_pos <- 0.44
grob1 <- grobTree(textGrob(paste("LAI_GroundTruth =", round(LAI_True,2)), x=grob_x_pos,  y=0.93, hjust=0,
                           gp=gpar(col="#F8766D", fontsize=grob_fontsize, fontface="bold")))
grob2 <- grobTree(textGrob(paste("LAI_LeafOnlyRotShif =", round(LAI_LeafOnlyRotShif,2)), x=grob_x_pos,  y=0.90, hjust=0,
                           gp=gpar(col="#00BA38", fontsize=grob_fontsize, fontface="bold")))
grob3 <- grobTree(textGrob(paste("LAI_LeafOnlyRotShif2M =", round(LAI_LeafOnlyRotShif2M,2)), x=grob_x_pos,  y=0.87, hjust=0,
                           gp=gpar(col="#619CFF", fontsize=grob_fontsize, fontface="bold")))

df_att_selected2m <- df_att_selected
df_att_selected2m[df_att_selected2m$flight=="LeafOnlyRotShif2M"]$pad <- df_att_selected2m[df_att_selected2m$flight=="LeafOnlyRotShif2M"]$pad*4
ggplot(df_att_selected2m, aes(x=ground_distance, y=pad/3600)) +
  geom_point(aes(color=factor(flight))) + 
  geom_line(aes(color=factor(flight))) +
  labs(color="Modalities") + 
  theme(legend.position = c(0.74, 0.25)) + 
  #scale_color_viridis_d() +
  xlim(0,30) +
  ylim(0,0.5) + 
  #ggtitle("Clumping Effect") +
  theme(plot.title = element_text(hjust=0.5 ,size=20), , legend.text=element_text(size=10), legend.title =element_text(size=12)) + 
  labs(y="LAD", x="Height Above Ground") +
  annotation_custom(grob1) +
  annotation_custom(grob2) +
  annotation_custom(grob3) +
  coord_flip()


ggplot(df_att_selected, aes(x=z, y=att)) +
  geom_point(aes(color=factor(flight))) + 
  geom_line(aes(color=factor(flight))) +
  #scale_color_viridis_d() +
  xlim(0,30) +
  ylim(0,0.5)+
  labs(y="attenuation", x="Absolute Tree Height")+
  coord_flip()

ggplot(pdf, aes(x=z, y=angle)) +
        geom_point(aes(color=factor(flight))) + 
        geom_line(aes(color=factor(flight))) +
        xlim(0,30) +
        coord_flip()

ggplot(pdf, aes(x=z, y=nbs)) +
        geom_point(aes(color=factor(flight))) + 
        geom_line(aes(color=factor(flight))) +
        xlim(0,40) +
        ylim(0,5000) +
        labs(y="shots per voxel")+
        coord_flip()

ggplot(pdf[grep("leafOnly", pdf$flight),], aes(x=z, y=nbs)) +
        geom_point(aes(color=factor(flight))) + 
        geom_line(aes(color=factor(flight))) +
        xlim(2,46) +
        labs(y="shots per voxel")+
        coord_flip()

ggplot(pdf[grep("leafOnly", pdf$flight),], aes(x=z, y=comp)) +
        geom_point(aes(color=factor(flight))) + 
        geom_line(aes(color=factor(flight))) +
        xlim(2,46) +
        labs(y = "prop.of sampled voxels")+
        coord_flip()

ggplot(pdf[grep("leafOnly", pdf$flight),], aes(x=z, y=dl)) +
        geom_point(aes(color=factor(flight))) + 
        geom_line(aes(color=factor(flight))) +
        xlim(2,46) +
        labs(y="mean distance to laser")+
        coord_flip()

pdf_only_leaf <- pdf[pdf$flight == "LeafOnly", ]
summary(pdf_only_leaf)


plot_lai$idx<-paste(plot_lai$x,plot_lai$y, plot_lai$z,sep="_")
plot_lai

########################## utility ####################

# 10/01/2024 ####
# plot(I(pmin(-log(voxlist[[4]]@data$transmittance),20))~voxlist[[4]]@data$attenuation_PPL_MLE)
# plot(I(pmin(-log(voxlist[[4]]@data$transmittance),20))~voxlist[[4]]@data$attenuation_FPL_unbiasedMLE)
# sum(voxlist[[4]]$transmittance==0)

# 20/02/2024 ####
# vx<-LeafOnlyRotShif_MulEcho_NoDiv_RelEcho
# hist(vx@data$attenuation_FPL_unbiasedMLE, breaks=100)
# range(vx@data$attenuation_FPL_unbiasedMLE)
# range(vx@data$attenuation_FPL_biasedMLE)
# hist(vx@data$nbSampling, breaks=100)
# sum(vx@data$nbSampling<25)
# ####
