##################################
# Analyse des donnees LAI mesurees et simulees sur le transect P9
################################## sept16

setwd(dir="C:/Users/badouard/Downloads/AnalyseTransectP9")

rm(list=ls())
library(geoR)
library(ape)

# Mesures terrain LAI sur transect ----------------------------------------------
dense<-read.csv("LAI_P9_Densification.csv",sep=";")
dense$posy2016=round(dense$posy2016,digits=2)
dense$index<-paste(dense$posx,dense$posy2016,sep="_")
dense<-dense[order(dense$posy2016),]

# Simulation amapvox sur transect -----------------------------------------------
densim<-read.table("simu_transectdense_newy_sept16.txt",sep="\t",header=T)
densim$posX=round(densim$posX,digits=0)
densim$posY=round(densim$posY,digits=2)

densim$index<-paste(densim$posX,densim$posY,sep="_")
densim<-densim[order(densim$posY),]

alldense<-merge(dense,densim,all.x=T,by="index")
alldense<-alldense[order(alldense$posy2016),]

names(alldense)
# GAP1-5 : transmittance or gap fraction ? for Ring1-5
# GAP.1..x : measured
# GAP.1..y : simulated
# posY : position along transect (m)

###simule
plot(densim$LAI~densim$posY,type="l")
title(main="simule tous les metres sur le transect E")
abline(a=mean(densim$LAI),b=0,col=2)

plot(densim$GAP.1.~densim$posY,ylim=c(0.001,0.3),type="l", 
     xlab="position along transect (m)", ylab="simulated transmittance")
lines(densim$GAP.2.~densim$posY,col="red")
lines(densim$GAP.3.~densim$posY,col="blue")
lines(densim$GAP.4.~densim$posY,col="green")
lines(densim$GAP.5.~densim$posY,col="orange")
legend("topleft", legend=c("ring 1", "ring 2","ring 3", "ring 4","ring 5"),
       col=c("black","red", "blue","green", "orange"), lty=1, cex=0.8)

###mesure
plot(dense$LAI~dense$posy,type="l")
title(main="mesures tous les metres sur le transect E")
abline(a=mean(dense$LAI),b=0,col=2)

plot(dense$GAP.1.~dense$posy,ylim=c(0.001,0.3),type="l",
     xlab="position along transect (m)", ylab="measured transmittance")
lines(dense$GAP.2.~dense$posy,col="red")
lines(dense$GAP.3.~dense$posy,col="blue")
lines(dense$GAP.4.~dense$posy,col="green")
lines(dense$GAP.5.~dense$posy,col="orange")
legend("topleft", legend=c("ring 1", "ring 2","ring 3", "ring 4","ring 5"),
       col=c("black","red", "blue","green", "orange"), lty=1, cex=0.8)

###correlation -----------------------------------------------------------------
mean(alldense$GAP.1..x)#mesure
mean(alldense$GAP.1..y)#simule
var(alldense$GAP.3..x)
var(alldense$GAP.3..y)
mean(alldense$GAP.3..x)#mesure
mean(alldense$GAP.3..y)#simule

mean(alldense$LAI.x)#mesure
var(alldense$LAI.x)
mean(alldense$LAI.y)#simule
var(alldense$LAI.y)

# L'écart quadratique moyen (RMSE) est l'écart-type des résidus (erreurs de prévision).
# Les résidus sont la mesure de l'écart entre les points de données et la ligne de régression.
RMSE<-function(X,Y) {sqrt(sum((Y-X)^2)/length(X))}

plot(alldense$GAP.1..x,alldense$GAP.1..y,xlab="Measured gap fraction",ylab="Simulated gap fraction",main="Ring 1 r2=0.91")
abline(a=0,b=1,col=2)
cor(alldense$GAP.1..x,alldense$GAP.1..y) # 0.91
lmGap1=lm(alldense$GAP.1..y~alldense$GAP.1..x)
RMSE(alldense$GAP.1..x,alldense$GAP.1..y)/(max(alldense$GAP.1..x)-min(alldense$GAP.1..x)) # 0.15
mean(alldense$GAP.1..x-alldense$GAP.1..y)/(max(alldense$GAP.1..x)-min(alldense$GAP.1..x)) # -0.04


plot(alldense$GAP.2..x,alldense$GAP.2..y,xlab="Measured gap fraction",ylab="Simulated gap fraction",main="Ring 2")
abline(a=0,b=1,col=2)
cor.test(alldense$GAP.2..x,alldense$GAP.2..y) # 0.82
RMSE(alldense$GAP.2..x,alldense$GAP.2..y)/(max(alldense$GAP.2..x)-min(alldense$GAP.2..x)) # 0.17
mean(alldense$GAP.2..x-alldense$GAP.2..y)/(max(alldense$GAP.2..x)-min(alldense$GAP.2..x)) # -0.03

cor.test(alldense$GAP.3..x,alldense$GAP.3..y) # 0.16
plot(alldense$GAP.3..x,alldense$GAP.3..y,xlab="Measured gap fraction",ylab="Simulated gap fraction",main="Ring 3")
RMSE(alldense$GAP.3..x,alldense$GAP.3..y)/(max(alldense$GAP.3..x)-min(alldense$GAP.3..x)) # 0.33
mean(alldense$GAP.3..x-alldense$GAP.3..y)/(max(alldense$GAP.3..x)-min(alldense$GAP.3..x)) # 0.07

cor.test(alldense$GAP.4..x,alldense$GAP.4..y) # 0.42
plot(alldense$GAP.4..x,alldense$GAP.4..y,xlab="Measured gap fraction",ylab="Simulated gap fraction",main="Ring 4")
RMSE(alldense$GAP.4..x,alldense$GAP.4..y)/(max(alldense$GAP.4..x)-min(alldense$GAP.4..x)) # 0.26
mean(alldense$GAP.4..x-alldense$GAP.4..y)/(max(alldense$GAP.4..x)-min(alldense$GAP.4..x)) # 0.18

cor.test(alldense$GAP.5..x,alldense$GAP.5..y) # 0.35
plot(alldense$GAP.5..x,alldense$GAP.5..y,xlab="Measured gap fraction",ylab="Simulated gap fraction",main="Ring 5")
RMSE(alldense$GAP.5..x,alldense$GAP.5..y)/(max(alldense$GAP.5..x)-min(alldense$GAP.5..x)) # 0.42
mean(alldense$GAP.5..x-alldense$GAP.5..y)/(max(alldense$GAP.5..x)-min(alldense$GAP.5..x)) # 0.36

###
mat=cbind(colMeans(alldense[,c("GAP.1..x", "GAP.2..x", "GAP.3..x", "GAP.4..x", "GAP.5..x")]), # matrix
          colMeans(alldense[,c("GAP.1..y", "GAP.2..y", "GAP.3..y", "GAP.4..y", "GAP.5..y")]))

colnames(mat) = c("lai_mesure", "lai_simule")

matplot(mat[,1], mat[,1:2], pch= 1, main="transect E", xlab="GAP fraction mesure", ylab="GAP fraction", col = c(1,2)) # Plot Columns of Matrices
matlines(mat[,1], mat[,1:2], pch =1, lty=1)
legend("topleft", c("GF mesure", "GF simule"),
       pch = "-", col = c(1, 2))


### test autocorrelation spatiale ----------------------------------------------

### Semivariogram --------------------------------------------------------------
library(geoR)

####### data.col= LAI

####LAI mesure -----------------------------------------------------------------
Dgeo<-as.geodata(dense, coords.col = c(17,19), data.col = 4) # data = LAI; coord = posy2016, posx
Dgeo # lists with two obligatory components: coords and data
class(Dgeo) # "geodata"
# Dgeo$coords[1:61,]
# Dgeo$data[0:61]
Dgeo$coords[1:67,]
Dgeo$data[0:67]

breaks = seq(0, 20, l = 21)
breaks
J<-variog(Dgeo, option="bin", breaks=breaks) # Compute Empirical Variograms (binned variogram)
plot(J) # Variogram

# Estimate covariance parameters by fitting a parametric model (correlation function) to a empirical variogram
# "npairs" indicates that the weights are given by the number of pairs in each bin
J2<-variofit(J,c(0.15,10),cov.model="gaussian",weights="npairs")
J3<-variofit(J,c(0.15,10),cov.model="spherical",weights="npairs")
J4<-variofit(J,c(0.15,10),cov.model="exponential",weights="npairs")
plot(J)
title(main="Semivariogram LAI seq(0,20, l=21)")
lines(J2) # gaussian
lines(J3,col="red") # spherical
lines(J4,col="green") # exponential

str(J2)
str(J3)
str(J4)

J2$cov.pars # 0.11 6.9
J2$practicalRange # 12.01
J3$cov.pars # 0.13 13.45
J3$practicalRange # 13.45
J4$cov.pars # 0.15 5.65
J4$practicalRange # 16.93


#data.col=GAP1  ###mesure 
Dgeo<-as.geodata(dense, coords.col = c(17,19), data.col = 5) # data = GAP1
Dgeo$coords[1:67,]
Dgeo$data[0:67]

breaks = seq(0,20, l = 21)
breaks
J<-variog(Dgeo, option="bin", breaks=breaks)
plot(J)

ini.vals=ini.vals <- expand.grid(seq(0.0007,0.0015,by=0.0001),seq(5,11,by=1))
J2<-variofit(J,ini.vals,cov.model="gaussian",weights="npairs")
J3<-variofit(J,ini.vals,cov.model="spherical",weights="npairs")
J4<-variofit(J,ini.vals,cov.model="exponential",weights="npairs")

J2<-variofit(J,c(0.0015,15),cov.model="gaussian",weights="npairs")
J3<-variofit(J,c(0.0015,15),cov.model="spherical",weights="npairs")
J4<-variofit(J,c(0.0015,15),cov.model="exponential",weights="npairs")

plot(J)
lines(J2) # gaussian
lines(J3,col="red") # spherical
lines(J4,col="green") # exponential
title(main="Semivariogram GAP1_mesure, seq(0,20,l=21)", cex.main = 1)
str(J2)

J2$cov.pars
J2$practicalRange
J3$cov.pars
J3$practicalRange
J4$cov.pars
J4$practicalRange

#data.col=GAP1  ###simule 
Dgeo<-as.geodata(densim, coords.col = 1:2, data.col = 5)
Dgeo$coords[1:67,]
Dgeo$data[0:67]

breaks = seq(0,20, l = 21)
breaks
J<-variog(Dgeo, option="bin", breaks=breaks)
plot(J)

ini.vals=ini.vals <- expand.grid(seq(0.002,0.003,by=0.0001),seq(9,15,by=1))
J2<-variofit(J,ini.vals,cov.model="gaussian",weights="npairs")
J3<-variofit(J,ini.vals,cov.model="spherical",weights="npairs")
J4<-variofit(J,ini.vals,cov.model="exponential",weights="npairs")

J2<-variofit(J,c(0.002,10),cov.model="gaussian",weights="npairs")
J3<-variofit(J,c(0.002,10),cov.model="spherical",weights="npairs")
J4<-variofit(J,c(0.002,10),cov.model="exponential",weights="npairs")

plot(J)
lines(J2)
lines(J3,col="red")
lines(J4,col="green")
title(main="Semivariogram GAP1_simule, seq(0,20,l=21)", cex.main = 1)
str(J2)

J2$cov.pars
J2$practicalRange
J3$cov.pars
J3$practicalRange
J4$cov.pars
J4$practicalRange


#data.col=GAP2   ###mesure 
Dgeo<-as.geodata(dense, coords.col = c(17,19), data.col = 6)
Dgeo$coords[1:67,]
Dgeo$data[0:67]

breaks = seq(0,20, l = 21)
breaks
J<-variog(Dgeo, option="bin", breaks=breaks)
plot(J)

J2<-variofit(J,c(0.0002,10),cov.model="spherical",weights="npairs")
plot(J)
lines(J2,col="red")
title(main="Semivariogram GAP2_mesure, seq(0,20,l=21)", cex.main = 1)
str(J2)

J2$cov.pars
J2$practicalRange

#data.col=GAP2  ###simule 
Dgeo<-as.geodata(densim, coords.col = 1:2, data.col = 6)
Dgeo$coords[1:67,]
Dgeo$data[0:67]

breaks = seq(0,20, l = 21)
breaks
J<-variog(Dgeo, option="bin", breaks=breaks)
plot(J)

J2<-variofit(J,c(0.0003,10),cov.model="spherical",weights="npairs")
plot(J)
lines(J2,col="red")
title(main="Semivariogram GAP2_simule, seq(0,20,l=21)", cex.main = 1)
str(J2)

J2$cov.pars
J2$practicalRange

#data.col=GAP3  ###mesure
Dgeo<-as.geodata(dense, coords.col = c(17,19), data.col = 7)
Dgeo$coords[1:67,]
Dgeo$data[0:67]

breaks = seq(0,20, l = 21)
breaks
J<-variog(Dgeo, option="bin", breaks=breaks)
plot(J)

J2<-variofit(J,c(0.000025,5),cov.model="spherical",weights="npairs")
plot(J)
lines(J2,col="red")
title(main="Semivariogram GAP3_mesure, seq(0,20,l=21)", cex.main = 1)
str(J2)

J2$cov.pars
J2$practicalRange

#data.col=GAP3  ###simule
Dgeo<-as.geodata(densim, coords.col = 1:2, data.col = 7)
Dgeo$coords[1:67,]
Dgeo$data[0:67]

breaks = seq(0,20, l = 21)
breaks
J<-variog(Dgeo, option="bin", breaks=breaks)
plot(J)

J2<-variofit(J,c(0.00003,7),cov.model="spherical",weights="npairs")
#J3<-variofit(J,c(0.00003,7),cov.model="exponential",weights="npairs")
plot(J)
lines(J2,col="red")
#lines(J3,col="red")
title(main="Semivariogram GAP3_simule", cex.main = 1)
str(J2)

J2$cov.pars
J2$practicalRange


#data.col=GAP4  ###mesure
Dgeo<-as.geodata(dense, coords.col = c(17,19), data.col = 8)
Dgeo$coords[1:67,]
Dgeo$data[0:67]

breaks = seq(0,20, l = 21)
breaks
J<-variog(Dgeo, option="bin", breaks=breaks)
plot(J)

J2<-variofit(J,c(0.000012,10),cov.model="spherical",weights="npairs")
plot(J)
lines(J2,col="red")
title(main="Semivariogram GAP4_mesure", cex.main = 1)
str(J2)
J2$cov.pars
J2$practicalRange


#data.col=GAP4  ###simule
Dgeo<-as.geodata(densim, coords.col = 1:2, data.col = 8)
Dgeo$coords[1:67,]
Dgeo$data[0:67]

breaks = seq(0,20, l = 21)
breaks
J<-variog(Dgeo, option="bin", breaks=breaks)
plot(J)

J2<-variofit(J,c(0.000012,10),cov.model="spherical",weights="npairs")
plot(J)
lines(J2,col="red")
title(main="Semivariogram GAP4_simule", cex.main = 1)
str(J2)

J2$cov.pars
J2$practicalRange


#data.col=GAP5  ####mesure
Dgeo<-as.geodata(dense, coords.col = c(17,19), data.col = 9)
Dgeo$coords[1:67,]
Dgeo$data[0:67]

breaks = seq(0,20, l = 21)
breaks
J<-variog(Dgeo, option="bin", breaks=breaks)
plot(J)

J2<-variofit(J,c(0.00000015,10),cov.model="spherical",weights="npairs")
plot(J)
lines(J2,col="red")
title(main="Semivariogram GAP5_mesure", cex.main = 1)
str(J2)
J2$cov.pars
J2$practicalRange

#data.col=GAP5  ###simule
Dgeo<-as.geodata(densim, coords.col = 1:2, data.col = 9)
Dgeo$coords[1:67,]
Dgeo$data[0:67]

breaks = seq(0,20, l = 21)
acf(dense$GAP.1.,type="correlation")
breaks
J<-variog(Dgeo, option="bin", breaks=breaks)
plot(J)

J2<-variofit(J,c(0.0000006,7),cov.model="spherical",weights="npairs")
plot(J)
lines(J2,col="red")
title(main="Semivariogram GAP5_simule", cex.main = 1)

J2$cov.pars
J2$practicalRange


################################## Densification Angle 3 -----------------------
setwd(dir="d:/FTH2014/")

library(geoR)
library(ape)

dense3<-read.csv("LAI_P9_Densification_angle3.csv",sep=";")
dense3$posx<-1
str(dense3)
shapiro.test(dense3$LAI)

plot(dense3$LAI~dense3$posy,type="l")
title(main="mesures tous les metres sur le transect E")

plot(dense3$GAP.1.~dense3$posy,ylim=c(0.001,0.22),type="l")
lines(dense3$GAP.2.~dense3$posy,col="red")
lines(dense3$GAP.3.~dense3$posy,col="blue")
lines(dense3$GAP.4.~dense3$posy,col="green")
lines(dense3$GAP.5.~dense3$posy,col="orange")

### Semivariogrammes -----------------------------------------------------------
library(geoR)
#data.col= LAI
Dgeo3<-as.geodata(dense3, coords.col = 15:16, data.col = 4)
Dgeo3$coords[1:61,]
Dgeo3$data[0:61]

breaks = seq(0, 20, l = 21)
breaks
K<-variog(Dgeo3, option="bin", breaks=breaks)
plot(K)

K2<-variofit(K,c(0.8,13),cov.model="spherical",weights="npairs")
plot(K)
title(main="Semivariogram LAI seq(0,20, l=21)")
lines(K2)
str(K2)

#data.col=GAP1
Dgeo3<-as.geodata(dense3, coords.col = 15:16, data.col = 5)
Dgeo3$coords[1:61,]
Dgeo3$data[0:61]

breaks = seq(0,20, l = 21)
breaks
K<-variog(Dgeo3, option="bin", breaks=breaks)
plot(K)

K2<-variofit(K,c(0.0015,15),cov.model="spherical",weights="npairs")
plot(K)
lines(K2)
title(main="Semivariogram GAP1, seq(0,20,l=21)", cex.main = 1)
str(K2)
