rm(list=ls())

# Test decimation impact on estimators -----------------------------------------
##########################################################################
# WARNINGS AMAPVox 1.10.3 used many errors regarding leaf point filtering!!!
##########################################################################

# remotes::install_github("umr-amap/amapvox")
library(AMAPVox)
library(mgcv)
library(corrplot)

### test decimation effect (99% and 90% decimation)

# setwd("c:/users/vincent/dropbox/PapierTLS/decimation/tree29/")
# file.list=dir(pattern="LeafOnly.vox")
# 
# dir()
setwd("c:/users/vincent/dropbox/PapierTLS/decimation/tree34/")
file.list=dir(pattern=".vox")

#select no wood voxelspaces
file.list<-file.list[-grep(x= file.list, pattern= "LW")]

#select non filtered
#file.list<-file.list[grep(x= file.list, pattern= "LW")]

#for all files cap all attenuation estimators to finite value (=20)
for (f in 1:length(file.list))
{
  vxsp.list[[f]]<-readVoxelSpace(file.list [f])
  strip.vox=strsplit(file.list[f], split="[.]")[[1]][1]
  names(vxsp.list)[f]<-paste0("vx_",strsplit(strip.vox, split="_")[[1]][3])
  #replace 0 transmittance with very low value to avoid infinite attenuation values
  vxsp.list[[f]]$transmittance=pmax(vxsp.list[[f]]$transmittance,exp(-20)) # max attenuation=20
  vxsp.list[[f]]@data$att_tr= -log(vxsp.list[[f]]@data$transmittance)
  vxsp.list[[f]]@data$attenuation_FPL_biasedMLE=pmin(vxsp.list[[f]]@data$attenuation_FPL_biasedMLE,20)
  vxsp.list[[f]]@data$attenuation_FPL_unbiasedMLE=pmin(vxsp.list[[f]]@data$attenuation_FPL_unbiasedMLE,20)
  vxsp.list[[f]]@data$attenuation_PPL_MLE=pmin(vxsp.list[[f]]@data$attenuation_PPL_MLE,20)
}

mean(vxsp.list[["vx_1pct"]]@data$transmittance, na.rm=T)
mean(vxsp.list[["vx_10pct"]]@data$transmittance)
mean(vxsp.list[["vx_full"]]@data$transmittance)

plot(att_tr~attenuation_PPL_MLE, data=vxsp.list[["vx_1pct"]]@data, 
     main="1pct", ylab="att_tr", xlab="attenuation_PPL_MLE")
abline(b=1,a=0, col="red", lwd=2)

#######!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
##there seems to be pbm with att_PPL_MLE which is vastly underestimated in case 
##few shots occur and are intercepted; check solver code !!!
#######!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

head(vxsp.list[["vx_1pct"]]@data[(att_tr>15 & attenuation_PPL_MLE<1),])


plot(att_tr~attenuation_PPL_MLE, data=vxsp.list[["vx_10pct"]]@data, 
     main="10pct",  ylab="att_tr", xlab="att_PPL")
abline(b=1,a=0, col="red", lwd=2)

plot(att_tr~attenuation_PPL_MLE, data=vxsp.list[["vx_full"]]@data, 
     main="full",  ylab="Att_tr", xlab="att_PPL")
abline(b=1,a=0, col="red", lwd=2)

plot(att_tr~attenuation_FPL_unbiasedMLE, data=vxsp.list[["vx_full"]]@data, 
     main="full",  ylab="Att_tr", xlab="att_FPL_unbiased")
abline(b=1,a=0, col="red", lwd=2)

# Effect on transmittance (1pct)
plot(vxsp.list[["vx_1pct"]]@data$transmittance~vxsp.list[["vx_full"]]@data$transmittance, main="Transmittance",
     xlab="full", ylab="1pct")
grid()
abline(b=1,a=0, col="red", lwd=2)
## by subsampling to 1pct we miss many non empty voxels (no interception detected)

# Effect on transmittance (10pct)
plot(vxsp.list[["vx_10pct"]]@data$transmittance~vxsp.list[["vx_full"]]@data$transmittance, main="Transmittance",
     xlab="full", ylab="10pct")
grid()
abline(b=1,a=0, col="red", lwd=2)


# Effect on attenuation (1pct data)
plot(vxsp.list[["vx_1pct"]]@data$attenuation_PPL_MLE~vxsp.list[["vx_full"]]@data$attenuation_PPL_MLE)
grid()
abline(b=1,a=0, col="red", lwd=2)

# Effect on attenuation (10pct)
plot(vxsp.list[["vx_10pct"]]@data$attenuation_PPL_MLE~vxsp.list[["vx_full"]]@data$attenuation_PPL_MLE)
grid()
abline(b=1,a=0, col="red", lwd=2)

# Select only non empty voxels (as detected at full density)
keep<-which(vxsp.list[["vx_full"]]@data$nbEchos>0)
mean(vxsp.list[["vx_1pct"]]@data$nbSampling[keep])
median(vxsp.list[["vx_1pct"]]@data$nbSampling[keep])
mean(vxsp.list[["vx_1pct"]]@data$bsEntering[keep])
mean(vxsp.list[["vx_10pct"]]@data$nbSampling[keep])
mean(vxsp.list[["vx_10pct"]]@data$bsEntering[keep])
mean(vxsp.list[["vx_full"]]@data$nbSampling[keep])
sd(vxsp.list[["vx_full"]]@data$nbSampling[keep])
mean(vxsp.list[["vx_full"]]@data$bsEntering[keep])

smoothScatter(vxsp.list[["vx_full"]]@data$transmittance~vxsp.list[["vx_1pct"]]@data$transmittance, main="transmittance")
smoothScatter(vxsp.list[["vx_full"]]@data$transmittance[keep]~vxsp.list[["vx_1pct"]]@data$transmittance[keep], main="transmittance")

smoothScatter(vxsp.list[["vx_full"]]@data$transmittance[keep]~vxsp.list[["vx_10pct"]]@data$transmittance[keep], main="transmittance")

# Evaluate effect of nbSampling on transmittance bias

full=vxsp.list[["vx_full"]]@data[,.(i,j,k,nbEchos,nbSampling,transmittance, att_tr,
                                    attenuation_PPL_MLE, attenuation_FPL_biasedMLE,
                                    attenuation_FPL_unbiasedMLE, bsEntering)]
names(full)=c("i", "j", "k", "nbEchos_full","nbSampling_full", "transmittance_full", "att_tr_full", 
              "attenuation_PPL_MLE_full","attenuation_FPL_biasedMLE_full", 
              "attenuation_FPL_unbiasedMLE_full", "bsEntering_full")

pct10=vxsp.list[["vx_10pct"]]@data[,.(i,j,k,nbEchos,nbSampling,transmittance, att_tr,
                                    attenuation_PPL_MLE, attenuation_FPL_biasedMLE,
                                    attenuation_FPL_unbiasedMLE, bsEntering)]
names(pct10)=c("i", "j", "k", "nbEchos_10pct","nbSampling_10pct", "transmittance_10pct", "att_tr_10pct",
              "attenuation_PPL_MLE_10pct","attenuation_FPL_biasedMLE_10pct",
              "attenuation_FPL_unbiasedMLE_10pct", "bsEntering_10pct")

pct1=vxsp.list[["vx_1pct"]]@data[,.(i,j,k,nbEchos,nbSampling,transmittance, att_tr,
                                      attenuation_PPL_MLE, attenuation_FPL_biasedMLE,
                                      attenuation_FPL_unbiasedMLE, bsEntering)]
names(pct1)=c("i", "j", "k", "nbEchos_1pct","nbSampling_1pct", "transmittance_1pct", "att_tr_1pct",
               "attenuation_PPL_MLE_1pct","attenuation_FPL_biasedMLE_1pct",
               "attenuation_FPL_unbiasedMLE_1pct", "bsEntering_1pct")

dat=merge(full,pct10,by=c("i","j","k"))
dat=merge(dat,pct1, by=c("i","j","k"))

sel=dat[keep,]
mean(sel$transmittance_full)
mean(sel$transmittance_full[sel$nbEchos_1pct>0])
mean(sel$transmittance_10pct)
mean(sel$transmittance_10pct[sel$nbEchos_10pct>0])
mean(sel$transmittance_10pct[sel$nbEchos_1pct>0])
length(sel$transmittance_1pct[sel$nbEchos_1pct>0])
mean(sel$transmittance_1pct, na.rm=T)
mean(sel$transmittance_1pct[sel$nbEchos_1pct>0])
length(sel$transmittance_10pct[sel$nbEchos_10pct>0])
length(sel$transmittance_full[sel$nbEchos_full>0])
sel[is.na(transmittance_1pct),]

mean(sel$att_tr_full)
mean(sel$att_tr_10pct)
mean(sel$att_tr_1pct, na.rm=T)

sum(sel$nbSampling_1pct==0)

mean(sel$att_tr_full[sel$nbEchos_1pct>0])
mean(sel$att_tr_10pct[sel$nbEchos_10pct>0])
mean(sel$att_tr_10pct[sel$nbEchos_1pct>0])

mean(sel$att_tr_1pct[sel$nbEchos_1pct>0])


##############################################################################
######## 1st consider 99% decimated and full data set (delta=decimated-full)----
##############################################################################
dat$delta_tr_1pct=dat$transmittance_1pct-dat$transmittance_full
dat$delta_tr_10pct=dat$transmittance_10pct-dat$transmittance_full
#delta=decimated-full

mean(dat$delta_tr_1pct[keep], na.rm=T)
mean(dat$delta_tr_1pct[keep], na.rm=T)/mean(dat$transmittance_full[keep]) #mean transmittance is 10% larger in decimated data
hist(dat$delta_tr_1pct[keep], breaks=100)

gam1=gam(delta_tr_1pct~s(nbSampling_1pct)+s(transmittance_full), data=dat[keep,])
gam0=gam(delta_tr_1pct~s(nbSampling_1pct), data=dat[keep,])
summary(gam1)
plot(gam1)
summary(gam0)
plot(gam0) #low sampling voxels tend to be overestimated most

plot(gam0, xlab="pulse per voxel", ylab="effect on transmittance", main="Tree34, 99% decimation")
text(x=40, y=0.3, paste0("deviance explained=", round(100*summary(gam0)$dev.expl, digits=1),"%"))
grid()

#examine piece-wise linear regression
library(segmented)
sel=dat[keep,]
fit=lm(delta_tr_1pct~nbSampling_1pct, data=sel)
segmented.fit <- segmented(fit, seg.Z = ~nbSampling_1pct)
summary(segmented.fit)

###1 pct
plot(sel$nbSampling_1pct[sel$nbEchos_1pct>0], sel$delta_tr_1pct[sel$nbEchos_1pct>0], pch=16, cex=0.5, col='steelblue',
     main="Tree34, 99% decimation",xlab="nbSampling after decimation",
     ylab="increase in estimated transmittance")
fit1=lm(delta_tr_1pct~nbSampling_1pct, data=sel[nbEchos_1pct>0])
fit2=lm(delta_tr_1pct~nbSampling_1pct, data=sel[nbEchos_1pct==0])
#text(x=40, y=0.8, paste0("adj. r2=", round(100*summary(segmented.fit)$ adj.r.squared, digits=1),"%"))
grid()
points(x=sel$nbSampling_1pct[sel$nbEchos_1pct==0], sel$delta_tr_1pct[sel$nbEchos_1pct==0], pch=16, cex=0.5, col="red")
abline(coef(fit1), col="steelblue")
#abline(coef(fit2), col="red")
plot(segmented.fit, add=T, col="black", lwd=1, lty=2)

##10pct
fit=lm(delta_tr_10pct~nbSampling_10pct, data=sel)
segmented.fit <- segmented(fit, seg.Z = ~nbSampling_10pct)
summary(segmented.fit)
plot(sel$nbSampling_10pct[sel$nbEchos_10pct>0], sel$delta_tr_10pct[sel$nbEchos_10pct>0], pch=16, cex=0.5, col='steelblue',
     main="Tree34, 90% decimation",xlab="nbSampling after decimation",
     ylab="increase in estimated transmittance")
fit1=lm(delta_tr_10pct~nbSampling_10pct, data=sel[nbEchos_10pct>0])
fit2=lm(delta_tr_10pct~nbSampling_10pct, data=sel[nbEchos_10pct==0])
#text(x=40, y=0.8, paste0("adj. r2=", round(100*summary(segmented.fit)$ adj.r.squared, digits=1),"%"))
grid()
points(x=sel$nbSampling_10pct[sel$nbEchos_10pct==0], sel$delta_tr_10pct[sel$nbEchos_10pct==0], pch=16, cex=0.5, col="red")
abline(coef(fit1), col="steelblue")
#abline(coef(fit2), col="red")
plot(segmented.fit, add=T, col="black", lwd=1, lty=2)

#examine piece-wise linear regression after excluding empty voxels in decimated data set
library(segmented)
sel=dat[keep,][transmittance_1pct<1,]
fit=lm(delta_tr_1pct~nbSampling_1pct, data=sel)
segmented.fit <- segmented(fit, seg.Z = ~nbSampling_1pct)
summary(segmented.fit)

# transmittance is now underestimated following decimation by ~9%
mean(sel$transmittance_1pct)/ mean(sel$transmittance_full)

plot(sel$nbSampling_1pct, sel$delta_tr_1pct, pch=16, cex=0.5, col='steelblue',
     main="Tree34, 99% decimation empty voxels (after decimation) ommitted",xlab="nbSampling after decimation",
     ylab="increase in estimated transmittance")
text(x=40, y=0.8, paste0("adj. r2=", round(100*summary(segmented.fit)$ adj.r.squared, digits=1),"%"))
grid()
#add segmented regression model
plot(segmented.fit, add=T, col="red", lwd=2)

plot(gam0, xlab="pulse per voxel", ylab="effect on transmittance", main="Tree34, 90% decimation")
text(x=40, y=0.3, paste0("deviance explained=", round(100*summary(gam0)$dev.expl, digits=1),"%"))
grid()

# mean(dat$delta_tr, na.rm=T)
# cor(dat$transmittance.y,dat$transmittance.x,use="complete.obs")
mean(dat$delta_tr_1pct[keep], na.rm=T)
cor(dat$transmittance_1pct[keep],dat$transmittance_full[keep],use="complete.obs")

#############################################
#same analysis on Atr (decimated 1pct - full)
dat$delta_Atr_1pct=dat$att_tr_1pct-dat$att_tr_full
dat$delta_Atr_10pct=dat$att_tr_10pct-dat$att_tr_full
sel=dat[keep,]
###1 pct
plot(jitter(sel$nbSampling_1pct[sel$nbEchos_1pct>0],1), sel$delta_Atr_1pct[sel$nbEchos_1pct>0], pch=16, cex=0.5, col='steelblue',
     main="Tree34, 99% decimation",xlab="nbSampling after decimation",
     ylab="increase in estimated attenuation")
fit1=lm(delta_Atr_1pct~nbSampling_1pct, data=sel[nbEchos_1pct>0])
fit2=lm(delta_Atr_1pct~nbSampling_1pct, data=sel[nbEchos_1pct==0])
#text(x=40, y=0.8, paste0("adj. r2=", round(100*summary(segmented.fit)$ adj.r.squared, digits=1),"%"))
grid()
points(x=jitter(sel$nbSampling_1pct[sel$nbEchos_1pct==0],1), sel$delta_Atr_1pct[sel$nbEchos_1pct==0], pch=16, cex=0.5, col="red")
abline(coef(fit1), col="steelblue")
#abline(coef(fit2), col="red")
plot(segmented.fit, add=T, col="black", lwd=1, lty=2)

##10pct
fit=lm(delta_Atr_10pct~nbSampling_10pct, data=sel)
segmented.fit <- segmented(fit, seg.Z = ~nbSampling_10pct)
summary(segmented.fit)
plot(jitter(sel$nbSampling_10pct[sel$nbEchos_10pct>0],1), sel$delta_Atr_10pct[sel$nbEchos_10pct>0], pch=16, cex=0.5, col='steelblue',
     main="Tree34, 90% decimation",xlab="nbSampling after decimation",
     ylab="increase in estimated attenuation")
fit1=lm(delta_Atr_10pct~nbSampling_10pct, data=sel[nbEchos_10pct>0])
fit2=lm(delta_Atr_10pct~nbSampling_10pct, data=sel[nbEchos_10pct==0])
#text(x=40, y=0.8, paste0("adj. r2=", round(100*summary(segmented.fit)$ adj.r.squared, digits=1),"%"))
grid()
points(x=jitter(sel$nbSampling_10pct[sel$nbEchos_10pct==0],1), sel$delta_Atr_10pct[sel$nbEchos_10pct==0], pch=16, cex=0.5, col="red")
abline(coef(fit1), col="steelblue")
#abline(coef(fit2), col="red")
plot(segmented.fit, add=T, col="black", lwd=1, lty=2)

#dat<-dat[is.finite(delta_Atr),]
# mean(dat$delta_Atr_1pct, na.rm=T)
# cor(dat$att_tr_1pct,dat$att_tr_full,use="complete.obs")
mean(dat$delta_Atr_1pct[keep], na.rm=T)
mean(dat$delta_Atr_1pct[keep], na.rm=T)/mean(dat$att_tr_full[keep])
cor(dat$att_tr_1pct[keep],dat$att_tr_full[keep],use="complete.obs")

gam0_Atr_1pct=gam(delta_Atr_1pct~s(nbSampling_1pct), data=dat[keep,])
gam1_Atr_1pct=gam(delta_Atr_1pct~s(nbSampling_1pct)+s(transmittance_full), data=dat[keep,])
summary(gam1_Atr_1pct)
summary(gam0_Atr_1pct)

gam0_absAtr_1pct=gam(abs(delta_Atr_1pct)~s(nbSampling_1pct), data=dat[keep,])
summary(gam0_absAtr_1pct)
plot(gam0_absAtr_1pct)
grid()
#same analysis on Atr (decimated 10pct - full )
dat$delta_Atr_10pct=dat$att_tr_10pct-dat$att_tr_full
#dat<-dat[is.finite(delta_Atr),]
# mean(dat$delta_Atr_10pct, na.rm=T)
# cor(dat$att_tr_10pct,dat$att_tr_full,use="complete.obs")
mean(dat$delta_Atr_10pct[keep], na.rm=T)
mean(dat$delta_Atr_10pct[keep], na.rm=T)/mean(dat$att_tr_full[keep])
cor(dat$att_tr_10pct[keep],dat$att_tr_full[keep],use="complete.obs")

gam0_Atr_10pct=gam(delta_Atr_10pct~s(nbSampling_10pct), data=dat[keep,])
gam1_Atr_10pct=gam(delta_Atr_10pct~s(nbSampling_10pct)+s(transmittance_full), data=dat[keep,])
summary(gam1_Atr_10pct)
summary(gam0_Atr_10pct)

# gam1_BSE_Atr=gam(delta_Atr~s(bsEntering.y), data=dat[keep,])
# gam1b_Atr=gam(delta_Atr~s(nbSampling.y), data=dat)
# summary(gam1_Atr)
# summary(gam1_BSE)
plot(gam0_Atr_1pct, xlab="pulse per voxel", ylab="effect on Atr", main="Tree34, 99% decimation")
text(x=40, y=0.3, paste0("deviance explained=", round(100*summary(gam0_Atr_1pct)$dev.expl, digits=1),"%"))
grid()

one_pct=na.omit(dat$att_tr_1pct[keep]) # attenuation from decimated data (no NAs)
hist(one_pct)
gam_pred=predict(Atr_1pct) #predicted effect of sampling intensity
hist(gam_pred)
plot(gam0_Atr_1pct)
plot(gam0_Atr_10pct)

mean(na.omit(dat$delta_Atr_1pct[keep]))


#####################################
# Check bias affecting attenuation PPL -----------------------------------------
#####################################

dat$delta_atPPL_1pct=dat$attenuation_PPL_MLE_1pct-dat$attenuation_PPL_MLE_full
dat$delta_atPPL_10pct=dat$attenuation_PPL_MLE_10pct-dat$attenuation_PPL_MLE_full
sel=dat[keep,]
mean(sel$attenuation_PPL_MLE_full)
mean(sel$attenuation_PPL_MLE_10pct)
mean(sel$attenuation_PPL_MLE_1pct, na.rm=T)

mean(sel$attenuation_PPL_MLE_full[sel$nbEchos_1pct>0])
mean(sel$attenuation_PPL_MLE_10pct[sel$nbEchos_10pct>0])
mean(sel$attenuation_PPL_MLE_10pct[sel$nbEchos_1pct>0])
mean(sel$attenuation_PPL_MLE_1pct, na.rm=T)
mean(sel$attenuation_PPL_MLE_1pct[sel$nbEchos_1pct>0])

cor(dat$attenuation_PPL_MLE_1pct[keep],dat$attenuation_PPL_MLE_full[keep],use="complete.obs")
# mean(dat$delta_atPPL_1pct, na.rm=T)
# cor(dat$attenuation_PPL_MLE_1pct,dat$attenuation_PPL_MLE_full,use="complete.obs")

# gam2=gam(delta_atPPL_1pct~s(nbSampling_1pct), data=dat[keep])
# summary(gam2)
# 
# plot(gam2, xlab="pulse per voxel", ylab="effect on att_PPL", 
#      main="Tree34, 99% decimation")
# text(x=40, y=0.5, paste0("deviance explained=", round(100*summary(gam2)$dev.expl, digits=1),"%"))
# grid()
# 
# mean(na.omit(dat$delta_AtPPL_1pct[keep])-predict(gam2))

#same analysis on Atr (decimated 1pct - full )
###1 pct
plot(jitter(sel$nbSampling_1pct[sel$nbEchos_1pct>0],1), sel$delta_atPPL_1pct[sel$nbEchos_1pct>0], pch=16, cex=0.5, col='steelblue',
     main="Tree34, 99% decimation",xlab="nbSampling after decimation",
     ylab="increase in estimated attenuation")
fit1=lm(delta_atPPL_1pct~nbSampling_1pct, data=sel[nbEchos_1pct>0])
fit2=lm(delta_atPPL_1pct~nbSampling_1pct, data=sel[nbEchos_1pct==0])
#text(x=40, y=0.8, paste0("adj. r2=", round(100*summary(segmented.fit)$ adj.r.squared, digits=1),"%"))
grid()
points(x=jitter(sel$nbSampling_1pct[sel$nbEchos_1pct==0],1), sel$delta_atPPL_1pct[sel$nbEchos_1pct==0], pch=16, cex=0.5, col="red")
abline(coef(fit1), col="steelblue")
#abline(coef(fit2), col="red")
plot(segmented.fit, add=T, col="black", lwd=1, lty=2)

##10pct
fit=lm(delta_atPPL_10pct~nbSampling_10pct, data=sel)
segmented.fit <- segmented(fit, seg.Z = ~nbSampling_10pct)
summary(segmented.fit)
plot(jitter(sel$nbSampling_10pct[sel$nbEchos_10pct>0],1), sel$delta_atPPL_10pct[sel$nbEchos_10pct>0], pch=16, cex=0.5, col='steelblue',
     main="Tree34, 90% decimation",xlab="nbSampling after decimation",
     ylab="increase in estimated attenuation")
fit1=lm(delta_atPPL_10pct~nbSampling_10pct, data=sel[nbEchos_10pct>0])
fit2=lm(delta_atPPL_10pct~nbSampling_10pct, data=sel[nbEchos_10pct==0])
#text(x=40, y=0.8, paste0("adj. r2=", round(100*summary(segmented.fit)$ adj.r.squared, digits=1),"%"))
grid()
points(x=jitter(sel$nbSampling_10pct[sel$nbEchos_10pct==0],1), sel$delta_atPPL_10pct[sel$nbEchos_10pct==0], pch=16, cex=0.5, col="red")
abline(coef(fit1), col="steelblue")
#abline(coef(fit2), col="red")
plot(segmented.fit, add=T, col="black", lwd=1, lty=2)


###################################################
# Check bias affecting attenuation FPL (uncorrected) ---------------------------
###################################################
dat$delta_atFPL_1pct=dat$attenuation_FPL_biasedMLE_1pct-dat$attenuation_FPL_biasedMLE_full
dat$delta_atFPL_10pct=dat$attenuation_FPL_biasedMLE_10pct-dat$attenuation_FPL_biasedMLE_full
sel=dat[keep,]

mean(sel$attenuation_FPL_biasedMLE_full)
mean(sel$attenuation_FPL_biasedMLE_10pct)
mean(sel$attenuation_FPL_biasedMLE_1pct, na.rm=T)
mean(sel$attenuation_FPL_biasedMLE_full[sel$nbEchos_1pct>0])

mean(sel$attenuation_FPL_biasedMLE_10pct[sel$nbEchos_10pct>0])
mean(sel$attenuation_FPL_biasedMLE_10pct[sel$nbEchos_1pct>0])

mean(sel$attenuation_FPL_biasedMLE_1pct[sel$nbEchos_1pct>0])

###1 pct
plot(jitter(sel$nbSampling_1pct[sel$nbEchos_1pct>0],1), sel$delta_atFPL_1pct[sel$nbEchos_1pct>0], pch=16, cex=0.5, col='steelblue',
     main="Tree34, 99% decimation",xlab="nbSampling after decimation",
     ylab="increase in estimated attenuation")
fit1=lm(delta_atFPL_1pct~nbSampling_1pct, data=sel[nbEchos_1pct>0])
fit2=lm(delta_atFPL_1pct~nbSampling_1pct, data=sel[nbEchos_1pct==0])
#text(x=40, y=0.8, paste0("adj. r2=", round(100*summary(segmented.fit)$ adj.r.squared, digits=1),"%"))
grid()
points(x=jitter(sel$nbSampling_1pct[sel$nbEchos_1pct==0],1), sel$delta_atFPL_1pct[sel$nbEchos_1pct==0], pch=16, cex=0.5, col="red")
abline(coef(fit1), col="steelblue")
#abline(coef(fit2), col="red")
plot(segmented.fit, add=T, col="black", lwd=1, lty=2)

##10pct
fit=lm(delta_atFPL_10pct~nbSampling_10pct, data=sel)
segmented.fit <- segmented(fit, seg.Z = ~nbSampling_10pct)
summary(segmented.fit)
plot(jitter(sel$nbSampling_10pct[sel$nbEchos_10pct>0],1), sel$delta_atFPL_10pct[sel$nbEchos_10pct>0], pch=16, cex=0.5, col='steelblue',
     main="Tree34, 90% decimation",xlab="nbSampling after decimation",
     ylab="increase in estimated attenuation")
fit1=lm(delta_atFPL_10pct~nbSampling_10pct, data=sel[nbEchos_10pct>0])
fit2=lm(delta_atFPL_10pct~nbSampling_10pct, data=sel[nbEchos_10pct==0])
#text(x=40, y=0.8, paste0("adj. r2=", round(100*summary(segmented.fit)$ adj.r.squared, digits=1),"%"))
grid()
points(x=jitter(sel$nbSampling_10pct[sel$nbEchos_10pct==0],1), sel$delta_atFPL_10pct[sel$nbEchos_10pct==0], pch=16, cex=0.5, col="red")
abline(coef(fit1), col="steelblue")
#abline(coef(fit2), col="red")
plot(segmented.fit, add=T, col="black", lwd=1, lty=2)

cor(cbind(sel$delta_atPPL_1pct, sel$delta_Atr_1pct, sel$delta_atFPL_1pct),  use="complete.obs")


# gam3=gam(delta_atFPL_1pct~s(nbSampling_1pct), data=dat[keep])
# gam3u=gam(delta_atFPLu_1pct~s(nbSampling_1pct), data=dat[keep])
# summary(gam3)
# summary(gam3u)
# plot(gam3, xlab="pulse per voxel", ylab="effect on raw att_FPL", 
#      main="Tree34, 99% decimation", ylim=c(-1.5,1))
# text(x=40, y=0.1, paste0("deviance explained=", round(100*summary(gam3)$dev.expl, digits=1),"%"))
# grid()
# 
# plot(gam3u,rug=T, xlab="pulse per voxel", ylab="effect on de-biased att_FPL", 
#      main="Tree34, 99% decimation")
# text(x=40, y=0.25, paste0("deviance explained=", round(100*summary(gam3u)$dev.expl, digits=1),"%"))
# grid()

###################################################
# Check bias affecting attenuation FPL (corrected) -----------------------------
###################################################
dat$delta_atFPLu_1pct=dat$attenuation_FPL_unbiasedMLE_1pct-dat$attenuation_FPL_unbiasedMLE_full
dat$delta_atFPLu_10pct=dat$attenuation_FPL_unbiasedMLE_10pct-dat$attenuation_FPL_unbiasedMLE_full
sel=dat[keep,]

mean(sel$attenuation_FPL_unbiasedMLE_full)
mean(sel$attenuation_FPL_unbiasedMLE_10pct)
mean(sel$attenuation_FPL_unbiasedMLE_1pct, na.rm=T)

mean(sel$attenuation_FPL_unbiasedMLE_full[sel$nbEchos_1pct>0])

mean(sel$attenuation_FPL_unbiasedMLE_10pct[sel$nbEchos_10pct>0])
mean(sel$attenuation_FPL_unbiasedMLE_10pct[sel$nbEchos_1pct>0])

mean(sel$attenuation_FPL_unbiasedMLE_1pct[sel$nbEchos_1pct>0])

###1 pct
plot(jitter(sel$nbSampling_1pct[sel$nbEchos_1pct>0],1), sel$delta_atFPLu_1pct[sel$nbEchos_1pct>0], pch=16, cex=0.5, col='steelblue',
     main="Tree34, 99% decimation",xlab="nbSampling after decimation",
     ylab="increase in estimated attenuation")
fit1=lm(delta_atFPLu_1pct~nbSampling_1pct, data=sel[nbEchos_1pct>0])
fit2=lm(delta_atFPLu_1pct~nbSampling_1pct, data=sel[nbEchos_1pct==0])
#text(x=40, y=0.8, paste0("adj. r2=", round(100*summary(segmented.fit)$ adj.r.squared, digits=1),"%"))
grid()
points(x=jitter(sel$nbSampling_1pct[sel$nbEchos_1pct==0],1), sel$delta_atFPLu_1pct[sel$nbEchos_1pct==0], pch=16, cex=0.5, col="red")
abline(coef(fit1), col="steelblue")
#abline(coef(fit2), col="red")
plot(segmented.fit, add=T, col="black", lwd=1, lty=2)

##10pct
fit=lm(delta_atFPLu_10pct~nbSampling_10pct, data=sel)
segmented.fit <- segmented(fit, seg.Z = ~nbSampling_10pct)
summary(segmented.fit)
plot(jitter(sel$nbSampling_10pct[sel$nbEchos_10pct>0],1), sel$delta_atFPLu_10pct[sel$nbEchos_10pct>0], pch=16, cex=0.5, col='steelblue',
     main="Tree34, 90% decimation",xlab="nbSampling after decimation",
     ylab="increase in estimated attenuation")
fit1=lm(delta_atFPLu_10pct~nbSampling_10pct, data=sel[nbEchos_10pct>0])
fit2=lm(delta_atFPLu_10pct~nbSampling_10pct, data=sel[nbEchos_10pct==0])
#text(x=40, y=0.8, paste0("adj. r2=", round(100*summary(segmented.fit)$ adj.r.squared, digits=1),"%"))
grid()
points(x=jitter(sel$nbSampling_10pct[sel$nbEchos_10pct==0],1), sel$delta_atFPLu_10pct[sel$nbEchos_10pct==0], pch=16, cex=0.5, col="red")
abline(coef(fit1), col="steelblue")
#abline(coef(fit2), col="red")
plot(segmented.fit, add=T, col="black", lwd=1, lty=2)

cor(cbind(sel$delta_atPPL_1pct, sel$delta_Atr_1pct, sel$delta_atFPL_1pct, sel$delta_atFPLu_1pct),  use="complete.obs")

cor(cbind(sel$delta_atPPL_10pct, sel$delta_Atr_10pct, sel$delta_atFPL_10pct, sel$delta_atFPLu_10pct),  use="complete.obs")


