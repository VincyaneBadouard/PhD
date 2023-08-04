
#compute energy weighting matrix based on vegetation only shots
temp=readLAS("Z:/users/VincyaneBadouard/ALS2022/P16_2022_dnz_4ha.laz")

range(temp$Z)

mnt <- rast("Z:/users/VincyaneBadouard/ALS2022/mnt_roi36ha_1m.asc")
plot(mnt)

temp_norm <- lidR::normalize_height(temp, mnt) #applati le relief
plot(temp_norm)# sol plat

temp_norm@data[(Z>(-0.2) & Z< 0.5), Classification := 2] # classification

table(temp_norm@data$Classification) # 7 = bruit, 2 = sol, vegetation = 3,4,5
# plot(temp_norm, color = "Classification")

temp <- lidR::unnormalize_height(temp_norm) # désanormalise


GP=temp@data[(Classification==2|Classification==7),] # ground points
GS=temp@data[gpstime %in% GP$gpstime,] # temps gps des tirs dont le echo est au sol
VP=temp@data[!(gpstime %in% GP$gpstime),] # temps gps qui n'ont pas touché le sol
rm(GP)
rm(GS)
VP$int <- VP$Intensity
#compute Intensity
VP$Intensity=10^(VP$Reflectance/10) # as fraction. # On transforme l'intensité en réflectance
# Réflectance apparente en ratio (albedo) = intensité qu'on va utiliser par la suite
# initialment en decibel (et selon une référence connue)
VP_sp <- sample(1:dim(VP)[1], 2000)
plot(int~Intensity, data = VP[VP_sp,])

### compute mean per NoRxRN
# get max return number (variable!)
mx_rn=max(VP[,ReturnNumber]) # nbr max de retours
# VP$ReturnNumber=as.factor(VP$ReturnNumber)
# VP$NumberOfReturns=as.factor(VP$NumberOfReturns)
#mod1=lm(Intensity~NumberOfReturns:ReturnNumber, data=VP)
#mean1=allEffects(mod1)
#mat1=matrix((mean1[[1]]$fit),nrow=mx_rn, ncol=mx_rn)

mat1=matrix(data=NA, nrow=mx_rn, ncol=mx_rn) # matrice vide

#fill in mean intensities
for (i in 1:mx_rn){
  for (j in 1:i)
  {
    mat1[i,j]<-(mean(VP[NumberOfReturns==i & ReturnNumber==j,Intensity],
                     na.rm=T)) # moy des intensités retours par rang d'écho
  }
}
# round()
mat_rel1=mat1/rowSums (mat1, na.rm = T, dims = 1) # poids relatifs à chacun des échos tous tirs confondus     
# il faut une matrice carré !!
# single=list(name=m, matrix=mat_rel1)
# len <- length(mat_list)
# mat_list[[len+1]] <- single



fwrite(mat_rel1, "Z:/users/VincyaneBadouard/ALS2022/WeightingMatrix.txt",
       sep= " ", na="NaN", row.names = F, col.names = F)
