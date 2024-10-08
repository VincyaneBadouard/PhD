vxsp <- readVoxelSpace(
   "//amap-data.cirad.fr/work/users/VincyaneBadouard/Anisotropy_test/P16_2023_C19C20_UAV_Vox_intensity1m.vox"
)

HLE <- function (
    vxsp, # espace voxel
    variable.name1 = "bsIntercepted", # nbr de pulses interceptés par l'empreinte du faisceau = surface occluse dans le faisceau
    variable.name2 = "weightedFreepathLength", # longueur des trajets optiques pondérée par l'empreinte du faisceau
    variable.min = -Inf, variable.max = Inf, # bornes pas forcément utiles
    variable.fallback, #  valeur d'atténuation si l'algorithme n'en a pas trouvée
    radius = 2, # rayon de voisinage en mètre
    pulse.min = 25 # nbr de pulses satisfaisant pour faire une estimation locale (threshold)
    
){
  # Checks
  stopifnot(is.VoxelSpace(vxsp))
  stopifnot(variable.name1 %in% colnames(vxsp@data),
            length(variable.name1) == 1)
  stopifnot(variable.name2 %in% colnames(vxsp@data),
            length(variable.name2) == 1)
  stopifnot(is.numeric(variable.min), length(variable.min) == 
              1, is.numeric(variable.max), length(variable.max) == 
              1, !is.na(variable.min))
  
  if (!missing(variable.fallback)) 
    stopifnot(is.numeric(variable.fallback), length(variable.fallback) == 1,
              !is.na(variable.fallback))
  
  if (missing(radius)) 
    radius <- max(getVoxelSize(vxsp)) # if no radius take the max voxelsize
  stopifnot(is.numeric(radius), length(radius) == 1, !is.na(radius))
  
  stopifnot(is.numeric(pulse.min), pulse.min >= 0, !is.na(pulse.min))
  
  # fct
  vxsp@data <- setDT(vxsp@data)
  vx <- vxsp@data
  
  # Put an NA if the pulse number is too low
  vx.na <- vx[nbSampling < pulse.min] # subset of voxels below sampling threshold level
  vx.pool <- vx # all voxels can contribute to local estimate!
  radius <- max(getVoxelSize(vxsp), radius) # radius would be at less the voxel size 
  
  # Get neighbors in immediate neighborhood (27 nearest voxels including target)
  # This k value could be set as a parameter of the HLE function (for ex. could be 125, i.e.a full step larger)
  
  neighbors <- RANN::nn2(data = getPosition(vxsp, vx.pool), # Nearest Neighbor Search
                         query = getPosition(vxsp, vx.na),
                         k = 27, # k = 27 : 3 voisins
                         searchtype = "radius", radius = radius)
  
  neighbors <- neighbors$nn.idx # keep list of indices only
  
  # Recompute attenuation from local neighborhood : sum(bsIntercepted)/sum(weightedFreepathLength)
  fill.value <- apply(
    neighbors, 1,
    function(nghb) vx.pool[as.vector(nghb), 
                           sum(get(variable.name1), na.rm = T)/sum(get(variable.name2), na.rm=T)]
  )
  
  fill.value[which(is.nan(fill.value))] <- NA # NaN to NA. why would this ever happen?
  
  # When no value was found, take the fallback value
  if (!missing(variable.fallback)) 
    fill.value[which(is.na(fill.value))] <- variable.fallback
  na.count <- length(which(is.na(fill.value))) # how many NA are left?
  
  if (na.count > 0) # if any NAs remain
    warning(paste(na.count, " NA left", "\nSomething went wrong somewhere..."))
  
  # Apply the thresholds
  fill.value[which(fill.value > variable.max)] <- variable.max
  fill.value[which(fill.value < variable.min)] <- variable.min
  
  # HLE = bsIntercepted/weightedFreepathLength
  vxsp@data[, HLE := .(get(variable.name1)/get(variable.name2))] # default attenuation
  vxsp@data[neighbors[,1], HLE := fill.value] # modified attenuation
  na.count <- length(which(is.nan(vxsp@data$HLE)))
  
  if (na.count > 0) # if any NAs remain
    warning(paste(na.count, " NA left", "\nSomething went wrong somewhere..."))
  
  return(vxsp)
}

OnlyVegetation <- function(vxsp){
  vxsp@data <- setDT(vxsp@data)
  bcnp <- belowCanopy(vxsp) # enlève tous les voxels au dessus de la canopée
  # summary(bcnp)
  
  dz = getVoxelSize(vxsp)["z"] # Gets the elemental size of a voxel (dx, dy, dz) in meter.
  
  # Considérer tout ce qui est à + de 0.5 m du z=0 comme de la végétation
  veg <- vxsp@data[bcnp, list(i,j,k,ground_distance), # combine data.tables on i,j,k columns
                   on = list(i, j, k)][ground_distance >= (0.5 * dz),
                                       list(i,j,k)] # keep only i,j,k columns
  fullcanopy <- vxsp
  fullcanopy@data <- vxsp@data[veg,,on = list(i,j,k)] # only veg part
  
  return(fullcanopy)
  
}

fullcanopy <- OnlyVegetation(vxsp)


pulsemin_fbv <- function(fullcanopy, thres = 20){
  # pulse.min <- median(fullcanopy@data$nbSampling) # Low : 8 ; High : 44
  # pulse.min
  pulse.min = thres # seq(4,28, by=8) =  4 12 20 28 44
  fbv <- median(fullcanopy@data[nbSampling>=pulse.min, na.omit(attenuation_FPL_biasedMLE)]) # 0.0257098
  # fbv
  return(list(pulse.min = pulse.min, fbv=fbv))
}

pulsemin_fbv <- pulsemin_fbv(fullcanopy)
pulsemin <- pulsemin_fbv$pulse.min
fbv <-  pulsemin_fbv$fbv


gc()
fullcanopy <- HLE(fullcanopy, # voxel space (only vegetation)
                  variable.name1 = "bsIntercepted",
                  variable.name2 = "weightedFreepathLength",
                  variable.min = -Inf, variable.max = Inf, 
                  variable.fallback = fbv, radius = 2, pulse.min = pulsemin)

gc()

Compute_PAD <- function(fullcanopy, vxsp, fbv){
  fullcanopy@data$HLE[which(is.na(fullcanopy@data$HLE))] <- fbv  # to allow later transmittance computations using AMAPVox
  
  fullcanopy@data$PadBVTotal <- 2*fullcanopy@data$HLE # *2 car hp d'orientation sphérique des feuilles
  
  # Import updated fields
  vxsp@data <- merge(vxsp@data,fullcanopy@data[,.(i,j,k,HLE,PadBVTotal)], by=c("i","j","k"), all.x=T) # veg and ground data
  
  # Fill-in above canopy and below ground voxels with 0 PadBVTotal value
  vxsp@data$PadBVTotal[which(is.na(vxsp@data$PadBVTotal))] <- 0  # to allow later transmittance computations using AMAPVox
  
  return(vxsp)
}

vxsp <- Compute_PAD(fullcanopy, vxsp, fbv)

gc()

path <- "//amap-data.cirad.fr/work/users/VincyaneBadouard/Anisotropy_test/P16_2023_C19C20_UAV_PadHLE_intensity1m.vox"
writeVoxelSpace(vxsp, path)
