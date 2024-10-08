# Compute Hovermap ST-X angles

# Classifier les points sol
# Calculer le CHM
# Fusionner le laz et la traj par le temps
# Compute and round angle from laz and traj spatial infos
# filter angle <=30°, <=10°
# Compute the proportion of ground pts for 10° and 30° laz 

##now add STX flights
setwd("x:/Paracou2023/Analysis_Ready/")
flight_tab <- fread("Flights_STX.txt",header = F)

flight_tab$name <- NA
for (l in 1:dim(flight_tab)[1])
{
  flight_tab$name[l] <- unlist(strsplit(as.character(flight_tab[l]),
                                        split >= "/"))[8]
}

setwd("x:/Paracou2023/Analysis_Ready/STX/")
lof <- sort(dir(getwd(),full.names = T))
to_process <- c(1,2) # files ?
for (d in to_process)
{
  print(d)
  flight_name <- flight_tab$name[d]
  setwd(lof[d])
  dir()
  
  # laz ------------------------------------------------------------------------
  fl <- dir(pattern = "PC_ICPCor.laz") # full PC!!
  las <- readLAS(fl, filter = "-keep_xy 286536.5 583737.6 286666.4 583868") # filter by coordinates
  
  las_n <- normalize_height(las,dtm) # normalize heights
  las_n@data[Z<0.5, Classification:=2] # Ground points classif
  st_crs(las_n) <- st_crs(ROI) # set crs
  las_n_clp <- clip_roi(las_n,sf::st_as_sf(ROI)) # clip le laz
  
  chm <- rasterize_canopy(las_n_clp, res = 0.5, p2r(na.fill = tin())) # compute CHM
  rm(las_n_clp)
  
  CHM <- mean(chm[], na.rm = T) # average CHM
  las <- unnormalize_height(las_n) # unnormalize heights
  rm(las_n)
  #lidR::plot(las_n,color = "Classification" )
  
  # traj -----------------------------------------------------------------------
  traj <- fread("Strip_traj_ICPCor.txt")
  names(traj) <- c("t1","t","x","y","z")
  # plot3d(traj[,.(x,y,z)], aspect = F)
  # bgplot3d({
  #   plot.new()
  #   title(main = flight_name, line = 2)
  # })
  traj$zgrd <- dtm[cellFromXY(dtm,as.matrix(traj[,.(x,y)]))] # Get cell number 
  flight_height <- median(traj$z-traj$zgrd) # median flight height
  las <- retrieve_pulses(las)
  dens <- rasterize_density(las, res = 1)
  pls_mean <- mean(dens[[2]][], na.rm = T)
  pls_sd <- sd(dens[[2]][], na.rm = T)
  #populate ScanAngleRank!!
  
  #### Euclidean distance calculation #### -------------------------------------
  ## GPS time (trajectory) vectors
  TGPS <- matrix(data = NA, nrow = length(traj$t), ncol = 3) # create an empty 3-column matrix
  # allocate GPS time (trajectory) to first column
  TGPS[,1] <- traj$t
  # allocate 0s for GPS (trajectory) points to second column
  TGPS[,2] <- 0
  # allocate index to third column
  TGPS[,3] <- 1:length(traj$t)
  
  ## LIDAR (target) time vectors
  TLIDAR <- matrix(data = NA,nrow = length(las@data$gpstime), ncol = 3) # create an empty 3-column matrix
  # allocate GPS time (LIDAR) to first column
  TLIDAR[,1] <- las@data$gpstime
  # allocate 1s for LIDAR points to second column
  TLIDAR[,2] <- 1
  # allocate index to third column
  TLIDAR[,3] <- 1:length(las@data$gpstime)
  
  # fusion GPS/LIDAR time vectors
  fusion <- rbind(TGPS,TLIDAR) # fusioned matrices
  
  # order fusion by time
  fusion_ordered <- fusion[order(fusion[,1]),] 
  
  # keep GPS (trajectory) points and write NAs for LIDAR points
  fusion_ordered[which(fusion_ordered[,2] == 1),1] <- NA # LiDAR time
  
  # fill empty spaces from left to right
  # LIDAR point takes the closest inferior GPS (trajectory) time available
  fusion_ordered[,1] <- zoo::na.locf(fusion_ordered[,1]) # Generic function for replacing each NA with the most recent non-NA prior to it.
  
  # keep only LIDAR points
  fusion_ordered <- fusion_ordered[which(fusion_ordered[,2] == 1),]
  
  # order by third column (index) (traj order), ID in tile450m12p
  fusion_ordered <- fusion_ordered[order(fusion_ordered[,3]),]
  
  # join to tile450m12p
  las@data$tgps <- fusion_ordered[,1] # laz <-  traj time
  
  # fusion by time
  las@data <- merge(las@data,traj, by.x = "tgps", by.y = "t", all.x = T) # fusion laz & traj by time
  
  # distance calculation
  # las@data$range <- mapply(dfdist, las@data$X, las@data$Y, las@data$Z,
  #                         las@data$x, las@data$y, las@data$z)
  
  # shot angle calculation -----------------------------------------------------
  las@data$theta <-
    ((acos((las@data$z-las@data$Z)/las@data$Range))/pi)*180 # compute angle
  
  las@data$ScanAngleRank <- round(las@data$theta) # round angle
  
  sca_min <- min(las@data$ScanAngleRank, na.rm = T) # minimum angle
  sca_max <- max(las@data$ScanAngleRank, na.rm = T) # maximum angle
  las30 <- las[abs(las@data$ScanAngleRank)<=30 & # filter angle <=30°
                 !is.na(las@data$ScanAngleRank)]
  rm(las)
  gc()
  
  # Pour le laz filtré à 30°, compter le nombre de lignes de dernier echo par classification (sol/non-sol)
  temp30 <- las30@data[NumberOfReturns == ReturnNumber, .N, by = Classification]
  
  las10 <- las30[abs(las30@data$ScanAngleRank)<=10 & # filter angle <=10°
                   !is.na(las30@data$ScanAngleRank)]
  rm(las30)
  gc()
  
  # idem pour le laz filtré à 10°
  temp10 <- las10@data[NumberOfReturns==ReturnNumber,.N, by = Classification]
  
  # Compute the proportion of ground pts for 10° and 30° laz 
  grd_pls_10 <- temp10[Classification==2, N]/temp10[Classification==0, N]
  grd_pls_30 <- temp30[Classification==2, N]/temp30[Classification==0, N]
  
  # Characteristics
  res_temp <- data.frame( 
    name = flight_name, number = d,
    flight_height = flight_height,
    sca_min = sca_min, sca_max = sca_max,
    grd_pls_10 = grd_pls_10, grd_pls_30 = grd_pls_30,
    pls_mean = pls_mean, pls_sd = pls_sd, CHM = CHM)
  
  res <- rbind(res, res_temp)
  
}



