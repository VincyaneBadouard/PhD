# Check echo range of Hovermap data
# ReturnNumber starts from 0
# gpstime are not unique, by ring neither

library(lidR)
options(digits = 22)
ech <- readLAS("Z:/users/VincyaneBadouard/Lidar/Hovermap/Data_test/out1_laz1_4.laz")
summary(ech@data)
table(ech@data$NumberOfReturns) # 0 
table(ech@data$ReturnNumber) # 0 1 2
nrow(ech@data) # 61052763
length(unique(ech@data$gpstime)) # 38148942 unique gpstime on 61052763
rm(ech)

# ech2<-readLAS("Z:/users/VincyaneBadouard/Lidar/Hovermap/20220908ST0412Scan/Output/st0142target_09_Output_gcp_global_laz1_4.laz")
# nrow(ech2@data) # 17227548
# length(unique(ech2@data$gpstime)) # 17053616
# table(ech2@data$ReturnNumber) # 0 1
# sum(table(ech2@data$ReturnNumber)) # 17227548
# rm(ech2)

ech2 <- readLAS("Z:/users/VincyaneBadouard/Lidar/Hovermap/Scans Hovermap ST-X/Escadrone180723_ss_filtre/local/out3_laz1_4.laz")
nrow(ech2@data) # 173213094
length(unique(ech2@data$gpstime)) # 88011839
sum(table(ech2@data$ReturnNumber)) # 173213094
dup <- ech2@data[duplicated(gpstime),.(gpstime)] # duplicated gpstime
check = ech2@data[gpstime %in% dup$gpstime, ] # trop lourd

# By ring
ring16 = ech2@data[Ring == 16, ]
sum(table(ring16$ReturnNumber)) # 5384990
length(unique(ring16$gpstime)) # 5122284 (still duplicated gpstime in 1 ring)
countN <- (ring16[,.N, by = gpstime]) # how many duplicated gpstime (N)
ring16 <- merge(ring16,countN, by = "gpstime", all.x = T)
ring16[N<(ReturnNumber+1),]

range(ring16$Range)
range(ech2@data$Range)

rm(ring16)
rm(dup)
rm(ech2)

