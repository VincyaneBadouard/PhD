dat <- laz_crop@data[40000:80000,]

dat$z > dat$Z
(dat$z-dat$Z) < dat$Range

acos((dat$z-dat$Z)/dat$Range)
(acos(20/50)*180)/pi # Acos donne un angles en radians, le multiplier par 180 et divisé par pi le transforme en degrees

(acos(50/20)*180)/pi # quand range < à z-Z -> NaN. Pq range < ?

dat[, ScanAngle := round(((acos((z-Z)/Range))*180)/pi)]

dat[, ScanAngle := round(((acos(min((z-Z)/Range, 1)))*180)/pi)]


((acos(min((dat$z-dat$Z)/dat$Range, 1)))*180)/pi

((min(acos((dat$z-dat$Z)/dat$Range), 1)/pi))*180