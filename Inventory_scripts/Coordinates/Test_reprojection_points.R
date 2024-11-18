

# essayer ça la prochaine fois : https://saga-gis.sourceforge.io/saga_tool_doc/7.2.0/pj_georeference_2.html

# SF ---------------------------------------------------------------------------
library(sf)

# Créer des données de points de départ
points <- data.frame(x = c(180, 180, 120, 120, 120, 180), y = c(120, 180, 180, 120, 160, 140))
points_sf <- st_as_sf(points, coords = c("x", "y"))

# Corners (bad coordinates)
corners <- data.frame(x = c(120, 120, 180, 180), y = c(120, 180, 180, 120))
corners_sf <- st_as_sf(corners, coords = c("x", "y"))

# Créer des données de points de référence
reference_points <- data.frame(x = c(100, 100, 200, 200), y = c(100, 200, 200, 100))
reference_points_sf <- st_as_sf(reference_points, coords = c("x", "y"))

ggplot()+
  geom_sf(data = points_sf, col = "red")+
  geom_sf(data = corners_sf, col = "orange")+
  geom_sf(data = reference_points_sf, col = "blue")

interpolated <- BIOMASS::bilinear_interpolation(
  coord = points, # relative coord
  from_corner_coord = corners,  # relative coord
  to_corner_coord = reference_points, # les corners doivent etre dans le meme ordre dans le from et le to
  ordered_corner = T)

interpolatedsf <- st_as_sf(interpolated, coords = c('x','y'))

ggplot()+
  geom_sf(data = points_sf, col = "red")+
  geom_sf(data = reference_points_sf, col = "blue") +
  geom_sf(data = interpolatedsf, col = "green")
  



# Définir le système de coordonnées pour les deux ensembles de points
# Vous pouvez remplacer "+proj=utm +zone=30 +datum=WGS84" par le système de coordonnées approprié
st_crs(points_sf) <- "+proj=utm +zone=30 +datum=WGS84"
st_crs(reference_points_sf) <- "+proj=utm +zone=30 +datum=WGS84"

# Calculer les distances entre chaque point et les points de référence
distances <- st_distance(points_sf, reference_points_sf)

# Trouver l'indice du point de référence le plus proche pour chaque point à reprojeter
nearest_index <- apply(distances, 1, which.min)

# Calculer la différence entre les points de départ et les points de référence les plus proches
difference <- st_centroid(reference_points_sf[nearest_index,]) - st_centroid(points_sf)

# Translate les points de départ selon la différence calculée
translated_points <- points_sf + difference

translated_points <- st_as_sf(translated_points$geometry)

st_crs(translated_points) <- st_crs(points_sf)

ggplot()+
# geom_sf(data = points_sf, col = "red")+
# geom_sf(data = reference_points_sf, col = "blue")+
geom_sf(data = translated_points, col = "green")

# Warp -------------------------------------------------------------------------
# Charger les bibliothèques
library(sf)
library(pracma)

# Créer des données de points de départ
points <- data.frame(x = c(10, 20, 30, 40), y = c(40, 50, 60, 70))
points_sf <- st_as_sf(points, coords = c("x", "y"))

# Créer des données de points de référence
reference_points <- data.frame(x = c(100, 200, 300), y = c(400, 500, 600))
reference_points_sf <- st_as_sf(reference_points, coords = c("x", "y"))

ggplot()+
  geom_sf(data = points_sf, col = "red")+
  geom_sf(data = reference_points_sf, col = "blue")

# Séparer les coordonnées x et y des points de départ et des points de référence
points_xy <- cbind(points$x, points$y)
ref_points_xy <- cbind(reference_points$x, reference_points$y)

# Réaliser un ajustement polynomiale
poly_coeff <- polyfit(reference_points$x, reference_points$y, 2)  # Polynomial degree 2 used here

# Appliquer la transformation aux points de départ
warped_points_xy <- polyval(poly_coeff, points_xy)

# Créer un objet sf à partir des points transformés
warped_points_sf <- st_as_sf(data.frame(x = warped_points_xy[,1], y = warped_points_xy[,2]), coords = c("x", "y"))

ggplot()+
  geom_sf(data = points_sf, col = "red")+
  geom_sf(data = reference_points_sf, col = "blue")+
  geom_sf(data = warped_points_sf, col = "green")


# GDAL -------------------------------------------------------------------------
# r : resampling_method
# Resampling method to use. Available methods are:
#   
#   near: nearest neighbour resampling (default, fastest algorithm, worst interpolation quality).
# 
#   bilinear: bilinear resampling.
# 
#   cubic: cubic resampling.
# 
#   cubicspline: cubic spline resampling.
#   
#   lanczos: Lanczos windowed sinc resampling.
# 
#   average: average resampling, computes the weighted average of all non-NODATA contributing pixels.
# 
#   rms root mean square / quadratic mean of all non-NODATA contributing pixels (GDAL >= 3.3)
# 
#   mode: mode resampling, selects the value which appears most often of all the sampled points. In the case of ties, the first value identified as the mode will be selected.
# 
#   max: maximum resampling, selects the maximum value from all non-NODATA contributing pixels.
# 
#   min: minimum resampling, selects the minimum value from all non-NODATA contributing pixels.
# 
#   med: median resampling, selects the median value of all non-NODATA contributing pixels.
# 
#   q1: first quartile resampling, selects the first quartile value of all non-NODATA contributing pixels.
# 
#   q3: third quartile resampling, selects the third quartile value of all non-NODATA contributing pixels.
# 
#   sum: compute the weighted sum of all non-NODATA contributing pixels (since GDAL 3.1)

library('gdalUtilities')


## Prepare file paths
td <- tempdir()
in_tif <- file.path(td, "tahoe.tif")
gcp_tif <- file.path(td, "tahoe_gcp.tif")
out_tif <- file.path(td, "tahoe_warped.tif")

## Set up some ground control points, then warp
file.copy(system.file("extdata/tahoe.tif", package = "gdalUtilities"),
          in_tif)
## Four numbers: column, row, x-coord, y-coord
gcp <- matrix(c(100, 300, -119.93226, 39.28977,  ## A
                0,   300, -119.93281, 39.28977,  ## B
                100, 400, -119.93226, 39.28922,  ## C
                0,   400, -119.93281, 39.28922,  ## lower-left
                400,   0, -119.93067, 39.29136,  ## upper-right
                400, 400, -119.93062, 39.28922,  ## lower-right
                0,     0, -119.93281, 39.29141), ## upper-left
              ncol = 4, byrow = TRUE)

## Add ground control points. (For some reason, this drops CRS, so
## it needs to be explicitly given via `a_srs` argument.)
gdal_translate(in_tif, gcp_tif, gcp = gcp, a_srs = "EPSG:4326")
gdalwarp(gcp_tif, out_tif, r = "bilinear") # r resampling_method

## Check that it worked
if(require(terra)) {
  op <- par(mfcol = c(1, 2))
  r1 <- plot(rast(in_tif), main = "Original raster")
  r2 <- plot(rast(out_tif), main = "Warped raster")
  par(op) ## Reset preexisting parameters
}


