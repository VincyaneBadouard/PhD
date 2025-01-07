# title: "LightPerTree"
# author: "Vincyane Badouard"
# date: 17/12/2024

warning("fournir mes fonctions (Relative2UTM,ReadLightFile)")  
warning("TOUT PASSER EN HIGH ALT 25HA BUFFER 50M + fill empty vox")  
warning("vérifier tous les chemins")  
warning("quel inventaire cirad ?")  

# Packages
library(tidyverse)
library(lidR)
library(readr)
library(data.table)
library(AMAPVox)
library(terra)
library(sf)
library(xml2)


args <- commandArgs(trailingOnly=TRUE)
ID <- args[1]
# ID <- "156765" # en local

# Paths ------------------------------------------------------------------------
# cluster
# path_input <- '/cirad_lotois/work/users/VincyaneBadouard/Lidar/ALS2023/HighAltitudeFlight/ByTree_Input/'
path_input <- '/lustre/badouardv/lightpertree/ByTree_Input/'
path_output <- '/lustre/badouardv/lightpertree/ByTree_Output/'
path_script <- '/home/badouardv/fonctions_utiles/ByTree_scripts/'
  
# local 
# path_input <- '//amap-data.cirad.fr/work/users/VincyaneBadouard/Lidar/ALS2023/HighAltitudeFlight/ByTree_Input/'
# path_output <- '//amap-data.cirad.fr/work/users/VincyaneBadouard/Lidar/ALS2023/HighAltitudeFlight/ByTree_Output/'
# path_script <- '//amap-data.cirad.fr/work/users/VincyaneBadouard/Lidar/ALS2023/HighAltitudeFlight/ByTree_scripts/'
  
IDtrees <- read_csv(paste(path_input,"IDtrees.csv", sep=""))
idtree <- IDtrees[ID,]$idTree
  
# Data -------------------------------------------------------------------------
## Trees
Tree <- read.csv(paste(path_input,"AllP16_filtred.csv",sep='')) %>% # 28 049 rows
           filter(idTree == idtree) %>% # "156765"
  filter(SubPlot %in% c(14,15,19,20)) %>% # TEMPORAIRE 4ha seulement # 10 315 rows
  select(idTree, Xutm, Yutm, TreeHeight, CrownHeight, CrownRadius)

# IDtrees <- Tree %>% select(idTree)
# write.csv(IDtrees, "//amap-data.cirad.fr/work/users/VincyaneBadouard/Lidar/ALS2023/HighAltitudeFlight/ByTree_Input/IDtrees.csv")

warning("temporairement filtré au 4ha")  

## Plot
MNT <- terra::rast(paste(path_input,"dtm2023_LowAlt_25ha_buffer.asc",sep=''))
crs(MNT) <- crs("EPSG:2972")
VOP <- as.matrix(read.table(paste(path_input,"VOP_P16_9ha.txt",sep='')))
warning("VOP à changer pour 25ha")  

## Initial Voxelspace
VX <- readVoxelSpace(paste(path_input,"P16_2023_4ha_buffer_HighAlt_PadHLE_intensity1m.vox",sep='')
                     # P16_2023_HighAlt_PadHLE_25ha_buffer50m_intensity1m.vox"
)


# For each tree ----------------------------------------------------------------
## Extract Zutm for each tree ------------------------------------------------
Treesf <- st_as_sf(Tree, coords = c('Xutm','Yutm'))
Treesf <- st_set_crs(Treesf, st_crs(MNT)) 


alt <- extract(MNT, Treesf) %>% # extract z
  rename(Zutm = `dtm2023_LowAlt_25ha_buffer`)

Tree <- Tree %>% cbind(alt)
foot <- Tree %>% select(Xutm, Yutm, Zutm) %>% setDT()


## Coordinates in local positions (use VOP) ----------------------------------
matrix <- as.matrix(foot[, `:=`(c = 0 , d = 1)][,.(Xutm, Yutm, c, d)]) # as matrix of the same dimensions, without Z

footlocal <- matrix %*% t(VOP) # same dimensions

footlocal <- as.data.frame(cbind(footlocal, foot[,Zutm]))[, -c(3,4)]  # add Zutm
names(footlocal) <- c("x","y","z")
footlocal$ID <- "foot" 


## Create crown's bounding box -----------------------------------------------
CrownRadius <- Tree$CrownRadius
TreeHeight <- Tree$TreeHeight
CrownHeight <- Tree$CrownHeight

lowleftfront = c(x = footlocal$x - CrownRadius, # left
                 y = footlocal$y + CrownRadius, # front
                 z = footlocal$z + (TreeHeight - CrownHeight)) # low
lowrightfront = c(x = footlocal$x + CrownRadius, # rght
                  y = footlocal$y + CrownRadius, # front
                  z = footlocal$z + (TreeHeight - CrownHeight)) # low

lowleftback = c(x = footlocal$x - CrownRadius, # left
                y = footlocal$y - CrownRadius, # back
                z = footlocal$z + (TreeHeight - CrownHeight)) # low
lowrightback = c(x = footlocal$x + CrownRadius, # right
                 y = footlocal$y - CrownRadius, # back
                 z = footlocal$z + (TreeHeight - CrownHeight)) # low

Highleftfront = c(x = footlocal$x - CrownRadius, # left
                  y = footlocal$y + CrownRadius, # front
                  z = footlocal$z + TreeHeight) # high
Highrightfront = c(x = footlocal$x + CrownRadius, # right
                   y = footlocal$y + CrownRadius, # front
                   z = footlocal$z + TreeHeight) # high

Highleftback = c(x = footlocal$x - CrownRadius, # left
                 y = footlocal$y - CrownRadius, # back
                 z = footlocal$z + TreeHeight) # high
Highrightback = c(x = footlocal$x + CrownRadius, # right
                  y = footlocal$y - CrownRadius, # back
                  z = footlocal$z + TreeHeight) # high

### Coordinates in the crown -------------------------------------------------
CrownBase = c(x = footlocal$x, # as foot
              y = footlocal$y, # as foot
              z = footlocal$z + (TreeHeight - CrownHeight)) # base
CrownCentre = c(x = footlocal$x, # as foot
                y = footlocal$y, # as foot
                z = footlocal$z + (TreeHeight - CrownHeight/2)) # base


points <- as.data.frame(rbind(lowleftfront, lowrightfront,
                              lowleftback, lowrightback,
                              Highleftfront, Highrightfront,
                              Highleftback, Highrightback,
                              CrownBase, CrownCentre)) %>% 
  rownames_to_column(var= "ID") %>% setDT() %>% rbind(footlocal)

### Crown bounding box in relative coord -------------------------------------
boundingbox <- points %>% filter(ID != 'foot' & ID != "CrownBase" & ID != "CrownCentre")
source(paste(path_script,"Relative2UTM.R",sep=''))
boundingboxUTM <- Relativ2UTM(boundingbox, VOP)

### Where estimate light -----------------------------------------------------
CrownLight <- points %>% filter(ID == "CrownBase" | ID == "CrownCentre")

print("boundingbox created")


## Empties crown voxels ------------------------------------------------------
# Put 0 to attenatuation (HLE)
warning("TOUT PASSER EN HIGH ALT 25HA BUFFER 50M + fill empty vox")  

# i, j, k = index not coordinates
VX@data[, c("x", "y", "z") := getPosition(VX)[, .(x, y, z)]] # in relative coordinates


### Modify HLE and PAD -------------------------------------------------------
VX@data[x>= min(boundingbox$x) & x <= max(boundingbox$x) &
          y>= min(boundingbox$y) & y <= max(boundingbox$y) &
          z>= min(boundingbox$z) & z <= max(boundingbox$z), `:=`(HLE = 0 , PadBVTotal = 0)] # empty voxels, PadBVTotal = 2*HLE

print("boundingbox emptied")

## Crop voxel space with a 50m buffer around the crown -----------------------
# 50m in x,y,z around the crown
subset <- VX@data[x >= max(min(boundingbox$x)-50, min(VX@data$x)) & # xmin
                    x <= min(max(boundingbox$x)+50, max(VX@data$x)) & # xmax
                    y >= max(min(boundingbox$y)-50, min(VX@data$y)) & # ymin
                    y <= min(max(boundingbox$y)+50, max(VX@data$y)) & # ymax
                    z >= max(min(boundingbox$z)-50, min(VX@data$z)) & # zmin
                    z <= min(max(boundingbox$z)+50, max(VX@data$z)), .(i,j,k)] # zmax
I <- range(subset$i) 
J <- range(subset$j) 
K <- range(subset$k) 

VX_crop <- AMAPVox::crop(VX, imin = I[1], imax = I[2], jmin = J[1], jmax = J[2], kmin = K[1], kmax = K[2]) 

### Only necessary columns
VX_crop@data <- VX_crop@data[, .(i,j,k, PadBVTotal, ground_distance)] 

writeVoxelSpace(VX_crop, 
                paste(path_output,"Tree_Vox/P16_2023_HighAlt_4ha_buffer_intensity1m_",ID,".vox", sep='')
)


print("new voxelspace writed")
## Edit xml file -------------------------------------------------------------
xml_file0 <- read_xml(paste(path_input,
  "P16_2023_HighAlt_4ha_buffer_Light_template_severalpts_intensity1m.xml", sep=''))

# Input voxelspace
VXin <- paste(path_output,"Tree_Vox/P16_2023_HighAlt_4ha_buffer_intensity1m_",ID,".vox", sep='')

# Output path
Outputpath <- paste(path_output,
  "Tree_Light/P16_2023_4ha_HighAlt_Light_severalpts_intensity1m_",
  ID,".txt", sep='')

# get node configuration/process
process <- xml_children(xml_file0)[[1]]

# update current input file
input_node <- xml_child(process, "input_file")
xml_attr(input_node, "src") <- VXin

# update current output file
output_node <- xml_child(xml_child(process, "output_files"))
xml_attr(output_node, "src") <- Outputpath

# warning("1 ou plsrs points ?")  
# update coordinates
# if 1 point
# coord_node <- xml_child(xml_child(process, "scanners-positions"))
# xml_attr(coord_node, "x") <- as.character(CrownLight[ID=="CrownBase"]$x)
# xml_attr(coord_node, "y") <- as.character(CrownLight[ID=="CrownBase"]$y)
# xml_attr(coord_node, "z") <- as.character(CrownLight[ID=="CrownBase"]$z)


# If several points:
coord_node <- xml_child(process, "scanners-positions")
coord_1 <- xml_find_first(process, "//position[1]")
xml_attr(coord_1, "x") <- as.character(CrownLight[ID=="CrownBase"]$x)
xml_attr(coord_1, "y") <- as.character(CrownLight[ID=="CrownBase"]$y)
xml_attr(coord_1, "z") <- as.character(CrownLight[ID=="CrownBase"]$z)
coord_2 <- xml_find_first(process, "//position[2]")
xml_attr(coord_2, "x") <- as.character(CrownLight[ID=="CrownCentre"]$x)
xml_attr(coord_2, "y") <- as.character(CrownLight[ID=="CrownCentre"]$y)
xml_attr(coord_2, "z") <- as.character(CrownLight[ID=="CrownCentre"]$z)


# File name
xml_fileT <- paste(
  path_output, 
  "Tree_XML/P16_2023_HighAlt_4ha_buffer_Light_severalpts_intensity1m_", # several points
  ID,".xml", sep=''
)
write_xml(xml_file0, xml_fileT)

print("new xml writed")

## Compute light for the tree ------------------------------------------------
# Distribution plagiophile des feuilles
# Pendant 1 an
AMAPVox::run(xml = xml_fileT, jvm.option = "-Xms16g") # v. 2.3.2

print("Light computed")
