library(data.table)
library(AMAPVox)


VX <- AMAPVox::readVoxelSpace(
  "//amap-data.cirad.fr/work/users/VincyaneBadouard/Lidar/ALS2023/HighAltitudeFlight/ForTrees/Vox/P16_2023_HighAlt_4ha_buffer_PadHLE_intensity1m_testnewEB.vox"
)
VX <- vxsp
# View(VX@data)
AMAPVox::plot(VX, variable.name = "ground_distance") 

range(VX@data$i); range(VX@data$j) ; range(VX@data$k) # 0 700

VX_crop <- AMAPVox::crop(VX, imin = 100, imax = 200, jmin = 100, jmax = 200) 

range(VX_crop@data$i); range(VX_crop@data$j) ; range(VX_crop@data$k) # 0 700

AMAPVox::plot(VX_crop) 

writeVoxelSpace(VX_crop,  
                "//amap-data.cirad.fr/work/users/VincyaneBadouard/Lidar/ALS2023/HighAltitudeFlight/ForTrees/Vox/P16_2023_HighAlt_1ha_buffer_PadHLE_intensity1m_testnewEB.vox"
)
