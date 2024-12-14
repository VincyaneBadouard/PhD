setwd("~/Rejou/Encadrement/Siriruk/AMAPvox/NewAnalyses/")

require(xml2) # to modidy xml
require(stringr)
require(lidR)

OUTfolder <- "/home/rejou/Rejou/Encadrement/Siriruk/AMAPvox/NewAnalyses/OUTPUT/"

# Get file names
FileListLas <- paste0(getwd(), "/", list.files("Mydataset/Clipdata/Corrected", full.names = T))
FileListTot <- paste0(getwd(), "/", list.files("Mydataset/Clipdata/Uncorrected", full.names = T))
FileListDTM <- FileListTot[grepl(".asc", FileListTot)]
FileListVOP <- FileListTot[grepl(".txt", FileListTot)]
# 
FileOut <- str_replace(FileListDTM, "/home/rejou/Rejou/Encadrement/Siriruk/AMAPvox/NewAnalyses/Mydataset/Clipdata/Uncorrected/", "")
FileOut <- str_replace(FileOut, "DTM", "")
FileOut <- str_replace(FileOut, ".asc", "")

if ((length(FileListLas) != length(FileListDTM)) |
  (length(FileListLas) != length(FileListVOP))) {
  stop("Different number of las, DTM and VOP files")
}

# Start loop over all the files
for (i in 1:length(FileListLas)) {
  # path of the reference xml file
  cfg <- "Mydataset/RefXML2.xml"
  # load xml document in R
  x <- read_xml(cfg)
  # get node configuration/process
  process <- xml_children(x)[[1]]

  ## input_file src attr
  input_node <- xml_child(process, "input_file")
  # update current input file str attr
  xml_attr(input_node, "src") <- FileListLas[i]

  ## output_file src attr
  output_node <- xml_child(process, "output_file")
  # update current input file str attr
  xml_attr(output_node, "src") <- paste0(OUTfolder, FileOut[i], ".vox")

  # Change DTM path
  dtm_node <- xml_child(process, "dtm-filter")
  xml_attr(dtm_node, "src") <- FileListDTM[i]

  # Update voxel space
  FileListLas <- c("/home/pverley/amap/travaux/maxime_rejou-mechain/BugAMAPvox/PointCloudOGS1_cor.laz")
  FileListVOP <- c("/home/pverley/amap/travaux/maxime_rejou-mechain/BugAMAPvox/VOPmatOGS1.txt")

  # read LAZ
  lasDat <- readLAS(FileListLas[i])
  # read VOP
  vop <- as.matrix(read.table(FileListVOP[i]))
  # bind LAS points
  points <- cbind(lasDat@data$X, lasDat@data$Y, lasDat@data$Z, 1)
  # transform
  points.t <- apply(points, MARGIN = 1, function(xyz, vop) vop %*% xyz, vop)
  # extract min and max
  vsmin.t <- apply(points.t, MARGIN = 1, FUN = min)
  vsmax.t <- apply(points.t, MARGIN = 1, FUN = max)


  vsmin.t <- vop %*% vsmin
  xml_attr(voxelspace_node, "xmin") <- as.character(vsmin.t[1])
  xml_attr(voxelspace_node, "ymin") <- as.character(vsmin.t[2])

  voxelspace_node <- xml_child(process, "voxelspace")
  xml_attr(voxelspace_node, "xmin") <- as.character(min(lasDat@data$X))
  xml_attr(voxelspace_node, "ymin") <- as.character(min(lasDat@data$Y))
  xml_attr(voxelspace_node, "zmin") <- as.character(min(lasDat@data$Z))
  xml_attr(voxelspace_node, "xmax") <- as.character(max(lasDat@data$X))
  xml_attr(voxelspace_node, "ymax") <- as.character(max(lasDat@data$Y))
  xml_attr(voxelspace_node, "zmax") <- as.character(max(lasDat@data$Z))
  res <- as.numeric(xml_attr(voxelspace_node, "resolution"))
  xml_attr(voxelspace_node, "splitX") <- as.character(ceiling((max(lasDat@data$X) - min(lasDat@data$X)) / res))
  xml_attr(voxelspace_node, "splitY") <- as.character(ceiling((max(lasDat@data$Y) - min(lasDat@data$Y)) / res))
  xml_attr(voxelspace_node, "splitZ") <- as.character(ceiling((max(lasDat@data$Z) - min(lasDat@data$Z)) / res))

  # Change VOP matrix
  transformation_node <- xml_child(process, "transformation")
  vop_node <- xml_child(transformation_node, "matrix")
  VOPfile <- read.table(FileListVOP[i])
  xml_text(vop_node) <- paste(apply(VOPfile, 1, function(x) paste(x, collapse = ", ")), collapse = "\n")

  # write xml in new file
  write_xml(x, paste0("AllXML/XMLamapVox_", FileOut[i], ".xml"))
  print(paste0(i, "/", length(FileOut)))
}
