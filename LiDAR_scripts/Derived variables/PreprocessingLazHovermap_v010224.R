##################### Correct Hovermap Laz files ##########################
# scanAngleRank & NumberOfReturns are not populated!!
# ReturnNumber needs updating (+1); Returns not coded by increasing distance!!

library(lidR)
library(data.table)

# Need to split for instance by gpstime (per mn) to reduce size of file 
rm(list=ls())

# Define the path for lastools executables
LAStoolsDir = "C:/Program Files/LAStools/bin/"

# Define the R function that calls lastools executables
LAStool <- function(tool, inputFile, ...){
  cmd = paste(paste(LAStoolsDir, tool, sep=''), '-i', inputFile , ...)
  cat(cmd)
  system(cmd)
  return(cmd)
}

# Define the directory for the ALS project
cloudPath <-  "D:/temp/dump/HovermapSTX/trop65m/" # je mets quoi ?

# Define las/laz files to be processed
inFiles = paste(cloudPath, 'Output_tropiscat_05_slow_no_filter_laz1_4.laz', sep='')
outFile = "FullDensity.laz"
#outFile = "SubSamp.laz"

# Define output directory (and creates it ... if doesn't exist)
outDir = paste(cloudPath, 'FullDens_split', sep='')
#outDir = paste(cloudPath, 'SubSamp_split', sep='')
dir.create(outDir, showWarnings = F)

# Call the lastools function
cores = 3
LASrun <- LAStool('lassplit64', inFiles,
                 '-cores', cores,
                 '-by_gps_time_interval 60',
                 '-odir', outDir,
                 '-odix _split',
                 '-o', outFile
                 )

lof <- dir(outDir, full.names=T)
for (f in 1:length(lof))
{
  #f=5
  lasf <- readLAS(lof[f])
  
  # Filter noise points (intensity=0) and points close to scanner (<1.5m)
  # maybe not enough; could be better to use noise filter in Aura adapted to ST-X scanner ???
  # lasf@data <- lasf@data[Range > 1.5 & Intensity!=0,]
  
  # get NumberOfReturns
  NoR = lasf@data[,.N, by = c("gpstime","Ring")]
  lasf@data <- merge(lasf@data, NoR, by = c("gpstime","Ring"))
  lasf@data$NumberOfReturns <- NULL
  names(lasf@data)[which(names(lasf@data) == "N")] <- "NumberOfReturns"
  
  # get ReturnNumber
  lasf@data[, ReturnNumber := frank(Range, ties.method = "min"), by = c("gpstime","Ring")]
  
  # ring_num=sort(unique(lasf@data$Ring))
  
  # Doing it ring by ring is not faster, maybe will save memory? Could be parallelized!
  # for (i in ring_num)
  # {
  #   i=1
  #   dat <- lasf@data[Ring==i,][,ReturnNumber := frank(Range, ties.method = "min"), by = gpstime]
  # }
  
  writeLAS(lasf,lof[f])
}

# table(lasf@data$NumberOfReturns, lasf@data$ReturnNumber)
# hist(lasf@data$Intensity, breaks=1000)
# hist(lasf@data$Range, breaks=1000)
# plot(lasf)

# Merge files once corrected
# outDir2 = paste(cloudPath, 'subsampled_cor', sep='')
outDir2 <- paste(cloudPath, 'FullDens_cor', sep='')
dir.create(outDir2, showWarnings = F)
inFiles <- paste(outDir, '/*.laz', sep='')
LASrun <- LAStool('lasmerge64', inFiles,
                 '-odir', outDir2,
                 '-odix _cor',
                 '-o', outFile
)

### Correct time stamp
library(lidR)
subsamp <- readLAS("d:/temp/Intercalib/Hovermap/65mOK/SubSamp_cor.laz")

subsamp@data$gpstime <- subsamp@data$gpstime +(10^-6) * subsamp@data$Ring

writeLAS(subsamp,"d:/temp/Intercalib/Hovermap/65mOK/SubSamp_cor_mod.laz")

