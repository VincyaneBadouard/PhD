##################### Correct Hovermap Laz files ##########################
# scanAngleRank (not necessary) & NumberOfReturns are not populated (all = 0)!!
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
cloudPath <-  "D:/temp/dump/HovermapSTX/trop65m/" # je mets quoi ? Y:\lidar\MLS\Paracou\Tropiscat\UMLS\65mOK

# Define las/laz files to be processed
inFiles = paste(cloudPath, 'Output_tropiscat_05_slow_no_filter_laz1_4.laz', sep ='')
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

lof <- dir(outDir, full.names=T) # list files in the named directory

# Loop for each file
for (f in 1:length(lof))
{
  #f=5
  lasf <- readLAS(lof[f]) # Laz
  
  # Filter noise points (intensity=0) and points close to scanner (<1.5m)
  # maybe not enough; could be better to use noise filter in Aura adapted to ST-X scanner ???
  # lasf@data <- lasf@data[Range > 1.5 & Intensity!=0,]
  
  # Get NumberOfReturns
  # Number of pulses of the same ring and gpstime
  NoR <- lasf@data[,.N, by = c("gpstime","Ring")] # number of rows of the same ring and gpstime
  lasf@data <- merge(lasf@data, NoR, by = c("gpstime","Ring"))
  lasf@data$NumberOfReturns <- NULL # remove the initial column
  names(lasf@data)[which(names(lasf@data) == "N")] <- "NumberOfReturns" # rename the new column
  
  # Get ReturnNumber
  # Returns coded by increasing distance
  lasf@data[, ReturnNumber := frank(Range, ties.method = "min"), by = c("gpstime","Ring")] # Give the minimum rank
  
  ## Parallelisation by ring
  # ring_num <- sort(unique(lasf@data$Ring))
  
  # for (i in ring_num)
  # {
  #   i=1
  #   dat <- lasf@data[Ring==i,][,ReturnNumber := frank(Range, ties.method = "min"), by = gpstime]
  # } # loop end for each ring
  
  writeLAS(lasf,lof[f])
  
} # loop end for each file

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

--------------------------------------------------------------------------------
### Correct time stamp
library(lidR)
subsamp <- readLAS("d:/temp/Intercalib/Hovermap/65mOK/SubSamp_cor.laz")

subsamp@data$gpstime <- subsamp@data$gpstime +(10^-6) * subsamp@data$Ring

writeLAS(subsamp,"d:/temp/Intercalib/Hovermap/65mOK/SubSamp_cor_mod.laz")

