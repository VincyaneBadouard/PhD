library(data.table)
library(parallel)
library(foreach)
library(microbenchmark)

data <- data.table(Ring = c(1, 1, 1, 2, 2),
                   gpstime = c(1, 1, 1, 2, 2),
                   Range = c(5,6, 3, 2, 1),
                   Patate = rep(0, 5))

data

data <- unique(setorder(data, gpstime,Ring,Range,Patate)) 

# microbenchmark(
  data[, Patate := frank(Range), by = .(gpstime, Ring)]
# ) # min = 893.6 mean = 1024.839  max = 1862.7 microsec (+ rapide)


# Paraleliisation --------------------------------------------------------------

data1 <- data[1:4]
data2 <- data[5]
datas <- list(data1, data2)
j <- length(datas)
cores <- detectCores()
cl <- parallel::makeCluster(cores)
doSNOW::registerDoSNOW(cl)

# microbenchmark(
# registerDoParallel()
foreach::foreach(
    i=1:j, 
    .packages = c("data.table")) %dopar% {
      datas[[i]][, Patate := frank(Range), by = .(gpstime, Ring)]
      cat(names(datas[[i]]))
      
      # Specify the file name for each i
      output_file <- paste0("//amap-data.cirad.fr/work/users/VincyaneBadouard/Lidar/HovermapUAV2023/P16_C14C15/Cor", i, ".csv")

      # Write the modified dataset to a CSV file
      fwrite(datas[[i]], file = output_file)
    }
# , unit="microseconds") #  min 9.0171 mean = 17.41185 max = 692.1256 milisec
stopCluster(cl)

