ReadLightFile <- function(path){
  
  Light <- fread(path, skip=10) # skip the 10 1st lines of header
  Light <- Light[Y != "MEAN"] # enlever les 2 lignes mean
  Light <- Light[, !c("V5"), with = FALSE] # remove empty column
  Light[, `:=`(X = as.numeric(X) , Y = as.numeric(Y))] # all as numeric
  Light <- Light[!is.na(X)] 
  
  return(Light)
}
