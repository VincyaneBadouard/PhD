#' DetectMultistem
#'
#' @param Data Dataset (data.frame or data.table) with these columns:
#' - Xutm and Yutm
#' - Genus and Species
#' - CensusYear
#' 
#' @details Detect multistem in trees inventory data from common coordinates
#' and botanical name
#' 
#' @return The input dataset (data.table) with a new *Comment* column with
#' information on the detection of potential multistem.
#' 
#' @import data.table
#' @export
#'
#' @examples
#' Rslt <- DetectMultistem(Data)
#' 
DetectMultistem <- function(Data){
  
  setDT(Data)
  
  # Check duplicated coordinates in a census to detect multistems --------------
  Data[, ScientificName:= paste(Genus, Species, sep = "_")]
  Data[, Coord:= paste(Xutm, Yutm, sep = "_")]
  
  if(!"Comment" %in% names(Data)) Data[, Comment := ""]
  
  DuplicatedID <- Data[duplicated(Data[, list(Coord, ScientificName, CensusYear)]), list(Coord, ScientificName, CensusYear)]
  
  if(nrow(DuplicatedID) > 0){
    
    DuplicatedID[, IDYear := paste(Coord, ScientificName, CensusYear, sep = "/")] # code to detect
    
    Data[, IDYear := paste(Coord, ScientificName, CensusYear, sep = "/")] # code to detect
    
    Data[IDYear %in% DuplicatedID[, IDYear],
         Comment := paste0(Comment, paste0("Multistem potential (same coordinates and species)"), sep ="/")]
    
    warning("Multistem potential")
    
    Data[, IDYear := NULL]
    
  } else message("No multistem potential detected")
  
  return(Data)
  
}
