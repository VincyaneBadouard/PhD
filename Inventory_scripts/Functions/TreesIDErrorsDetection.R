#' TreesID errors detection
#'
#' @param Data Dataset (data.frame or data.table)
#'
#' @details Detect errors
#'   - Remove **duplicated rows**
#'   - Check **missing value** in
#'      X-YTreeUTM/PlotArea/Plot/Subplot/Year/TreeFieldNum/
#'      IdTree/IdStem/Diameter/POM/HOM/Family/Genus/Species/VernName
#'   - Check **missing value** (NA/0) in the measurement variables: "Diameter",
#'      "HOM", "TreeHeight", "StemHeight"
#'   - Check of the **unique association of the IdTree with plot, subplot**
#'      **and TreeFieldNum** (at the site scale)
#'   - Check **duplicated IdTree/IdStem** in a census (at the site scale)
#'   - Check **invariant coordinates per IdTree/IdStem**
#'
#' @return The input dataset (data.table) with a new *Comment* column with error
#'   type informations.
#'
#' @importFrom stats na.omit
#'
#' @export
#'
#' @examples
#' library(data.table)
#' data("TestData")
#'
#' Rslt <- TreesIDErrorsDetection(TestData)
#'
TreesIDErrorsDetection <- function(
    Data
){
  
  #### Arguments check ####
  
  # Data
  if (!inherits(Data, c("data.table", "data.frame")))
    stop("Data must be a data.frame or data.table")
  
  # In data.table
  setDT(Data)
  
  # IdStem or IdTree? ---------------------------------------------------------------------------------------
  # If no IdStem take IdTree
  if((!"IdStem" %in% names(Data) | all(is.na(Data$IdStem))) &
     ("IdTree" %in% names(Data) & any(!is.na(Data$IdTree))) ){
    ID <- "IdTree"
    Data[, IdTree := as.character(IdTree)]
    
  }else{ ID <- "IdStem"
  Data[, IdStem := as.character(IdStem)]
  }
  
  if(!any(c("IdStem", "IdTree") %in% names(Data)) | (all(is.na(Data$IdStem)) &  all(is.na(Data$IdTree))) )
    stop("The 'IdStem' or 'IdTree' column is missing in your dataset")
  # ---------------------------------------------------------------------------------------------------------
  
  Data[, Subplot := as.character(Subplot)]
  
  #### Function ####
  
  # Check duplicate rows ------------------------------------------------------------------------------------
  # if there are duplicate rows, delete them
  
  if(anyDuplicated(Data) != 0)
    Data <- unique(Data)
  
  # Check duplicated IdTree/IdStem in a census ------------------------------------------------------------------------
  DuplicatedID <- Data[duplicated(Data[, list(get(ID), Year)]), list(get(ID), Year)]
  
  if(nrow(DuplicatedID) > 0){
    
    DuplicatedID[, IDYear := paste(V1, Year, sep = "/")] # code to detect
    
    Data[, IDYear := paste(get(ID), Year, sep = "/")] # code to detect
    
    # Data <- GenerateComment(Data,
    #                         condition = Data$IDYear %in% DuplicatedID[, IDYear],
    #                         comment = paste0("Duplicated '", ID, "' in the census"))
    
    a <- Data[IDYear %in% DuplicatedID[, IDYear], .(Year, Plot, Subplot, IdTree, Stem.nb, DBH, get(ID))]
    setnames(a, "V7", ID)
    a <- a[order(get(ID), Year)]
    b <- capture.output(a)
    c <- paste(b, "\n", sep = "")
    
    warning("Duplicated '", ID, "' in the census:\n", c, "\n")

    Data[, IDYear := NULL]
    
  } else message("No duplicated IdStem in a census")
  
  # Check unique combinaison between IdTree and IdStem ---------------------------------------------------------------------
  
  duplicated_ID <- CorresIDs <- vector("character") # empty vector
  
  # For each site
  for (s in unique(na.omit(Data$Site))) {
    
    CoordIDCombination <- na.omit(unique(
      Data[Data$Site == s, .(IdTree, IdStem)]
    ))
    
    CorresIDs <- CoordIDCombination[, IdStem] # .(IdTree) all the Idtree's having a unique X-YTreeUTM) combination
    
    if(!identical(CorresIDs, unique(CorresIDs))){ # check if it's the same length, same ids -> 1 asso/ID
      
      duplicated_ID <- unique(CorresIDs[duplicated(CorresIDs)]) # identify the Idtree(s) having several P-SubP-TreeFieldNum combinations
      
      # Data <- GenerateComment(Data,
      #                         condition =
      #                           Data[,Site] == s
      #                         & Data[,get(ID)] %in% duplicated_ID,
      #                         comment = paste0("Different coordinates (XTreeUTM, YTreeUTM) for a same '", ID,"'"))
      
      warning(("Different coordinates IdTree-IdStem association"))
      
    } else message("Unique IdTree-IdStem association")
  } # end site loop
  
  
  # Check invariant coordinates per IdTree/IdStem ---------------------------------------------------------------------
  
  duplicated_ID <- CorresIDs <- vector("character") # empty vector
  
  # For each site
  for (s in unique(na.omit(Data$Site))) {
    
    CoordIDCombination <- na.omit(unique(
      Data[Data$Site == s, c(ID, "XTreeUTM", "YTreeUTM"), with = FALSE]
    ))
    
    CorresIDs <- CoordIDCombination[, get(ID)] # .(IdTree) all the Idtree's having a unique X-YTreeUTM) combination
    
    if(!identical(CorresIDs, unique(CorresIDs))){ # check if it's the same length, same ids -> 1 asso/ID
      
      duplicated_ID <- unique(CorresIDs[duplicated(CorresIDs)]) # identify the Idtree(s) having several P-SubP-TreeFieldNum combinations
      
      # Data <- GenerateComment(Data,
      #                         condition =
      #                           Data[,Site] == s
      #                         & Data[,get(ID)] %in% duplicated_ID,
      #                         comment = paste0("Different coordinates (XTreeUTM, YTreeUTM) for a same '", ID,"'"))
      
      warning(paste0("Different coordinates (XTreeUTM, YTreeUTM) for a same '", ID,"' (",duplicated_ID,")"))
      
    } else message("Unique IdStem-coordinates association")
  } # end site loop
  
  # unique(Data[IdTree %in% duplicated_ID,
  #             .(IdTree = sort(IdTree), XTreeUTM, YTreeUTM, Comment)]) # to check

  
  # return(Data)
}