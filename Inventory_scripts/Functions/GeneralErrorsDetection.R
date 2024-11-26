#' General errors detection
#'
#' @param Data Dataset (data.frame or data.table)
#'
#' @details Detect errors
#'   - Detect **duplicated rows**
#'   - Check **missing value** in
#'      X-YTreeUTM/PlotArea/Plot/Subplot/Year/TreeFieldNum/
#'      IdTree/IdStem/Diameter/POM/HOM/Family/Genus/Species/VernName
#'   - Check **missing value** (NA/0) in the measurement variables: "Diameter",
#'      "HOM", "TreeHeight", "StemHeight"
#'   - Check of the **unique association of the IdTree and IdStem** (at the site scale)
#'   - Check **duplicated IdTree/IdStem** in a census (at the site scale)
#'   - Check **invariant coordinates per IdTree/IdStem**
#'
#' @return The input dataset (data.table) with a new *Comment* column with error
#'   type informations.
#'
#' @import data.table
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
GeneralErrorsDetection <- function(
    Data
){
  
  #### Arguments check ####
  
  # Data
  if (!inherits(Data, c("data.table", "data.frame")))
    stop("Data must be a data.frame or data.table")
  
  # In data.table
  setDT(Data)
  if(!"Comment" %in% names(Data)) Data[, Comment := ""]
  
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
  
  # Check duplicated rows ------------------------------------------------------------------------------------
  # if there are duplicated rows, comment them
  
  if(anyDuplicated(Data) != 0){
    
    Data[duplicated(Data), Comment := "Duplicated row"]
    
    warning("Duplicated rows")
  }
  
  # Missing values ----------------------------------------------------------------------------------------------------
  # If the column exists, but have NA values
  
  # Check bota : Family/Genus/Species/ScientificName/VernName
  # Check size : Diameter, POM(?)
  Vars <- c("IdTree", "IdStem", "XTreeUTM", "YTreeUTM",
            "Diameter", "POM", 
            "Family", "Genus", "Species")
  
  for (v in 1:length(Vars)) {
    
    if(Vars[v] %in% names(Data)){ # If the column exists
      if(!all(is.na(Data[,get(Vars[v])]))){ # if the column is not completely empty
        
        Data[is.na(get(Vars[v])), Comment := paste0(Comment, paste0("Missing value in ", Vars[v]), sep ="/")]
        
        
        warning(paste0("Missing value in ", Vars[v]))
        
      } # not empty column
    } # column exists
  } # Vars loop
  
  # Data[grepl("Missing value", Comment)] # to check
  
  
  # Measurement variables = 0 -----------------------------------------------------------------------------------------
  
  Vars <- c("Diameter", "HOM")
  
  for (v in 1:length(Vars)) {
    if(Vars[v] %in% names(Data)){ # If the column exists
      
      Data[get(Vars[v])== 0, Comment := paste0(Comment, paste0(Vars[v]," cannot be 0"), sep ="/")]
      
      
      warning(paste0(Vars[v]," cannot be 0"))
    }
  }
  
  
  # Data[grepl("cannot be 0", Comment)] # to check
  
  # Check duplicated IdTree/IdStem in a census ------------------------------------------------------------------------
  DuplicatedID <- Data[duplicated(Data[, list(get(ID), Year)]), list(get(ID), Year)]
  
  if(nrow(DuplicatedID) > 0){
    
    DuplicatedID[, IDYear := paste(V1, Year, sep = "/")] # code to detect
    
    Data[, IDYear := paste(get(ID), Year, sep = "/")] # code to detect
    
    Data[IDYear %in% DuplicatedID[, IDYear],
         Comment := paste0(Comment, paste0("Duplicated '", ID, "' in the census"), sep ="/")]
    
    a <- Data[IDYear %in% DuplicatedID[, IDYear], .(Year, Plot, Subplot, IdTree, Stem.nb, Diameter, get(ID))]
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
    
    CorresIDs <- CoordIDCombination[, IdStem] # .(IdTree) 
    
    if(!identical(CorresIDs, unique(CorresIDs))){ # check if it's the same length, same ids -> 1 asso/ID
      
      duplicated_ID <- unique(CorresIDs[duplicated(CorresIDs)]) # identify the Idtree(s) having several combinations
      
      
      Data[Site == s & get(ID) %in% duplicated_ID,
           Comment := paste0(Comment, "Non-unique combinaison of IdTree and IdStem", sep ="/")]
      
      
      warning("Non-unique combinaison of IdTree and IdStem")
      
    } else message("Unique IdTree-IdStem associations")
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
      
      Data[Site == s & get(ID) %in% duplicated_ID,
           Comment := paste0(Comment, paste0("Different coordinates (XTreeUTM, YTreeUTM) for a same '", ID,"'"), sep ="/")]
      
      warning(paste0("Different coordinates (XTreeUTM, YTreeUTM) for a same '", ID,"' (",duplicated_ID,")"))
      
    } else message("Unique IdStem-coordinates associations")
  } # end site loop
  
  # unique(Data[IdTree %in% duplicated_ID,
  #             .(IdTree = sort(IdTree), XTreeUTM, YTreeUTM, Comment)]) # to check
  
  
  return(Data)
}

#' GenerateComment
#'
#' @description For all rows concerned by the condition, the function add a
#'   string in the chosen column of a data.table. If there is already a
#'   value, the string is pasted after the "/" separator.
#'
#' @param Data Dataset (data.table)
#' @param condition Vector of logicals
#' @param comment The string to add in the column (character)
#' @param column Column name to fill (character)
#'
#' @return The input data.table with the filled column.
#'
#' @export
#' @examples
#' \dontrun{
#' library(data.table)
#' dt <- data.table(A = c(2, 1, 3), B = c(6, 10, 6))
#'
#' dt[A == 2,
#'    "information" := paste0("A = 2")] # 1st comment
#'
#' GenerateComment(dt, condition = dt[,B] == 6, comment = "B = 6", column = "information")
#'}
#'
GenerateComment <- function(Data, condition, comment, column = "Comment"){
  
  if(!column %in% names(Data)) Data[, column] <- ""
  
  # Apply the function 'CommentByRow' by row
  for (r in 1:nrow(Data[condition,])) {
    Data[condition,][r,] <- CommentByRow(Data[condition,][r,], comment, column)
  }
  
  return(Data) # in data.table
}


#' CommentByRow
#'
#' @description Add a string in a chosen column of a dataset.
#' If there is already a value, the string is pasted after the "/" separator.
#'
#' @param row A 1 row dataset (data.frame)
#' @param comment The string to add in the chosen column (character)
#' @param column Column name to fill (character)
#'
#' @return The input data.frame with the filled chosen column.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' dt <- data.frame(A = c(2), B = c(6))
#'
#' dt <- CommentByRow(dt, comment = "A = 2", column = "information") # 1st comment
#'
#' CommentByRow(dt, comment = "B = 6", column = "information")
#'}
CommentByRow <- function(row, comment, column = "Comment"){
  
  setDF(row) # as data.frame, because it is easier to code in R base than in data.table because of the lazy evaluation
  
  if(!column %in% names(row)) row[, column] <- ""
  
  row[, column] <- ifelse(row[, column] == "",
                          comment,
                          paste(row[, column], comment, sep ="/"))
  return(row)
}
