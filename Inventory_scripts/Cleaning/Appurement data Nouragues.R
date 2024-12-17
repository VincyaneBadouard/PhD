# Code Giacomo Sellan
### Comments GD: I put my initial before my comments


#>>>> GD: here I write two functions that we will be using to fill in the lines with problem
# see explanation for missing DBH
  # first, we make a function that replace for a given row with a problem
  fill_apur <- function(row, code) {
    row["Problem"] <- TRUE 
    row["Type_of_Problem"] <- ifelse(row["Type_of_Problem"] == "",
                                       code, # we put the code of problem if empty
                                       paste(row["Type_of_Problem"],code,sep =",")) # and paste if there is already something
    # the second part is to avoid having code like "NA, Missing...." or ",Missing..." but have instead "Missing...
    return(row)
  }
  
  
# use fill_apur to fill all the row with problems
fill_apurAll <- function(Data, code) {
  # apply the function fill_apur by row
  for (r in 1:nrow(Data)) {
    Data[r,] <- fill_apur(Data[r,], code)
  }
  return(Data)
}
#>>>> GD: I think there is a better way than the loop but I can't find it straight away 
# and I don't want to do something to complicated



# Packages:
library(tidyverse)
#>>>> GD: why di you load the package but don't use it (apparently, because you don't transform it as a tibble)?

# Import the data
#Nouragues.T2 <- read.csv2("C:/Users/giacomo.sellan/Desktop/Appurement/Nouragues-T2.csv")
#>>>> GD: no need for the full link as you use a Rstudio project
Nouragues.T2 <- read.csv2("Nouragues-T2.csv")
# View(Nouragues.T2)

#>>>> GD: to make your code more generic, give it a more generic name (MyData or Data2check or whaterver)
# it will be better for when you make a function out of your code

# Substitute commas with points 
Nouragues.T2$DistX<- as.numeric(gsub(",", ".", Nouragues.T2$DistX))
Nouragues.T2$DistY<- as.numeric(gsub(",", ".", Nouragues.T2$DistY))
Nouragues.T2$DBH<- as.numeric(gsub(",", ".", Nouragues.T2$DBH))
#>>>> GD: Here the problem is that you have some text in DBH so this cannot be transformed as a numeric
# that's why you get the following error message
# Warning message: NAs introduced by coercion 
# which means that all the text has been replaced by NA

Nouragues.T2$M<- as.numeric(gsub(",", ".", Nouragues.T2$M))
Nouragues.T2$M.1<- as.numeric(gsub(",", ".", Nouragues.T2$M.1))
Nouragues.T2$M.2<- as.numeric(gsub(",", ".", Nouragues.T2$M.2))
Nouragues.T2$M.3<- as.numeric(gsub(",", ".", Nouragues.T2$M.3))
#>>>> GD: what are these 4 columns and why to you want to transform them in numeric
# ok I see now that they are the DBH of the smaller stems
# the first one seems to be comment, by transforming it in numeric, you get NA and loose the comments (see above)

# Check the structure of the dataset
str(Nouragues.T2)
#>>>> GD: you don't record the POM ?

# Replace all the missing values with NAs
Nouragues.T2 <- replace(Nouragues.T2, Nouragues.T2 == "", NA)

# Specify that the df is a data frame
Nouragues.T2<-as.data.frame(Nouragues.T2)
#>>>> GD: no need, as read.csv2 creates a dataframe. 

# Remove all the rows that contain only NAs
Nouragues.T2 <- filter(Nouragues.T2, rowSums(is.na(Nouragues.T2)) != ncol(Nouragues.T2))
#>>>> GD: ok that should work butyou can't test it as you don't have the case in the data

# Remove all rows where there is the NumArbre but not other data (trees that are on the other tablet)
Nouragues.T2<-dplyr::filter(Nouragues.T2,  !is.na(Nouragues.T2[6:20]))
#>>>> GD: I recommend not calling columns by their number in case you add some later
#>>>> GD: also, check this very carefully because with a fake example I made, 
# there seems to be rows that are dropped but shouldn't be
# but I have no more time to check further



# Signal if "DistX", "DistY", "DBH" or "M" are characters
#>>>> GD: they will never be as you transformed them already, 
# so you should do this before if you want to check it (str does that)

# Create a new column "Problem" 
# Nouragues.T2$Problem<-NA
#>>>> GD: it's better to always specify the type of object, here you want a logical
Nouragues.T2$Problem<-as.logical(NA)


# Create a new column "type of problem"
# Nouragues.T2$Type_of_Problem<-NA
#>>>> GD : here you want to put something empty instead of NA otherwise you end up with "NA, missing..." and not just "Missing..."
Nouragues.T2$Type_of_Problem<- as.character("")



#>>>> GD : from here we are going to use the functions we made 
# we use it only when there is only one error
# see for missing DBH for explaination of what was wrong in your code
# You will need to check it with a fake dataset that contains all possible errors and some combinaison of errors


# Check that all the X are there by subsetting all the lines without "DistX"
# Nouragues.T2$Problem[which(is.na(Nouragues.T2$DistX))]<-TRUE
# Nouragues.T2$Type_of_Problem[which(is.na(Nouragues.T2$DistX))]<-"Missing_X"
if (dim(Nouragues.T2[which(is.na(Nouragues.T2$DistX)),])[1]>0) { # apply the function only if there is at least one error of this kind
  Nouragues.T2[which(is.na(Nouragues.T2$DistX)),] <- fill_apurAll(Data=Nouragues.T2[which(is.na(Nouragues.T2$DistX)),],
                                                                  code="Missing_X")
}



# Check that all the Y are there by subsetting all the lines without "DistY"
# Nouragues.T2$Problem[which(is.na(Nouragues.T2$DistY))]<-TRUE
#Nouragues.T2$Type_of_Problem[which(is.na(Nouragues.T2$DistY))]<-paste(Nouragues.T2$Type_of_Problem, "Missing_Y", sep=",")
#>>>> GD: This is problematic as the vector on the left and on the right don't have the same length
# here you don't see it because you don't have missing DistY
# but see below for missing DBH
if (dim(Nouragues.T2[which(is.na(Nouragues.T2$DistY)),])[1]>0) { # apply the function only if there is at least one error of this kind
  Nouragues.T2[which(is.na(Nouragues.T2$DistY)),] <- fill_apurAll(Data=Nouragues.T2[which(is.na(Nouragues.T2$DistY)),],
                                                                  code="Missing_Y")
}


# Check that all the DBH are there by subsetting all the lines without "DBH"
# Nouragues.T2$Problem[which(is.na(Nouragues.T2$DBH))]<-TRUE
#>>>> GD: this would work but we are going to use our function to do all at once

# Nouragues.T2$Type_of_Problem[which(is.na(Nouragues.T2$DBH))]<-paste(Nouragues.T2$Type_of_Problem, "Missing_DBH", sep=",")
#>>>> GD: here there is a problem because you have length 73 on the left 
# length(Nouragues.T2$Type_of_Problem[which(is.na(Nouragues.T2$DBH))])
# and 1585 on the right
# length(paste(Nouragues.T2$Type_of_Problem, "Missing_DBH", sep=","))
#>>>> GD: so you would have to select also on the right part
# length(paste(Nouragues.T2$Type_of_Problem[which(is.na(Nouragues.T2$DBH))], "Missing_DBH", sep=",")) # => 73
#>>>> GD: but let's use our functions to avoid ending up with ",Missing_DBH"
if (dim(Nouragues.T2[which(is.na(Nouragues.T2$DBH)),])[1]>0) { # apply the function only if there is at least one error of this
  Nouragues.T2[which(is.na(Nouragues.T2$DBH)),] <- fill_apurAll(Data=Nouragues.T2[which(is.na(Nouragues.T2$DBH)),],
                                                              code="Missing_DBH")
}



# Check that all the DBHs are bigger than 0.8 cm 
#>>>> GD: why do you put a threshold at 0.8 and not 1?
# Nouragues.T2$Problem[which(Nouragues.T2$DBH< 0.8)]<-TRUE
# Nouragues.T2$Type_of_Problem[which(Nouragues.T2$DBH< 0.8)]<-paste(Nouragues.T2$Type_of_Problem, "DBH_Too_Small", sep=",")
if (dim(Nouragues.T2[which(Nouragues.T2$DBH< 0.8),])[1]>0) { # apply the function only if there is at least one error of this
  Nouragues.T2[which(Nouragues.T2$DBH< 0.8),] <- fill_apurAll(Data=Nouragues.T2[which(Nouragues.T2$DBH< 0.8),],
                                                                code="DBH_Too_Small")
}
# Check that all the multiple diameters are bigger than 0.8 cm 
#Nouragues.T2$Problem[which(Nouragues.T2[13:16]< 0.8)]<-TRUE
#>>>>GD: I did a test with a fake data set and it doesn't seem to work because you call several columns in the which
# I suggest a for loop instead
# also, I recommend never caling columns by theyr number and always by their names, in case you add a column
#Nouragues.T2$Type_of_Problem[which(Nouragues.T2[13:16]< 0.8)]<-paste(Nouragues.T2$Type_of_Problem, "DBH_Multiple_Too_Small", sep=",")
for (c in c("M", "M.1", "M.2", "M.3")) {
  if (dim(Nouragues.T2[which(Nouragues.T2[,c]< 0.8),])[1]>0) { # apply the function only if there is at least one error of this
    Nouragues.T2[which(Nouragues.T2[,c]< 0.8),] <- fill_apurAll(Data=Nouragues.T2[which(Nouragues.T2[,c]< 0.8),],
                                                                code="DBH_Multiple_Too_Small")
  }
}

#>>>>> GD: I stop my comments here as I think the rest is just similar to what is above in term of coding

# Check that all the DBHs are smaller than 100 cm 
Nouragues.T2$Problem[which(Nouragues.T2$DBH> 100)]<-TRUE
Nouragues.T2$Type_of_Problem[which(Nouragues.T2$DBH< 0.8)]<-paste(Nouragues.T2$Type_of_Problem, "DBH_Too_Big", sep=",")

# Check that all the multiple diameters are smaller than 100 cm 
Nouragues.T2$Problem[which(Nouragues.T2[13:16]>100)]<-TRUE
Nouragues.T2$Type_of_Problem[which(Nouragues.T2[13:16]>100)]<-paste(Nouragues.T2$Type_of_Problem, "DBH_Multiple_Too_Big", sep=",")

# Check that there are no negative values across the numeric columns of the dataset
Nouragues.T2$Problem[which(Nouragues.T2[6:7]< 0)]<-TRUE
Nouragues.T2$Type_of_Problem[which(Nouragues.T2[6:7]< 0)]<-paste(Nouragues.T2$Type_of_Problem, "Coord_Negative_Value", sep=",")

# Check if X and Y are bigger than 11.5 by selecting lines with X and Y > 11.5
Nouragues.T2$Problem[which(Nouragues.T2[6:7]> 11.5)]<-TRUE
Nouragues.T2$Type_of_Problem[which(Nouragues.T2[6:7]> 11.5)]<-paste(Nouragues.T2$Type_of_Problem, "Coord_Too_Big", sep=",")


#>>>>> GD: here you want to keep only the row for which you have a problem
# (when you transform this to a function, that's what you will return)
# and you probably want to save this file to go back to the field the next day to check
ToCHECK <- Nouragues.T2[Nouragues.T2$Problem==TRUE,]
