#' Function to convert a vector list into a data frame
#'
#' @param The List
#' @return A dataframe
#' @export
#' @examples
#' boxPopDF(list)

boxPopDF <- function(loadMatList){       #converts normalize vec Vs list to data frame
  
  #get values from upper triangular matrix of each population
  upTriVecLs <- lapply(loadMatList, matToUpTriVec)
  
  #transfer the names too
  loadNames <- names(loadMatList)
  names(upTriVecLs) <- loadNames
  
  #population names are
  popNames <- vecGetPopName(loadNames)
  
  #use the vectors of this list to get a data frame for box plot
  #for the values column we have elements from the list
  vals <- as.vector(unlist(upTriVecLs))
  
  #names will be repeated each with 
  #length of respective vector from the list
  pNamesVec <- rep(popNames, times = lengths(upTriVecLs))
  
  #make the data frame for these two columns
  box_df <- data.frame(PopName = pNamesVec, Value = vals)
  
  box_df
}