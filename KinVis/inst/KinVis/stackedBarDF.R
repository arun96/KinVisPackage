#' Helper function to generate a data frame for a stacked bar plot
#'
#' @param Vector, maximum k-value (defaults to 9)
#' @return Dataframe for the plot
#' @export
#' @examples
#' stackedBarDF(Vector, Maximum K Value)

stackedBarDF <- function(Vs, maxKVal = 9){
  
  
  #input max K value should be numeric
  maxKVal <- as.numeric(maxKVal)
  
  #if maxKVal is greater then Vs vectors' length 
  #we just compute till that length of the vectors 
  maxVsLen <- length(Vs[[1]])
  
  if (maxKVal >= maxVsLen){
    maxKVal <- maxVsLen - 1
  }
  
  #gives Vs the function converts to norm and then adds in data frame
  dat_df <- vecToDataFrame(Vs, maxKVal)
  names(dat_df) <- c("K_Value", "Pop_Name", "Normalized_Count", "popNumbers")
  
  dat_df
}