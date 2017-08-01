#' Helper function to convert a vector to a dataframe for MDS
#'
#' @param List of vectors
#' @return Created data frame for the MDS Plot
#' @export
#' @examples
#' vecToDFMDS(list of vectors)

vecToDFMDS <- function (vecVsList) {
  
  #following finds anmes of each population to display
  Population_Name <- vecGetPopName(names(vecVsList))
  
  if (length(vecVsList) == 1){
    MDS_dataframe <- data.frame(P_Name = Population_Name, X = c(0), Y = c(0))
    
    #no eigen values here. So, set to 0
    eigenVals <- 0
  }
  else if (length(vecVsList) == 2){
    MDS_dataframe <- data.frame(P_Name = Population_Name, X = c(-0.5, 0.5), Y = c(0, 0))
    
    #eig values to 0
    eigenVals <- c(0, 0)
  }
  else{
    #calculating distance matrix for the Vs list of vectors
    distMat <- vecEucDistances(vecVsList) 
    
    #calculate the MDS for this symmetrical matrix
    MDSVals <- cmdscale(distMat,eig = TRUE)
    
    #get the X and Y points
    MDSx <- MDSVals$points[,1]
    MDSy <- MDSVals$points[,2]
    
    #convert the matrix into a data frame
    MDS_dataframe <- data.frame(P_Name = Population_Name, X = MDSx, Y = MDSy)
    
    eigenVals <- MDSVals$eig
  }
  
  
  returnList <- list(Pop_Name = Population_Name, MDS_df = MDS_dataframe, eigVals = eigenVals)
  
  returnList
}