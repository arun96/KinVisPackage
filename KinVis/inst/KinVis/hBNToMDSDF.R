#' Helper function to convert hBN to the MDS Dataframe
#'
#' @param Loaded list
#' @return A data frame
#' @export
#' @examples
#' hBNToMDSDF(List)

hBNToMDSDF <- function(loadedList){
  #get names for populations
  popFNames <- names(loadedList)
  popNames <- vecGetPopName(popFNames)
  
  #get input as loadedList
  eigenList <- lapply(loadedList, eigen)
  
  eigValList <- sapply(eigenList, '[', 'values')
  
  #non zero max index
  maxIndNZList <- lapply(eigValList, maxNZeroIndx)
  
  #get the minimum from the list of elements
  lim <- min(as.vector(unlist(maxIndNZList)))
  lim <- as.numeric(lim)
  
  #the diemnsion of space is now till 'lim'
  #get a list of eigen values for matrices (from 1 to lim)
  listDataEig <- lapply(eigValList, '[', 1:lim)
  
  #for "MDS Plot"
  #make a matrix from the list
  matDataEig <- matrix(  as.vector(unlist(listDataEig))
                         , nrow = length(popNames) 
                         , ncol = lim
                         , byrow = TRUE)
  
  #make data frame from matrix
  dataEig_df <- data.frame(matDataEig)
  
  #----------------------------- MDS PLOT -------------------------------#
  #distance matrix for MDS population
  distDataEig <- dist(dataEig_df, method = "euclidean")
  
  dataEigMat <- as.matrix(distDataEig)
  
  #if matrix has 1/2 columns/rows in matrix then give points hard coded
  if (nrow(dataEigMat) == 1){
    MDSdf <- data.frame(PopName = popNames, X = c(0), Y = c(0))
    
    #no eigen values here. So, set to 0
    eigenVals <- 0
  }
  else if (nrow(dataEigMat) == 2){
    MDSdf <- data.frame(PopName = popNames, X = c(-0.5, 0.5), Y = c(0, 0))
    
    #eig values to 0
    eigenVals <- c(0, 0)
  }
  else{ 
    MDSvals <- cmdscale(dataEigMat, eig = TRUE)
    
    MDSx <- MDSvals$points[,1]
    
    if (ncol(MDSvals$points) == 1){
      MDSy <- rep(0, length(MDSx))
    }
    else{
      MDSy <- MDSvals$points[,2]
    }
    
    #convert the matrix into a data frame
    MDS_dataframe <- data.frame(P_Name = popNames, X = MDSx, Y = MDSy)
    
    eigenVals <- MDSvals$eig
  }
  
  returnList <- list(Pop_Name = popNames, MDS_df = MDS_dataframe, eigVals = eigenVals)
  
  returnList
  
}