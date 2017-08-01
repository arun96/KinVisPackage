#' Helper function to return distance matrix
#'
#' @param List of multiple vectors
#' @return Distance matrix, computed using Euclidean distance formula
#' @export
#' @examples
#' vecEucDistance(List of vectors)

vecEucDistances <- function (vecVsList){                  #takes a list containing multiple vectors
                                                          #and returns a dustance matrix using the
                                                          #Euclidian distance formula
  
  #make the matrix out of the vector list
  dataMat <- do.call(rbind, vecVsList)
  
  #use euclid distances between row of matrices to get a distance matrix
  eucDistMat <- dist(dataMat, method = "euclidian")
  
  #convert into an actual matrix
  as.matrix(eucDistMat)
}