#' Helper function to convert covariance matrices to distance
#'
#' @param Covariance matrices
#' @return List of covariance
#' @export
#' @examples
#' multiCovToDist(covariance matrices)

multiCovToDist <- function (multiCovMat){
  
  multiCovList <- list()
  
  #going through the list of covariance matrices
  for (i in seq_along(multiCovMat)){
    
    multiCovList[[i]] <- covToDist(multiCovMat[[i]])     #equates corresponding Vs matrices
                                                         #to the converted matrices (from Ds)
    
    names(multiCovList)[[i]] <- names(multiCovMat)[[i]]    #equating names of each matrix
  }
  
  multiCovList
}