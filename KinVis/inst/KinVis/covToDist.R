#' Function to convert covariance to distance
#'
#' @param Covariance matrix
#' @return Covariance Distance Matrix
#' @export
#' @examples
#' covToDis(CovarianceMatrix)

covToDist <- function (covMat){
  
  covDistMat <- matrix(0, nrow = nrow(covMat), ncol = ncol(covMat))

  for (i in 1:nrow(covMat)){
    for (j in 1:ncol(covMat)){

      covDistMat [i, j] = sqrt(covMat[i, i] + covMat[j, j] - abs(covMat[i, j]))
    }
  }
  
  #assign names to the new matrix
  rownames(covDistMat) <- rownames(covMat)
  colnames(covDistMat) <- colnames(covMat)
  
  covDistMat
}