#' Helper function to get min and max of the matrix
#'
#' @param hIBS Matrix
#' @return Matrix
#' @export
#' @examples
#' hIBSMsTo0To1(Matrix)
hIBSMsTo0To1 <- function(hIBSMs) {
  
  #get max and min of the matrix 
  maxMat <- max(hIBSMs)
  minMat <- min(hIBSMs)
  
  hIBSMs <- (hIBSMs - minMat)/(maxMat - minMat)
  
  hIBSMs
}