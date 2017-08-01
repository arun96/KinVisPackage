#' Helper function to get true values of upper triangular matrix
#'
#' @param elements of the matrix
#' @return upper triangular matrix values
#' @export
#' @examples
#' matToUpTriVec(elements)

matToUpTriVec <- function(matElm){
  
  #subset the true values of upper triangular matrix
  matElm[upper.tri(matElm)]
}