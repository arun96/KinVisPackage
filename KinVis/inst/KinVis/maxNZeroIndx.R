#' Helper function to find maximum non-zero index in a vector
#'
#' @param Vector in which to find max index
#' @return Maximum index
#' @export
#' @examples
#' maxNZeroIndx(vector)

maxNZeroIndx <- function(vec){     #find max non zero index in a vector
   
  #indices with non zero values
  indNZero <- which(vec != 0)
  
  #max index
  maxIndNZero <- max(indNZero)
  
  maxIndNZero
}