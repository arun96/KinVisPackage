#' Helper function to replace NA values with 'C'
#'
#' @param The input matrix, and the maximum value
#' @return The modified input matrix
#' @export
#' @examples
#' fillNA(input matrix, maximum value)

fillNA <- function(matInp, maxValC){
  
  #find NA's in input matrix mat
  #and replace with value of 'C'
  matInp[is.na(matInp)] <- maxValC
  
  matInp
}