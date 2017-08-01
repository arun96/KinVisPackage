#' Helper function to get population names from the vector of file names
#'
#' @param Vector of file names
#' @return Vector of population names
#' @export
#' @examples
#' vecGetPopName(vector of file names)

vecGetPopName <- function(fileNameVec){
  
  popNameVec <- vapply(fileNameVec, getPopulationName, FUN.VALUE = character(1))
  
  as.vector(popNameVec)
}