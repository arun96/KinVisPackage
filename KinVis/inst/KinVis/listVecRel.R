#' Helper function to get data names
#'
#' @param Input vector, the displaced vector, the vector list
#' @return Names
#' @export
#' @examples
#' listVecRel(input vector, displayed vector, list)

listVecRel <- function(inputVec, dispVec, dispVecList){
  
  loadDataNames <- vector("character")
  
  for (i in seq_along(inputVec)){
    if (inputVec[i] %in% dispVec){
      posFound <- match(inputVec[i], dispVec)
      loadDataNames[i] <- names(dispVecList)[[posFound]]
    }
  }
  
  loadDataNames
}