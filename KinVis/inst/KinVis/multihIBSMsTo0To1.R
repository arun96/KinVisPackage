#' Helper function
#'
#' @param hIBS Matrix List
#' @return Modified List
#' @export
#' @examples
#' multihIBSMsTo0To1(List of matrices)

multihIBSMsTo0To1 <- function(hIBSMsList){
  
  #apply the required function over a list
  hIBSMsList <- lapply(hIBSMsList, hIBSMsToDs)
  
  hIBSMsList
}