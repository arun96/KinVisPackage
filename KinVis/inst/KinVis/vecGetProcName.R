#' Helper function to get process name from the vector of file names
#'
#' @param Vector of file names
#' @return Process names
#' @export
#' @examples
#' vecGetProcName(vector of file names)

vecGetProcName <- function(fileNameVec){
  
  procNameVec <- vector("character")
  
  for (i in seq_along(fileNameVec)){
    
    procNameVec[i] <- getProcessName(fileNameVec[i])
  }
  
  procNameVec
}