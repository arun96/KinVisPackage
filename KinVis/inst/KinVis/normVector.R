#' Helper function to normalize vector
#'
#' @param Vector to be normalized
#' @return Normalized vector
#' @export
#' @examples
#' normVector(vector)

normVector <- function(Vec){
  
  if (class(Vec) == "list"){ 
    print("Use of this function found in 2 places only! Looks like there is another place!")
    return(NULL)
  }
  Vec <- Vec/sum(Vec)
  
  Vec
}