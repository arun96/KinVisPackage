#' Helper function to get maximum value
#'
#' @param List of values
#' @return Maximum Value in list
#' @export
#' @examples
#' listMaxVal(List)

listMaxVal <- function (listInput){
  
  maxVal <- max(unlist(lapply(listInput, max, na.rm = TRUE)))

  maxVal
}