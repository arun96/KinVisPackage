#' Helper function to convert list of matrices to dissimilarity matrix
#'
#' @param Matrix list
#' @return Dissimilarity Matrix
#' @export
#' @examples
#' multiMsToDs(Matrix list)

multiMsToDs <- function(multiMsMat){                        #takes a matrix list converts to
                                                            #dissimilarity matrix Ds
  
  multiLinMat <- list()
  
  #list applying function MsToDs
  multiLinMat <- lapply(multiMsMat, MsToDs)
  
  #prints warnign if lineage list has any NA values
  if (identical(multiLinMat, list())){
    stop("WARNING: All NA's not accessed in multMsToDs!!!")
  }
  else{ #continue normal computation if 
        #multiLinMat has some elements to compute
    #maxVal of all elements in list
    C <- listMaxVal(multiLinMat) + 1
    
    #replacing all NA's with 'C'
    multiDs <- lapply(multiLinMat, FUN = fillNA, C)
    
    multiDs
  }
}