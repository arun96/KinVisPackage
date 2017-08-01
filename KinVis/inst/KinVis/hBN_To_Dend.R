#' Helper function to convert hBN to a Dendrogram
#'
#' @param Loaded Matrix, the method, and the k-value
#' @return Plot and object to select from
#' @export
#' @examples
#' hBN_To_Dend(matrix, method, k-value)

hBN_To_Dend <- function(loadedMat, METHOD = NULL, K = NULL){
  # METHOD from select input clustMeth
  # K from slider input cTree
  
  #calculated distance matrix using covariance formula
  datadist <- covToDist(loadedMat)
  
  #as a distance mat
  dfdist <- as.dist(datadist)
  
  #if no method given default is ward.D2
  if (is.null(METHOD)){
    METHOD <- "ward.D2"
  }
  
  #do clustering based on selected METHOD
  hr <- hclust(dfdist, method = METHOD)
  
  #obtain the dendrogram followed by some settings change
  dhc <- as.dendrogram(hr)
  dhc <- set(dhc, "labels_cex", 0.5)
  
  #cut tree using the selected value by as input from user
  clustdhc <- cutree(dhc,k = K)
  
  #coloring dendrogram clusters branches and labels
  dhc <- color_branches(dhc, col = clustdhc[order.dendrogram(dhc)])
  dhc <- color_labels(dhc, col = clustdhc[order.dendrogram(dhc)])
  
  
  #pass the plot and the clustdhc to select individual groups from
  return( list(DHC = dhc, clustDHC = clustdhc) )
}