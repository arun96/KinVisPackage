#' Helper function to convert a DS into a normalized Vector
#'
#' @param The DS, and the maximum Value
#' @return A normalized vector
#' @export
#' @examples
#' DSToVS(DS, Maximum value)

DsToVs <- function (Ds, maxVal){    #returns a vector (having frequency of k
                                                          #values) which is normalized  
  
  #get vector of upper triangle of Ds
  upTriVec <- Ds[upper.tri(Ds)]
  
  #use table to get frequency
  #make it a data table
  freq_DT <- as.data.table( table(upTriVec) )
  
  #$upTriVec gives vector index positions
  #$N gives frequency of each position
  indxPos <- as.integer(freq_DT$upTriVec)
  freqPos <- as.integer(freq_DT$N)
  
  #increment indxPos by 1 to accomodate 
  #for 0 at index position 1
  indxPos <- indxPos + 1
  
  #make a vector and fill it's index
  #positions with the frequencies
  VsVec <- vector(length = (maxVal + 1))
  VsVec[indxPos] <- freqPos
  
  VsVec
}