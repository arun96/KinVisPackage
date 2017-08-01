#' Helper function to convert a vector to list of frequences
#'
#' @param Vector to convert
#' @return Vector of frequencies
#' @export
#' @examples
#' VecToFrq(vector, max K)

VecToFrq <- function(vec, maxK = 9){
  
  #remove NA's
  vec <- as.vector(na.omit(vec))
  
  #remove values greater than maxK
  vec <- vec[vec <= maxK]
  
  if (length(vec) == 0){
    FrqVec <- rep(0, times = (maxK + 1))
  }
  else{
    #find the frequency of each number in 
    #the vec received using the table function
    frq_DT <- as.data.table( table(vec) )
    
    #$vec gives vector index positions
    #$N gives frequency of each position
    inxPos <- as.integer(frq_DT$vec)
    frqPos <- as.integer(frq_DT$N)
    
    #increment inxPos by 1 to accomodate 
    #for 0 at index position 1
    inxPos <- inxPos + 1
    
    #make a vector and fill it's index
    #positions with the frequencies
    FrqVec <- vector(length = (maxK + 1))
    FrqVec[inxPos] <- frqPos
  }
  
  FrqVec
  
}