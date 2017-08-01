#' Helper function to calculate Euclid distance between two vectors
#'
#' @param Two vectors
#' @return Euclidean distance between two input vectors
#' @export
#' @examples
#' vecEucDistCalc(vector1, vector2)


vecEucDistCalc <- function (vec1, vec2){              #calculates euclid distance between 
                                                      #two vectors vec1 and vec2
  
  #length of both the vectors
  vec1Len <- length(vec1)
  vec2Len <- length(vec2)
  
  #find minimum and maximum of the lengths
  minVecLen <- min(vec1Len, vec2Len)
  maxVecLen <- max(vec1Len, vec2Len)
  
  #keep adding the distances at each index to sumDist
  sumDist <- 0
  
  #iterate through the vector indices till the 
  #length of the minimum one and the distances 
  #for the rest of the indices for maximum sized
  #vector equals to the value at their indices
  for (i in 1:minVecLen){
    sumDist <- sumDist + ((vec2[i] - vec1[i])^2)
  }
  
  #further calcute distance for the rest 
  #of elements of max sized vector
  if (vec1Len != vec2Len){
    for (j in (minVecLen + 1) : maxVecLen){
      if (vec1Len == minVecLen){
        sumDist <- sumDist + ((vec2[j])^2)
      }
      else if (vec2Len == minVecLen){
        sumDist <- sumDist + ((vec1[j])^2)
      }
    }
  }
  sqrt(sumDist)
}