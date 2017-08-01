#' Helper function to convert a vector to a dataframe
#'
#' @param Vector, max K value
#' @return Created data frame
#' @export
#' @examples
#' vecToDataFrame(vector, max K)

vecToDataFrame <- function(Vs, maxKSet){
  
  nVs_vec <- vector()
  popName <- vector()
  popNumbers <- vector()
  maxKSetPlus <- as.numeric(maxKSet)+1
  
  #get the normalized vector for each Vs in list
  nVs_list <- lapply(Vs, normVector)
  
  #select only first max K values
  selectedVs <- lapply(Vs, "[", 1:maxKSetPlus)
  selectednVs <- lapply(nVs_list, "[", 1:maxKSetPlus)
  
  #GET THE VECTOR FOR POPULATION NUMBERS from selectedVs
  popNumbers <- as.vector(unlist(selectedVs))
  
  #use popNumbers to get nVs vec
  nVs_vec <- as.vector(unlist(selectednVs))
  
  popName <- vecGetPopName(names(Vs))
  
  kVals <- rep(0:maxKSet, times = length(Vs))
  popNameVec <- rep(popName, each = maxKSetPlus)
  
  nVs_df <- data.frame(as.factor(kVals), popNameVec, nVs_vec, popNumbers) #AS.FACTOR(KVALS) GIVES
                                                                          #DISCONTINUOUS SCALE
                                                                          #AS.NUMERIC GIVES
                                                                          #CONTINUOUS SCALE
  
  nVs_df
}

