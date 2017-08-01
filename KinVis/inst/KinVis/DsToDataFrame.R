#' Helper function to convert a DS to a Data Frame
#'
#' @param Selected DS, and the max K value (defaults to 9)
#' @return A data frame
#' @export
#' @examples
#' DSToDataFrame(Selected Ds, maximum K)

DsToDataFrame <- function(selectedDs, maxK = 9){
  
  # #declare variables
  # data_df <- data.frame()
  # valVec <- vector()
  # freqVec <- vector()
  # nameInd <- vector()
  # indNames <- as.vector(rownames(selectedDs))
  # 
  # #browser()
  # 
  # #length required to normalise is nrow of Ds - 1
  # normLen <- nrow(selectedDs)
  # 
  # #get the minimum of the matrix selectedDs
  # matMin <- min(selectedDs)
  # 
  # #unique elements of selectedDs
  # uniqDsElem <- unique(as.vector(selectedDs)) 
  # 
  # #remove any zeroes unique elements
  # uniqDsElem <- uniqDsElem[ -(uniqDsElem == 0) ]
  # 
  # for (i in 1:nrow(selectedDs)){
  #   rowVector <- selectedDs[i, ]
  #   
  #   #remove diag (diagonal of matrix) value
  #   rmDiagRow <- rowVector[-i]
  #   
  #   #set the missing elements in the vector to zero
  #   missingElem <- setdiff(uniqDsElem, rmDiagRow)
  #   
  #   # #if maxK is greater than a Ds row's length 
  #   # #we just compute till that length of the vectors 
  #   # if (maxK > max(rmDiagRow)){
  #   #   maxK <- max(rmDiagRow)
  #   # }
  #   # else if (maxK < min(rmDiagRow)){
  #   #   maxK <- min(rmDiagRow)
  #   # }
  #    
  #   #remove any values greater than maxK
  #   rmDiagRow <- rmDiagRow[rmDiagRow <= maxK]
  #   
  #   if (length(unique(rmDiagRow)) == 0) {
  #     
  #     valVec <- c(  valVec, missingElem  )
  #     
  #     freqVec <- c(  freqVec, rep(0, times = length(missingElem))  )
  #     
  #     nameInd <- c(  nameInd, rep(indNames[i], times = length(missingElem))  )
  #   } else {
  #     
  #     data1 <- as.data.frame(table(rmDiagRow))
  #     
  #     #values vector
  #     valVec <- c( valVec, as.numeric(as.vector(data1$rmDiagRow)))
  #     
  #     #frequency vector
  #     freqVec <- c( freqVec, (data1$Freq/normLen))   #USE THIS IF NORMALIZING IS NEEDED
  #     #freqVec <- c( freqVec, data1$Freq)                     USE THIS IF NO NORMALIZING
  #     
  #     #get individual names to put in data frame
  #     nameInd <- c(  nameInd, rep(indNames[i], length(unique(rmDiagRow)))  )
  #   }
  #   
  # }
  # #browser()
  # 
  # data_df <- data.frame(nameInd, as.factor(valVec), freqVec)
  # 
  # if (
  #      identical(valVec, numeric(0))||
  #     (identical(freqVec, numeric(0)))||
  #     (identical(nameInd, numeric(0)))
  # ){
  #   return(NULL)
  # }
  # else {
  #   data_df
  # }
  
  #put NA's at diagonals
  diag(selectedDs) <- NA
  
  #convert selected Ds to list
  DsList <- split(t(selectedDs), rep(1:ncol(selectedDs), each = ncol(selectedDs)))
  
  #apply vector to frequency vectors using VecToFrq
  vecList <- lapply(DsList, VecToFrq, maxK)
  
  #make a vector for frequencies
  freq <- unlist(vecList)
  
  #get index values by repeating
  maxKPlus <- maxK + 1
  listLen <- length(vecList)
  vals <- rep(0:maxK, times = listLen)
  
  #similarly get the names
  indNames <- rep(rownames(selectedDs), each = maxKPlus)
  
  #make a dataframe
  DFrame <- data.frame(Ind_Name = indNames, Value = vals, Frequency = freq)
    
  DFrame  
  
}