#' Helper function to load multiple population data
#'
#' @param File names vector, data paths vector, process name, file extension
#' @return Matrix with data
#' @export
#' @examples
#' loadMultiPopData(file names, data paths, process, extension)

loadMultiPopData <- function (fileNameVec, fileDPVec, prcsName, extn){ #file DP Vec is data paths vector
  
  
  #go to the directory with only matrix files present
  
  multiMatData <- list()   #declaring multiMatData
  
  #specific actions for different file names
  if (extn == "genome"){
    for (i in seq_along(fileDPVec)){
      #read genome data
      genomeKinData <- read.table(fileDPVec[i], header = TRUE)
      
      #get population name
      popnName <- getPopulationName(fileNameVec[i])
      
      #get the actual kinship matrix
      genomeMatrix <- complKinMat(genomeKinData, popnName)
      
      #add to the list
      multiMatData <- c(multiMatData, list(genomeMatrix))
    }
  }
  else if (extn == "kinf"){
    #process name of hIBS has different computation
    if ((prcsName == "indep.hIBS")||(prcsName == "pairwise.hIBS")){
      for(i in seq_along(fileDPVec)){
        dataMatrix <- read.table(fileDPVec[i])
        
        #if it is a hIBS matrix convert the values to 0 to 1 
        #(originally it has values as 1 or close to 1)
        kinfMatrix <- hIBSMsTo0To1(dataMatrix)
        
        #add to the list
        multiMatData <- c(multiMatData, list(kinfMatrix))
      }
    }
    else{ #otherwise it is indep.hBN or pairwise.hBN (double the matrix)
      for(i in seq_along(fileDPVec)){
        kinfMatrix <- read.table(fileDPVec[i])
        
        kinfMatrix <- 2*kinfMatrix
        
        #add to the list
        multiMatData <- c(multiMatData, list(kinfMatrix))
      }
    }
  }
  else if (extn == "rdata"){
    for (i in seq_along(fileDPVec)){
      #read matrix
      rdataMatrix <- read.table(fileDPVec[i])
      
      #add in the list
      multiMatData <- c(multiMatData, list(rdataMatrix))
    }
  }
  
  #check for other file types or no files at all
  if (identical(multiMatData, list())){
    stop("No files present in the current directory!!!")
  }
  else{
    names(multiMatData) <- fileNameVec
    
    multiMatData
  }
}