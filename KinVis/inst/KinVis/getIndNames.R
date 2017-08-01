#' Helper function to get the names of individuals
#'
#' @param The filename of the population
#' @return A list of individual names
#' @export
#' @examples
#' getIndNames(filename)

getIndNames <- function(fileName){         #fileName will give info on the population
  #PUT ALL LOADED DATA IN ONE OBJECT TO EASE ACCESS. THIS OBJECT WILL BE USED HERE ONCE IT IS MADE.
  if (is.null(fileName) || (length(fileName) > 1)) return(NULL)
  
  #extension name if genome then header TRUE otherwise FALSE
  extnNm <- file_ext(fileName)
  
  dataPath <- .dataLocation
  
  print(dataPath)
  
  currentDir <- getwd()
  setwd(dataPath)
  if (extnNm == "genome"){
      readData <- read.table(fileName, header = TRUE, sep = "\t")
    }
    else{
      readData <- read.table(fileName, sep = "\t")
    }
    
  setwd(currentDir)
  
  #find the population number
  popNumber <- nrow(readData)
  
  #FOR .GENOME WE NEED TO GET THE ACTUAL ROWS
  
  popnName <- getPopulationName(fileName)           #get population name from filename
  prcsName <- getProcessName(fileName)              #get process info from filename
  
  noExtName <- paste(popnName, prcsName, sep = "-") #name with no extension
  
  extName <- gsub(noExtName,"",fileName)            #replace non-extension part with ""
                                                    #(empty chracaters) to get extension
  if (extName == ".genome"){
    popNumber <- floor(sqrt(popNumber * 2)) + 1
  }
  
  #the individual names will be I1 to I<popNumber>
  indNames <- paste0(popnName, "_I", 1:popNumber)
  
  setwd(currentDir)
  
  indNames
}

