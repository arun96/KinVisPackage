#' Helper function to get population name from a file
#'
#' @param The filename containing population data
#' @return The population name
#' @export
#' @examples
#' getPopulationName(filename)

getPopulationName <- function(fileName){       #returns a character class
  
  if(is.null(fileName)) { return(NULL) }
  i <- 1
  while (substr(fileName, i,i) != ""){      #substr to get sub string 
    if (substr(fileName, i, i) == "-"){     #from fileName string
      popName <- substr(fileName, 1, i-1)
    }
    i = i+1
  }
  popName
}