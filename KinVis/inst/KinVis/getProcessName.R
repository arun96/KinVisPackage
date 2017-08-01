#' Helper function to get the process name from an input file 
#'
#' @param File name
#' @return Process name
#' @export
#' @examples
#' getProcessName(Filename)

getProcessName <- function(fileName){                       #returns a character class name
  
  dotCount1 <- 0                                            #counts all number of dots
  hyphenPos <- 0
  
  i <- 1
  while (substr(fileName, i,i) != ""){                      #substr to get sub string 
    if (substr(fileName, i, i) == "."){                     #from file name string
      dotCount1 = dotCount1 + 1
    }
    if (substr(fileName, i, i) == "-"){                     #additionally find the
      hyphenPos <- i                                        #postion of hyphen
    }
    i = i+1
  }
  
  dotCount2 <- 0                                            #identify the last dot
                                                            #just before extension      
                                                            #of the file name 
  
  j <- 1
  while (substr(fileName, j,j) != ""){                      #substr to get sub string 
    if (substr(fileName, j, j) == "."){                     #from file name string
      dotCount2 = dotCount2 + 1
      if (dotCount1 == dotCount2){                          #found the dot just 
                                                            #before the extension
        procName <- substr(fileName, hyphenPos + 1, j-1)
      }
    }
    j = j+1
  }
  procName
}