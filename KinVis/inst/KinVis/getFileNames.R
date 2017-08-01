#' Helper function to get filenames from data directory
#'
#' @param No inputparamters
#' @return List of file names
#' @export
#' @examples
#' getFileNames()

getFileNames <- function(){
  
  #to return back to current directory to store it
  currentDir <- getwd()
  
  #store all the file names
  fNames <- list.files("C:/Users/QCRI/Desktop/GIVEN DATA")
  
  #divide into a list of different kinds of processes
  fNamesList <- list(   indep = fNames[vecGetProcName(fNames) == "indep"]
                      , indep.hBN = fNames[vecGetProcName(fNames) == "indep.hBN"]
                      , indep.hIBS = fNames[vecGetProcName(fNames) == "indep.hIBS"]
                      , pairwise = fNames[vecGetProcName(fNames) == "pairwise"]
                      , pairwise.hBN = fNames[vecGetProcName(fNames) == "pairwise.hBN"]
                      , pairwise.hIBS = fNames[vecGetProcName(fNames) == "pairwise.hIBS"])
  
  fNamesList
}