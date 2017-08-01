#' Function that runs the application
#'
#' @param Absolute path to the data folder
#' @return Nothing - executes the application
#' @export
#' @examples
#' runKinVis(C://Data)

runKinVis <- function(dataPath){
  .GlobalEnv$.dataLocation <- dataPath
  on.exit(rm(.dataLocation, envir=.GlobalEnv))
  #shiny::runApp()
  shiny::runApp(appDir = system.file("KinVis", package = "KinVis"))
}