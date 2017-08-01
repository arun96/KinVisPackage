#' Helper function for MDS Plots
#'
#' @param x, y, the radius, and the number of points.
#' @return Data on the MDS Plot
#' @export
#' @examples
#' circlePts(x,y, radius = r, pts = z)

circlePts <- function(xCent, yCent, radius, pts =100){
  radPts <- seq(0,2*pi,length.out = pts)
  x <- xCent + (radius * sin(radPts))
  y <- yCent + (radius * cos(radPts))
  
  circleData <- data.frame(x, y)
  names(circleData) <- c("X", "Y")
  
  circleData
}

