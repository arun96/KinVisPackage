#' Helper function for MDS Plots
#'
#' @param The x and y for the MDS Plot
#' @return The points on the plot
#' @export
#' @examples
#' circleData(x, y)

circleData <- function(xMDS, yMDS){
  
  #----------------------------------CIRCLE RADIUS AND CENTER--------------------------------#
  options(digits = 22)
  
  if (length(xMDS) < 3){
    centroidX <- 0
    centroidY <- 0
    
    radius <- 1
  }
  else{
    #for all the points centroid comes out to be 0
    centroidX <- (max(xMDS) + min(xMDS))/2
    centroidY <- (max(yMDS) + min(yMDS))/2
    
    #first include the centroid (first value) and all points as a matrix.
    #make a distance matrix and get the max value of the first column
    #as it gives the max distance between centroid and further most point.
    pointsMat <- cbind(c(centroidX, xMDS), c(centroidY, yMDS))                 
    distMat <- as.matrix(dist(pointsMat))
    
    #use the max of distMat with an offset of 10% as the radius
    r <- as.numeric(max(distMat[1, ]))
    radius <- 1.3 * r           #+10% offset
  }
  
  #function to calculate needed points
  circlePoints <- circlePts(centroidX, centroidY, radius, pts = 1000)
  
  circlePoints
  #----------------------------------CIRCLE RADIUS AND CENTER--------------------------------#
}