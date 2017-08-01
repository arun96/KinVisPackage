#' Function to convert covariance matrix into a plot
#'
#' @param The covariance matrix, and any highlighted data
#' @return The plot
#' @export
#' @examples
#' covToPlot(Matrix, Highlighted Data)

covToPlot <- function(covMatrix, hglData = NULL){
  
  #---------------------------WORKING HEATMAP-------------------------#
  if (is.null(hglData)){
    #melt the required data
    meltedCovmat <- melt(covMatrix)
    
    #plot using the melted values
    plot <- ggplot(data = meltedCovmat, aes(x=Var1, y=Var2)) +
      geom_raster(aes(fill = value)) +
      scale_fill_gradient(name="Individual\nCorrelation") +
      coord_equal(ratio = 1) +
      theme(
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank()
      )
  }
  else{
    
    return(NULL)
  }
}