#' Helper function to convert a data frame into a MDS plot
#'
#' @param The data frame to be plotted, the points on the plot, label names, eigenvalues, and the selected values
#' @return An MDS plot
#' @export
#' @examples
#' DFToMDSPlot(MDS Dataframe, Points list, Name of Label, Eigenvalues, Selected Values)

DFToMDSPlot <- function(MDS_DF, circlePoints, Label_Name, eigVals, slctVals = NULL){
  
  if (identical(unique(eigVals), 0)){
    accurPerc <- 100
    
    titleCol <- "#228B22"
  }
  else {
    #calculate accuracy of the graph drawn
    accurPerc <- (sum( head(eigVals, 2) ) / sum( eigVals )) * 100
    
    #round the accuracy value to maximum two digits after decimal point
    accurPerc <- round(accurPerc, digits = 2)
    
    #set colour for title according to accuracy
    if (accurPerc < 85){
      titleCol <- "#a30000"
    }
    else{
      titleCol <- "#228B22"
    }
  }
  
  if (!is.null(slctVals)){
    
    #identify the Pop Names to select
    temp <- as.character(MDS_DF$P_Name) %in% slctVals

    #change true to Selected and false to Unselected
    temp[temp == "FALSE"] <- "Un-Selected"
    temp[temp == "TRUE"] <- "Selected"
    
    #make a new "Chosen" column with the selected and unselected values
    MDS_DF$Chosen = factor(temp)
    
    #plot graph using MDS and circle data frames (WITH GREYED OUT POINTS)
    plot1 <- ggplot(data = MDS_DF, mapping = aes(x = X, y = Y, text = P_Name)) + 
                geom_point(size = 0.3, aes(colour = MDS_DF$Chosen)) +
                geom_text(label = paste0("        ", Label_Name)
                         , size = 2.85, aes(colour = MDS_DF$Chosen)) +
                scale_colour_manual(values = c("black", "lightgrey")) +
                geom_path(data = circlePoints, mapping = aes(x = X, y = Y, text = NULL)) +
                labs(title = 
                            paste0( "MDS Scatter Plot (", accurPerc, "%", " Variance)")) +
                theme(axis.title.x = element_blank(),
                           axis.title.y = element_blank(),
                           axis.ticks = element_blank(),
                           axis.line = element_blank(),
                           axis.text = element_blank(),
                           panel.grid.major = element_blank(),
                           panel.grid.minor = element_blank(),
                           panel.background = element_blank(),
                           legend.position = "none",
                           plot.title = element_text(size = 9, colour = titleCol)
                ) +
                coord_fixed(ratio = 1) + coord_equal()
  }
  else{
    #plot graph using MDS and circle data frames
    plot1 <- ggplot(data = MDS_DF, mapping = aes(x = X, y = Y, text = P_Name)) + 
                geom_point(size = 0.3) +
                geom_text(label = paste0("        ", Label_Name)
                          , size = 2.85) +
                geom_path(data = circlePoints, mapping = aes(x = X, y = Y, text = NULL)) +
                labs(title = 
                            paste0( "MDS Scatter Plot (", accurPerc, "%", " Variance)")) +
                theme(axis.title.x = element_blank(),
                            axis.title.y = element_blank(),
                            axis.ticks = element_blank(),
                            axis.text = element_blank(),
                            axis.line = element_blank(),
                            panel.grid.major = element_blank(),
                            panel.grid.minor = element_blank(),
                            panel.background = element_blank(),
                            plot.title = element_text(size = 9, colour = titleCol)
                ) +
                coord_fixed(ratio = 1) + coord_equal()
  }
}

