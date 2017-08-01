#' Helper function to convert a file into a data frame of populations
#'
#' @param The loaded matrix, max K value, and the type of plot
#' @return A data frame of the individuals
#' @export
#' @examples
#' fnameToPopDF(matrix, k-value, type of plot)

fnameToPopDF <- function(loadedList, maxKVal = NULL, plotType = NULL){   #takes fileNames 
                                                                        #and makes data frames
  if (is.null(plotType)){
    #convert kinship to lineage dissimilarity matrix Ds
    Ds <- multiMsToDs(loadedList)
    
    #convert dissimilarity matrix to vectors Vs
    Vs <- multiDsToVs(Ds)
    
    if (is.null(maxKVal)){
      #normalize Vs 
      normalizeVs <- lapply(Vs, normVector)
      
      #using vec list to get circle and MDS data frames
      MDS_list <- vecToDFMDS(normalizeVs)
    }
    else{
      #use this Vs and max K val to plot a stacked bar graph
      stackPlot_df <- stackedBarDF(Vs, maxKVal)
    }
  }
  else{
    #no computation for box plot
    if (plotType == "Box"){
      #use normalized vs for data frame
      box_DF <- boxPopDF(loadedList)
    }
  }
}