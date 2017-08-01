#' Helper function to convert a file into a data frame of individuals
#'
#' @param The loaded matrix, population file name, individual names, type of plot, origInd, maximum value individuals, the selected process, the number of selected, and the zoom of the heatmap
#' @return A data frame of the individuals
#' @export
#' @examples
#' fnameToIndDF(matrix, filename, individuals names, type of plot)


fnameToIndDF <- function(loadedMat, popFName, indNames, plotType, origInd = NULL
                         , maxKValInd = NULL, prcs =NULL, slctNum = NULL, heatZmLvl = NULL){
  
  onlyName <- getPopulationName(popFName)
  
  if (plotType == "MDS Plot"){
    #convert kinship to lineage dissimilarity matrix Ds
    Ds <- MsToDs(loadedMat, TRUE)
    
    #change row names or col names only if they are set by default (not given by user)???????
    #if (is.null(rownames(Ds))){
      #give names for the Ds matrix 
      rownames(Ds) <- paste0(onlyName, "_I", 1:nrow(Ds))
      colnames(Ds) <- paste0(onlyName, "_I", 1:ncol(Ds))
    #}
  
    #only take the info for ones with indNames chosen
    selectedDs <- Ds[indNames, indNames]
    
    #use this selectedDs to get Data Frame
    MDS_list <- matToDFMDS(selectedDs, indNames)
  }
  else if (plotType == "HeatMap"){
    
    #change row names or col names only if they are set by default (not given by user)????????
    #if (is.null(rownames(loadedMat))){
      
      #names for loadedMat matrix: (I1, I2) representing individual 1 and 2 ...
      rownames(loadedMat) <- paste0(onlyName, "_I", 1:nrow(loadedMat))
      colnames(loadedMat) <- paste0(onlyName, "_I", 1:ncol(loadedMat))
    #}
    
    #get  slected loadedMat part
    if (is.null(origInd)){ origInd <- indNames } 
    slctdLdMat <- loadedMat[origInd, origInd]
    
    #use this matrix to generate heamap for BN for the IBD process
    BDBS_To_Heat(slctdLdMat, onlyName, prcs, indNames, slctNum, heatZmLvl)
    
  }
  else{
    #convert kinship to lineage dissimilarity matrix Ds
    Ds <- MsToDs(loadedMat, TRUE)
    
    #convert Ds to Vs list
    Vs <- DsToVs(Ds, max(Ds))
    
    #here Vs is vector convert it to list
    Vs <- list(Vs)
    names(Vs) <- popFName
    
    #change row names or col names only if they are set by default (not given by user)???????
    #if (is.null(rownames(Ds))){
      #give names for the Ds matrix 
      rownames(Ds) <- paste0(onlyName, "_I", 1:nrow(Ds))
      colnames(Ds) <- paste0(onlyName, "_I", 1:ncol(Ds))
    #}
    
    #only take the info for ones with indNames chosen
    selectedDs <- Ds[indNames, indNames]
    
    if (plotType == "Bar GraphP"){
      #just plot
      plot <- Ds_dfToPlot(selectedDs, Vs, plotType, maxKValInd)
      
      plot
    }
    else if (plotType == "Bar GraphI"){
      #get the data frame to plot
      Ds_df <- Ds_dfToPlot(selectedDs, Vs, plotType, maxKValInd)
      
      Ds_df
    }
  }
}