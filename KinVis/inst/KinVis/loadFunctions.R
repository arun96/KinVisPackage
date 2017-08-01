#' Helper function to load relevant functions
#'
#' @param No input paramters
#' @return No output - loads the functions
#' @export
#' @examples
#' loadFunctions()

loadFunctions <- function(){
  
  source("fnameToPopDF.R")
  source("fnameToIndDF.R")
  #for hBN to dend not using fnametoind
  source("hBN_To_Dend.R")
  
  source("getFileNames.R")
  source("getIndNames.R")
  
  #load data
  source("loadMultiPopData.R")
  source("complKinMat.R")
  
  #get Populatiopn or Process name in single or a vector
  source("getPopulationName.R")
  source("getProcessName.R" )
  source("vecGetPopName.R")
  source("vecGetProcName.R")
  
  #max values from the lists to replace the zeroes
  source("listMaxVal.R")
  
  #euclidean distance matrix calculator (NOT SURE WHERE I USED. MOSTLY USED dist() FUNCTION)
  source("vecEucDistCalc.R")
  source("vecEucDistances.R")
  #---------------------------------------------------------------------------------------->
  
  #Ms to Ds
  source("MsToDs.R")
  source("fillNA.R")
  source("multiMsToDs.R")
  
  #special for hBN
  source("maxNZeroIndx.R")
  source("hBNToMDSDF.R")
  #individuals heatmap hBN - USES HBN_TO_DEND.R
  
  #special calculation for IBS matrix
  source("hIBSMsTo0To1.R")
  source("multihIBSMsTo0To1.R")
  
  #Ds to Vs
  source("DsToVs.R")
  source("multiDsToVs.R")
  
  #normalize vector
  source("normVector.R")
  
  #MDS plot Populations
  source("vecToDFMDS.R")
  source("circlePts.R")
  source("circleData.R")
  source("DFToMDSPlot.R")
  
  #Stacked Bar Populations
  source("vecToDataFrame.R")
  source("stackedBarDF.R")
  source("DFToBarPlot.R")
  
  #Box Plot Populations
  source("matToUpTriVec.R")
  source("boxPopDF.R")
  source("DFToBoxPlot.R")
  
  #INDIVIDUALS PART
  #individuals constant bar graph
  source("barLegendInd.R")
  
  #MDS plot Individuals plot
  source("matToDFMDS.R")
  
  #heatmap or dendrogram Individuals
  source("BDBS_To_Heat.R")
  source("covToDist.R")
  source("covToPlot.R")
  #size selection function
  source("heatSlctSize.R")
  
  #Stacked Bar (2 graphs) Individuals
  source("VecToFrq.R")
  source("DsToDataFrame.R")
  source("Ds_dfToPlot.R")
  source("popBarPlotInd.R")
  source("BarDFToPlot.R")
  }