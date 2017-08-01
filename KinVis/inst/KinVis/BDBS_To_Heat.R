#' Helper function to generate a heatmap
#'
#' @param The loaded matrix, population file name, process that has been selected, the individual names, the selected value, the zoom level (now obsolete)
#' @return An object that is used to generate the heatmap
#' @export
#' @examples
#' BDBS_To_Heat(matrix, population name, process, individual names, selected value)

BDBS_To_Heat <- function(loadedMat, popName, procs, iNames, slctInt = NULL, heatZmLvl = NULL){
  
  #set rcInds to null it will change when the 
  #required pairs need to be highlighted
  rcInds <- NULL
  
  #process names differentiated to show as graph title
  if (procs == "indep"){
    procsName1 <- "IBD"
    procsName2 <- "indep"
  }
  else if (procs == "pairwise"){
    procsName1 <- "IBD"
    procsName2 <- "pairwise"
  }
  else if (procs == "indep.hIBS"){
    procsName1 <- "IBS"
    procsName2 <- "indep"
  }
  else{
    procsName1 <- "IBS"
    procsName2 <- "pairwise"
  }
  
  #define lineage colors to be used in heat map
  lineageColors=c('dodgerblue3', 'darkorange', 'forestgreen', 'firebrick',
                  'mediumpurple', 'sienna4', 'orchid', 'seashell4','yellow3','gray95');
  
  #---------------------GRAPH COMPUTATION BEGINS------------------
  #cells containing 0
  ind0 <- which(loadedMat == 0)
  
  #get minimum of non-zero value
  indp <- which(loadedMat > 0)
  mn <- min(loadedMat[indp])
  
  #replace every 0 by this min
  loadedMat[ind0] <- mn
  
  #compute lineage
  datalineage <- (-log2(loadedMat))
  
  #get rounded Value
  datalineageround <- round(-log2(loadedMat))
  
  #any cells greater than 8 get values 9
  indsup <- which(datalineageround > 8)
  datalineage[indsup] <- 9
  datalineageround[indsup] <- 9
  
  #compute dendrogram on non rounded lineage
  dfdist <- as.dist(datalineage)
  
  hr <- hclust(dfdist, method = "single")
  dhc <- as.dendrogram(hr)
  
  #vectors to use on margins of heat map
  dtlr <- datalineageround
  
  #set diagonal of this matrix to high value say 100 for min
  diag(dtlr) <- 100
  #vector of minimum values for each individual
  indmincol <- as.vector(1 + ( apply(dtlr, 2, min) ))
  
  #diag to -100 for max
  diag(dtlr) <- -100
  #vector of maximum values for each individual
  indmaxcol <- as.vector(1 + ( apply(dtlr, 2, max) ))
  
  #order the heatmap based on the dendrogram
  rank <- rev(order.dendrogram(dhc))
  
  if (is.null(slctInt)){
    if (length(iNames) != 1){
      
      #heatmap 2 function
      hMap <- heatmap.2(datalineageround,
                breaks=c(-0.5,0.5,1.5,2.5,3.5,4.5,5.5,6.5,7.5,8.5,9.5),
                key = FALSE,
                trace="none",
                Rowv=dhc,
                Colv=rev(dhc),
                dendrogram='none',
                col = lineageColors,
                ColSideColors = as.character(lineageColors[rev(indmaxcol)]),
                RowSideColors = as.character(lineageColors[indmincol]),
                scale="none",
                margins = c(5,10),
                #lwid = c(0,2,4,8),
                #lhei = c(0,2,4,8)
                main = paste0(procsName1, "-", popName, "-", procsName2)
      )
    }
    else{
      
      #make matrix of needed characters
      slctMat <- matrix(data = "", nrow = nrow(loadedMat), ncol = ncol(loadedMat))
      
      #get the index number from iNames
      indNum <- as.numeric( str_extract(iNames, "[[:digit:]]+") )
      
      #change the only row to "O"
      slctMat[indNum, ] <- "O"
      # #similarly change the column to "O"
      # slctMat[, indNum] <- "O"
      
      #-----ROW COLUMN SELECTED INDICES-----
      #check which positions have "O"
      rcInds <- as.data.frame(which(slctMat == "O", arr.ind = TRUE))
      #-----
      
      #heatmap 2 function for selected ones
      hMap <- heatmap.2(datalineageround,
                breaks=c(-0.5,0.5,1.5,2.5,3.5,4.5,5.5,6.5,7.5,8.5,9.5),
                key = FALSE,
                cellnote = slctMat,
                notecex = 8,#heatSlctSize(heatZoom = heatZmLvl, numInds = nrow(loadedMat)),
                trace="none",
                Rowv=dhc,
                Colv=rev(dhc),
                dendrogram='none',
                col = lineageColors,
                ColSideColors = as.character(lineageColors[rev(indmaxcol)]),
                RowSideColors = as.character(lineageColors[indmincol]),
                scale="none",
                margins = c(5,10),
                #lwid = c(0,2,4,8),
                #lhei = c(0,2,4,8)
                main = paste0(procsName1, "-", popName, "-", procsName2)
      )
    }
  }
  #heatmap SELECTION
  else{
    
    #get numbers of individuals to select the rows
    #get numbers from ind names
    indNums <- as.numeric( str_extract(iNames, "[[:digit:]]+") )
    
    #get indices of a number
    dataMat <- datalineageround
    indxSlct <- which(dataMat == slctInt)
    indxNSlct <- which(dataMat != slctInt)
    
    #make matrix of needed characters
    slctMat <- matrix(data = "", nrow = nrow(loadedMat), ncol = ncol(loadedMat))
    
    #slct gets "O" and non slct gets ""
    slctMat [ indxSlct ] <- "O"
    slctMat [indxNSlct] <- ""
    
    #-----ROW COLUMN SELECTED INDICES-----
    #check which positions have "O"
    rcInds <- as.data.frame(which(slctMat == "O", arr.ind = TRUE))
    #-----
    
    #heatmap 2 function for selected ones
    hMap <- heatmap.2(datalineageround,
              breaks=c(-0.5,0.5,1.5,2.5,3.5,4.5,5.5,6.5,7.5,8.5,9.5),
              key = FALSE,
              cellnote = slctMat,
              notecex = 8,#heatSlctSize(heatZoom = heatZmLvl, numInds = nrow(loadedMat)),
              trace="none",
              Rowv=dhc,
              Colv=rev(dhc),
              dendrogram='none',
              col = lineageColors,
              ColSideColors = as.character(lineageColors[rev(indmaxcol)]),
              RowSideColors = as.character(lineageColors[indmincol]),
              scale="none",
              margins = c(5,10),
              #lwid = c(0,2,4,8),
              #lhei = c(0,2,4,8)
              main = paste0(procsName1, "-", popName, "-", procsName2)
    )
  }
  
  #--------get the row, cols indices and the number selected in a data frame------
  if (!is.null(rcInds)){
    
    #get the vector of rows and subset it with matrix to get the names of rows
    slctRows <- rownames(loadedMat)[ as.vector(rcInds$row) ]
    slctCols <- colnames(loadedMat)[ as.vector(rcInds$col) ]
    
    #repeat the required lineage valuewhose rows and cols were found
    linVals <- rep(slctInt, times = length(rcInds$row))
    
    #make a data frame
    pair_DF <- data.frame(Source = slctRows, Target = slctCols, Lineage_Value = linVals)
  }
  else{
    pair_DF <- NULL
  }
  #---------
  
  #return a list of heatMap and row, column matrix of pairs data
  retList <- list(HeatMap = hMap, PairsData = pair_DF)
}