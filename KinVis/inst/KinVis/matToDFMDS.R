#' Helper function to convert matrix to MDS data frame
#'
#' @param Data, individual names
#' @return List with values for MDS
#' @export
#' @examples
#' matToDFMDS(DS, Names)

matToDFMDS <- function(Ds, iNames){
  
  #change indNames to just I1, I2, I3.....
  indnames <- sapply(strsplit(iNames, "\\_"), `[[`, 2)
  
  #get the individual identities
  Individual_Identity <- indnames
  
  if (identical(Ds, 0)){
    MDS_df <- data.frame(P_Name = Individual_Identity, X = 0, Y = 0)
    
    eigenValues <- 0
  }
  else if(nrow(Ds) == 2){
    MDS_df <- data.frame(P_Name = Individual_Identity, X = c(-0.5, 0.5), Y = c(0, 0))
    
    eigenValues <- c(0, 0)
  }
  else{
    #get the MDS values
    MDSInfo <- cmdscale(Ds, eig = TRUE)
    
    #the matrix to be used is stored in 'points'
    xMDS <- as.vector(MDSInfo$points[,1]) #col 1 is x points
    if (ncol(MDSInfo$points) == 1){
      yMDS <- rep(0, times = nrow(MDSInfo$points))
    }
    else{
      yMDS <- as.vector(MDSInfo$points[,2]) #col 2 is y points
    }
    
    #convert the matrix MDS into data frame
    MDS_df <- data.frame(P_Name = Individual_Identity, X = xMDS, Y = yMDS)
    
    eigenValues <- MDSInfo$eig
  }
  
  returnList <- list(MDS_DF = MDS_df, Ind_Names = Individual_Identity, eigValues = eigenValues)
  
  returnList
}