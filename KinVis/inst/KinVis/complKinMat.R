#' Function that creates the kinship matrix
#'
#' @param Kinship Matrix Data, and the Population Name
#' @return A new matrix
#' @export
#' @examples
#' complKinMat(matrix, CLM)


complKinMat <- function (kinMatData, popName){                #returns a newly formed
                                                              #matrix
  
  #get ith and jth indices as a vector
  kinValsi <- as.vector(kinMatData[[1]])
  kinValsj <- as.vector(kinMatData[[3]])

  #apply gsub to subset out the numbers
  indI <- as.integer(sub(popName,"",kinValsi))
  indJ <- as.integer(sub(popName,"",kinValsj))
  
  #get the rows of computed matrix
  rows <- length(unique(indI)) + 1
  
  #matrix to insert values into
  kinshipMatrix <- matrix(nrow = rows, ncol = rows)

  #loop through this vector and assign values to the matrix
  for (i in 1:length(indI)){

    #assign PI_HAT values (9th column of the mat data)
    kinshipMatrix[indI[i], indJ[i]] <- kinMatData[[9]][i]
  }
  
  #put the lower triangular matrix values too
  kinshipMatrix <- as.matrix(  forceSymmetric(kinshipMatrix, uplo = "U")  )
  
  #set diagonal to 1
  diag(kinshipMatrix) <- 1
  
  if (anyNA(kinshipMatrix)){
    print("complKin Matrix function has created NA's. Returned NULL.")
    return(NULL)
  }
  else {
    kinshipMatrix
  }
}