#' Helper function to convert MS matrix to DS matrix
#'
#' @param MS, toChange boolean
#' @return DS
#' @export
#' @examples
#' MSToDS(MS)

MsToDs <- function (Ms, toChange = FALSE){
  
  #copy of Ms matrix
  MsCpy <- Ms
  
  #first make all values between 0 to 1
  #values < = 0 go to 0
  MsCpy[ MsCpy <= 0] <- NA

  #for values greater than 1 go to 1
  MsCpy[ MsCpy > 1] <- 1

  #now apply log calculations
  LogMsCpy <- floor((0.5 - (log(MsCpy, base = 2))))

  #if this function is applied only on one matrix toChange is TRUE
  if (toChange == TRUE){
    C <- max(LogMsCpy, na.rm = TRUE)
    
    LogMsCpy <- fillNA(LogMsCpy, C)
    
    LogMsCpy
  }
  else{
    LogMsCpy
  }

  # Ds <- matrix(NA, nrow = nrow(Ms), ncol = ncol(Ms))              #create a matrix Ds
  # #NA values only
  # 
  # #zeroVec <- vector("numeric")                                    #create a vector that
  # #stores indices of the
  # #input matrix having
  # #values zero
  # 
  # #zeroVecCount <- 1                                               #index counter for
  # #the zero vector
  # 
  # #going through the matrix Ms
  # for (i in 1:nrow(Ms)){
  #   for (j in 1:ncol(Ms)){
  # 
  #     #calculate log for values in (0,1]
  #     if ((Ms[i, j] > 0)&&(Ms[i, j] < 1)){
  #       Ds[i, j] <- floor((0.5 - (log((Ms[i, j]), base = 2))))
  #     }
  # 
  #     #greater then 1 values are assigned 1
  #     else if (Ms[i, j] >= 1){
  #       Ds[i, j] <- 0
  #     }
  # 
  #     #lesser then 0 values (negative) are assigned 0
  #     #and then all the 0 values get a max value of matrix
  #     else if (Ms[i, j] <= 0){
  #       Ds[i, j] <- NA                                            #temporarily assign the 0 values
  #                                                                 #as NA so that we can easily use
  #                                                                 #max and na.rm
  # 
  #       #-----zeroVec[zeroVecCount] <- i
  #       #-----zeroVecCount <- zeroVecCount + 1
  #       #-----zeroVec[zeroVecCount] <- j
  #       #-----zeroVecCount <- zeroVecCount + 1
  #     }
  #   }
  # }
  # 
  # #-----C <- 1 + max(Ds, na.rm = TRUE)                                  #find max value of matrix
  # #after removing NA values
  # 
  # #go through the zeroVec to assign values with
  # #zero the max + 1 value of matrix
  # 
  # #-----k <- 1
  # 
  # #-----while (k <= length(zeroVec)){
  # 
  #   #-----zeroIndexi <- zeroVec[k]                                      #index i for zero values
  #   #-----zeroIndexj <- zeroVec[k + 1]                                  #index j for zero values
  # 
  #   #for values as 0 assign the max + 1 value
  #   #-----if (Ms[zeroIndexi, zeroIndexj] <= 0){
  #     #-----Ds[zeroIndexi, zeroIndexj] <- C
  #   #-----}
  # 
  #   #incrementing k by 2 because we need
  #   #every 2 values for i and j indices
  #   #-----k <- k + 2
  # #-----}
  # 
  # #CHECKS IF Ds HAS ANY NA VALUES:
  # #PRINTS A WARNING IF IT DOES HAVE EVEN ONE NA
  # #-----if (any(is.na(Ds))){
  #   #-----print ("WARNING: Ds has NA values!!!")
  # #-----}
  # #-----else{
  #   Ds
  # #-----}
}
