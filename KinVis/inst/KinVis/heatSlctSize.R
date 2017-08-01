#' OBSOLETE - Helper function to help zoom in heatmap
#'
#' @param Current zoom, number of individuals
#' @return Selected size 
#' @export
#' @examples
#' heatSlctSize(Zoom, Number of Individuals)

heatSlctSize <- function(heatZoom, numInds){#function inputs = either width/height of heatmap zoom levels
                                            #and total number of individuals
  
  #define "O" size numbers for 5 (slct5) and 
  #100 (slct100) individuals based in zoom levels (400, 800, 1200, 1600)
  slct5 <- switch (as.character(heatZoom),
                    "400" = 7,
                    "800" = 16,
                    "1200" = 27,
                    "1600" = 35
                  )
  slct99 <- switch (as.character(heatZoom),
                     "400" = 0.5,
                     "800" = 1,
                     "1200" = 1.2,
                     "1600" = 1.8
                    )
  
  #calculate alpha value (this is used in formula calculation of size)
  alpha <- (numInds - 5) / (99 - 5)
  
  #Calculation of size
  slctSize <- (slct5 * (1 - alpha)) + (slct99 * alpha)
  
  #return result
  slctSize
}