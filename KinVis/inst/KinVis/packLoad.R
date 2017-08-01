#' Helper function to load packages
#'
#' @param Package/list of packages
#' @return None - packages are loaded
#' @export
#' @examples
#' packLoad(list of packages)

packLoad <- function(pkg){
  
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  
  #package not installed i.e. it is new install it
  if (length(new.pkg)) {
    install.packages(new.pkg, dependencies = TRUE)
  }
  
  #for multiple packages
  sapply(pkg, require, character.only = TRUE)
}