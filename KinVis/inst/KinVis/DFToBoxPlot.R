#' Helper function to convert a data frame into a box plot
#'
#' @param The data frame to be plotted, and the means by which the plot will be ordered
#' @return A box plot
#' @export
#' @examples
#' DFToBoxPlot(Dataframe, Ordering)

DFToBoxPlot <- function(IBS_df, ordByMed){
  
  IBS_df$PopName<-factor(IBS_df$PopName)
  
  # IBS_df$POP.name.idpordered<-factor(IBS_df$POP.name
  #                                    ,levels=levels(IBS_df$POP.name)[order(medIBSindep)]
  #                                    ,ordered=TRUE)
  
  # indep boxplot
  boxPlot <- ggplot(IBS_df, aes(x=ordByMed
                          , y=IBS_df$Value )) +
                                   #,colour=SUPERPOP.name
             geom_boxplot() +
             coord_flip(ylim = c(0.85,1)) + 
             ylab("IBS") +  
             xlab("Population name") #+
             #scale_colour_discrete(name="Superpopulation")
}

