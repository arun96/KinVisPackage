#' Helper function to convert a Dataframe into a Bar Plot
#'
#' @param The data frame, individual names, and highlighted data (defaults to NULL)
#' @return A bar plot
#' @export
#' @examples
#' BarDFToPlot(Dataframe, Names, NULL)


BarDFToPlot <- function(Ds_df, Individual_Name, hglData = NULL){
  #COLORS AND NAMES TO USE FOR LEGEND
  lineageColors=c('dodgerblue3', 'darkorange', 'forestgreen', 'firebrick','mediumpurple'
                  , 'sienna4', 'orchid', 'seashell4','yellow3','gray95','gray95','gray95');
  
  lineageNames=c("0 - Self; Mono-zygote twins",
                 
                 "1 - Parent-offspring, Full sib.",
                 
                 "2 - Half-sib.; Unc.-neph; Dbl. 1st cous.",
                 
                 "3 - Grdchild-grdparent; 1st cous.",
                 
                 "4",
                 
                 "5 - 2nd cousins",
                 
                 "6","7","8","9...")
  
  #PLOT
  if (is.null(hglData)){
    barPlotI <- ggplot(Ds_df, aes(x=Individual_Name, y=Frequency)) +
      #scale_y_continuous(limits = c(0,1)) +#, breaks = ((0:1)/20) ) +
      geom_bar(width = 0.5
               , stat = "identity", aes(text = paste0("Frequency: "
                                                    ,  as.factor(Frequency))
                                      , fill = factor(Value))) +
      scale_fill_manual(labels = lineageNames
                        , values = lineageColors) +
      labs(fill = "Lineage") +
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            legend.position = "none"
      ) +
      coord_flip()
  }
  else{
    #get places at which the selected pop names are
    temp <- as.character(Ds_df$Ind_Name %in% hglData)
    
    #replace TRUE with Selected and FALSE with Un-Selected
    temp[temp == "FALSE"] <- "Un-Selected"
    temp[temp == "TRUE"] <- "Selected"
    
    #make a new "Chosen" column with the selected and unselected values
    Ds_df$Chosen = factor(temp)
    
    barPlotI <- ggplot(data = Ds_df) +
      #scale_y_continuous(limits = c(0,1)) +#, breaks = ((0:1)/20) ) +
      geom_bar(mapping = aes(x=Individual_Name
                             , y=Frequency
                             , text = paste0("Frequency: "
                                                    ,  as.factor(Frequency))
                             , fill = factor(Value)
                             , alpha = Chosen
                             )
               #, color = "black"
               , stat = "identity"
               , width = 0.5) +
      scale_alpha_manual(values= c(1, 0.2)) +
      scale_fill_manual(labels = lineageNames
                        , values = lineageColors) + 
      coord_flip() +
      theme(axis.title.x = element_blank()
            , axis.title.y = element_blank()
            , legend.position = "none"
      )
  }
}