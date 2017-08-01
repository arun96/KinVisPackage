#' Helper function to convert a data frame into a bar plot
#'
#' @param The data frame to be plotted, highlighted data, population name, and a name parameter (defaults to FALSE)
#' @return A bar plot
#' @export
#' @examples
#' DFToBarPlot(Dataframe, HighlightedData, Population)

DFToBarPlot <- function(toPlot_DF, highlightData = NULL, Population_Name, isName = FALSE){
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
  
  #values above 9 get 9 as number
  valVec <- as.numeric(toPlot_DF$K_Value)
  if (10 %in% valVec){
    toPlot_DF$K_Value[as.numeric(toPlot_DF$K_Value) > 9] <- 9
  }
  
  #PLOT
  if ( (is.null(highlightData)) ){ # || (length(highlightData) == 1)
    #plotting using ggplot and plotly for no selected data
    plot2 <- ggplot(toPlot_DF, aes(x = Population_Name, y = Normalized_Count)) +
      scale_y_continuous(limits = c(0, 1)) +
      geom_bar(stat = "identity", aes(#text = paste0("Population_Number: " , as.vector(toPlot_DF$popNumbers)),
                                      fill = K_Value
                                      )
               , width = 0.5) +
      scale_fill_manual(labels = lineageNames
                        , values = lineageColors) +
      coord_flip() +
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            panel.background = element_rect(fill = "gray95"),
            legend.position = "right"
      ) +
      guides(fill = guide_legend(title = "Lineage"
                                 , title.theme = element_text(size = 12, angle = 0)))
  }
  else{
    #if highlight data is a name
    if (isName == TRUE){
      #get vector of names from file names
      slct_Names <- highlightData
    }
    else{
      #get vector of names from file names
      slct_Names <- vecGetPopName(highlightData)
    }
    
    #get places at which the selected pop names are
    temp <- as.character(toPlot_DF$Pop_Name %in% slct_Names)
    
    #replace TRUE with Selected and FALSE with Un-Selected
    temp[temp == "FALSE"] <- "Un-Selected"
    temp[temp == "TRUE"] <- "Selected"
    
    #make a new "Chosen" column with the selected and unselected values
    toPlot_DF$Chosen = factor(temp)
    
    #plotting using ggplot and plotly for highlighted (LASSO SELECTED) data
    plot2 <- ggplot(data = toPlot_DF) +
      scale_y_continuous(limits = c(0, 1)) +
      geom_bar(mapping = aes(x = Population_Name
                             , y = Normalized_Count
                             # , text = paste0("Population_Number: "
                             #                 , as.vector(toPlot_DF$popNumbers))
                             , fill = K_Value
                             , alpha = Chosen
      )
      #, color = "black"
      , stat = "identity"
      , width = 0.5) +
      # scale_fill_brewer(name = "K VALUES"
      #                   , palette = "Paired") +
      
      scale_alpha_manual(values= c(1, 0.2), guide = "none") +
      scale_fill_manual(name = "Lineage"
                        , labels = lineageNames
                        , values = lineageColors) +
      coord_flip() +
      theme(axis.title.x = element_blank()
            , axis.title.y = element_blank()
            , legend.title = element_text(size = 9)
            , legend.title.align = 1,
            panel.background = element_rect(fill = "gray95"),
            legend.position = "right"
            #, aspect.ratio = 0.5
      ) +
      guides(fill = guide_legend(title = "Lineage"
                                 , title.theme = element_text(size = 12, angle = 0)))
  }
}