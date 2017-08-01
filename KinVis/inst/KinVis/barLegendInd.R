#' Helper function to generate a Legend, provided a K-value
#'
#' @param The K-value
#' @return A legend object
#' @export
#' @examples
#' barLegendInd(K-Value)

barLegendInd <- function(kVal){
  
  ###                                 VALUE -     COLOUR
  #                                     0   -   dodgerblue3
  #                                     1   -   darkorange
  #                                     2   -   forestgreen
  #                                     3   -   firebrick
  #                                     4   -   mediumpurple
  #                                     5   -   sienna4
  #                                     6   -   orchid
  #                                     7   -   seashell4
  #                                     8   -   yellow3
  #                                     9   -   gray95
  #                                     10  -   gray95
  #                                     11  -   gray95
  
  #COLORS AND NAMES TO USE FOR LEGEND
  lineageColors= rev(c('dodgerblue3', 'darkorange', 'forestgreen', 'firebrick','mediumpurple'
                  , 'sienna4', 'orchid', 'seashell4','yellow3','gray95'))
  
  lineageNames=c("0 - Self; Mono-zygote twins",
                 
                 "1 - Parent-offspring, Full sib.",
                 
                 "2 - Half-sib.; Unc.-neph; Dbl. 1st cous.",
                 
                 "3 - Grdchild-grdparent; 1st cous.",
                 
                 "4",
                 
                 "5 - 2nd cousins",
                 
                 "6","7","8","9...")
  
  #subset only the required K values
  if(kVal > 9) kVal <- 9
  kValPls <- kVal + 1
  lineageColors <- tail(lineageColors, kValPls)
  lineageNames <- lineageNames[1:kValPls]
  
  #make a data frame for these colours
  barLgndDF <- data.frame(Value = rep(1, times = kValPls), LinNam = rev(lineageNames))
  
  #order graph according to the way in data
  barLgndDF$LinNam <- as.character(barLgndDF$LinNam)
  barLgndDF$LinNam <- factor(barLgndDF$LinNam, levels=barLgndDF$LinNam)
  
  #plot the data frame
  lgndPlot <- ggplot(data = barLgndDF
                 , mapping = aes(x = LinNam
                                 , y = Value
                                 , text = "Select this colour on heatmap     "
                                 )
                 ) +
  scale_y_discrete(limits = c(0, 1)) +
  geom_bar(stat = "identity"
           , fill = lineageColors) +
  coord_flip() +
  labs(title = " ") +
  theme(
        axis.title.x = element_blank()
        , axis.title.y = element_blank()
        , axis.text.x = element_blank()
        , axis.text.y = element_text(size = 7)
        , axis.line = element_blank()
        , axis.ticks = element_blank()
        , panel.grid.major = element_blank()
        , panel.grid.minor = element_blank()
        , panel.background = element_blank()
        , legend.position = "none"
    )
}