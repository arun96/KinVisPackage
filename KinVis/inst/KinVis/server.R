#-----source and load functions -----#
# print(getwd())
source("loadFunctions.R")
loadFunctions()
#-----load the rquired functions-----#

#-------------------------------------------GLOBAL VARIABLES-------------------------------------------#
#variable to store selected population for individual overview
popSlctd <- NULL
#variable to store population required for pairs download
pairsPop <- NULL
#variable to store constant starting zoom values for HeatMap
heatWidth <- 475
heatHeight <- 475
#variable to store constant starting zoom values for Bar Graph Individuals
barWidth <- 475
barHeight <- 475
#variables for changing zoom levels of heatmap and bar graph
heatWChng <- heatWidth
heatHChng <- heatHeight
barWChng <- barWidth
barHChng <- barHeight
#parameter for increasing bar and heat graphs size
zoomVal <- 400
#variable to store pairs data
pairsMat <- NULL

# GROUPS POP & IND #
GroupLsPop <- list()
GroupLsInd <- list()
#variables for Group creation for population
crVecPop <- vector()
#variables for Group creation for individual
crVecInd <- vector()

#VARIABLE storing ALL MATRIX DATA LOADED
allMatList <- NULL
#------------------------------------------GLOBAL VARIABLES END----------------------------------------#

#-----------------------------------SERVER        START--------------------------------------#
server <- 
  function(input, output, session){
    #-----        PRE-LOAD VALUES ON STARTUP        -----#
    #POP part: shows text as instruction to select one population
    output$textPopSlct <- renderText({
      "Select population(s) from the list below."
    })
    
    #POP part: shows selected Populations and Process info
    output$textPop1 <- renderText({
      "No populations selected."
    })
    
    #IND part: shows selected Population and Process info
    output$textInd1 <- renderText({
      "Select a single population in Populations Overview Tab."
    })
    #-----            PRE-LOAD VALUES END           -----#
    
    #-----       LOAD SAVED VALUES FROM WORKSPACE        -----#
    observeEvent(input$loadALL, {
      #store loaded value
      loadAll <- input$loadALL
      
      #if no workspace loaded do nothing
      if (is.null(loadAll)){ return(NULL) }
      
      #output the name of workspace
      output$loadedWS <- renderText({
        paste0("Workspace loaded: ", loadAll$name)
      })
      
      #read value from datapath
      loadedList <- readRDS(loadAll$datapath)
      
      #store matrix data
      allMatList <<- loadedList$MatData
      
      #update checkbox list of matrix data names
      output$popChkBox <- renderUI({
        checkboxGroupInput(inputId = "pop"
                           , label = "Populations List"
                           , choices = names(allMatList[[input$slctProc]])
                           , selected = NULL)
      })
      
      #store groups list
      GroupLsPop <<- loadedList$GrpsLsPop
      GroupLsInd <<- loadedList$GrpsLsInd
      #also store crNVec and crVec values
      crNVecPop <<- loadedList$popCrNVec
      crVecPop <<- loadedList$popCrVec
      crNVecInd <<- loadedList$indCrNVec
      crVecInd <<- loadedList$indCrVec
      
      if (!identical( GroupLsPop, list() )){
        #update the groups for populations
        updateCheckboxGroupInput(session = session
                                 , inputId = "popChkGrp"
                                 , label = "Created Groups:"
                                 , inline = TRUE
                                 , choices = names(GroupLsPop)
                                 , selected = NULL)
      }
      
      if (!identical( GroupLsInd, list() )){
        #update the groups for individuals
        updateCheckboxGroupInput(session = session
                                 , inputId = "indChkGrp"
                                 , label = "Created Groups:"
                                 , inline = TRUE
                                 , choices = names(GroupLsInd)
                                 , selected = NULL)
      }
      
      #store notes
      notes <- loadedList$Notes
      
      #update the notes text
      updateTextInput(session = session
                      , "notesTxt"
                      , value = notes)
    })
    #-----          LOAD WORKSPACE VALUES END            -----#
    
    #-----                          GET DATA NAMES FROM FILE SELECTED                          -----#
    dataInfo <- eventReactive(input$dataFile, {
      
      #unless loaded, input$dataFile is null
      if (is.null(input$dataFile)){
        return(NULL)
      }
      
      #store the data
      dataFrame <- input$dataFile
      
      #store the file names and data path
      dFPath <- as.vector(dataFrame$datapath)
      dFNames <- as.vector(dataFrame$name)
      dFProcs <-as.vector(vecGetProcName(dFNames))
      
      #get indices for each process
      i_Indx <- which( dFProcs == "indep" )
      p_Indx <- which( dFProcs == "pairwise" )
      iBN_Indx <- which( dFProcs == "indep.hBN" )
      pBN_Indx <- which( dFProcs == "pairwise.hBN" )
      iIBS_Indx <- which( dFProcs == "indep.hIBS" )
      pIBS_Indx <- which( dFProcs == "pairwise.hIBS" )
      
      #convert the data to list
      dataList <- list( 
        indep = data.frame( FileName = dFNames[ i_Indx ], DataPath = dFPath[ i_Indx ] ) 
        , pairwise = data.frame( FileName = dFNames[ p_Indx ], DataPath = dFPath[ p_Indx ] )
        , indep.hBN = data.frame( FileName = dFNames[ iBN_Indx ], DataPath = dFPath[ iBN_Indx ] )
        , pairwise.hBN = data.frame(FileName = dFNames[ pBN_Indx ], DataPath = dFPath[ pBN_Indx ])
        , indep.hIBS = data.frame(FileName = dFNames[ iIBS_Indx ], DataPath = dFPath[ iIBS_Indx ])
        , pairwise.hIBS = data.frame(FileName = dFNames[ pIBS_Indx ], DataPath = dFPath[ pIBS_Indx ])
      )
      
      #set to null whenever indices have length 0
      if (length(i_Indx) == 0){ dataList$indep <- 0 }
      if (length(p_Indx) == 0){ dataList$pairwise <- 0 }
      if (length(iBN_Indx) == 0){ dataList$indep.hBN <- 0 }
      if (length(pBN_Indx) == 0){ dataList$pairwise.hBN <- 0 }
      if (length(iIBS_Indx) == 0){ dataList$indep.hIBS <- 0 }
      if (length(pIBS_Indx) == 0){ dataList$pairwise.hIBS <- 0 }
      
      dataList
    })
    #-----                                  GETTING DATA END                                   -----#
    
    #-----                                 STORE REQUIRED DATA                          -----#
    matData <- eventReactive(input$slctProc, {
      #if the global variable has NULL then no workspace loaded
      if (is.null(allMatList)){
        #unless loaded, input$dataFile is null
        if (is.null( dataInfo() )){
          return(NULL)
        }
        
        #if any of the processes has 0 value in dataInfo()
        #that means no data loaded for that process
        if (all (dataInfo()[[input$slctProc]] == 0) ){
          return(NULL)
        }
        else{
          #loading function returns data in a list of matrices
          loadMultiPopData( fileNameVec = as.vector(dataInfo()[[input$slctProc]]$FileName), 
                            fileDPVec = as.vector(dataInfo()[[input$slctProc]]$DataPath),
                            prcsName = input$slctProc,
                            extn = file_ext( as.vector(dataInfo()[[input$slctProc]]$FileName[1]) )
          )
        }
      }
      else{
        allMatList[[input$slctProc]]
      }
    })
    #-----                                STORING DATA END                              -----#
    
    #-----  PROCESS SELECT EVENT -----#
    observeEvent(input$slctProc, {
      #store process - to be used multiple times
      slctdProcess <- input$slctProc
      
      #update text to say no populations selected
      output$textPop1 <- renderText({
        "No populations selected."
      })
      
      #update text in ind tab to instruct user to select population
      output$textInd1 <- renderText({
        "Select a single population in Populations Overview Tab."
      })
      
      #POP part: shows selected Process info
      output$textPop2 <- renderText({
        input$slctProc
      })
      
      #IND part: shows selected Process info
      output$textInd2 <- renderText({
        input$slctProc
      })
      
      #move to the Population tab for hIBS process as it has nothing in individuals part
      if ( any(input$slctProc == c( "indep.hIBS", "pairwise.hIBS" )) ) {
        updateTabsetPanel(session = session
                          , inputId = "mainTab"
                          , selected = "tabPop")
      }
      
      #remove checked for Population's ALL checkbox
      updateCheckboxInput(session = session
                          , inputId = "popAll"
                          , value = FALSE)
      
      #remove checked for Individual's ALL checkbox
      updateCheckboxInput(session = session
                          , inputId = "indAll"
                          , value = FALSE)
      
      #if allMatlist is not null update in different way
      if (!is.null(allMatList)){
        #update Population names
        output$popChkBox <- renderUI({
          checkboxGroupInput(inputId = "pop"
                             , label = "Populations List"
                             , choices = names( allMatList[[input$slctProc]] )
                             , selected = NULL)
        })
      }
      else{
        #update Population names
        output$popChkBox <- renderUI({
          checkboxGroupInput(inputId = "pop"
                             , label = "Populations List"
                             , choices = as.vector(dataInfo()[[input$slctProc]]$FileName)
                             , selected = NULL)
        })
      }
      
      #update text to no populations selected
      if (is.null(input$pop)){
        output$textPop1 <- renderText({
          "No Populations selected."
        })
      }
      
      #update Individual names
      output$indChkBox <- renderUI({
        #BLANK checkbox group
        checkboxGroupInput(inputId = "ind"
                           , label = "Individuals List"
                           , choices = NULL
                           , selected = NULL)
      })
      
      #-----            CHANGE GRAPHICS OUTPUT PROCESS-WISE             -----#
      #                       PROCESS: IBD                      #
      if ( any(input$slctProc == c( "indep", "pairwise" )) ){
        # POP: MDS and Bar Graphs | IND: BarPop, HeatMap, BarInd and BarLgnd Graphs
        #Bar Graph Title update
        output$titlePop <- renderUI({
          strong("Lineage distribution of the population")
        })
        
        #slider input update
        output$barBox <- renderUI({
          sliderInput(inputId = "slInp"
                      , label = "Select the lineage value used to order the individuals:"
                      , min = 0, max = 10, value = 5, round = TRUE, animate = FALSE
          )
        })
        
        # output$barBoxPlot <- renderPlotly({
        #   ggplotly(barplot_ggplot())
        # })
        
        #bar plot
        output$barBoxPlot <- renderUI({
          plotOutput("Barplot", width = 700, height = 300
                     , click = "plot_click"
                     , hover = "plot_hover"
                     , brush = "plot_brush")
        })
        
        
        #remove any Dend Selection inputs
        output$dendSelect <- renderUI({
        })
        
        #a WELL PANEL for output graphs of IBD Process
        output$indGraphs <- renderUI({
          wellPanel(id = "indPanel",
                    style = "overflow-x:scroll; max-width: 1000px,
                    overflow-y:scroll; max-height: 535px",
                    #ROW- MDS, Slider Input, BarPop
                    fluidRow(
                      column(6, 
                             flowLayout(
                               strong("Lineage distribution of the Population")
                               
                             )
                             , plotOutput(outputId = "BarPlotPop"
                                          , width = 575, height = 200
                                          )
                             ,br(),
                             sliderInput(inputId = "slInpInd"
                                         , label = "Select the lineage value used to order the individuals:"
                                         , min = 0, max = 10, value = 5, round = TRUE, animate = FALSE
                             )
                      )
                      , 
                      column(4
                             , flowLayout(strong("Lineage"))
                                         , plotlyOutput(outputId = "BarLgnd"
                                                        , width = 250, height = 400)
                             , offset = 1
                      )
                    )
                    ,
                    fluidRow(
                      
                      #HeatMap
                      column(6,
                             flowLayout(strong("Lineage of each pair of individuals")),
                             plotlyOutput(outputId = "HeatPlotInd"
                                       , width = heatWidth, height = heatHeight
                             )
                            
                      ),
                      column(4,
                             flowLayout(strong("Lineage Distribution of each Individual")),
                             plotlyOutput(outputId = "BarPlotInd"
                                        , width = barWidth, height = barHeight
                                        )
                             , offset = 1
                      )
                      )
                    )
        })
      }
      #                     PROCESS: IBD END                    #
      #                             PROCESS: BN                              #
      else if ( any(input$slctProc == c( "indep.hBN", "pairwise.hBN" )) ){
        # POP: MDS Graphs | IND: Dendrograms
        #update title to show nothing
        output$titlePop <- renderUI({
        })
        
        #remove any previous slider inputs
        output$barBox <- renderUI({
        })
        
        #remove any previous plots
        output$barBoxPlot <- renderUI({
        })
        
        #no heat or dend tile no graph at all
        output$heatdendTitle <- renderUI({
          strong("Dendrogram")
        })
        
        #create a list for select input clustering
        #values to include
        clustLs <- list("ward.D", "ward.D2", "single"
                        , "complete", "average" , "mcquitty"
                        , "median", "centroid" )
        #names shown to the user
        names(clustLs) <- c("ward.D", "ward.D2 (default)", "single"
                            , "complete", "average" , "mcquitty"
                            , "median", "centroid" )
        
        #Selection Inputs: 1 select input, 2 slider inputs
        output$dendSelect <- renderUI({
          flowLayout(
            #select input for Clustering Method
            selectInput(inputId = "clustMeth"
                        , label = "Cluctering Method"
                        , choices = clustLs
                        , selected = "ward.D2"
            )
            ,
            #slider input for K cuts in tree
            sliderInput(inputId = "cTree"
                        , label = "Cut Tree"
                        , min = 1
                        , max = 2
                        , value = 1
                        , step = 1
            )
            ,
            #slider input for selecting group within a cut tree
            sliderInput(inputId = "sGroup"
                        , label = "Select Group"
                        , min = 1
                        , max = 2
                        , value = 1
                        , step = 1
            )
          )
        })
        
        #WELL PANEL containing only Dendrogram and other selection inputs
        output$indGraphs <- renderUI({
          wellPanel(id = "tPanel",
                    style = "overflow-x:scroll; max-width: 1000px,
                    overflow-y:scroll; max-height: 550px",
                    
                    #Dendrogram Graph
                    plotOutput(outputId = "DendPlotInd"
                               , width = 800, height = 1000
                               , click = "plot_click"
                               , hover = "plot_hover"
                               , brush = "plot_brush")
          )
        })
      }
      #                           PROCESS: BN END                            #
      #      PROCESS: IBS     #
      else{
        # POP: MDS and Box Graphs | IND: NONE (display a message)
        #update title to Box Plot
        output$titlePop <- renderUI({
          strong("Box Plot")
        })
        
        #remove previous slider inputs
        output$barBox <- renderUI({
        })
        
        #change plot to box plot
        output$barBoxPlot <- renderUI({
          plotOutput("Boxplot", width = 700, height = 250
                     , click = "plot_click"
                     , hover = "plot_hover"
                     , brush = "plot_brush")
        })
        
        #remove any Dend Selection inputs
        output$dendSelect <- renderUI({
        })
        
        #display message for NO GRAPHS IN IBS SECTION
        output$indGraphs <- renderUI({
          strong("NO INDIVIDUAL GRAPHS FOR THE IBS PROCESS")
        })
      }
      #    PROCESS: IBS END   #
      #-----              PROCESS-WISE GRAPHICS OUTPUT END              -----#
    })
    #-----   PROCESS SELECT END  -----#
    
    #-----  SELECTING ALL POPULATIONS FROM THE LIST  -----#
    observeEvent(input$popAll, {
      isolate({
        #if Selected or TRUE select all
        if (input$popAll == TRUE){
          updateCheckboxGroupInput(session = session
                                   , inputId = "pop"
                                   , choices = as.vector(dataInfo()[[input$slctProc]]$FileName)
                                   , selected = as.vector(dataInfo()[[input$slctProc]]$FileName)
          )
        }
        
        #if Selected or FALSE unselect all
        else if (input$popAll == FALSE){
          updateCheckboxGroupInput(session = session
                                   , inputId = "pop"
                                   , choices = as.vector(dataInfo()[[input$slctProc]]$FileName)
                                   , selected = NULL
          )
        }
      })
    })
    #-----      SELECTING ALL POPULATIONS END        -----#
    
    #-----  SELECTING ALL INDIVIDUALS FROM THE LIST  -----#
    observeEvent(input$indAll, {
      isolate({
        #if Selected or TRUE select all
        if (input$indAll[1] == TRUE){
          updateCheckboxGroupInput(session = session
                                   , inputId = "ind"
                                   , choices = getIndNames(tail(input$pop, 1))
                                   , selected = getIndNames(tail(input$pop, 1))
          )
        }
        
        #if Selected or FALSE unselect all
        if (input$indAll[1] == FALSE){
          updateCheckboxGroupInput(session = session
                                   , inputId = "ind"
                                   , choices = getIndNames(tail(input$pop, 1))
                                   , selected = NULL
          )
        }
      })
    })
    #-----      SELECTING ALL INDIVIDUALS END        -----#
    
    #-----TRIGGER POPULATION SELECTION TO SHOW INDIVIDUALS $ NAMES SELECTED-----#
    observeEvent(input$pop, {
      isolate({
        #if no populations selected display info
        if (is.null(input$pop)){
          output$textPop1 <- renderText({
            "No Populations selected."
          })
        }
        #for MULTI clicks show names and for SINGLE click also show ind list
        else{
          #SINGLE CLICK - show Pop name and open individuals list
          if (length(input$pop) == 1){
            #store the File and Pop name
            fileName <- input$pop
            popName <- vecGetPopName(input$pop)
            
            #show Pop name selected
            output$textPop1 <- renderText({
              popName
            })
            
            #update text to show the Pop name selected
            output$textInd1 <- renderText({
              paste0("Population File Selected: ", fileName)
            })
            
            #remove tick for Select ALL in individuals list
            updateCheckboxInput(session = session
                                , inputId = "indAll"
                                , value = FALSE)
            
            #update the INDIVIDUALS LIST
            output$indChkBox <- renderUI({
              checkboxGroupInput(inputId = "ind"
                                 , label = "Individuals List"
                                 , choices = getIndNames(input$pop)
                                 , selected = NULL)
            })
            
            #store the selected population in global variable
            popSlctd <<- ifelse( (length(input$pop) == 1), input$pop, NULL  )   
          }
          #MULTIPLE CLICKS - display selected Pop names
          else{
            #store all the Pop names selected
            popNames <- vecGetPopName(input$pop)
            
            #show Pop name selected
            output$textPop1 <- renderText({
              popNames
            })
          }
        }
        #not null input$pop case end
      })
    })
    #-----               POPULATION SELECTION TRIGGERS END                 -----#
    
    #-----update slider input of hBN Dendrogram whenever Cut Tree changes-----#
    observeEvent(input$cTree, {
      updateSliderInput(session = session
                        , inputId = "sGroup"
                        , min = 1
                        , max = input$cTree
                        , value = 1
      )
    })
    #-----                      Slider input update end                  -----#
    
    #----- POPULATIONS PLOT BUTTON & INTERACTIONS -----#
    observeEvent(input$PlotPop, {
      isolate({
        #if no data loaded
        if ( is.null(matData()) ){
          info("No data loaded! Please load your data before plotting.")
          
          return(NULL)
        }
        
        #get names loaded
        origNames <- input$pop
        
        #if no pop name selected
        if ( is.null(origNames) ){
          info("Select populations from the list.")
          
          return(NULL)
        }
        
        #get the selected process
        slctdProcess <- input$slctProc
        
        #get file names
        fNames <- names(matData())
        
        #get the extension for the files
        extName <- file_ext(origNames[1])
        
        #get only matData for loaded data
        loadData <- matData()[origNames]
        
        #if the Process is IBD
        if (any(slctdProcess == c("indep", "pairwise"))){
          
          #MDS Plot Population
          output$MDSplot <- renderPlotly({
            if (identical(input$pop, origNames)){
              MDS_list <- fnameToPopDF(loadData)
              
              #get values of data from list
              MDS_df <- MDS_list$MDS_df
              
              #get circle data
              cirPts <- circleData(MDS_df$X, MDS_df$Y)
              
              #use function to return a ggplotly object
              plot1 <- DFToMDSPlot(MDS_df, cirPts, MDS_list$Pop_Name, MDS_list$eigVals)
              
              ggplotly(plot1, tooltip = "text", source = "MDSPOP")
            }
            else{
              
              #get the common values for both the origNames and input$pop vectors
              elmToPlot <- intersect(input$pop, origNames)
              
              #change elemToPlot to only names
              namesToPlot <- vecGetPopName(input$pop) 
              
              #start plotting (WITH THE GREYED OUT VALUES)
              MDS_list <- fnameToPopDF(loadData)
              
              #get values of data from list
              MDS_df <- MDS_list$MDS_df
              
              #get circle data
              cirPts <- circleData(MDS_df$X, MDS_df$Y)
              
              #use function to return a ggplotly object
              plot1 <- DFToMDSPlot(MDS_df, cirPts, MDS_list$Pop_Name
                                   , MDS_list$eigVals, namesToPlot)
              
              ggplotly(plot1, tooltip = "text", source = "MDSPOP")
            }
          })
          
          #Bar Graph Population with Slider Input trigger
          observeEvent(input$slInp, {
            
            barplot_ggplot <- reactive({
              #get the data frame required to plot
              toPlot_df <- fnameToPopDF(loadData, input$slInp)
              
              #get population names reordered as decreasing
              Popn_Name <- reorder(toPlot_df$Pop_Name, toPlot_df$Normalized_Count, FUN = sum)
              
              if (!is.null(input$pop) && identical(input$pop, origNames)){
                #use the function to get ggplot object for the graph
                plot2 <- DFToBarPlot (toPlot_DF = toPlot_df, Population_Name = Popn_Name)
                
                plot2
                
              }
              else if (all(input$pop %in% origNames)){
                #if (all(input$pop %in% origNames) && (length(input$pop) < length(origNames))){
                #use the function to get ggplot object for the graph
                plot2 <- DFToBarPlot (toPlot_df, input$pop, Popn_Name)
                
                plot2
              }
              else{
                #keep the plot as it is (case where names outside of orig names are selected)
                if (all(origNames %in% input$pop)){
                  #plot original graph
                  plot2 <- DFToBarPlot (toPlot_DF = toPlot_df, Population_Name = Popn_Name)
                  
                  plot2
                  
                }
                else{
                  #plot selected values graph
                  plot2 <- DFToBarPlot (toPlot_df, input$pop, Popn_Name)
                  
                  plot2
                  
                }
              }
              
            })
            
            output$Barplot <- renderPlot({
              
              barplot_ggplot()
              
            })
            
          })
        }
        #if the Process is IBS
        else if (any(slctdProcess == c("indep.hIBS", "pairwise.hIBS"))){
          
          #MDS Plot Population
          output$MDSplot <- renderPlotly({
            if (identical(input$pop, origNames)){
              MDS_list <- fnameToPopDF(loadData)
              
              #get values of data from list
              MDS_df <- MDS_list$MDS_df
              
              #get circle data
              cirPts <- circleData(MDS_df$X, MDS_df$Y)
              
              #use function to return a ggplotly object
              plot1 <- DFToMDSPlot(MDS_df, cirPts, MDS_list$Pop_Name, MDS_list$eigVals)
              
              ggplotly(plot1, tooltip = "text", source = "MDSPOP")
            }
            else{
              
              #get the common values for both the origNames and input$pop vectors
              elmToPlot <- intersect(input$pop, origNames)
              
              #change elemToPlot to only names
              namesToPlot <- vecGetPopName(input$pop) 
              
              #start plotting (WITH THE GREYED OUT VALUES)
              MDS_list <- fnameToPopDF(loadData)
              
              #get values of data from list
              MDS_df <- MDS_list$MDS_df
              
              #get circle data
              cirPts <- circleData(MDS_df$X, MDS_df$Y)
              
              #use function to return a ggplotly object
              plot1 <- DFToMDSPlot(MDS_df, cirPts, MDS_list$Pop_Name
                                   , MDS_list$eigVals, namesToPlot)
              
              ggplotly(plot1, tooltip = "text", source = "MDSPOP")
            }
          })
          
          boxplot_ggplot <- reactive({
            #get vector list from file names and make the data frame
            boxDf <- fnameToPopDF(loadedList = loadData, plotType = "Box")
            
            #get population names ordered by median
            ordPNmsMed <- reorder(boxDf$PopName, boxDf$Value, FUN = median)
            # medPopNames <- factor(boxDf$PopName
            #                        ,levels=levels(boxDf$PopName)[order(medIBSindep)]
            #                        ,ordered=TRUE)
            #give the data frame to df to box pop function
            plot3 <- DFToBoxPlot(boxDf, ordPNmsMed)
            
            plot3
          })
          
          output$Boxplot <- renderPlot({
            
            boxplot_ggplot()
            
          })
          
        }
        #if the Process is BN
        else{
          
          #MDS Plot Population only for hBN different computation
          output$MDSplot <- renderPlotly({
            if (identical(input$pop, origNames)){
              #get data frame for BN MDS pop
              BN_MDS_Ls <- hBNToMDSDF(loadData)
              
              #separate values from the returned list
              Pop_Names <- BN_MDS_Ls$Pop_Name
              BN_MDS_df <- BN_MDS_Ls$MDS_df
              eigen <- BN_MDS_Ls$eigVals
              
              #get circle data
              cirPts <- circleData(BN_MDS_df$X, BN_MDS_df$Y)
              
              plot1 <- DFToMDSPlot(BN_MDS_df, cirPts, Pop_Names
                                   , eigen)
              
              ggplotly(plot1, tooltip = "text", source = "MDSPOP")
            }
            else{
              
              #get the common values for both the origNames and input$pop vectors
              elmToPlot <- intersect(input$pop, origNames)
              
              #change elemToPlot to only names
              namesToPlot <- vecGetPopName(input$pop) 
              
              #get data frame for BN MDS pop
              BN_MDS_Ls <- hBNToMDSDF(loadData)
              
              #separate values from the returned list
              Pop_Names <- BN_MDS_Ls$Pop_Name
              BN_MDS_df <- BN_MDS_Ls$MDS_df
              eigen <- BN_MDS_Ls$eigVals
              
              #get circle data
              cirPts <- circleData(BN_MDS_df$X, BN_MDS_df$Y)
              
              plot1 <- DFToMDSPlot(BN_MDS_df, cirPts, Pop_Names
                                   , eigen, namesToPlot)
              
              ggplotly(plot1, tooltip = "text", source = "MDSPOP")
            }
          })
          
          #BN matrices have no Bar/Box plot - empty
          output$Barplot <- renderUI({
          })
          output$Boxplot <- renderUI({
          })
        }
        
        #MDS Plot click event--------------
        observeEvent(event_data("plotly_click", source = "MDSPOP"), {
          isolate({
            #get the clicked data
            clkedData <- event_data("plotly_click", source = "MDSPOP")
            
            if (!is.null(clkedData)){
              
              #points not on circle
              if (clkedData$pointNumber < length(origNames)){
                #if curveNumber is 1 we are looking at greyed out portion
                if (clkedData$curveNumber == 1){
                  #get the greyed out names
                  greyNames <- setdiff(origNames, input$pop)
                  
                  #sebset from grey out
                  #increment index by one
                  indexPos <- clkedData$pointNumber + 1
                  
                  #subset this name from the greyed out populations
                  clkedName <- greyNames[indexPos]
                }
                else{
                  #get the clicked population file name
                  #increment index by one
                  indexPos <- clkedData$pointNumber + 1
                  
                  #subset this name from the originally selected populations
                  clkedName <- input$pop[indexPos]
                }
                
                #update the filename to the only clicked one
                updateCheckboxGroupInput(session = session
                                         , inputId = "pop"
                                         , choices = fNames
                                         , selected = clkedName)
              }
              #for points on the circle
              else{ return(NULL) }
            }
          })
        })
        #MDS Plot click end-----------------
      })
    })
    #-----           POPULATIONS PLOT END         -----#
    
    #-----      POPULATIONS MDS PLOT LASSO SELECT EVENT       -----#
    observeEvent(event_data("plotly_selected", source = "MDSPOP"), {
      isolate({
        
        fNames <- names(matData())
        
        #get the selected data
        slctedData <- event_data("plotly_selected", source = "MDSPOP")
        
        if (is.null(slctedData)) { return(NULL) }
        
        #get the indices for the selected ones
        slctdNameIndx <- slctedData$pointNumber[slctedData$curveNumber == 0]
        
        #increase all indices by 1
        slctdNameIndx <- slctdNameIndx + 1
        
        #subset these names from input checkboxes
        slctdNames <- input$pop[slctdNameIndx]
        
        #update the pop list
        updateCheckboxGroupInput(session = session
                                 , "pop"
                                 , choices = fNames
                                 , selected = slctdNames)
      })
    })
    #-----            POPULATIONS LASSO SELECT END            -----#
    
    #----- INDIVIDUALS PLOT BUTTON $ INTERACTIONS -----#
    observeEvent(input$PlotInd, {
      isolate({
        
        #use the pop selected global variable to get its data
        loadData <- matData()[[popSlctd]]
        
        #store original selected ind names
        origNamesInd <- input$ind
        
        if (is.null(origNamesInd)){
          info("No individuals selected!")
          
          return(NULL)
        }
        
        #store the loaded names
        origNamesPop <- names(matData())
        
        #get the selected process
        slctdProcess <- getProcessName(popSlctd)
        
        #get the names of individuals
        indNames <- getIndNames(popSlctd)
        
        #for Process IBD
        if (any(slctdProcess == c("indep", "pairwise"))){
          
          #Bar Graph for one Population
          observeEvent(input$slInpInd, {
            output$BarPlotPop <- renderPlot({
              isolate({
                
                if (length(input$ind) <= 0){
                  output$prgrsInd <- renderPrint({
                    cat("SELECT INDIVIDUALS FROM THE ABOVE LIST.")
                  })
                  return(NULL)
                }
                
                plot6_1 <- fnameToIndDF(loadedMat = loadData, popFName = popSlctd
                                        , indNames = input$ind
                                        , plotType =  "Bar GraphP", maxKValInd = input$slInpInd)
                
                plot6_1
              })
            })
          })
          
          #constant legend for bar graph
          output$BarLgnd <- renderPlotly({
            plotLgd <- barLegendInd(input$slInpInd)
            
            ggplotly(plotLgd, width = 300, height = 300, tooltip = "text", source = "LgndPlot")
          })
          
          #Heat Map Individuals - PairsMat STORES THE PAIRS DATA IN A MATRIX
          observeEvent(input$ind, {
            output$HeatPlotInd <- renderPlotly({
              if (length(origNamesInd) <= 1){ return(NULL) }

              lineageColors= c("#1E90FF", "#FF8C00", "#228B22", "#B22222","#9370DB", "#A0522D", "#DA70D6", "#8B8682","#FFFF66","#F2F2F2", "#F2F2F2", "#F2F2F2")
              
              # ordering the axes
              barPlot_df <- fnameToIndDF(loadData, popSlctd, origNamesInd, "Bar GraphI", maxKValInd = input$slInpInd)
              
              Ind_Name <- reorder(barPlot_df$Ind_Name, barPlot_df$Frequency, FUN = sum)
              
              target <- (levels(Ind_Name))
              
              target2 <- c(target)
              
            
              #legend click data
              lgndClk <- event_data("plotly_click", source = "LgndPlot")
              
              #two cases legend clicked or not clicked
              if (is.null(lgndClk)){#not clicked
                #two cases here bar graph I partly clicked or all clicked
                if (identical(origNamesInd, input$ind)){#all clicked
                  
                  plot5List <- fnameToIndDF(loadData, popSlctd
                                            , input$ind, "HeatMap", prcs = input$slctProc)
                  
                  df <- as.data.frame(plot5List$HeatMap$carpet)
                  c <- colnames(df)
                  r <- rownames(df)
                  
                  df2 <- data.matrix(df)
                  
                  df3 <- df2[target, ]
                  
                  df3 <- df3[,target]
                  
                  # comparing the two dataframes (before and after sorting)
                  # for (i in target2){
                  #   for (j in target2){
                  #     if (df2[i, j] != df3[i, j]){
                  #       print(i)
                  #       print(j)
                  #     }
                  #   }
                  # }
                    
                  
                  p <- plot_ly(x = c, y = r, z = df3, type="heatmap", colorbar = list(title = "Legend", tickvals=c(0:12)), colors = lineageColors)
                  #print(p)
                  
                  p
                  
                }
                else{#partial clicked
                  #pass the only selected rows to heatmap computation
                  plot5List <- fnameToIndDF(loadData, popSlctd
                                            , input$ind, "HeatMap"
                                            , origInd = origNamesInd
                                            , prcs = input$slctProc
                                            , heatZmLvl = heatWChng)
                  
                  #save pairs data along with population
                  pairsMat <<- plot5List$PairsData
                  pairsPop <<- popSlctd
                  
                  df <- as.data.frame(plot5List$HeatMap$carpet)
                  c <- colnames(df)
                  r <- rownames(df)
                  df2 <- data.matrix(df)
                  df3 <- df2[target, ]
                  df3 <- df3[,target]
                  
                  p <- plot_ly(x = c, y = r, z = df3, type="heatmap", colorbar = list(title = "Legend"), colors = lineageColors)
                  #print(p)
                  p
                  
                  #plot5List$HeatMap
                }
              }
              else{#clicked
                #store slider input value
                slInp <- input$slInpInd
                
                #if slider = 10 change to 9
                if (slInp == 10){
                  slInp <- 9
                }
                
                #the lineage numbers are
                linNum <- slInp:0
                
                #get y value from cliked data
                linNumIndx <- lgndClk$y
                
                #subset this integer from linNum
                slctdInt <- linNum[linNumIndx]
                
                #pass this matrix to heatmap computation
                plot5List <- fnameToIndDF(loadData, popSlctd
                                          , input$ind, "HeatMap", origInd = origNamesInd
                                          , prcs = input$slctProc, slctNum = slctdInt, heatZmLvl = heatWChng)
                
                #save pairs data along with population
                pairsMat <<- plot5List$PairsData
                pairsPop <<- popSlctd
                
                df <- as.data.frame(plot5List$HeatMap$carpet)
                c <- colnames(df)
                r <- rownames(df)
                df2 <- data.matrix(df)
                df3 <- df2[target, ]
                df3 <- df3[,target]
                p <- plot_ly(x = c, y = r, z = df3, type="heatmap", colorbar = list(title = "Legend"), colors = lineageColors)
                #print(p)

                p
                
                #plot5List$HeatMap
              }
              
            })
          })
          
          #Bar Graph for Individuals
          observeEvent(input$slInpInd, {
            output$BarPlotInd <- renderPlotly({
              #isolate({
              
              if (length(input$ind) == 0){
                output$prgrsInd <- renderPrint({
                  cat("SELECT INDIVIDUALS FROM THE ABOVE LIST.")
                })
                return(NULL)
              }
              
              if (length(input$ind) == 1){
                return(NULL)
              }
              
              #get the data frame required to plot
              barPlot_df <- fnameToIndDF(loadData, popSlctd
                                         , origNamesInd, "Bar GraphI"
                                         , maxKValInd = input$slInpInd)
              
              if (!is.null(input$ind) && (length(input$ind) < length(origNamesInd))){
                
                #get reordered ind names
                Ind_Name <- reorder(barPlot_df$Ind_Name, barPlot_df$Frequency, FUN = sum)
                
                #use the function to get ggplot object for the graph
                plot2 <- BarDFToPlot (barPlot_df, Ind_Name, input$ind)
                
                ggplotly(plot2)
              }
              else{
                if (!is.null(barPlot_df)){
                  #get reordered ind names
                  Ind_Name <- reorder(barPlot_df$Ind_Name, barPlot_df$Frequency, FUN = sum)
                  
                  
                  #also draw the selected graph
                  plot6_2 <- BarDFToPlot(barPlot_df, Ind_Name)
                  
                  ggplotly(plot6_2)
                }
                else{
                  stop("All the values in the matrix are greater than your selected value!
                       Please select a greater value.")
                  return(NULL)
                }
              }
              #})
              })
            })
          
          #Plot Click event for Individuals bar graph
          observeEvent(input$ind, {
            observeEvent(input$plot_click, {
              output$BarPlotInd <- renderPlotly({
                isolate({
                  
                  #get the file name from lsFName
                  flNm <- popSlctd
                  
                  #get currently checked boxes
                  currChks <- input$ind
                  
                  #store clicked data
                  clkData <- input$plot_click
                  
                  #get 'y' value for the index 
                  clkDataY <- round(clkData$y)
                  
                  #get the data frame required to plot
                  barPlot_df <- fnameToIndDF(loadData, flNm
                                             , origNamesInd, "Bar GraphI"
                                             , maxKValInd = input$slInpInd)
                  
                  #get individual names reordered as decreasing
                  Ind_Name <- reorder(barPlot_df$Ind_Name, barPlot_df$Frequency, FUN = sum)
                  
                  #get levels from Ind_Name
                  IndNameLvls <- attr(Ind_Name, "levels")
                  
                  #index/subset from Ind_Name
                  toSlctNames <- IndNameLvls[clkDataY]
                  
                  #get length of current checks and orig names
                  lenCurrChksPls <- length(currChks) + 1
                  lenOrigNmsInd <- length(origNamesInd)
                  
                  #if you click on something that is not present in current checked
                  if (!all(toSlctNames %in% currChks)){
                    #when currently selected is originally selected names
                    #i.e. at the very first when you are selecting a bar
                    if (identical(currChks, origNamesInd)){
                      #dont do anything in this case
                      return(NULL)
                    }
                    #if current selected is not origNamesInd but within it
                    else if (all(currChks %in% origNamesInd)){
                      #if you are going to multiple select the last one which is going to be selected
                      if (lenCurrChksPls == lenOrigNmsInd){
                        #update the individuals checkbox
                        updateCheckboxGroupInput(session = session
                                                 , "ind"
                                                 , choices = getIndNames(flNm)
                                                 , selected = origNamesInd)
                        
                        #also draw the full graph with all selected
                        plot6_2 <- BarDFToPlot(barPlot_df, Ind_Name)
                        
                        ggplotly(plot6_2)
                      }
                      else{
                        #get ind names for the vector
                        toSlctNames <- c(currChks, toSlctNames)
                        
                        #update the individuals checkbox
                        updateCheckboxGroupInput(session = session
                                                 , "ind"
                                                 , choices = getIndNames(flNm)
                                                 , selected = toSlctNames)
                        
                        #also draw the selected graph
                        plot6_2 <- BarDFToPlot(barPlot_df, Ind_Name, hglData = toSlctNames)
                        
                        ggplotly(plot6_2)
                      }
                    }
                  }
                  #if you click on something that is already present in the current
                  #checks remove from the current checks vector and plot that
                  else{
                    
                    if (identical(currChks, origNamesInd)){
                      #remove the one to be unselected from currchecks
                      toSlctNames <- setdiff(currChks, toSlctNames)
                      
                      #update the individuals checkbox
                      updateCheckboxGroupInput(session = session
                                               , "ind"
                                               , choices = getIndNames(flNm)
                                               , selected = toSlctNames)
                      
                      #also draw the graph
                      plot2 <- BarDFToPlot(barPlot_df, Ind_Name, hglData = toSlctNames)
                      
                      ggplotly(plot2)
                    }
                    else if (all(currChks %in% origNamesInd)){
                      
                      #if the last one is only selected and is to be unselected
                      if (length(currChks) == 1){
                        #update the individuals checkbox
                        updateCheckboxGroupInput(session = session
                                                 , "ind"
                                                 , choices = getIndNames(flNm)
                                                 , selected = origNamesInd)
                        
                        #also draw the selected graph
                        plot6_2 <- BarDFToPlot(barPlot_df, Ind_Name)
                        
                        ggplotly(plot6_2)
                      }
                      else{
                        #remove the one to be unselected from currchecks
                        toSlctNames <- setdiff(currChks, toSlctNames)
                        
                        #update the individuals checkbox
                        updateCheckboxGroupInput(session = session
                                                 , "ind"
                                                 , choices = getIndNames(flNm)
                                                 , selected = toSlctNames)
                        
                        #also draw the graph
                        plot2 <- BarDFToPlot(barPlot_df, Ind_Name, hglData = toSlctNames)
                        
                        ggplotly(plot2)
                      }
                    }
                  }
                })
              })
            })
          })
        }
        #for Process BN
        else if (any(slctdProcess == c("indep.hBN", "pairwise.hBN"))){
          #return NULL if data has less than or equal to 1 input
          if (length(origNamesInd) <= 1){ return(NULL) }
          
          #-----SUBSET THE MATRIX DATA if names selected is not all the individual names-----#
          if (!identical(origNamesInd, indNames)){
            
            #get the numbers to index from the origNamesInd
            toPlotSlctd <- as.numeric( str_extract(origNamesInd, "[[:digit:]]+") )
            
            #give names to the loadData matrix if it has no names
            if (is.null(rownames(loadData))){
              rownames(loadData) <- indNames
              colnames(loadData) <- indNames
            }
            #else let the names be as it is
            
            #get only the data required for individuals from the loadData
            loadData <- loadData[toPlotSlctd, toPlotSlctd]
          }
          #-----SUBSET MATRIX END-----#
          
          #Dendrogram mapping and selection
          output$DendPlotInd <- renderPlot({
            
            #condition to have extra ui dend controls for hBN
            #update cut tree slider to have max = number of indivduals
            updateSliderInput(session = session
                              , inputId = "cTree"
                              , min = 1
                              , max = length(origNamesInd)
                              , step = 1
            )
            #redo dendrogram for cluster tree cutting
            plotList <- hBN_To_Dend(loadedMat = loadData, METHOD = input$clustMeth, K = input$cTree)
            
            #store slider group info
            slGrp <- input$sGroup
            
            #extract DHC and clustDHC info from plotList
            clustdhc <- plotList$clustDHC
            
            #get the index info for the selected group from clustdhc
            clustIndx <- which(clustdhc == slGrp)
            
            #get the selected numbers from the names of clustIndx
            slctdIndx <- as.numeric(names(clustIndx))
            
            #subset these from ind Names to get the selected ones
            slctdInds <- indNames[slctdIndx]
            
            #update check box to contain only those names
            updateCheckboxGroupInput(session = session,
                                     inputId = "ind",
                                     choices = indNames,
                                     selected = slctdInds)
            
            #plot the final dhc by accessing it from the list
            plot(plotList$DHC, horiz=TRUE)
          })
        }
        #for Process IBS
        else{
          return(NULL)
        }
        })
      })
    #-----       INDIVIDUALS PLOT BUTTON END      -----#
    
    #-----POP GROUPS DOWNLOAD START-----#
    output$downGroupPop <- downloadHandler (
      filename = function() {
        downloadAs <- "Populations_Groups.txt"
        
        downloadAs
      },
      
      content = function(file) {
        
        for (name in names(GroupLsPop)){
          
          namesToSlct <- GroupLsPop[[name]]
          
          for (n in namesToSlct){
            
            write(paste(n, name, sep = ", "), file = file, append = TRUE)
            
          }
        }
        
      }
    )
    
    #-----POP GROUPS DOWNLOAD END-----#
    
    #-----IND GROUPS DOWNLOAD START-----#
    output$downGroupInd <- downloadHandler (
      filename = function() {
        downloadAs <- "Individuals_Groups.txt"
        
        downloadAs
      },
      
      content = function(file) {
        
        for (name in names(GroupLsInd)) {
          
          namesToSlct <- GroupLsInd[[name]]
          
          for (n in namesToSlct){
            
            write(paste(n, name, sep = ", "), file = file, append = TRUE)
            
          }

        }
        
      }
    )
    
    #-----IND GROUPS DOWNLOAD END-----#
    
    #-----PAIRS DOWNLOAD AS .csv-----#
    output$downInd <- downloadHandler(
      filename = function() {
        #get to download as name
        downloadAs <- input$downNameInd
        
        #give a default name if nothing is there in downloadAs
        downloadAs <- ifelse( 
          #condition
          (downloadAs == "")
          #true
          , paste(getPopulationName(pairsPop), '_'
                  , getProcessName(pairsPop), '.csv', sep = '')
          #false
          , paste(downloadAs, '.csv', sep = '')  )
        
        downloadAs
      },
      content = function(file) {
        #if pairs is null put info about not selected pairs
        if (is.null(pairsMat)){
          info("Did not download! No pairs selected.")
          
          return(NULL)
        }
        
        write.csv(pairsMat, file, row.names = FALSE)
      }
    )
    #-----  PAIRS DOWNLOAD END  -----#
    
    #----- GROUPS CREATE, RENAME AND DELETE -----#
    #             Population Groups              #
    #            CREATE POP            #
    observeEvent(input$createPop, {
      #store the input$pop group
      newPopGroup <- input$pop
      
      #if newPopGroup is null; nothing is selected; don't make the group
      if (is.null(newPopGroup)){
        info("Did not create group! No Populations selected to make a group")
        
        return(NULL)
      }
      
      #if no groupes created the first group is number one
      if ( length(crVecPop) == 0 ){
        crVecPop <<- "Group_1"
        
        #add the newPopGroup names in a list under the name of crVecPop
        GroupLsPop[[crVecPop]] <<- newPopGroup
      }
      #else the new group created is 1 plus the number 
      #from last of currently present groups
      else{
        #store value of crVecPop in temp place
        tempPop <- crVecPop
        
        #remove any names without "Group_"
        tempPop <- tempPop[ grepl("Group_", tempPop) ]
        
        #check if the last pop is a default name provided by us
        crNVecPop <- as.numeric( str_extract(tempPop, "[[:digit:]]+") )
        
        #add "Group_" before the numbers
        crVecPop <<- c( crVecPop, paste0("Group_", ( last(crNVecPop) + 1)) )
        
        #add new group to existing list under LAST NAME of crVecPop
        GroupLsPop[[ last(crVecPop) ]] <<- newPopGroup  
      }
      
      #update the checkbox group for Populations
      updateCheckboxGroupInput(session = session
                               , inputId = "popChkGrp"
                               , inline = TRUE
                               , choices = crVecPop
                               , selected = last(crVecPop)
      )
    })
    #          DELETE POP           #
    observeEvent(input$delPop, {
      #store selected names to delete
      toDelNames <- input$popChkGrp
      
      #if nothing is selected and delete is pressed
      if (is.null(toDelNames)){
        info("No group(s) selected to delete!")
        
        return(NULL)
      }
      
      #set the one's to delete names to NULL in GroupLsPop
      GroupLsPop[toDelNames] <<- NULL
      
      #remove elements from crVecPop
      crVecPop <<- setdiff(crVecPop, toDelNames)
      
      #if length of crVecPop is 0 set it to 0
      if (length(crVecPop) == 0){
        crVecPop <<- 0
      }
      
      #update names of the Populations checkbox
      updateCheckboxGroupInput(session = session
                               , inputId = "popChkGrp"
                               , inline = TRUE
                               , choices = names(GroupLsPop)
                               , selected = NULL
      )
    })
    #            RENAME POP            #
    observeEvent(input$renamePop, {
      #store the name which has to be renamed
      toRename <- input$popChkGrp
      
      #if NULL was stored i.e. nothing selected then do nothing
      if (is.null(toRename)){
        info("No name selected to re-name!")
        
        return(NULL)
      }
      
      #ALSO NO MORE THAN ONE NAME SELECTED
      if (length(toRename) > 1){
        info("Select one name to rename!")
        
        return(NULL)
      }
      
      #store the rename text
      renName <- input$renTextPop
      
      #if no text in rename part give message
      if (renName == ""){
        info("No text given to rename!")
        
        return(NULL)
      }
      
      #if "Group_" is used dont allow it
      if (grepl("Group_", renName)){
        info("'Group_' is used for default naming. Please use other names.")
        
        return(NULL)
      }
      
      #store names of groups created
      grpPopNames <- names(GroupLsPop)
      
      #"which" has the name "toRename": change it to "renName"
      grpPopNames[ which(grpPopNames == toRename) ] <- renName
      
      #modify grouplspop names
      names(GroupLsPop) <<- grpPopNames
      
      #update crVecPop
      crVecPop <<- grpPopNames
      
      #update the checkbox Populations group
      updateCheckboxGroupInput(session = session
                               , inputId = "popChkGrp"
                               , inline = TRUE
                               , choices = crVecPop
                               , selected = renName
      )
      
      #update text input to remove the renamed name
      updateTextInput(session = session
                      , inputId = "renTextPop"
                      , value = "")
    })
    #             Individual Groups              #
    #            CREATE IND            #
    observeEvent(input$createInd, {
      #store the input$ind group
      newIndGroup <- input$ind
      
      #if newIndGroup is null; nothing is selected; don't make the group
      if (is.null(newIndGroup)){
        info("Did not create group! No Individuals selected to make a group")
        
        return(NULL)
      }
      
      #if no groups created the first group is number one
      if ( length(crVecInd) == 0 ){
        crVecInd <<- "Group_1"
        
        #add the newIndGroup names in a list under the name of crVecInd
        GroupLsInd[[crVecInd]] <<- newIndGroup
      }
      #else the new group created is 1 plus the number 
      #from last of currently present groups
      else{
        #store value of crVecInd in temp place
        tempInd <- crVecInd
        
        #remove any names without "Group_"
        tempInd <- tempInd[ grepl("Group_", tempInd) ]
        
        #check if the last ind is a default name provided by us
        crNVecInd <- as.numeric( str_extract(tempInd, "[[:digit:]]+") )
        
        #add "Group_" before the numbers
        crVecInd <<- c( crVecInd, paste0("Group_", ( last(crNVecInd) + 1)) )
        
        #add new group to existing list under LAST NAME of crVecInd
        GroupLsInd[[ last(crVecInd) ]] <<- newIndGroup  
      }
      
      #update the checkbox group for Individuals
      updateCheckboxGroupInput(session = session
                               , inputId = "indChkGrp"
                               , inline = TRUE
                               , choices = crVecInd
                               , selected = last(crVecInd)
      )
    })
    #          DELETE IND           #
    observeEvent(input$delInd, {
      #store selected names to delete
      toDelNames <- input$indChkGrp
      
      #if nothing is selected and delete is pressed
      if (is.null(toDelNames)){
        info("No group(s) selected to delete!")
        
        return(NULL)
      }
      
      #set the one's to delete names to NULL in GroupLsInd
      #it is a way to remove those elements from a list
      GroupLsInd[toDelNames] <<- NULL
      
      #remove elements from crVecInd
      crVecInd <<- setdiff(crVecInd, toDelNames)
      
      #if length of crVecInd is 0 set it to 0
      if (length(crVecInd) == 0){
        crVecInd <<- 0
      }
      
      #update names of the Individuals checkbox
      updateCheckboxGroupInput(session = session
                               , inputId = "indChkGrp"
                               , inline = TRUE
                               , choices = names(GroupLsInd)
                               , selected = NULL
      )
    })
    #            RENAME IND            #
    observeEvent(input$renameInd, {
      #store the name which has to be renamed
      toRename <- input$indChkGrp
      
      #if NULL was stored i.e. nothing selected then do nothing
      if (is.null(toRename)){
        info("No name selected to re-name!")
        
        return(NULL)
      }
      
      #ALSO NO MORE THAN ONE NAME SELECTED
      if (length(toRename) > 1){
        info("Select one name to rename!")
        
        return(NULL)
      }
      
      #store the rename text
      renName <- input$renTextInd
      
      #if no text in rename part give message
      if (renName == ""){
        info("No text given to rename!")
        
        return(NULL)
      }
      
      #if "Group_" is used dont allow it
      if (grepl("Group_", renName)){
        info("'Group_' is used for default naming. Please use other names.")
        
        return(NULL)
      }
      
      #store names of groups created
      grpIndNames <- names(GroupLsInd)
      
      #"which" has the name "toRename": change it to "renName"
      grpIndNames[ which(grpIndNames == toRename) ] <- renName
      
      #modify grouplsind names
      names(GroupLsInd) <<- grpIndNames
      
      #update crVecInd
      crVecInd <<- grpIndNames
      
      #update the checkbox Individuals group
      updateCheckboxGroupInput(session = session
                               , inputId = "indChkGrp"
                               , inline = TRUE
                               , choices = crVecInd
                               , selected = renName
      )
      
      #update text input to remove the renamed name
      updateTextInput(session = session
                      , inputId = "renTextInd"
                      , value = "")
    })
    #-----     CREATE RENAME DELETE END     -----#
    
    #-----  HIGHLIGHT NAMES ON SELECTED CREATED GROUPS  -----#
    #  Select Populations for a Group  #
    observeEvent(input$popChkGrp, {
      #store the value of selected group
      slctdGroup <- input$popChkGrp
      
      #proceed only if length of slctdGroup is 1
      if (length(slctdGroup) != 1){
        return(NULL)
      }
      else{
        
        #match names of GroupLsPop with name selected in check box pop
        namesToSlct <- GroupLsPop[[slctdGroup]]
        
        #update pop chkbox with these names
        updateCheckboxGroupInput(session = session
                                 , inputId = "pop"
                                 , choices = names(matData())
                                 , selected = namesToSlct)
      }
    })
    #  Select Individuals for a Group  #
    observeEvent(input$indChkGrp, {
      #store the value of selected group
      slctdGroup <- input$indChkGrp
      
      #proceed only if length of slctdGroup is 1
      if (length(slctdGroup) != 1){
        return(NULL)
      }
      else{
        
        #match names of this list with name selected in check box ind
        namesToSlct <- GroupLsInd[[slctdGroup]]
        
        #update checkbox group for Ind
        updateCheckboxGroupInput(session = session
                                 , "ind"
                                 , choices = getIndNames(popSlctd)
                                 , selected = namesToSlct)
      }
    })
    #-----        HIGHLIGHT SELECTED GROUPS END         -----#
    
    #-----   SAVE SESSION - Groups, Notes and All Loaded Matrices   -----#
    output$saveALL <- downloadHandler(
      filename = function() {
        #default name
        if (input$saveText == ""){
          saveName <- "Saved_Workspace"
        }
        #name from the user
        else{
          saveName <- input$saveText
        }
        
        saveName
      },
      content = function(save) {
        #we need one object saving all of the required values
        #get matrix data for all the processes in a list form
        allMatData <- list()
        
        #if no data loaded
        if (is.null(matData())){
          allMatData <- NULL
        }
        else{
          #this computation takes time we have a progress bar
          withProgress(
            expr = {
              
              if (all(dataInfo()$indep != 0)){
                allMatData$indep <- 
                  loadMultiPopData( fileNameVec = as.vector(dataInfo()$indep$FileName), 
                                    fileDPVec = as.vector(dataInfo()$indep$DataPath),
                                    prcsName = "indep",
                                    extn = "genome"
                  )
              }
              incProgress(amount = 1/6)
              
              if (all(dataInfo()$indep.hBN != 0)){
                allMatData$indep.hBN <- 
                  loadMultiPopData( fileNameVec = as.vector(dataInfo()$indep.hBN$FileName), 
                                    fileDPVec = as.vector(dataInfo()$indep.hBN$DataPath),
                                    prcsName = "indep.hBN",
                                    extn = "kinf"
                  )
              }
              incProgress(amount = 1/6)
              
              if (all(dataInfo()$indep.hIBS != 0)){
                allMatData$indep.hIBS <- 
                  loadMultiPopData( fileNameVec = as.vector(dataInfo()$indep.hIBS$FileName), 
                                    fileDPVec = as.vector(dataInfo()$indep.hIBS$DataPath),
                                    prcsName = "indep.hIBS",
                                    extn = "kinf"
                  )
              }
              incProgress(amount = 1/6)
              
              if (all(dataInfo()$pairwise != 0)){
                allMatData$pairwise <- 
                  loadMultiPopData( fileNameVec = as.vector(dataInfo()$pairwise$FileName), 
                                    fileDPVec = as.vector(dataInfo()$pairwise$DataPath),
                                    prcsName = "pairwise",
                                    extn = "genome"
                  )
              }
              incProgress(amount = 1/6)
              
              if (all(dataInfo()$pairwise.hBN != 0)){
                allMatData$pairwise.hBN <- 
                  loadMultiPopData( fileNameVec = as.vector(dataInfo()$pairwise.hBN$FileName), 
                                    fileDPVec = as.vector(dataInfo()$pairwise.hBN$DataPath),
                                    prcsName = "pairwise.hBN",
                                    extn = "kinf"
                  )
              }
              incProgress(amount = 1/6)
              
              if (all(dataInfo()$pairwise.hIBS != 0)){
                allMatData$pairwise.hIBS <- 
                  loadMultiPopData( fileNameVec = as.vector(dataInfo()$pairwise.hIBS$FileName), 
                                    fileDPVec = as.vector(dataInfo()$pairwise.hIBS$DataPath),
                                    prcsName = "pairwise.hIBS",
                                    extn = "kinf"
                  )
              }
              incProgress(amount = 1/6)
            }
            , value = 0
            , message = "Saving all the Matrix Data..."
          )
        }
        
        #MATRIX DATA
        saveMatData <- allMatData
        #GROUPS  POP
        saveGrpsPop <- GroupLsPop
        #GROUPS  IND
        saveGrpsInd <- GroupLsInd
        #NOTES - store notes
        saveNotes <- input$notesTxt
        
        #STORE A LIST OF THE ABOVE OBJECTS
        saveALLList <- list(MatData = saveMatData, GrpsLsPop = saveGrpsPop
                            , GrpsLsInd = saveGrpsInd, Notes = saveNotes
                            , popCrVec = crVecPop, indCrVec = crVecInd)
        
        #Save this object
        saveRDS(saveALLList, save)
      }
    )
    #-----                     SAVE SESSION END                     -----#
  }
#-----------------------------------SERVER        END----------------------------------------#