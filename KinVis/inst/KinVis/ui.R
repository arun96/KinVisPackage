#-----                               install and load all packages                                  -----
source("packLoad.R")
pkgVec <- c("cowplot", "shiny", "shinyjs", "shinyBS", "stringr", "plotly", "shinythemes", "plyr", "miniUI"
            , "reshape2", "readr", "data.table", "dendextend", "gplots", "ggplot2", "ggrepel", "ggdendro"
            , "ggExtra", "gridExtra", "shinydashboard", "markdown", "tools", "Matrix", "RColorBrewer", "dplyr")
packLoad(pkgVec)
#-------------------------------------------------------------------------------------------------------#

#---------------------------------------------USER       INTERFACE----------------------------------------#
ui <- 
fluidPage (
  #shiny js for functions like delay, hide, show...
  useShinyjs(),
  
  #give title to web page and web browser
  titlePanel(
    #web page title at the top
    title = h1("KinVis",align = "center"),
    
    #title shown in tab of browsers
    windowTitle = "KinVis"
  ),
  
  #--------------------------------MAIN INTERFACE---------------------------------
  flowLayout(
    #load previous workspaces
    fileInput("loadALL", label = "Load Workspace"),
    #choose which file(s) to load from
    fileInput(inputId = "dataFile", label = "Select File(s) to load", multiple = TRUE)
  ),
  
  #text output showing loaded workspace
  verbatimTextOutput("loadedWS"),
  
  #selecting process input
  selectInput(inputId = "slctProc"
              , label = "Select Process: "
              , choices = c("indep", "indep.hBN", "indep.hIBS"
                            ,"pairwise","pairwise.hBN","pairwise.hIBS")
              , selected = "indep"),
  hr(),
  #---------------------------------------
  #START OF POPULATION AND INDIVIDUAL TABS
  tabsetPanel(
    id = "mainTab"
    , selected = NULL
    , type = "tabs",
    #, position = "right",
    #two tabs population and individuals
    #-----  POPULATIONS OVERVIEW  -----#
    tabPanel(
      title = "POPULATIONS Overview",
      value = "tabPop",
      br(),
      
      #text to give instruction to select one Population
      textOutput("textPopSlct"),
      tags$style("#textPopSlct{
                   text-align: left;
                   font-weight: bold
                 }"
      ),
      
      #POP side bar layout
      #SIDE BAR LAYOUT FOR CHECKBOX LIST AND PLOTS
      sidebarLayout(
        #SIDE BAR PANEL
        sidebarPanel(
          #all populations checkbox
          checkboxInput(inputId = "popAll", label = "Select ALL", value = FALSE),
          
          #shows population names loaded along with process selected
          tags$u("Selected Populations"), textOutput("textPop1"),
          tags$u("Selected Process"), textOutput("textPop2"),
          
          #SCROLL HTML CODE - for POPULATIONS check box list
          tags$head(
            tags$style(
              type = 'text/css',
              'form.well { max-height: 390px; overflow-y: auto; }'
            )
          ),
          
          #dynamically created checkbox for Populations
          uiOutput( "popChkBox" )
          # #checkbox group input POPULATIONS
          # checkboxGroupInput(inputId = "pop"
          #                    , label = "Populations List"
          #                    , choices = NULL)
          , width = 3
        ),
        #side bar panel end
        
        #MAIN BAR PANEL
        mainPanel(
          #PLOT Button for POPULATIONS overview along with PROGRESS text
          fixedRow(
            column(6, actionButton(inputId = "PlotPop"
                                   , label = "Plot"
                                   , icon = icon("bar-chart")
                                   #, width = '35%'
                                   , style='font-size:140%'))
            #,column(6, verbatimTextOutput("prgrsPop"))
          ),
          
          br(),
          
          #A ROW OF PLOTTING: (1). MDS, (2). BOX/BAR Graphs
          fixedRow(
            column(4, strong("Populations similarity map based on lineage/correlation")
                   #plotting MDS plots
                   , plotlyOutput(outputId = "MDSplot"
                                  , width = 260, height = 300),
                    strong("Instructions for MDS Plot"),
                    p("Double-click to restore default zoom."),
                    p("To focus on the lasso- or box- selected regions, please plot the graph again."),
                    p("To unselect lasso- or box-select regions, please select the relevant points, and plot the graph again."))
            
            #plotting bar/box plots
            ,column(7,
                    #title changing
                    uiOutput("titlePop"),
                    #different graph types
                    uiOutput("barBoxPlot"),
                    #plotlyOutput(outputId = "barBoxPlot", width = 500, height = 300),
                    #slider input needed or not
                    uiOutput("barBox")
            )
          ),
      hr(),
      
      #SHOW CREATED GROUPS
      checkboxGroupInput(inputId = "popChkGrp"
                         , label = "Created Groups:"
                         , inline = TRUE
                         , choices = NULL
                         , selected = NULL),
      
      #GROUP CREATION FOR POPULATIONS TAB
      strong("Manage Groups:"),
      
      br(),
      
      #CREATE, NAME/RENAME TEXT & NAME/RENAME GROUPS Button
      fluidRow(
        column(2, actionButton("createPop", label = "Create Group", icon = icon("plus")))
        ,
        column(2, actionButton("delPop", label = "Remove Group(s)", icon = icon("times")))
        ,
        column(2, textInput(inputId = "renTextPop", label = NULL
                            , placeholder = "Rename Selected Group"))
        ,
        column(2, actionButton("renamePop", label = "Rename Group"
                               , icon = icon("pencil")))
        ,
        column(2, downloadButton("downGroupPop", label = "Download Groups"))
      )
      
      )
      #main panel end
      )
      #sidebar layout end
    )
    #POPULATIONS TAB END
    ,
    #-----  INDIVIDUALS OVERVIEW  -----#
    tabPanel(
      title = "INDIVIDUALS Overview",
      value = "tabInd",
      br(),
      
      #show which population loaded
      textOutput("textInd1"),
      tags$style("#textInd1{
                 text-align: left;
                 font-weight: bold
                 }"
      ),
      
      #IND side bar layout
      #SIDE BAR LAYOUT FOR CHECKBOX LIST AND PLOTS
      sidebarLayout(
        #SIDE BAR PANEL
        sidebarPanel(
          #all individuals checkbox
          checkboxInput(inputId = "indAll", label = "Select ALL", value = FALSE),
          
          #shows individual names loaded along with process selected
          tags$u("Selected Process"), "- ", textOutput("textInd2"),
          
          #SCROLL HTML CODE - for INDIVIDUALS check box list
          tags$head(
            tags$style(
              type = 'text/css',
              #'form.well { max-height: 390px; overflow-y: auto; }'
              'form.well { max-height: 600px; overflow-y: auto; }'
            )
          ),
          
          #dynamically created checkbox for Individuals
          uiOutput( "indChkBox" )
          # #checkbox group input INDIVIDUALS
          # checkboxGroupInput(inputId = "ind"
          #                    , label = "Individuals List"
          #                    , choices = NULL)
          , width = 3
        ),
        #side bar panel end
        
        #MAIN BAR PANEL
        mainPanel(
          #PLOT Button for INDIVIDUALS overview along with PROGRESS text
          fluidRow(
            column(6, actionButton(inputId = "PlotInd"
                                   , "Plot"
                                   , icon = icon("bar-chart-o")
                                   , style='font-size:140%')),
            #column(6, verbatimTextOutput("prgrsInd")),
            
            
            #DOWNLOAD PAIRS - NAME TEXT AND BUTTON
            fluidRow(
              column(2, textInput(inputId = "downNameInd"
                                  , label = NULL
                                  , placeholder = "<name>_<process>.csv")
                     , offset = 1)
              ,
              column(1, downloadButton("downInd", label = "Download Pairs (.csv)"))
            )
          ),
          
          #SELECTION ONLY FOR Dendrogram
          uiOutput("dendSelect")
          ,
          #CONTAINS DYNAMICALLY CREATED OUTPUT GRAPHS OUT OF
          #  (1). MDS (IBD), (2). BAR POPULATION (IBD), (3). HEATMAP (IBD)
          #  (4). DENDROGRAM (BN), (5). BAR INDIVIDUALS (IBD), (6). CONSTANT BAR LEGEND (IBD)
          #  and other minor controls like zooming and selection for graphs
          uiOutput("indGraphs")
        )
        #main panel end
      ),
      #sidebar layout end
      
      hr(),
      
      #SHOW CREATED GROUPS
      checkboxGroupInput(inputId = "indChkGrp"
                         , label = "Created Groups:"
                         , choices = NULL
                         , selected = NULL),
      
      
      #GROUP CREATION FOR POPULATIONS TAB
      strong("Manage Groups:"),
      
      br(),
      
      #CREATE, NAME/RENAME TEXT & NAME/RENAME GROUPS Button
      fluidRow(
        column(1, actionButton("createInd", label = "Create Group", icon = icon("plus")))
        ,
        column(2, actionButton("delInd", label = "Remove Group", icon = icon("times")))
        ,
        column(2, textInput(inputId = "renTextInd", label = NULL
                            , placeholder = "Rename Selected Group"))
        ,
        column(2, actionButton("renameInd", label = "Rename Group"
                               , icon = icon("pencil")))
        ,
        column(2, downloadButton("downGroupInd", label = "Download Groups"))
      )
    )
    #INDIVIDUALS TAB END
  ),
  #END OF TABS
  #---------------------------------------
  hr(),
  
  #notes and save workspace section
  fluidRow(
    #text box for notes
    column(8, HTML(paste0('<div class="form-group shiny-input-container">
                           <label for="', "notesTxt", '">', "NOTES BOX-",'</label>
                           <textarea id="', "notesTxt"
                           , '" placeholder="', "Use this space for your own notes."
                           ,'" rows="', 3
                           ,'" cols="', 120
                           ,'">', "", '</textarea></div>'))
    ),
    #text to enter for download name
    column(2, br(), textInput("saveText", label = "Workspace Name")),
    #save workspace button
    column(1, br(), br(), downloadButton("saveALL", label = "Save Workspace"))
  )
  
)