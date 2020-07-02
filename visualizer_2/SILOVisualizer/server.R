
library(shiny)

shinyServer(function(input, output) {
  
##  source(paste(here(),"/visualizer_2/SILOVisualizer/functions/fileReader.R", sep='/'))
    
## Directory selector
    shinyDirChoose(input, 'dir', roots=c(home = getwd()), filetypes=c('csv','shp'))
    global <- reactiveValues(datapath = getwd())
    dir <- reactive(input$dir)
    output$dir <- renderText({
        global$datapath
    })
    observeEvent(ignoreNULL = TRUE,
                 eventExpr = {
                     input$dir
                 },
                 handlerExpr = {
                     if (!"path" %in% names(dir())) return()
                     home <- normalizePath("~", winslash = "/")
                     global$datapath <-
                         file.path(getwd(), paste(unlist(dir()$path[-1]), collapse = .Platform$file.sep))
                 })
## Directory selector for scenarios
    shinyDirChoose(input, "dir2", roots=c(home=getwd()), filetypes=c('csv','shp'))
    global <- reactiveValues(datapath2 = getwd())
    dir2 <- reactive(input$dir2)
    output$dir2 <- renderText({
      global$datapath2
    })
    observeEvent(ignoreNULL = TRUE,
                 eventExpr = {
                 input$dir2
                 },
                 handlerExpr = {
                   if (!"path" %in% names(dir2())) return()
                   home <- normalizePath("~", winslash = "/")
                   global$datapath2 <-
                     file.path(getwd(), paste(unlist(dir2()$path[-1]), collapse = .Platform$file.sep))
                 })

# Example (to be deleted)
    data <- reactive({
        if (input$num <= 50){
        }
        else{
        rnorm(input$num)}
    })
    output$hist <- renderPlot({hist(data())})
    output$value <-renderPrint({summary(data())})
    output$table <-renderDataTable({householdsTable()})
    
    observeEvent(global$datapath, {loadfiles()})

    
    
## File loader
    loadfiles <- reactive({
        if (global$datapath == getwd()){}
        else {
            print(global$datapath)
            
            ## Define all tables
            o_hhsize <- read.csv(paste(global$datapath,"aveHhSize.csv",sep="/", collapse = NULL))
            o_c_owne <- read.csv(paste(global$datapath,"carOwnership.csv",sep="/", collapse = NULL))
            o_com_di <- read.csv(paste(global$datapath,"commutingDistance.csv",sep="/", collapse = NULL))
            o_dwelli <- read.csv(paste(global$datapath,"dwellings.csv",sep="/", collapse = NULL))
            o_eventc <- read.csv(paste(global$datapath,"eventCounts.csv",sep="/", collapse = NULL))
            o_hhAvIn <- read.csv(paste(global$datapath,"hhAveIncome.csv",sep="/", collapse = NULL))
            o_hhSatR <- read.csv(paste(global$datapath,"hhSatisfactionByRegion.csv",sep="/", collapse = NULL))
            o_hhSize <- read.csv(paste(global$datapath,"hhSize.csv",sep="/", collapse = NULL))
            o_hhType <- read.csv(paste(global$datapath,"hhType.csv",sep="/", collapse = NULL))
            o_laPaRa <- read.csv(paste(global$datapath,"labourParticipationRate.csv",sep="/", collapse = NULL))
            o_lanReg <- read.csv(paste(global$datapath,"landRegions.csv",sep="/", collapse = NULL))
            o_popYea <- read.csv(paste(global$datapath,"popYear.csv",sep="/", collapse = NULL))
        }
    })
## Scenario file loader
## Scenario File loader
    observeEvent(global$datapath2, {loadfiles2()})
    
    loadfiles2 <- reactive({
      if (global$datapath2 == getwd()){}
      else {
        print(global$datapath2)
        
        ## Define all tables
        c_hhsize <- read.csv(paste(global$datapath2,"aveHhSize.csv",sep="/", collapse = NULL))
        c_c_owne <- read.csv(paste(global$datapath2,"carOwnership.csv",sep="/", collapse = NULL))
        c_com_di <- read.csv(paste(global$datapath2,"commutingDistance.csv",sep="/", collapse = NULL))
        c_dwelli <- read.csv(paste(global$datapath2,"dwellings.csv",sep="/", collapse = NULL))
        c_eventc <- read.csv(paste(global$datapath2,"eventCounts.csv",sep="/", collapse = NULL))
        c_hhAvIn <- read.csv(paste(global$datapath2,"hhAveIncome.csv",sep="/", collapse = NULL))
        c_hhSatR <- read.csv(paste(global$datapath2,"hhSatisfactionByRegion.csv",sep="/", collapse = NULL))
        c_hhSize <- read.csv(paste(global$datapath2,"hhSize.csv",sep="/", collapse = NULL))
        c_hhType <- read.csv(paste(global$datapath2,"hhType.csv",sep="/", collapse = NULL))
        c_laPaRa <- read.csv(paste(global$datapath2,"labourParticipationRate.csv",sep="/", collapse = NULL))
        c_lanReg <- read.csv(paste(global$datapath2,"landRegions.csv",sep="/", collapse = NULL))
        c_popYea <- read.csv(paste(global$datapath2,"popYear.csv",sep="/", collapse = NULL))
      }
    })

## Reactive to choose type of representation
    observeEvent(input$renderType,{renderSelector()})
    renderSelector <-reactive({
      if(input$renderType =='aspatial'){
        # Call plot function
      }
      else{
        # Call map function
        
      }
    })
    
    
## Example siloMap
    
    output$siloMap <- renderLeaflet({
       attribute <- input$spatialLevel
       style <- 'pretty'
       categories <- 10
       d <- FALSE
       if(d == FALSE){
         #attribute <- "households"
         farbe <- "-RdBu"
       } else {
         #attribute <- "households"
         farbe <- "-RdBu"
       }
       
      if(input$spatialLevel == 'dwellings'){
        attribute <- input$sDwelling
        groupedTable <-prepareSiloMap(spatialData, input$year, input$zone_level, attribute)
        print(input$sDwelling)
        msmMap(groupedTable, attribute, farbe, 'Dwelling','Test',
               input$siloMapStyle, input$siloMapCategories)
      }else if(input$spatialLevel == 'income'){
        attribute <- input$sIncome
        groupedTable <-prepareSiloMap(spatialData, input$year, input$zone_level, attribute)
        msmMap(groupedTable, attribute, farbe, 'Dwelling','Test',
               input$siloMapStyle, input$siloMapCategories)
      }else if(input$spatialLevel == 'accessibilities'){
        attribute <- input$sAcc
        groupedTable <-prepareSiloMap(spatialData, input$year, input$zone_level, attribute)
        msmMap(groupedTable, attribute, farbe, 'Dwelling','Test',
               input$siloMapStyle, input$siloMapCategories)
      }else {
      groupedTable <-prepareSiloMap(spatialData, input$year, input$zone_level, input$spatialLevel)
      print(groupedTable)
      msmMap(groupedTable, input$spatialLevel, farbe,input$spatialLevel,"Label",
             input$siloMapStyle, input$siloMapCategories)
      }
      

      #if (input$spatialLevel == "dwellings"){
      #  
      #} else if (input$spatialLevel == "accessibilities"){
      #  msmMap(groupedTable, attribute, farbe, "Viviendas","Label", 
      #         input$siloMapStyle, input$siloMapCategories)
      #} else if (input$spatialLevel == "incomes"){
      #  msmMap(groupedTable, attribute, farbe, "Viviendas","Label",
      #         input$siloMapStyle, input$siloMapCategories)
      #} else {
        
        #tmap_leaflet(tm_shape(groupedTable) +
        #               tm_polygons("households", palette = farbe, title ="Hello World",style = 'pretty', n = 10) +
        #               tm_layout("Hii"))
      #}
    })
    
    
    

    
    ## Observer to render new panel
    #observeEvent(input$comparison, {renderFileLoader2()})
    
    ##File loader2
    #renderFileLoader2 <- reactive({
    #  if (input$comparison == TRUE){
    #    shinyDirButton("dir2","Select scenario directory", "Select directory")
    #  } else{}
    #})

    ## Filter function example
    filterSpatialTable <- function(chosenYear,data){
      joinTable <- filter(data,year == chosenYear)
      spatialTable <- left_join(zones, joinTable, by="shp_id")
    }
    

    
        
    ## Table implementation example
    householdsTable <-reactive({
        if (global$datapath == getwd()){}
        else {
            renderDataTable(hhsize)
            
        }
    })
    

# 

    
    
        
    #dtest <- eventReactive(global$datapath, {renderText(getdir)}, ignoreNULL = TRUE)
    #output$folder <- 'hello'
    
    #output$folder <- renderText({
    #dtest()
    ##})
    
    ###

    output$distPlot <- renderPlot({

        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')

    })
    

})
