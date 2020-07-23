
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

## File loader
    observeEvent(global$datapath, {loadfiles()})
    
    loadfiles <- reactive({
    if (global$datapath == getwd()){}
    else {
      print(global$datapath)
    
    ## Define all tables
      global$o_hhsize <- read.csv(paste(global$datapath,"aveHhSize.csv",sep="/", collapse = NULL))
      global$o_c_owne <- read.csv(paste(global$datapath,"carOwnership.csv",sep="/", collapse = NULL))
      global$o_com_di <- read.csv(paste(global$datapath,"commutingDistance.csv",sep="/", collapse = NULL))
      global$o_dwelli <- read.csv(paste(global$datapath,"dwellings.csv",sep="/", collapse = NULL))
      global$o_eventc <- read.csv(paste(global$datapath,"eventCounts.csv",sep="/", collapse = NULL))
      global$o_hhAvIn <- read.csv(paste(global$datapath,"hhAveIncome.csv",sep="/", collapse = NULL))
      global$o_hhSatR <- read.csv(paste(global$datapath,"hhSatisfactionByRegion.csv",sep="/", collapse = NULL))
      global$o_hhSize <- read.csv(paste(global$datapath,"hhSize.csv",sep="/", collapse = NULL))
      global$o_hhType <- read.csv(paste(global$datapath,"hhType.csv",sep="/", collapse = NULL))
      global$o_laPaRa <- read.csv(paste(global$datapath,"labourParticipationRate.csv",sep="/", collapse = NULL))
      global$o_lanReg <- read.csv(paste(global$datapath,"landRegions.csv",sep="/", collapse = NULL))
      global$o_popYea <- read.csv(paste(global$datapath,"popYear.csv",sep="/", collapse = NULL))
      global$o_spatialData <-read.csv(paste(global$datapath,"resultFileSpatial.csv",sep="/", collapse= NULL))
      global$o_spatialData <-rename(global$o_spatialData, "shp_id" = "zone")
            }
    })

## Scenario File loader
    observeEvent(global$datapath2, {loadfiles2()})
    
    loadfiles2 <- reactive({
      if (global$datapath2 == getwd()){}
      else {
        print(global$datapath2)
        
        ## Define all tables
      global$c_hhsize <- read.csv(paste(global$datapath2,"aveHhSize.csv",sep="/", collapse = NULL))
      global$c_c_owne <- read.csv(paste(global$datapath2,"carOwnership.csv",sep="/", collapse = NULL))
      global$c_com_di <- read.csv(paste(global$datapath2,"commutingDistance.csv",sep="/", collapse = NULL))
      global$c_dwelli <- read.csv(paste(global$datapath2,"dwellings.csv",sep="/", collapse = NULL))
      global$c_eventc <- read.csv(paste(global$datapath2,"eventCounts.csv",sep="/", collapse = NULL))
      global$c_hhAvIn <- read.csv(paste(global$datapath2,"hhAveIncome.csv",sep="/", collapse = NULL))
      global$c_hhSatR <- read.csv(paste(global$datapath2,"hhSatisfactionByRegion.csv",sep="/", collapse = NULL))
      global$c_hhSize <- read.csv(paste(global$datapath2,"hhSize.csv",sep="/", collapse = NULL))
      global$c_hhType <- read.csv(paste(global$datapath2,"hhType.csv",sep="/", collapse = NULL))
      global$c_laPaRa <- read.csv(paste(global$datapath2,"labourParticipationRate.csv",sep="/", collapse = NULL))
      global$c_lanReg <- read.csv(paste(global$datapath2,"landRegions.csv",sep="/", collapse = NULL))
      global$c_popYea <- read.csv(paste(global$datapath2,"popYear.csv",sep="/", collapse = NULL))
      global$c_spatialData <-read.csv(paste(global$datapath2,"resultFileSpatial.csv",sep="/", collapse= NULL))
      global$c_spatialData <-rename(global$c_spatialData, "shp_id" = "zone")
      }
    })

## Dummy function to trigger maps
    dummyfunc <-eventReactive(input$update, {dummycall(1)})
    
## Plot Map
    output$siloMap <- renderLeaflet({
      n <-dummyfunc()
      
      ## Map color selection
      if(input$comparisonSelector ==1){
        farbe <- "-RdBu"
      } else if (input$comparisonSelector ==2){
        farbe <- "-RdBu"
      } else {
        if(input$scenarioSelector == 1){
        } else {
        }
        farbe <- "YlOrBr"
      }
      ## Execute maps
      
      print("Execute map calculation")
      print(names(spatialData()[[1]])[2])
      dataSubset <-spatialData()[[1]]
      print(dataSubset)
      legend <- spatialData()[[2]]
      attribute <- spatialData()[[3]]
      print(attribute)
      print(legend[2])
      msmMap(dataSubset,attribute, farbe,legend[1],legend[2],
             input$siloMapStyle, input$siloMapCategories)

    })
  
  
  
#################################
  # Modularize spatial inputs
  spatialData <-reactive({
    n <-dummyfunc()
    print("Module spatialData is running")
    if(input$spatialLevel == 'dwellings'){
      
      attribute <- input$sDwelling
      aggregationType <- "density"
      
    }else if(input$spatialLevel == 'income'){
      attribute <- input$sIncome
      aggregationType <- "density"
      
    }else if(input$spatialLevel == 'accessibilities'){
      attribute <- input$sAcc
      aggregationType <- "average"
      
    }else if(input$spatialLevel == 'avePrice'){
      attribute <- input$spatialLevel
      aggregationType <- "average"
      
    }else {
      attribute = input$spatialLevel
      aggregationType <- "density"
      
    }
    ## Database logic choose scenario and comparison type
    if(input$comparisonSelector ==1){

      isComparison <-TRUE
      legend <- prepareSiloMapLabels(myLabels, "siloSpatial", attribute, isComparison)
      originalDataSet <-prepareSiloMap(global$o_spatialData, input$year, input$zone_level, attribute, aggregationType)
      scenarioDataSet <-prepareSiloMap(global$c_spatialData, input$year, input$zone_level, attribute, aggregationType)
      
      groupedTable <- compareScenarios(originalDataSet, scenarioDataSet,attribute)  
      
    } else if (input$comparisonSelector ==2){
      farbe <- "-RdBu"
      isComparison <-TRUE
      
      legend <- prepareSiloMapLabels(myLabels, "siloSpatial", attribute, isComparison)
      originalDataSet <-prepareSiloMap(global$o_spatialData, initialYear, input$zone_level, attribute, aggregationType)
      scenarioDataSet <-prepareSiloMap(global$o_spatialData, input$year, input$zone_level, attribute, aggregationType)
      groupedTable <- compareScenarios(originalDataSet, scenarioDataSet,attribute)
      
    } else {
      if(input$scenarioSelector == 1){
        spatialDataSet <-global$o_spatialData
      } else {
        spatialDataSet <-global$c_spatialData
      }

      isComparison <-FALSE
      legend <- prepareSiloMapLabels(myLabels, "siloSpatial", attribute, isComparison)
      print("Entering groupedTable")
      groupedTable <-prepareSiloMap(spatialDataSet, input$year, input$zone_level, attribute, aggregationType)
      print("Groupedtable finished")

    }
    print("Module spatialData finished correctly")
    print(groupedTable)
    
    return(list(groupedTable, legend, attribute))
    
  })
#################################
    
#################################
## Experimental map reactivity
    geo <- eventReactive(input$map_shape_click, {
      shinyjs::show("reactiveOutput6")
      shinyjs::show("reactiveOutput5")
      shinyjs::show("reactiveOutput4a1")
      shinyjs::show("reactiveOutput4a")
      shinyjs::show("reactiveOutput4")
      shinyjs::show("reactiveOutput3")
      shinyjs::show("reactiveOutput2a")
      shinyjs::show("reactiveOutput2b")
      shinyjs::show("reactiveOutput2c")
      shinyjs::show("reactiveOutput1")
      shinyjs::removeClass(class = "shinyjs-hide", selector = "hr")
      shinyjs::removeClass(class = "shinyjs-hide", selector = ".kpi-group")
      
      click <- input$map_shape_click
      print(click$id)
      as.numeric(click$id)
    })
#################################
  output$gisTable <- renderDataTable(
    
    select(as.data.frame(spatialData()[1]),-geometry)
    )
    
  ##  select(as.data.frame(global$exportTable),-geometry)
    
  ## Downloader
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("export_example", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(select(as.data.frame(spatialData()[1]),-geometry), file, row.names = TRUE)
    }
  )
  output$downloadShape<-downloadHandler(
    filename = function() { paste("shpExport","zip", sep = ".") },
    content = function(file){
      data <- spatialData()
      temp_shp <- tempdir()
      st_write(data, "temp_shp.shp")

      zip_file <- file.path(temp_shp, "shapefile_shp.zip")
      shp_files <- list.files(temp_shp,
                              "temp_shp",
                              full.names = TRUE)
      zip_command <- paste("zip -j", 
                           zip_file, 
                           paste(shp_files, collapse = " "))
      system(zip_command)
      file.copy(zip_file, file)
      file.remove(zip_file, shp_files)
    }
  )
  #################################
  ## Graphic logic
  getAspatialData <- reactive({
    n <-dummyfunc()
    print("Running aspatial tables preparation")
    if(input$aspatialLevel == 'overview'){
      population_men <- aggregate(global$o_popYea$men, by=list(popYear= global$o_popYea$year), FUN=sum)
      population_women <- aggregate(global$o_popYea$women, by=list(popYear= global$o_popYea$year), FUN=sum)
      dwellings <- aggregate(global$o_dwelli$count, by=list(dwelling= global$o_dwelli$year), FUN=sum)
      households <- aggregate(global$o_hhSize$count, by=list(households = global$o_hhSize$year), FUN = sum)
      ## Join data
      men <- population_men['x']
      women <- population_women['x']
      households <- households['x']
      dwellings <- dwellings['x']
      population <- data.frame(population_men['popYear'],population_men['x'],population_women['x'],households['x'],dwellings['x'])
      population <- population%>%rename(men = x, women = x.1, households = x.2, dwellings = x.3 )
      population$population <- population$men + population$women
      return(population)
    }else if (input$aspatialLevel == 'households'){
      
    }
    

  })
  fig <- reactive({

    fig <-plot_ly(getAspatialData(), x = ~popYear)
    
    fig <-fig%>%add_trace(y = ~population, name = 'Population', type = 'scatter', mode = 'lines')
    fig <-fig%>%add_trace(y = ~men, name = 'Men',  type = 'scatter', mode ='lines') 
    fig <-fig%>%add_trace(y = ~women, name = 'Women',  type = 'scatter', mode ='lines')
    fig <-fig%>%add_trace(y = ~households, name = 'Households',  type = 'scatter', mode ='lines') 
    fig <-fig%>%add_trace(y = ~dwellings, name = 'Dwellings',  type = 'scatter', mode ='lines')
    fig<- fig%>%layout(title= "Overview")
    return(fig)
  })
  output$siloPlot <-renderPlotly(
    fig()
  )
    

})
