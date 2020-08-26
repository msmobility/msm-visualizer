
library(shiny)

shinyServer(function(input, output, session) {
    
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
    
    ## File loader
    
    observeEvent(global$datapath, {loadfiles()})
    
    loadfiles <- reactive({
        if (global$datapath == getwd()){}
        else {
            print(global$datapath)
            
            ## Define all tables
            global$o_aveHhsize <- read.csv(paste(global$datapath,"aveHhSize.csv",sep="/", collapse = NULL))
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
            
            ## New parsed files, run the python code first
            global$o_hhReIn <- read.csv(paste(global$datapath,"hhRentAndIncome.csv",sep="/", collapse = NULL))
            global$o_perMig <- read.csv(paste(global$datapath,"persMigrants.csv",sep="/", collapse = NULL))
            global$o_dwelQu <- read.csv(paste(global$datapath,"dwellingQualityLevel.csv",sep="/", collapse = NULL))
            global$o_perRac <- read.csv(paste(global$datapath,"persByRace.csv",sep="/", collapse = NULL))
            
        }
    })
    
    ## Scenario File loader
    observeEvent(global$datapath2, {loadfiles2()})
    
    loadfiles2 <- reactive({
        if (global$datapath2 == getwd()){}
        else {
            print(global$datapath2)
            
            ## Define all tables
            global$c_aveHhsize <- read.csv(paste(global$datapath2,"aveHhSize.csv",sep="/", collapse = NULL))
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
            
            ## New parsed files, run the python code first
            global$c_hhReIn <- read.csv(paste(global$datapath2,"hhRentAndIncome.csv",sep="/", collapse = NULL))
            global$c_perMig <- read.csv(paste(global$datapath2,"persMigrants.csv",sep="/", collapse = NULL))
            global$c_dwelQu <- read.csv(paste(global$datapath2,"dwellingQualityLevel.csv",sep="/", collapse = NULL))
            global$c_perRac <- read.csv(paste(global$datapath2,"persByRace.csv",sep="/", collapse = NULL))
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
    observe({
        event <-input$map_shape_click
        print(input$map_shape_click)
        #output$cnty <- renderText(shape$NAME[shape$CNTY_ID == event$id])
    })
    #################################
    output$gisTable <- renderDataTable(
        
        select(as.data.frame(spatialData()[1]),-geometry)
    )
    output$aspatialTable <-renderDataTable(
        getAspatialData()
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
    output$downloadAspatialData<-downloadHandler(
        filename = function(){paste("csvExport","csv",sep=".")},
        content = function(file){
            write.csv(getAspatialData(),file, row.names = TRUE)
        }
    )
    #################################
    ## Aspatial logic procedure, calculate the dataTable output based on the input parameters
    getAspatialData <- reactive({
        n <-dummyfunc()
        
        if(input$aspatialLevel == 'overview'){
            if(input$comparison == FALSE){
                dataTable <- siloAspatialOverview(global$o_popYea, global$o_dwelli, global$o_hhSize)
            } else {
                o_overview <- siloAspatialOverview(global$o_popYea, global$o_dwelli, global$o_hhSize)
                c_overview <- siloAspatialOverview(global$c_popYea, global$c_dwelli, global$c_hhSize)
                dataTable <- siloAspatialTableComparator(o_overview, c_overview)
            }
        }else if (input$aspatialLevel == 'households'){
            if(input$HHLevel == 'hhSizInc'){
                if(input$comparison == FALSE){
                    dataTable <- siloAspatialHHSizeIncome(global$o_hhType)
                } else {
                    o_households <- siloAspatialHHSizeIncome(global$o_hhType)
                    c_households <- siloAspatialHHSizeIncome(global$c_hhType)
                    dataTable <- siloAspatialTableComparator(o_households, c_households)
                }
            } else if(input$HHLevel == 'hhRace'){
                if(input$comparison == FALSE){
                    dataTable <- siloAspatialRace(global$o_spatialData)
                    
                } else {
                    o_households <- siloAspatialRace(global$o_spatialData)
                    c_households <- siloAspatialRace(global$c_spatialData)
                    dataTable <- siloAspatialTableComparator(o_households, c_households)
                }
            } else if(input$HHLevel =='hhSize'){
                if(input$comparison == FALSE){
                    dataTable <- siloAspatialHHSize(global$o_hhType)
                }else {
                    o_hhSize <-siloAspatialHHSize(global$o_hhType)
                    c_hhSize <-siloAspatialHHSize(global$c_hhType)
                    dataTable <- siloAspatialTableComparator(o_hhSize, c_hhSize)
                }
            }else if(input$HHLevel == 'hhAvSize'){
                if(input$comparison == FALSE){
                    dataTable <- global$o_aveHhsize
                }else {
                    dataTable <- siloAspatialTableComparator(global$o_aveHhsize, global$c_aveHhsize)
                }
            }else if(input$HHLevel =='hhInc'){
                if(input$comparison == FALSE){
                    dataTable <- global$o_hhAvIn  
                }else{
                    global$o_hhAvIn[['variable']] <-as.character(global$o_hhAvIn[['variable']])
                    global$c_hhAvIn[['variable']] <-as.character(global$c_hhAvIn[['variable']])
                    dataTable <- siloAspatialTableComparator(global$o_hhAvIn, global$c_hhAvIn)
                }
            }else if(input$HHLevel == 'hhCarOwnLev'){
                if(input$comparison == FALSE){
                    global$o_c_owne[['carOwnershipLevel']]<-as.character(global$o_c_owne[['carOwnershipLevel']])
                    dataTable <- global$o_c_owne
                    print(global$o_c_owne)
                }else{
                    global$o_c_owne[['carOwnershipLevel']]<-as.character(global$o_c_owne[['carOwnershipLevel']])
                    global$c_c_owne[['carOwnershipLevel']]<-as.character(global$c_c_owne[['carOwnershipLevel']])
                    dataTable <- siloAspatialTableComparator(global$o_c_owne, global$c_c_owne)
                }
            }else if(input$HHLevel == 'hhRentIncome'){
                if(input$comparison == FALSE){
                    dataTable <- siloAspatialHHRentIncome(global$o_hhReIn)
                }else{
                    o_hhRentInc <- siloAspatialHHRentIncome(global$o_hhReIn)
                    c_hhRentInc <- siloAspatialHHRentIncome(global$c_hhReIn)
                    dataTable <- siloAspatialTableComparator(o_hhRentInc, c_hhRentInc)
                }
            }else if(input$HHLevel == 'hhAvRent'){
                if(input$comparison == FALSE){
                    dataTable <- siloAspatialHHAvRent(global$o_hhReIn)
                }else{
                    o_hhRentInc <- siloAspatialHHAvRent(global$o_hhReIn)
                    c_hhRentInc <- siloAspatialHHAvRent(global$c_hhReIn)
                    dataTable <- siloAspatialTableComparator(o_hhRentInc,c_hhRentInc)
                }
            }
        } else if (input$aspatialLevel == 'persons'){
            if(input$personsLevel == 'peAgeGend'){
                if(input$comparison == FALSE){
                    print('we are in the correct place')
                    dataTable <-siloAspatialPopAge(global$o_popYea)    
                }else{
                    o_popAge <-siloAspatialPopAge(global$o_popYea)
                    c_popAge <-siloAspatialPopAge(global$c_popYea)
                    dataTable <- siloAspatialTableComparator(o_popAge, c_popAge)
                }
            }else if(input$personsLevel == 'peRace'){
                if(input$comparison == FALSE){
                    dataTable <- siloAspatialPopRace(global$o_perRac)
                }else{
                    o_perRace <- siloAspatialPopRace(global$o_perRac)
                    c_perRace <- siloAspatialPopRace(global$c_perRac)
                }
            }else if (input$personsLevel == 'peLaborPartRate'){
                if(input$comparison == FALSE){
                    dataTable <-siloAspatialPopParticippation(global$o_laPaRa)
                }else{
                    o_popPart <-siloAspatialPopParticippation(global$o_laPaRa)
                    c_popPart <-siloAspatialPopParticippation(global$c_laPaRa)
                    dataTable <-siloAspatialTableComparator(o_popPart, c_popPart)
                }
                    
            }else if(input$personsLevel == 'pemigration'){
                if(input$comparison == FALSE){
                    dataTable <-siloAspatialpopMigration(global$o_perMig)    
                }else{
                    o_popMig <-siloAspatialpopMigration(global$o_perMig)
                    c_popMig <-siloAspatialpopMigration(global$c_perMig)
                    dataTable <-siloAspatialTableComparator(o_popMig, c_popMig)
                }
            }
        } else if (input$aspatialLevel =='dwellings'){
            if (input$dwellingsLevel == 'dwellQuality'){
                if(input$comparison == FALSE){
                    dataTable <-siloAspatialDwellingQuality(global$o_dwelQu)
                }else{
                    o_dewllQuality <-siloAspatialDwellingQuality(global$o_dwelQu)
                    c_dewllQuality <-siloAspatialDwellingQuality(global$c_dwelQu)
                    dataTable <-siloAspatialTableComparator(o_dewllQuality, c_dewllQuality)
                }
            }else if(input$dwellingsLevel == 'dwellType'){
                if(input$comparison == FALSE){
                    dataTable <- siloAspatialDwellings(global$o_dwelli,'count')
                }else{
                    o_dwellType <- siloAspatialDwellings(global$o_dwelli,'count')
                    c_dwellType <- siloAspatialDwellings(global$c_dwelli,'count')
                    dataTable <- siloAspatialTableComparator(o_dwellType, c_dwellType)
                }
                
            }else if(input$dwellingsLevel == 'dwellAvMonPrice'){
                if(input$comparison == FALSE){
                    dataTable <- siloAspatialDwellings(global$o_dwelli,'price')
                }else{
                    o_dwellAvPr <-siloAspatialDwellings(global$o_dwelli,'price')
                    o_dwellAvPr <-siloAspatialDwellings(global$c_dwelli,'price')
                    dataTable <- siloAspatialTableComparator(o_dwellAvPr, c_dwellAvPr)
                }
                
            }else if(input$dwellingsLevel == 'dwellVacancy'){
                if(input$comparison == FALSE){
                    dataTable <- siloAspatialDwellings(global$o_dwelli,'vacancy')
                    
                }else{
                    o_dwellVac <-siloAspatialDwellings(global$o_dwelli,'vacancy')
                    c_dwellVac <-siloAspatialDwellings(global$c_dwelli,'vacancy')
                    dataTable <- siloAspatialTableComparator(o_dwellVac, c_dwellVac)
                }
            }
        } else if (input$aspatialLevel =='regional'){
            ## This one could be moved to the map
            
        } else if (input$aspatialLevel == 'events'){
            print('Entering to the events logic')
            if(input$eventsLevel == 'hhEvents'){
               if(input$comparison == FALSE){
                   print('Entering to hh events logic')
                    dataTable <-siloAspatialEvents(global$o_eventc, hhEvents)
               }else{
                   dataTable <-siloAspatialEvents(global$o_eventc, hhEvents)
               }
            }else if(input$eventsLevel == 'peEvents'){
                if(input$comparison == FALSE){
                    print('Entering to persons events logic')
                     dataTable <-siloAspatialEvents(global$o_eventc, perEvents)
                }    
            }else if(input$eventsLevel == 'dwellEvents'){
                if(input$comparison == FALSE){
                    print('Entering to dwelling events logic')
                    dataTable <-siloAspatialEvents(global$o_eventc, dwellEvents)
                }
                
            }
            
        }
        return(dataTable)
        
    })
    
    
    fig <- reactive({
        msmSequential <- viridisLite::viridis(10, direction = -1)
        if(input$aspatialLevel == 'overview'){
            ## Parametrize
            
            fig <-plot_ly(getAspatialData(), x = ~popYear)
            
            fig <-fig%>%add_trace(y = ~population, name = 'Population', type = 'scatter', mode = 'lines')
            fig <-fig%>%add_trace(y = ~men, name = 'Men',  type = 'scatter', mode ='lines') 
            fig <-fig%>%add_trace(y = ~women, name = 'Women',  type = 'scatter', mode ='lines')
            fig <-fig%>%add_trace(y = ~households, name = 'Households',  type = 'scatter', mode ='lines') 
            fig <-fig%>%add_trace(y = ~dwellings, name = 'Dwellings',  type = 'scatter', mode ='lines')
            fig<- fig%>%layout(title= "Overview")
        }else if (input$aspatialLevel == 'households'){
            if(input$HHLevel == 'hhSizInc'){
                fig <- msmAnimatedLines(getAspatialData,msmSequential,input$switchView)
            } else if(input$HHLevel == 'hhRace'){
                fig <- plot_ly(getAspatialData(), x= ~year)
                fig <- fig%>% add_trace(y= ~shWhite, name ='White', type = 'scatter', mode = 'lines')
                fig <- fig%>% add_trace(y= ~shBlack, name ='Black', type = 'scatter', mode = 'lines')
                fig <- fig%>% add_trace(y= ~shHispanic, name ='Hispanic', type = 'scatter', mode = 'lines')
                fig <- fig%>% add_trace(y= ~shOther, name ='Other', type = 'scatter', mode = 'lines')
                fig <- fig%>% layout(title = "Households by Race")
            } else if(input$HHLevel == 'hhSize'){
                
                #fig<-msmSimpleLines(getAspatialData, msmSequential)
                
                fig <- plot_ly(getAspatialData(), x= ~Year, y= ~Households, type = 'scatter', mode = 'markers', 
                                        color = ~hh_size,colors = msmSequential, line = list(simplify = F))
                #fig<-plot_ly(getAspatialData(), x = ~year, y = ~Households, type = "scatter", mode = "lines", color = ~as.factor(hh_size), colors = msmSequential)
            }else if(input$HHLevel == 'hhAvSize'){
                fig <-plot_ly(getAspatialData(), x = ~year)
                fig<-fig%>%add_trace(y= ~size, type='scatter', mode = 'lines')
                #fig<- plot_ly(getAspatialData(), X=~year, y= ~size, type='scatter', mode='markers',
                #              colors = msmSequential, line = list(simplify = F))
            }else if(input$HHLevel =='hhInc'){
                fig <-plot_ly(getAspatialData(), x=~year, y=~value, type='scatter', color=~variable, colors = msmSequential, line = list(simplify = F))

            }else if(input$HHLevel == 'hhCarOwnLev'){
                fig<-plot_ly(getAspatialData(), x=~year, y=~households, type='scatter', color=~carOwnershipLevel, colors = msmSequential, line = list(simplify = F))
            }else if(input$HHLevel == 'hhRentIncome'){
                fig <- msmAnimatedLines(getAspatialData,msmSequential,input$switchView)
            }else if(input$HHLevel == 'hhAvRent'){
                fig <- msmSimpleLines(getAspatialData, msmSequential)
            }
        }else if (input$aspatialLevel == 'persons'){
            if(input$personsLevel == 'peAgeGend'){
                if(input$pyramid == TRUE){
                    
                    fig<- msmPyramid(getAspatialData, msmSequential[c(3, 4)], c(-60000, -40000, -20000, 0, 20000, 40000, 60000),
                                     c("60k", "40k", "20k", "0", "20k", "40k", "60k"))
                }else{
                    fig<- msmPyramidLines(getAspatialData,msmSequential)
                }
            }else if(input$personsLevel == 'peRace'){
                fig <-msmSimpleLines(getAspatialData, msmSequential)
            }else if(input$personsLevel == 'peLaborPartRate'){
                fig <-msmAnimatedBars (getAspatialData, msmSequential)
            }else if(input$personsLevel == 'pemigration'){
                fig <-msmSimpleLines(getAspatialData, msmSequential)
            }
        }else if (input$aspatialLevel == 'dwellings'){
            fig <-msmSimpleLines(getAspatialData, msmSequential)
        }else if(input$aspatialLevel == 'regional'){
            
        }else if(input$aspatialLevel == 'events'){
            fig <-msmSimpleLines(getAspatialData, msmSequential)
        }
        return(fig)
    })
    
    output$siloPlot <-renderPlotly(
        fig()
    )

        
})
