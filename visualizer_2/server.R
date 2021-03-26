## Server logic
shinyServer(function(input, output, session) {
    
    ################################# File reader plugin  #################################
    ## Directory selector original scenario
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
    ## Directory selector for comparison scenario
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
    ################################# File reader functions #################################
    observeEvent(input$update, {loadfiles()})
    
    loadfiles <- reactive({
        if (global$datapath == getwd()){}
        else {
            ## File reader, loop fileList vector and store variables in session$userData$scenario_filename
            ## Define session menu Settings Database
            session$userData$menuSettings <- menuSettings
            j= 1
            for(i in fileList){
                filePath = paste(global$datapath,i,sep="/", collapse = NULL)
                file_exists <- file_test("-f", filePath) ##Test path
                if(file_exists == TRUE){
                    print(filePath)
                    varName <-fileNames[j]
                    varName <-paste('o',varName, sep='_')
                    # Store databases in session$userData, each database starts with o_ + values found in fileNames
                    session$userData[[varName]] <- read.csv(paste(global$datapath,fileList[j],sep="/", collapse = NULL))
                    ## Reset click on region filter
                    if(i == "regionAvCommutingTime.csv"){
                        updateCheckboxInput(session, "showClickPlot1", value = TRUE)
                    }else if (i == "regionAvailableLand.csv"){
                        updateCheckboxInput(session, "showClickPlot2", value = TRUE)
                    }else if (i == "jobsBySectorAndRegion.csv"){
                        updateCheckboxInput(session, "showClickPlot3", value = TRUE)
                    }
                }else{
                    print(paste("Warning , ",filePath, " does not exists in this scenario, dependent plots will be hidden"))
                    ## Here, pending to chose a modify options from vectors :
                    session$userData$menuSettings <- filter(session$userData$menuSettings, required_file_1 != fileList[j] | is.na(required_file_1))
                    session$userData$menuSettings <- filter(session$userData$menuSettings, required_file_2 != fileList[j] | is.na(required_file_2))
                    session$userData$menuSettings <- filter(session$userData$menuSettings, required_file_3 != fileList[j]| is.na(required_file_3))
                    ## Set click on region filters
                    if(i == "regionAvCommutingTime.csv"){
                        updateCheckboxInput(session, "showClickPlot1", value = FALSE)
                        print("No database for commuting time, click on region plot will be disabled")
                        session$userData$clickAvCommuting <- FALSE
                    }else if (i == "regionAvailableLand.csv"){
                        updateCheckboxInput(session, "showClickPlot2", value = FALSE)
                        print("No database for available land, click on region plot will be disabled")
                    }else if (i == "jobsBySectorAndRegion.csv"){
                        updateCheckboxInput(session, "showClickPlot3", value = FALSE)
                        print("No database for jobs by region and sector, click on region plot will be disabled")
                    }
                }
                j=j+1
            }
            session$userData$o_spatialData <-rename(session$userData$o_spatialData, "shp_id" = "zone")
        }

        pathElements <-str_split(global$datapath, '/', simplify = TRUE)
        global$implementation_value <-pathElements[length(pathElements)-1]
    })
    ## Scenario File loader
    observeEvent(input$update, {loadfiles2()})
    
    loadfiles2 <- reactive({
        if (global$datapath2 == getwd()){}
        else {
            ## File reader, loop fileList vector and store variables in session$userData$scenario_filename
            
            j= 1
            for(l in fileList){
                filePath2 = paste(global$datapath2,l,sep="/", collapse = NULL)
                file_exists <- file_test("-f", filePath2) ##Test path
                if(file_exists == TRUE){
                    print(filePath2)
                    varName <-fileNames[j]
                    varName <-paste('c',varName, sep='_')
                    # Store databases in session$userData, each database starts with o_ + values found in fileNames
                    session$userData[[varName]] <- read.csv(paste(global$datapath2,fileList[j],sep="/", collapse = NULL))
                }else{
                    print(paste("Warning, ",filePath2, " does not exists in this scenario, dependent plots will be hidden"))
                    ## Here, pending to chose a modify options from vectors :
                    
                    session$userData$menuSettings <- filter(session$userData$menuSettings, required_file_1 != fileList[j] | is.na(required_file_1))
                    session$userData$menuSettings <- filter(session$userData$menuSettings, required_file_2 != fileList[j] | is.na(required_file_2))
                    session$userData$menuSettings <- filter(session$userData$menuSettings, required_file_3 != fileList[j]| is.na(required_file_3))
                    ## Set click on region filters
                    if(l == "regionAvCommutingTime.csv"){
                        updateCheckboxInput(session, "showClickPlot1", value = FALSE)
                        print("No database for commuting time, click on region plot will be disabled")
                        session$userData$clickAvCommuting <- FALSE
                    }else if (l == "regionAvailableLand.csv"){
                        updateCheckboxInput(session, "showClickPlot2", value = FALSE)
                        print("No database for available land, click on region plot will be disabled")
                    }else if (l == "jobsBySectorAndRegion.csv"){
                        updateCheckboxInput(session, "showClickPlot3", value = FALSE)
                        print("No database for jobs by region and sector, click on region plot will be disabled")
                    }
                }
                j=j+1
            }
            session$userData$c_spatialData <-rename(session$userData$c_spatialData, "shp_id" = "zone")
        }
    })
    ## Regional plots checking
    output$showRegionalPlots <-reactive({
        regionalPlots <-session$userData$enableRegionalPlots
        return(regionalPlots)
    })
    outputOptions(output, "showRegionalPlots", suspendWhenHidden = FALSE)
    ## Update elements from implementation
    observeEvent(input$update, {updateData()})
    updateData <-reactive({
        print("Updating menu")
        updateVar <-input$update
        initialYear <- as.numeric(min(session$userData$o_popYea$year))
        session$userData$initialYear <- initialYear
        finalYear <- as.numeric(max(session$userData$o_popYea$year))
        updateSliderInput(session, "year", min = initialYear, max = finalYear)
        
        ## Update menu options
        
        session$userData$aspatialMenu <- unlist(filter(session$userData$menuSettings, visualization == 'aspatial' & attribute_name == 'Menu')%>%select(category_value))
        names(session$userData$aspatialMenu) <- unlist(filter(session$userData$menuSettings, visualization == 'aspatial' & attribute_name == 'Menu')%>%select(category_name))
        
        session$userData$aHH <- unlist(filter(session$userData$menuSettings, visualization =='aspatial'& attribute_name =='Households') %>%select(category_value))
        names(session$userData$aHH) = unlist(filter(session$userData$menuSettings, visualization =='aspatial'& attribute_name =='Households')%>%select(category_name))
        
        session$userData$aPerson <- unlist(filter(session$userData$menuSettings, visualization == 'aspatial' & attribute_name == 'Persons')%>%select(category_value))
        names(session$userData$aPerson) <-unlist(filter(session$userData$menuSettings, visualization == 'aspatial' & attribute_name == 'Persons')%>%select(category_name))
        
        session$userData$aDwelling <- unlist(filter(session$userData$menuSettings, visualization == 'aspatial' & attribute_name == 'Dwellings')%>%select(category_value))
        names(session$userData$aDwelling) <-unlist(filter(session$userData$menuSettings, visualization == 'aspatial' & attribute_name == 'Dwellings')%>%select(category_name))
        
        session$userData$aRegional <- unlist(filter(session$userData$menuSettings, visualization == 'aspatial' & attribute_name == 'Regional')%>%select(category_value))
        names(session$userData$aRegional) <-unlist(filter(session$userData$menuSettings, visualization == 'aspatial' & attribute_name == 'Regional')%>%select(category_name))
        
        session$userData$aEvent <- unlist(filter(session$userData$menuSettings, visualization == 'aspatial' & attribute_name == 'Events')%>%select(category_value))
        names(session$userData$aEvent) <-unlist(filter(session$userData$menuSettings, visualization == 'aspatial' & attribute_name == 'Events')%>%select(category_name))
        
        ## Update function
        
        updateSelectInput(session, 'aspatialLevel',"Select aspatial attribute",(session$userData$aspatialMenu))
        updateSelectInput(session, 'HHLevel',"Select household level", (session$userData$aHH))
        updateSelectInput(session, 'personsLevel',"Select person level", (session$userData$aPerson))
        updateSelectInput(session, 'dwellingsLevel',"Select dwelling level", (session$userData$aDwelling))
        updateSelectInput(session, 'regionalLevel',"Select regional level", (session$userData$aRegional))
        updateSelectInput(session, 'eventsLevel',"Select event attribute", (session$userData$aEvent))
        
        ## Update zones shapefile
        #global$zones <- st_read(paste(here(),"shapefiles",global$implementation_value,"zone_system.shp",sep="/"))
        global$zones <- st_read(paste(here(),"use_cases",global$implementation_value,"zone_shapefile","zone_system.shp",sep="/"))
    })
    ## Dummy function to trigger maps
    dummyfunc <-eventReactive(input$update, {dummycall(1)})

    ## Plot Map
    output$siloMap <- renderLeaflet({
        n <-dummyfunc()
        ## Select color
        ## Identify type of process (1 scenario / 2 scenarios)
        if(input$comparison == TRUE){
            if(input$comparisonSelectorDouble ==1){
                farbe <- "-RdBu" 
            } else if (input$comparisonSelectorDouble ==2){
                farbe <- "-RdBu"
            } else {
                if(input$scenarioSelector == 1){
                } else {
                }
                farbe <- "YlOrBr"
            }    
        }else{
            if(input$comparisonSelectorSingle ==2){
                farbe <- "-RdBu" 
            } else {
                if(input$scenarioSelector == 1){
                } else {
                }
                farbe <- "YlOrBr"
            }
        }

        ## Map color selection
        
        ## Execute maps

        dataSubset <-spatialData()[[1]]
        legend <- spatialData()[[2]]
        attribute <- spatialData()[[3]]

        msmMap(dataSubset,attribute, farbe,legend[1],legend[2],
               input$siloMapStyle, input$siloMapCategories)
        
    })

    ################################# Spatial Data procedures #################################
    # Modularize spatial inputs
    spatialData <-reactive({
        n <-dummyfunc()
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
        if(input$comparison == TRUE){
            comparisonSelector <- input$comparisonSelectorDouble
        }else{
            comparisonSelector <- input$comparisonSelectorSingle
        }
            
        if(comparisonSelector ==1){

            isComparison <-TRUE
            legend <- prepareSiloMapLabels(myLabels, "siloSpatial", attribute, isComparison)
            originalDataSet <-prepareSiloMap(session$userData$o_spatialData, input$year, input$zoneAgg, attribute, aggregationType, global$zones)
            scenarioDataSet <-prepareSiloMap(session$userData$c_spatialData, input$year, input$zoneAgg, attribute, aggregationType, global$zones)
            
            groupedTable <- compareScenarios(originalDataSet, scenarioDataSet,attribute)  
            
        } else if (comparisonSelector ==2){
            
            farbe <- "-RdBu"
            isComparison <-TRUE
            
            legend <- prepareSiloMapLabels(myLabels, "siloSpatial", attribute, isComparison)
            ## Set here conditions based on select scenario (pending)
            if(input$scenarioSelector == 1){
                originalDataSet <-prepareSiloMap(session$userData$o_spatialData, session$userData$initialYear, input$zoneAgg, attribute, aggregationType, global$zones)
                scenarioDataSet <-prepareSiloMap(session$userData$o_spatialData, input$year, input$zoneAgg, attribute, aggregationType, global$zones)
                groupedTable <- compareScenarios(originalDataSet, scenarioDataSet,attribute)
            }else{
                originalDataSet <-prepareSiloMap(session$userData$c_spatialData, session$userData$initialYear, input$zoneAgg, attribute, aggregationType, global$zones)
                scenarioDataSet <-prepareSiloMap(session$userData$c_spatialData, input$year, input$zoneAgg, attribute, aggregationType, global$zones)
                groupedTable <- compareScenarios(originalDataSet, scenarioDataSet,attribute)
            }
        } else {
            if(input$scenarioSelector == 1){
                spatialDataSet <-session$userData$o_spatialData
            } else {
                spatialDataSet <-session$userData$c_spatialData
            }
            
            isComparison <-FALSE
            legend <- prepareSiloMapLabels(myLabels, "siloSpatial", attribute, isComparison)
            groupedTable <-prepareSiloMap(spatialDataSet, input$year, input$zoneAgg, attribute, aggregationType, global$zones)
            
        }

        
        return(list(groupedTable, legend, attribute))
        
    }) 
    output$gisTable <- renderDataTable(
        select(as.data.frame(spatialData()[1]),-geometry)
    )
    output$aspatialTable <-renderDataTable(
        getAspatialData()
    )
    ##  select(as.data.frame(global$exportTable),-geometry)
    ################################# Table download #################################
    output$downloadData <- downloadHandler(
        filename <- function() {
            paste("export_example", ".csv", sep = "")
        },
        content <- function(file) {
            write.csv(select(as.data.frame(spatialData()[1]),-geometry), file, row.names = TRUE)
        }
    )
    output$downloadShape<-downloadHandler(
        filename <- function() { paste("shpExport","zip", sep = ".") },
        content <- function(file){
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
    ################################# Aspatial Logic #################################
    ## Calculate dataTable output used in plots based on the input parameters
    getAspatialData <- reactive({
        n <-dummyfunc()
        
        if(input$aspatialLevel == 'overview'){
            if(input$comparison == FALSE){
                dataTable <- siloAspatialOverview(session$userData$o_popYea, session$userData$o_dwelli, session$userData$o_hhSize)
            } else {
                o_overview <- siloAspatialOverview(session$userData$o_popYea, session$userData$o_dwelli, session$userData$o_hhSize)
                c_overview <- siloAspatialOverview(session$userData$c_popYea, session$userData$c_dwelli, session$userData$c_hhSize)
                dataTable <- siloAspatialTableComparator(o_overview, c_overview)
            }
        }else if (input$aspatialLevel == 'households'){
            if(input$HHLevel == 'hhSizInc'){
                if(input$comparison == FALSE){
                    dataTable <- siloAspatialHHSizeIncome(session$userData$o_hhType)
                } else {
                    o_households <- siloAspatialHHSizeIncome(session$userData$o_hhType)
                    c_households <- siloAspatialHHSizeIncome(session$userData$c_hhType)
                    dataTable <- siloAspatialTableComparator(o_households, c_households)
                }
            } else if(input$HHLevel == 'hhRace'){
                if(input$comparison == FALSE){
                    dataTable <- siloAspatialRace(session$userData$o_spatialData)
                    
                } else {
                    o_households <- siloAspatialRace(session$userData$o_spatialData)
                    c_households <- siloAspatialRace(session$userData$c_spatialData)
                    dataTable <- siloAspatialTableComparator(o_households, c_households)
                }
            } else if(input$HHLevel =='hhSize'){
                if(input$comparison == FALSE){
                    dataTable <- siloAspatialHHSize(session$userData$o_hhType)
                }else {
                    o_hhSize <-siloAspatialHHSize(session$userData$o_hhType)
                    c_hhSize <-siloAspatialHHSize(session$userData$c_hhType)
                    dataTable <- siloAspatialTableComparator(o_hhSize, c_hhSize)
                }
            }else if(input$HHLevel == 'hhAvSize'){
                if(input$comparison == FALSE){
                    dataTable <- session$userData$o_aveHhSize
                }else {
                    dataTable <- siloAspatialTableComparator(session$userData$o_aveHhSize, session$userData$c_aveHhSize)
                }
            }else if(input$HHLevel =='hhInc'){
                if(input$comparison == FALSE){
                    dataTable <- session$userData$o_hhAvIn
                    dataTable <-dataTable%>%rename(Year = year, Key = variable, Value = value)
                    print("HH average income table")
                    print(dataTable)
                }else{
                    session$userData$o_hhAvIn[['variable']] <-as.character(session$userData$o_hhAvIn[['variable']])
                    session$userData$c_hhAvIn[['variable']] <-as.character(session$userData$c_hhAvIn[['variable']])
                    dataTable <- siloAspatialTableComparator(session$userData$o_hhAvIn, session$userData$c_hhAvIn)
                    dataTable <-dataTable%>%rename(Year = year, Key = variable, Value = value)
                }
            }else if(input$HHLevel == 'hhCarOwnLev'){
                if(input$comparison == FALSE){
                    dataTable = siloAspatialHHCarOwnership(session$userData$o_c_owne)
                    #global$o_c_owne[['carOwnershipLevel']]<-as.character(global$o_c_owne[['carOwnershipLevel']])
                    #dataTable <- global$o_c_owne
                    #print(global$o_c_owne)
                }else{
                    session$userData$o_c_owne[['carOwnershipLevel']]<-as.character(session$userData$o_c_owne[['carOwnershipLevel']])
                    session$userData$c_c_owne[['carOwnershipLevel']]<-as.character(session$userData$c_c_owne[['carOwnershipLevel']])
                    dataTable <- siloAspatialTableComparator(session$userData$o_c_owne, session$userData$c_c_owne)
                    dataTable <-dataTable%>%rename(Year = year, Key = carOwnershipLevel, Value = households)
                }
            }else if(input$HHLevel == 'hhRentIncome'){
                if(input$comparison == FALSE){
                    dataTable <- siloAspatialHHRentIncome(session$userData$o_hhReIn)
                    #print("the datatable is")
                    #print(dataTable)
                }else{
                    o_hhRentInc <- siloAspatialHHRentIncome(session$userData$o_hhReIn)
                    c_hhRentInc <- siloAspatialHHRentIncome(session$userData$c_hhReIn)
                    
                    dataTable <- siloAspatialTableComparator(o_hhRentInc, c_hhRentInc)
                    #print("the datatable is")
                    #print(dataTable)
                }
            }else if(input$HHLevel == 'hhAvRent'){
                if(input$comparison == FALSE){
                    dataTable <- siloAspatialHHAvRent(session$userData$o_hhReIn)
                }else{
                    o_hhRentInc <- siloAspatialHHAvRent(session$userData$o_hhReIn)
                    c_hhRentInc <- siloAspatialHHAvRent(session$userData$c_hhReIn)
                    dataTable <- siloAspatialTableComparator(o_hhRentInc,c_hhRentInc)
                }
            }
        } else if (input$aspatialLevel == 'persons'){
            if(input$personsLevel == 'peAgeGend'){
                if(input$comparison == FALSE){
                    dataTable <-siloAspatialPopAge(session$userData$o_popYea)    
                }else{
                    o_popAge <-siloAspatialPopAge(session$userData$o_popYea)
                    c_popAge <-siloAspatialPopAge(session$userData$c_popYea)
                    dataTable <- siloAspatialTableComparator(o_popAge, c_popAge)
                }
            }else if(input$personsLevel == 'peRace'){
                if(input$comparison == FALSE){
                    dataTable <- siloAspatialPopRace(session$userData$o_perRac)
                }else{
                    o_perRace <- siloAspatialPopRace(session$userData$o_perRac)
                    c_perRace <- siloAspatialPopRace(session$userData$c_perRac)
                }
            }else if (input$personsLevel == 'peLaborPartRate'){
                if(input$comparison == FALSE){
                    dataTable <-siloAspatialPopParticipation(session$userData$o_laPaRa)
                }else{
                    o_popPart <-siloAspatialPopParticipation(session$userData$o_laPaRa)
                    c_popPart <-siloAspatialPopParticipation(session$userData$c_laPaRa)
                    dataTable <-siloAspatialTableComparator(o_popPart, c_popPart)
                }
                    
            }else if(input$personsLevel == 'pemigration'){
                if(input$comparison == FALSE){
                    dataTable <-siloAspatialpopMigration(session$userData$o_perMig)    
                }else{
                    o_popMig <-siloAspatialpopMigration(session$userData$o_perMig)
                    c_popMig <-siloAspatialpopMigration(session$userData$c_perMig)
                    dataTable <-siloAspatialTableComparator(o_popMig, c_popMig)
                }
            }
        } else if (input$aspatialLevel =='dwellings'){
            if (input$dwellingsLevel == 'dwellQuality'){
                if(input$comparison == FALSE){
                    dataTable <-siloAspatialDwellingQuality(session$userData$o_dwelQu)
                }else{
                    o_dewllQuality <-siloAspatialDwellingQuality(session$userData$o_dwelQu)
                    c_dewllQuality <-siloAspatialDwellingQuality(session$userData$c_dwelQu)
                    dataTable <-siloAspatialTableComparator(o_dewllQuality, c_dewllQuality)
                }
            }else if(input$dwellingsLevel == 'dwellType'){
                if(input$comparison == FALSE){
                    dataTable <- siloAspatialDwellings(session$userData$o_dwelli,'count')
                }else{
                    o_dwellType <- siloAspatialDwellings(session$userData$o_dwelli,'count')
                    c_dwellType <- siloAspatialDwellings(session$userData$c_dwelli,'count')
                    dataTable <- siloAspatialTableComparator(o_dwellType, c_dwellType)
                }
                
            }else if(input$dwellingsLevel == 'dwellAvMonPrice'){
                if(input$comparison == FALSE){
                    dataTable <- siloAspatialDwellings(session$userData$o_dwelli,'price')
                }else{
                    o_dwellAvPr <-siloAspatialDwellings(session$userData$o_dwelli,'price')
                    c_dwellAvPr <-siloAspatialDwellings(session$userData$c_dwelli,'price')
                    dataTable <- siloAspatialTableComparator(o_dwellAvPr, c_dwellAvPr)
                }
                
            }else if(input$dwellingsLevel == 'dwellVacancy'){
                if(input$comparison == FALSE){
                    dataTable <- siloAspatialDwellings(session$userData$o_dwelli,'vacancy')
                    
                }else{
                    o_dwellVac <-siloAspatialDwellings(session$userData$o_dwelli,'vacancy')
                    c_dwellVac <-siloAspatialDwellings(session$userData$c_dwelli,'vacancy')
                    dataTable <- siloAspatialTableComparator(o_dwellVac, c_dwellVac)
                }
            }
        } else if (input$aspatialLevel =='regional'){
            if(input$regionalLevel == 'reAvCommDist'){
                varColumn = 'minutes'
                dataTable <- session$userData$o_regCoT
                dataTable<- siloAspatialRegions(dataTable, global$zones, varColumn)
            }else if(input$regionalLevel == 'reAvailableLand'){
                varColumn = 'land'
                dataTable <- session$userData$o_lanReg
                dataTable<- siloAspatialRegions(dataTable, global$zones, varColumn)
            }else if (input$regionalLevel == 'reTotalJobs'){
                varColumn = 'total'
                dataTable <- session$userData$o_regJoS
                dataTable<- siloAspatialRegions(dataTable, global$zones, varColumn)
            }else if(input$regionalLevel == 'reJobsSect'){
                dataTable <- siloAspatialJobsReg(session$userData$o_regJoS)
            }
        } else if (input$aspatialLevel == 'events'){
            if(input$eventsLevel == 'hhEvents'){
               if(input$comparison == FALSE){
                    dataTable <-siloAspatialEvents(session$userData$o_eventc, hhEvents)
               }else{
                   o_eventCount <-siloAspatialEvents(session$userData$o_eventc, hhEvents)
                   c_eventCount <-siloAspatialEvents(session$userData$c_eventc, hhEvents)
                   dataTable <- siloAspatialTableComparator(o_eventCount, c_eventCount)
               }
            }else if(input$eventsLevel == 'peEvents'){
                if(input$comparison == FALSE){
                     dataTable <-siloAspatialEvents(session$userData$o_eventc, perEvents)
                }else{
                    o_eventCount <-siloAspatialEvents(session$userData$o_eventc, perEvents)
                    c_eventCount <-siloAspatialEvents(session$userData$c_eventc, perEvents)
                    dataTable <- siloAspatialTableComparator(o_eventCount, c_eventCount)
                }    
            }else if(input$eventsLevel == 'dwellEvents'){
                if(input$comparison == FALSE){
                    dataTable <-siloAspatialEvents(session$userData$o_eventc, dwellEvents)
                }else{
                    o_eventCount <-siloAspatialEvents(session$userData$o_eventc, dwellEvents)
                    c_eventCount <-siloAspatialEvents(session$userData$c_eventc, dwellEvents)
                    dataTable <- siloAspatialTableComparator(o_eventCount, c_eventCount)
                }
            }
        }
        return(dataTable)
    })
    ################################# Regional click on map plots #################################
    
    regionalTrigger <-eventReactive(input$siloMap_shape_click, {dummycall2(1)})
    ClickOnMapCommTime <- reactive({
        n <- regionalTrigger()
        database <- siloRegionalPlot(session$userData$o_regCoT, "commuteTime",input$siloMap_shape_click[1])
        if(input$comparison == TRUE){
            comparison <-siloRegionalPlot(session$userData$c_regCoT, "commuteTime",input$siloMap_shape_click[1])
            database <- siloAspatialTableComparator(database, comparison)
        }
        return (database)
    })
    ClickOnMapAvLand <- reactive({
        n <- regionalTrigger()
        database <- siloRegionalPlot(session$userData$o_lanReg, "availableLand",input$siloMap_shape_click[1])
        if(input$comparison == TRUE){
            comparison <- siloRegionalPlot(session$userData$c_lanReg, "availableLand",input$siloMap_shape_click[1])
            database <- siloAspatialTableComparator(database, comparison)
        }
        return (database)
    })
    ClickOnMapJobs<- reactive({
        n <- regionalTrigger()
        database <- siloRegionalPlot(session$userData$o_regJoS, "jobsByType",input$siloMap_shape_click[1])
        if(input$comparison == TRUE){
            comparison <- siloRegionalPlot(session$userData$c_regJoS, "jobsByType",input$siloMap_shape_click[1])
            database <- siloAspatialTableComparator(database, comparison)     
        }
        return (database)
    })
    
    ################################# Aspatial figure logic #################################
    ##Figure Logic
    fig <- reactive({
        msmSequential <- viridisLite::viridis(10, direction = -1)
        if(input$aspatialLevel == 'overview'){
            ## Parametrize
            fig <-msmSimpleLines(getAspatialData, msmSequential)
        }else if (input$aspatialLevel == 'households'){
            if(input$HHLevel == 'hhSizInc'){
                fig <- msmAnimatedLines(getAspatialData,msmSequential,input$switchView)
            } else if(input$HHLevel == 'hhRace'){
                fig <-msmSimpleLines(getAspatialData, msmSequential)
                fig <- fig%>% layout(title = "Households by Race")
            } else if(input$HHLevel == 'hhSize'){
                fig <- plot_ly(getAspatialData(), x= ~Year, y= ~Households, type = 'scatter', mode = 'markers', 
                                        color = ~hh_size,colors = msmSequential, line = list(simplify = F))
            }else if(input$HHLevel == 'hhAvSize'){
                fig <-plot_ly(getAspatialData(), x = ~year)
                fig<-fig%>%add_trace(y= ~size, type='scatter', mode = 'lines')
            }else if(input$HHLevel =='hhInc'){
                fig <-msmSimpleLines(getAspatialData, msmSequential)
            }else if(input$HHLevel == 'hhCarOwnLev'){
                fig <-msmSimpleLines(getAspatialData, msmSequential)
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
            if (input$regionalLevel == 'reJobsSect'){
                fig <-msmAnimatedBands(getAspatialData)
            }else{
                fig <-msmBands(getAspatialData, 'minutes')
            }
        }else if(input$aspatialLevel == 'events'){
            fig <-msmSimpleLines(getAspatialData, msmSequential)
        }
        ### Here, the labels will be implemented
        fig <- generateLabels(fig, "siloAspatial", input)

        
        return(fig)
    })
    ### Click on map figures
    figReg1 <- reactive({
        msmSequential <- viridisLite::viridis(10, direction = -1)
        
        figReg1 <-msmSimpleLines(ClickOnMapCommTime, msmSequential)
        figReg1 <-generateRegionalClickLabels(figReg1, "Average commuting time","Year", "Commuting time (m)", "Legend", input)
        return(figReg1)
    })
    figReg2 <- reactive({
        msmSequential <- viridisLite::viridis(10, direction = -1)
        figReg2 <- msmSimpleLines(ClickOnMapAvLand, msmSequential)
        figReg2 <-generateRegionalClickLabels(figReg2, "Available land","Year", "Available area", "Legend", input)
        return(figReg2)
    })
    figReg3 <- reactive({
        msmSequential <- viridisLite::viridis(10, direction = -1)
        figReg3 <-msmAnimatedLines(ClickOnMapJobs, msmSequential, FALSE)
        figReg3 <-generateRegionalClickLabels(figReg3, "Jobs by sector","Year", "Amount of jobs", "Legend", input)
        return(figReg3)
    })
    
    ################################# Output renders for aspatial plots #################################
    output$siloPlot <-renderPlotly(
        fig()
    )
    output$regCommPlot <-renderPlotly(
        figReg1() 
    )
    output$avLandPlot <- renderPlotly(
        figReg2()
    )
    output$jobsBySectorPlot <-renderPlotly(
        figReg3()
    )
})
