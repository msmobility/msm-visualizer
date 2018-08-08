## DEFINING LOGIC FOR SPATIAL DATA
# CREATE A REACTIVE THAT HOLDS THE CLEANED SPATIAL DATA
clean_spatial <- reactive({
  req(input$resultFiles)
  myFiles <- input$resultFiles %>% 
    arrange(size)
  if(nrow(myFiles) > 1){
    link <- myFiles$datapath[2]
  } else {
    link <- myFiles$datapath[1]
  }
  link %>%  
    read_csv() %>% 
    inner_join(zones, by = c("Zone" = "id")) %>%
    mutate(AllP = sum(c(HBWA, HBEA, HBSA, HBOA, NHBWA, NHBOA)),
           AllA = sum(c(HBWA, HBEA, HBSA, HBOA, NHBWA, NHBOA))) %>% 
    mutate_at(paste0(myPurposeCodes, "P"), funs(1000000 * . / Area)) %>% 
    mutate_at(paste0(myPurposeCodes, "A"), funs(1000000 * . / Area)) %>% 
    mutate_at(paste0(myPurposeCodes, "P"), funs(round)) %>% 
    mutate_at(paste0(myPurposeCodes, "A"), funs(round)) %>% 
    st_sf()
})

# CREATE A REACTIVE THAT HOLDS THE CLEANED SPATIAL DATA OF THE COMPARISON SCENARIO
clean_spatial2 <- reactive({
  req(input$scenarioFiles)
  myFiles <- input$scenarioFiles %>% 
    arrange(size)
  if(nrow(myFiles) > 1){
    link <- myFiles$datapath[2]
  } else {
    link <- myFiles$datapath[1]
  }
  link %>%  
    read_csv() %>% 
    inner_join(zones, by = c("Zone" = "id")) %>%
    mutate(AllP = sum(c(HBWA, HBEA, HBSA, HBOA, NHBWA, NHBOA)),
           AllA = sum(c(HBWA, HBEA, HBSA, HBOA, NHBWA, NHBOA))) %>% 
    mutate_at(paste0(myPurposeCodes, "P"), funs(1000000 * . / Area)) %>% 
    mutate_at(paste0(myPurposeCodes, "A"), funs(1000000 * . / Area)) %>% 
    mutate_at(paste0(myPurposeCodes, "P"), funs(round)) %>% 
    mutate_at(paste0(myPurposeCodes, "A"), funs(round)) %>% 
    st_sf()
})

# CREATE A REACTIVE THAT HOLDS THE SELECTED SPATIAL ATTRIBUTE TO BE DISPLAYED
map_subset <- reactive({
  attribute <- paste0(input$purpose, input$spatialData)
  df <- clean_spatial() %>% 
    select(Zone, attribute, geometry)
  df[[2]] = round(df[[2]], digits = 2)
  df
})

# CREATE A REACTIVE THAT HOLDS THE SELECTED SPATIAL ATTRIBUTE FROM BOTH SCENARIOS
map_subset2 <- reactive({
  attribute <- paste0(input$purpose, input$spatialData)
  df <- clean_spatial2() %>% 
    st_set_geometry(NULL) %>% 
    select(Zone, attribute) 
  df[[2]] = round(df[[2]], digits = 2)
  df <- left_join(map_subset(), df, by = "Zone") %>% 
    mutate(Difference = .[[3]] - .[[2]])
  df
})

# CREATE A TABLE THAT CONTAINS THE PLOTTED SPATIAL DATA
output$mapTable <- DT::renderDataTable({
  myAttribute <- myNaming(mySpatial, mySpatialCodes, input$spatialData)
  if (input$scenario == FALSE){
    df <- map_subset() 
    names(df)[2] <- myAttribute
  } else {
    df <- map_subset2()
    names(df)[2] <- paste(myAttribute, " (Base)")
    names(df)[3] <- paste(myAttribute, " (Comparison)")
  }
  df <- df %>%
    st_set_geometry(NULL)
  DT::datatable(df, rownames = FALSE, extensions = "Buttons", 
                options = list(pageLength = 15, dom = "Bfrtip", 
                               buttons = c("copy", "csv", "excel", "print", "pdf")))
})

# CREATE A MAP THAT DISPLAYS SPATIAL ATTRIBUTES
output$map <- renderLeaflet({
  if (input$scenario == FALSE) {
    df <- map_subset()
    myAttribute <- names(df)[2]
  } else {
    df <- map_subset2()
    myAttribute <- names(df)[4]
  }
  if(input$manualStyle == FALSE){
    myStyle <- myNaming(mySpatialStyles, mySpatialCodes, input$spatialData)
    myBreak <- NULL
  } else {
    myStyle <- "fixed"
    myBreak <- as.numeric(unlist(strsplit(input$breaks, ",")))
  }
  tmap_leaflet(tm_shape(df) +
                 tm_fill(myAttribute,
                         title = myNaming(mySpatial, mySpatialCodes, input$spatialData),
                         style = myStyle, breaks = myBreak, n = input$categories))
})