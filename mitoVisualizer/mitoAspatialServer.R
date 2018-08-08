
## DEFINING LOGIC FOR ASPATIAL DATA
# CREATE A REACTIVE THAT HOLDS THE CLEANED ASPATIAL DATA
clean_aspatial <- reactive({
  req(input$resultFiles)
  myFiles <- input$resultFiles %>% 
    arrange(size)
  link <- myFiles$datapath[1] 
  link %>% 
    read_csv() %>% 
    separate(Attribute, into = c("Feature", "Alternatives"), sep = "_") %>% 
    mutate_at(vars(HBW:NHBO), funs(as.numeric)) %>% 
    mutate_at(vars(HBW:NHBO), funs(replace_na), replace = 0) %>% 
    group_by(Feature) %>%
    mutate_at(vars(HBW:NHBO), funs(s = 100 * . / sum(.))) %>% 
    ungroup() %>% 
    mutate_at(vars(HBW_s:NHBO_s), funs(replace_na), replace = 0) %>% 
    mutate_at(vars(HBW_s:NHBO_s), funs(round), digits = 2)
})

# CREATE A REACTIVE THAT HOLDS THE CLEANED ASPATIAL DATA OF THE COMPARISON SCENARIO
clean_aspatial2 <- reactive({
  req(input$scenarioFiles)
  myFiles <- input$scenarioFiles %>% 
    arrange(size)
  link <- myFiles$datapath[1] 
  link %>% 
    read_csv() %>% 
    separate(Attribute, into = c("Feature", "Alternatives"), sep = "_") %>% 
    mutate_at(vars(HBW:NHBO), funs(as.numeric)) %>% 
    mutate_at(vars(HBW:NHBO), funs(replace_na), replace = 0) %>% 
    group_by(Feature) %>%
    mutate_at(vars(HBW:NHBO), funs(s = 100 * . / sum(.))) %>% 
    ungroup() %>% 
    mutate_at(vars(HBW_s:NHBO_s), funs(replace_na), replace = 0) %>% 
    mutate_at(vars(HBW_s:NHBO_s), funs(round), digits = 2)
})

# CREATE A REACTIVE THAT HOLDS THE SELECTED ASPATIAL ATTRIBUTE TO BE DISPLAYED
plot_subset <- reactive({
  attribute <- myNaming(myXLabels, myAspatialCodes, input$aspatialData)
  absVal <- myNaming(myYLabels, myAspatialCodes, input$aspatialData)
  relVal <- paste(absVal, " (%)")
  df <- clean_aspatial() %>% 
    filter(Feature == input$aspatialData) %>%
    select(Alternatives, input$purpose, paste(input$purpose, "_s", sep = ""))
  if(input$aspatialData != "ModeShare"){
    df <- mutate(df, Alternatives = as.numeric(Alternatives))
  } else {
    df <- mutate(df, Alternatives = mapvalues(Alternatives, from = myModeCodes, 
                                              to = myModeNames, warn_missing = FALSE))
  }
  names(df)[1:3] <- c(attribute, absVal, relVal)
  df
})

# CREATE A REACTIVE THAT HOLDS THE SELECTED ASPATIAL ATTRIBUTE FROM BOTH SCENARIOS
plot_subset2 <- reactive({
  attribute <- myNaming(myXLabels, myAspatialCodes, input$aspatialData)
  absVal <- myNaming(myYLabels, myAspatialCodes, input$aspatialData)
  relVal <- paste(absVal, " (%)")
  df <- clean_aspatial2() %>% 
    filter(Feature == input$aspatialData) %>%
    select(Alternatives, input$purpose, paste(input$purpose, "_s", sep = ""))
  if(input$aspatialData != "ModeShare"){
    df <- mutate(df, Alternatives = as.numeric(Alternatives))
  } else {
    df <- mutate(df, Alternatives = mapvalues(Alternatives, from = myModeCodes, 
                                              to = myModeNames, warn_missing = FALSE))
  }
  names(df)[1:3] <- c(attribute, absVal, relVal)
  df <- left_join(plot_subset(), df, by = attribute, suffix = c(" (Base)", " (Comparison)")) %>% 
    mutate(Difference = .[[4]] - .[[2]], `Difference (%)` = round(.[[5]] - .[[3]], digits = 2))
  df
})

# CREATE A TABLE THAT CONTAINS THE PLOTTED ASPATIAL DATA
output$plotTable <- DT::renderDataTable({
  if (input$scenario == FALSE){
    df <- plot_subset()
  } else {
    df <- plot_subset2()
  }
  DT::datatable(df, rownames = FALSE, extensions = "Buttons", 
                options = list(pageLength = 15, dom = "Bfrtip", 
                               buttons = c("copy", "csv", "excel", "print", "pdf")))
})

# CREATE A CHART THAT DISPLAYS THE ASPATIAL ATTRIBUTES
output$charts <- renderPlotly({
  if (input$scenario == FALSE){
    df <- plot_subset()
  } else {
    df <- plot_subset2() %>% 
      select(1, 6, 7)
  }
  realNames <- colnames(df)      
  colnames(df) <- c("myAttr", "myAbs", "myRel")    
  if(input$aspatialData == "ModeShare" & input$scenario == FALSE){
    plot_ly(df, labels = ~myAttr, values = ~myRel, type = 'pie', sort = FALSE, 
            marker = list(colors = brewer.pal(9, "Pastel1"))) %>% 
      layout(showlegend = T)
  } else{
    if (input$absRel == FALSE){
      df <- mutate(df, myColor = ifelse(myAbs < 0, "#fbb4ae", "#b3cde3"))
      plot_ly(df, y = ~myAbs, x = ~myAttr, type = "bar", mode = "markers", color = ~I(myColor)) %>% 
        layout(yaxis = list(title = realNames[2]),
               xaxis = list(title = realNames[1]))      
    } else {
      df <- mutate(df, myColor = ifelse(myRel < 0, "#fbb4ae", "#b3cde3"))
      plot_ly(df, y = ~myRel, x = ~myAttr, type = "bar", mode = "markers", color = ~I(myColor)) %>% 
        layout(yaxis = list(title = realNames[3]),
               xaxis = list(title = realNames[1]))
    }      
  }
})