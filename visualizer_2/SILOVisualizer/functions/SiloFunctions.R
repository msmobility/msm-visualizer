# function to add labels to silo aspatial plots
labelSiloAspatial <- function(plot, attribute, change, switch){
  df <- filter(myLabels, myDataType == "siloAspatial" & myItem == attribute & mySwitch == switch)
  valueLabel <- df$myY
  keyLabel <- df$myX
  if (change == TRUE){
    if (attribute == "Age" & switch == 2){
      keyLabel <- paste0("% Change in ", keyLabel)
    } else {
      valueLabel <- paste0("% Change in ", valueLabel)
    }
  }
  myPlot <- plot()
  if(attribute %in% c("hhByType", "Income", "Age", "jobByRegion")){
    myPlot <- myPlot %>%
      animation_slider(currentvalue =list(prefix = paste0(df$myFrame, ": ")))
  }
  layout(myPlot, title = df$myTitle, yaxis = list(title = valueLabel), xaxis = list(title = keyLabel),
         annotations = list(yref = 'paper', xref = 'paper', y = 0.9, x = 1.02, text = sprintf("<b>%s</b>", df$myLegend),
                            xanchor = "left", yanchor = "bottom", legendtitle = TRUE, showarrow = F)) %>%
    layout(legend = list(y = 0.9, yanchor = "top"))
}
generateLabels <- function(fig, visualizationType, input) {
  print("We are in the function")
  print(menuSettings)
  print("Hehehe")
  print(input$aspatialLevel)
  ## Filter labels
  filteredLabels = filter(myLabels,myDataType == visualizationType)
  if(input$aspatialLevel == 'overview'){
    filteredLabels = filter(filteredLabels, myItem == input$aspatialLevel)
  }else if(input$aspatialLevel == 'households'){
    filteredLabels = filter(filteredLabels, myItem == input$HHLevel)
  }else if(input$aspatialLevel == 'persons'){
    print("PErsons levels, ")
    print(input$personsLevel)
    filteredLabels = filter(filteredLabels, myItem == input$personsLevel)
  }else if(input$aspatialLevel == 'dwellings'){
    filteredLabels = filter(filteredLabels, myItem == input$dwellingsLevel)
  }else if(input$aspatialLevel == 'regional'){
    filteredLabels = filter(filteredLabels, myItem == input$regionalLevel)
  }else if(input$aspatialLevel == 'events'){
    filteredLabels = filter(filteredLabels, myItem == input$eventsLevel)
  }
  if(input$switchView == FALSE){
    switch <-1
    filteredLabels = filter(filteredLabels, mySwitch == switch)
  }else{
    switch <-2
    filteredLabels = filter(filteredLabels, mySwitch == switch)
  }
  
  fig <-fig %>%layout(
    title =filteredLabels$myTitle,
    xaxis =list(title = filteredLabels$myX),
    yaxis =list(title = filteredLabels$myY),
    annotations = list(yref = 'paper',xref = 'paper', y = 0.95, x = 1.05, text = filteredLabels$myLegend)
    #legend =list(title = "filteredLabels$myLegend")#,
    #sliders =list(title = filteredLabels$myFrame)
  )
  ## Second filter level
  #if(inp)
  
  
  #print(input)
  #print(input$aspatialLevel)

  print("Filtered options")
  print(filteredLabels)
  ##session$userData$menuSettings <- filter(session$userData$menuSettings, required_file_1 != fileList[j] | is.na(required_file_1))
  
  return(fig)
}