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
  ## Legend background properties
  l <- list(font = list(
      family = "sans-serif",
      size = 12,
      color = "#000"),
    bgcolor = "#E2E2E2",
    bordercolor = "#FFFFFF",
    borderwidth = 2)
  
  
  

  ## Filter labels
  filteredLabels = filter(myLabels,myDataType == visualizationType)
  containsFrame <-FALSE #Default value

  if(input$aspatialLevel == 'overview'){
    filteredLabels = filter(filteredLabels, myItem == input$aspatialLevel)
  }else if(input$aspatialLevel == 'households'){
    filteredLabels = filter(filteredLabels, myItem == input$HHLevel)
    if((input$HHLevel == 'hhSizInc') || (input$HHLevel == 'hhRentIncome')){
      containsFrame <- TRUE
    }
  }else if(input$aspatialLevel == 'persons'){
    filteredLabels = filter(filteredLabels, myItem == input$personsLevel)
    if(input$personsLevel == 'peAgeGend'){
      containsFrame <- TRUE
    }
  }else if(input$aspatialLevel == 'dwellings'){
    filteredLabels = filter(filteredLabels, myItem == input$dwellingsLevel)
  }else if(input$aspatialLevel == 'regional'){
    filteredLabels = filter(filteredLabels, myItem == input$regionalLevel)
  }else if(input$aspatialLevel == 'events'){
    filteredLabels = filter(filteredLabels, myItem == input$eventsLevel)
  }
  if((input$switchView == FALSE) && (input$pyramid == FALSE)){
    switch <-1
    filteredLabels = filter(filteredLabels, mySwitch == switch)
    

  }else{
    switch <-2
    filteredLabels = filter(filteredLabels, mySwitch == switch)
    
  }
  myLegend <-filteredLabels$myLegend
  myLegend <- paste( "<b>", myLegend,"</b>", sep=" ")
  if(input$comparison == TRUE){
    myYLabel <- paste("% of change in",filteredLabels$myY, sep = " ")
  }else{
    myYLabel <- filteredLabels$myY
  }
  

  
  fig <-fig %>%layout(
    title =filteredLabels$myTitle,
    xaxis =list(title = filteredLabels$myX),
    yaxis =list(title = myYLabel),
    legend=list(title=list(text=myLegend),
                x = 15,
                y = 0.5,
                bgcolor = "#E2E2E2",
                bordercolor = "#FFFFFF",
                borderwidth = 2)
    
  )
  ## Add frame labels
  if(containsFrame == TRUE){
    print(filteredLabels)
    print("Entered ")
    
    fig<-fig%>%animation_slider(currentvalue = list(prefix = paste(filteredLabels$myFrame,":",sep=" "))) 
                                  
  }
  return(fig)
}
generateRegionalClickLabels <-function(fig, title, axX, axY, legend,input){
  legend <-paste("<b>",legend,"</b>",sep = "")
  if(input$comparison == TRUE){
    axY <- paste("% of change in",axY, sep = " ")
  }
  fig <-fig%>%layout(
    title = title,
    xaxis = list(title = axX),
    yaxis = list(title = axY),
    legend = list(title = list(text= legend),
      y = 0.5,
      bgcolor = "#E2E2E2",
      bordercolor = "#FFFFFF",
      borderwidth = 2)
  )
  
  return(fig)
}