# set the properties of the plot to display for the selected mito aspatial attribute
baseMitoPlot <- reactive({
  if(input$mitoAspatialLevel == "ModeShare" & input$mitoScenario == FALSE){
    myPlot <- msmPieChart(mitoPlotSubset, msmPastel)
  } else {
    if (input$mitoAbsRel == TRUE){
      myPlot <- msmBar(mitoPlotSubset, "relative")
    } else {
      myPlot <- msmBar(mitoPlotSubset, "absolute")
    }
  }
  myPlot
})


# add appropriate labels to the displayed mito plots
output$mitoPlot <- renderPlotly({
  labelMitoAspatial(baseMitoPlot, input$mitoAspatialLevel, input$mitoScenario, input$mitoAbsRel, input$mitoPurpose)
})


# set the properties of the map to display for the selected mito spatial attribute
output$mitoMap <- renderLeaflet({
  style <- input$mitoMapStyle
  categories <- input$mitoMapCategories
  d <- input$mitoScenario
  label <- labelMitoSpatial(input$mitoSpatialLevel, d, input$mitoPurpose)
  if(d == FALSE){
    attribute <- names(mitoMapSubset())[2]
    farbe <- "YlOrBr"
  } else {
    attribute <- names(mitoMapSubset())[4]
    farbe <- "-RdBu"
  }
  msmMap(mitoMapSubset, attribute, farbe, label[1], label[2], style, categories)
})



### functions for creating figure labels ###

# function to add labels to mito aspatial plots
labelMitoAspatial <- function(plot, attribute, change, relative, purpose){
  df <- filter(myLabels, myDataType == "mitoAspatial" & myItem == attribute)
  valueLabel <- df$myY
  keyLabel <- df$myX
  titleLabel <- paste0(df$myTitle, " for ", purpose, " trips")
  if (change == FALSE & relative == TRUE){
    valueLabel <- paste0(valueLabel, " (%)")
  } else if (change == TRUE){
    valueLabel <- paste0("% Change in ", valueLabel)
  }
  myPlot <- plot()
  if (attribute == "ModeShare" & change == FALSE){
    layout(myPlot, title = titleLabel, showlegend = T)
  } else {
    layout(myPlot, title = titleLabel, yaxis = list(title = valueLabel), xaxis = list(title = keyLabel))
  }
}


# function to add labels to mito maps
labelMitoSpatial <- function(attribute, change, purpose){
  df <- filter(myLabels, myDataType == "mitoSpatial" & myItem == attribute)
  if (change == FALSE){
    c(paste0(df$myTitle, " for ", purpose, " trips"), df$myLegend)
  } else {
    c(paste0(df$myTitle, " for ", purpose, " trips"), paste0("% Change in ", df$myTitle))
  }
}