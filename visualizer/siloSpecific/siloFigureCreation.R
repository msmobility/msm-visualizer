# set the properties of the plot to display for the selected silo aspatial attribute
baseSiloPlot <- reactive({
  if(input$siloAspatialLevel == "Overview"){
    myPlot <- msmOverviewLines(siloPlotSubset, msmQualitative[c(6,4,3,11,8)])
  } else if(input$siloAspatialLevel == "Households"){
    if(input$siloHhLevel == "hhByType"){
      myPlot <- msmAnimatedLines(siloPlotSubset, msmSequential, input$siloSizeIncome)
    } else if(input$siloHhLevel == "Income"){
      myPlot <- msmAnimatedLines(siloPlotSubset, msmSequential, input$siloRentIncome)
    } else if(input$siloHhLevel %in% c("hhBySize", "carOwnershipLevel", "Average rent")){
      myPlot <- msmSimpleLines(siloPlotSubset, msmSequential)
    } else{
      myPlot <- msmDummyLines(siloPlotSubset)
    }
  } else if(input$siloAspatialLevel == "Persons"){
    if(input$siloPpLevel == "Age"){
      if(input$pyramid == FALSE){
        myPlot <- msmPyramidLines(siloPlotSubset, msmQualitative[c(3, 4)] )
      } else{
        if (input$siloScenario == FALSE){
          myPlot <- msmPyramid(siloPlotSubset, msmQualitative[c(3, 4)], c(-60000, -40000, -20000, 0, 20000, 40000, 60000),
                               c("60k", "40k", "20k", "0", "20k", "40k", "60k"))
        } else {
          myPlot <- msmPyramid(siloPlotSubset, msmQualitative[c(3, 4)], c(-60, -40, -20, 0, 20, 40, 60),
                               c("60", "40", "20", "0", "20", "40", "60"))
        }
      }
    } else if (input$siloPpLevel == "laborParticipationRateByAge"){
      myPlot <- msmAnimatedBars(siloPlotSubset, msmQualitative[c(3, 4)] )
    } else{
      myPlot <- msmDummyLines(siloPlotSubset)
    }
  } else if(input$siloAspatialLevel == "Dwellings"){
    if(input$siloDdLevel == "Quality"){
      myPlot <- msmSimpleLines(siloPlotSubset, msmSequential)
    } else{
      myPlot <- msmDummyLines(siloPlotSubset)
    }
  } else if(input$siloAspatialLevel == "Regional"){
    if(input$siloRrLevel == "jobByRegion"){
      myPlot <- msmAnimatedDummyLines(siloPlotSubset, input$siloSectReg)
    } else{
      myPlot <- msmDummyLines(siloPlotSubset)
    }
  } else if(input$siloAspatialLevel == "Events"){
    myPlot <- msmDummyLines(siloPlotSubset)
  }
  myPlot
})


# add appropriate labels to the displayed silo plots
output$siloPlot <- renderPlotly({
  d <- input$siloScenario
  if (input$siloAspatialLevel == "Overview"){
    labelSiloAspatial(baseSiloPlot, input$siloAspatialLevel, d, 1)
  } else if (input$siloAspatialLevel == "Households"){
    if ((input$siloHhLevel == "hhByType" & input$siloSizeIncome == TRUE) |
        (input$siloHhLevel == "Income" & input$siloRentIncome == TRUE)){
      labelSiloAspatial(baseSiloPlot, input$siloHhLevel, d, 2)
    } else {
      labelSiloAspatial(baseSiloPlot, input$siloHhLevel, d, 1)
    }
  } else if (input$siloAspatialLevel == "Persons"){
    if (input$siloPpLevel == "Age" & input$pyramid == TRUE){
      labelSiloAspatial(baseSiloPlot, input$siloPpLevel, d, 2)
    } else {
      labelSiloAspatial(baseSiloPlot, input$siloPpLevel, d, 1)
    }
  } else if (input$siloAspatialLevel == "Dwellings"){
    labelSiloAspatial(baseSiloPlot, input$siloDdLevel, d, 1)
  } else if (input$siloAspatialLevel == "Regional"){
    if(input$siloRrLevel == "jobByRegion" & input$siloSectReg == TRUE){
      labelSiloAspatial(baseSiloPlot, input$siloRrLevel, d, 2)
    } else {
      labelSiloAspatial(baseSiloPlot, input$siloRrLevel, d, 1)
    }
  } else if (input$siloAspatialLevel == "Events"){
    labelSiloAspatial(baseSiloPlot, input$siloEeLevel, d, 1)
  }
})


# set the properties of the map to display for the selected silo spatial attribute
output$siloMap <- renderLeaflet({
  style <- input$siloMapStyle
  categories <- input$siloMapCategories
  d <- input$siloScenario
  if(d == FALSE & input$siloGrowth == FALSE){
    attribute <- names(siloMapSubset())[2]
    farbe <- "YlOrBr"
  } else {
    attribute <- names(siloMapSubset())[4]
    farbe <- "-RdBu"
  }
  if (input$siloSpatialLevel == "dwellings"){
    msmMap(siloMapSubset, attribute, farbe, labelSiloSpatial(input$siloDdType, d)[1], 
           labelSiloSpatial(input$siloDdType, d)[2], style, categories)
  } else if (input$siloSpatialLevel == "accessibilities"){
    msmMap(siloMapSubset, attribute, farbe, labelSiloSpatial(input$siloAccType, d)[1], 
           labelSiloSpatial(input$siloAccType, d)[2], style, categories)
  } else if (input$siloSpatialLevel == "incomes"){
    msmMap(siloMapSubset, attribute, farbe, labelSiloSpatial(input$siloIncomes, d)[1], 
           labelSiloSpatial(input$siloIncomes, d)[2], style, categories)
  } else {
    msmMap(siloMapSubset, attribute, farbe, labelSiloSpatial(input$siloSpatialLevel, d)[1], 
           labelSiloSpatial(input$siloSpatialLevel, d)[2], style, categories)
  }
})



### functions for creating figure labels ###

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


# function to add labels to silo maps
labelSiloSpatial <- function(attribute, change){
  df <- filter(myLabels, myDataType == "siloSpatial" & myItem == attribute)
  if (change == FALSE){
    c(df$myTitle, df$myLegend)
  } else {
    c(df$myTitle, paste0("% Change in ", df$myLegend))
  }
}