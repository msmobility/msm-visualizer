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