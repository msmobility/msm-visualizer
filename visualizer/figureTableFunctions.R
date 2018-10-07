### functions to create different kinds of plots and maps for visualization ###

# function to create table output
msmDataTable <- function(data){
  DT::datatable(data, rownames = FALSE, extensions = "Buttons",
                options = list(pageLength = 15, dom = "Bfrtip",
                               buttons = c("copy", "csv", "excel", "print", "pdf")))
}


# function to create map
msmMap <- function(data, myAttribute, myColors, myTitle, myLegend, myStyle, categories){
  tmap_leaflet(tm_shape(data()) +
                 tm_fill(myAttribute,
                         palette = myColors, title = myLegend,
                         style = myStyle, n = categories) + tm_layout(myTitle)) %>%
    setView(11.51416, 48.348645, zoom = 8.5)
}


# function to create bar charts where negative and positive values are colored blue and red respectively
msmBar <- function(data, valueType){ 
  if(valueType == "absolute"){
    df <- mutate(data(), myColor = ifelse(Value < 0, "#b3cde3", "#fbb4ae"))
    plot_ly(df, x = ~Key, y = ~Value, type = "bar", mode = "markers", color = ~I(myColor))
  } else if(valueType == "relative"){
    df <- mutate(data(), myColor = ifelse(Share < 0, "#b3cde3", "#fbb4ae"))
    plot_ly(df, x = ~Key, y = ~Share, type = "bar", mode = "markers", color = ~I(myColor)) 
  }
}


# function to create pie chart
msmPieChart <- function(data, myColors){
  plot_ly(data(), labels = ~Key, values = ~Value, type = 'pie', sort = FALSE,
          marker = list(colors = myColors))
}


# function to create line charts (general)
msmSimpleLines <- function(data, myColors){
  plot_ly(data(), x = ~Year, y = ~Value, type = "scatter", mode = "lines", color = ~as.factor(Key), colors = myColors)
}


# function to create line charts (specific to the silo summary plot)
msmOverviewLines <- function(data, myColors){
  plot_ly(data(), x = ~Year, y = ~Value, type = "scatter", mode = "lines", color = ~as.factor(Key), colors = msmQualitative[c(6,4,3,11,8)])
}


# function to create animated line charts (general)
msmAnimatedLines <- function(data, myColors, switchView){
  if (switchView == FALSE){
    plot_ly(data(), x = ~Year, y = ~Value, frame = ~Key2, type = "scatter", mode = "lines", color = ~as.factor(Key),
            colors = myColors, line = list(simplify = F))
  } else {
    plot_ly(data(), x = ~Year, y = ~Value, frame = ~Key, type = "scatter", mode = "lines", color = ~as.factor(Key2),
            colors = myColors, line = list(simplify = F))
  }
}


# function to create population pyramid 
msmPyramid <- function(data, myColors, tickvalues, ticklabels){
  df_male <- filter(data(), Key == "Male") %>%
    mutate(Value = Value * -1)
  df_female <- filter(data(), Key == "Female")
  
  df <- bind_rows(df_male, df_female) %>%
    mutate(AbsVal = abs(Value))
  plot_ly(df, x = ~Value, y = ~as.numeric(Key2), frame = ~Year, type = "bar", mode = "markers", orientation = "h",
          color = ~as.factor(Key), hoverinfo = "y+text", text = ~AbsVal, colors = myColors) %>%
    layout(bargap = 0.1, barmode = "overlay", xaxis = list(tickmode = "array",
                                                           tickvals = tickvalues,
                                                           ticktext = ticklabels))
}


# function to create animated bar charts
msmAnimatedBars <- function(data, myColors){
  plot_ly(data(), x = ~Key2, y = ~Value, frame = ~Year, type = "bar", mode = "markers", color = ~as.factor(Key), 
          colors = myColors)
}


# function to create animated line charts (specific to population pyramid values)
msmPyramidLines <- function(data, myColors){
  plot_ly(data(), x = ~as.numeric(Key2), y = ~Value, frame = ~Year, type = "scatter", mode = "lines", color = ~as.factor(Key),
          colors = myColors, line = list(simplify = F))
}


# function to create line charts for a variable with unknown distinct values
msmDummyLines <- function(data){
  myDummy <- n_distinct(data()$Key)
  myColor <- colorRampPalette(msmQualitative)(myDummy)
  plot_ly(data(), x = ~Year, y = ~Value, type = "scatter", mode = "lines", color = ~as.factor(Key), colors = myColor)
}


# function to create animated line charts for a variable with unknown distinct values
msmAnimatedDummyLines <- function(data, switchView){
  if (switchView == FALSE){
    myDummy <- n_distinct(data()$Key)
    myColor <- colorRampPalette(msmQualitative)(myDummy)
    plot_ly(data(), x = ~Year, y = ~Value, frame = ~Key2, type = "scatter", mode = "lines", color = ~as.factor(Key),
            colors = myColor, line = list(simplify = F))
  } else {
    myDummy <- n_distinct(data()$Key2)
    myColor <- colorRampPalette(msmQualitative)(myDummy)
    plot_ly(data(), x = ~Year, y = ~Value, frame = ~Key, type = "scatter", mode = "lines", color = ~as.factor(Key2),
            colors = myColor, line = list(simplify = F))
  }
}
