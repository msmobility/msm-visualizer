# CREATE A CAPTION FOR THE DISPLAYED DATA
output$caption <- renderText({
  if(input$dataType == "Aspatial"){
    paste(myNaming(myAspatial, myAspatialCodes, input$aspatialData), " for ",
          myNaming(myPurposeNames, myPurposeCodes, input$purpose), " Trips")
  } else if(input$dataType == "Spatial"){
    paste(myNaming(mySpatial, mySpatialCodes, input$spatialData), " for ",
          myNaming(myPurposeNames, myPurposeCodes, input$purpose), " Trips")
  }
})


## PANEL TO DISPLAY OUTPUTS
mitoOutputs <- fillCol(flex = c(NA, 1),
        h4(textOutput("caption")),
        conditionalPanel("input.dataType == 'Aspatial'",
                         tabsetPanel(type = "tabs",
                                     tabPanel("Figure",
                                              plotlyOutput("charts", height = "700px")),
                                     tabPanel("Data",
                                              br(),
                                              DT::dataTableOutput("plotTable")))),
        conditionalPanel("input.dataType == 'Spatial'",
                         tabsetPanel(type = "tabs",
                                     tabPanel("Figure",
                                              leafletOutput("map", height = "700px")),
                                     tabPanel("Data",
                                              br(),
                                              DT::dataTableOutput("mapTable"))))
)