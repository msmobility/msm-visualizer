### file reader module ###

# UI for uploading result files
fileReaderUI <- function(id, label = "Upload result files"){
  ns <- NS(id)
  tagList(
    fileInput(inputId = ns("resultFiles"),
              label = label,
              multiple = TRUE,
              accept = ".csv")
  )
}

# Server logic for reading result files
fileReader <- function(input, output, session, model){
  reactive({
    req(input$resultFiles)
    myFiles <- input$resultFiles %>% 
      arrange(size)
    if (model == "silo"){
      if(nrow(myFiles) > 1){
        myData <- list(aspatial = read_table2(myFiles$datapath[1], col_names = FALSE),
                       spatial = read_table2(myFiles$datapath[2], col_names = FALSE))
      } else {
        myData <- list(data = read_table2(myFiles$datapath[1], col_names = FALSE))
      }
    } else if (model == "mito"){
      if(nrow(myFiles) > 1){
        myData <- list(aspatial = read_csv(myFiles$datapath[1]),
                       spatial = read_csv(myFiles$datapath[2]))
      } else {
        myData <- list(data = read_csv(myFiles$datapath[1]))
      }
    }
    myData
  })
}