  # uploaded file meta data
  output$filedf <- renderTable({
    if(is.null(input$file)){return ()}
    rv$zipFile <- fixUploadedFilesNames(input$file) # the file input data frame object that contains the file attributes
  })

  output$home <- renderPrint({
    rv$zipFile$datapath
  })

  # Unzipping files on click of button and then rendering the result to dataframe
  # file gets unzipped to the location where zip file was located in the local system
  observeEvent(input$unzip,
               output$unzipped <- renderTable({
                 unzip(rv$zipFile$datapath, exdir = paste0(Sys.getenv("HOME"),"/leaftool/")) # path to extracted files can be defined using exdir argument
               })
  )




############################################
## Input Directory path with images to resize
############################################
shinyDirChoose(
  input,'dirInResize',
  filetypes = c('', "png", "PNG","jpg","JPG","jpeg","JPEG", "TIF", "tif"),
  roots = allVolumesAvail,
  session = session,
  restrictions = system.file(package = 'base')
)

updateDirAnalysis <- observeEvent(input$dirInResize,{
  if (!is.integer(input$dirInResize))
  {
    rv$dirResize <- normalizePath(parseDirPath(ui_volumes, input$dirInResize))
    # render to UI
    output$dirResize <- renderText({
      rv$dirResize
    })

    rv$resizePathOut <- resizeImageDirectory(rv$dirResize, rv$factor, mode="GUI")

  }
})

output$resizePathOut <- renderText({
  rv$resizePathOut
})

###### factor
observeEvent(input$factor,{
  feedbackDanger(
      inputId = "factor",
      condition = is.na(input$factor),
      text = "Please add number 'or 2 will be use'"
    )
  req(input$factor)
  if (is.na(input$factor) || as.numeric(input$factor) <= 0){
    updateNumericInput(session,"factor", value = 2)
    rv$factor <- 2
  }
  else{
    rv$factor <- as.numeric(input$factor)
  }
})

############################################
## Input Directory path with images to resize
############################################
shinyDirChoose(
  input,'dirInSplit',
  filetypes = c('', "png", "PNG","jpg","JPG","jpeg","JPEG", "TIF", "tif"),
  roots = allVolumesAvail,
  session = session,
  restrictions = system.file(package = 'base')
)

updateDirAnalysis <- observeEvent(input$dirInSplit,{
  if (!is.integer(input$dirInSplit))
  {
    rv$dirSplit <- normalizePath(parseDirPath(ui_volumes, input$dirInSplit))
    # render to UI
    output$dirSplit <- renderText({
      rv$dirSplit
    })

    rv$splitPathOut <- splitImages(rv$dirSplit, rv$splitVertical, rv$splitHorizontal, rv$outputNumber, mode="GUI")

  }
})

output$splitPathOut <- renderText({
  rv$splitPathOut
})

###### splitVertical
observeEvent(input$splitVertical,{
  feedbackDanger(
      inputId = "splitVertical",
      condition = is.na(input$splitVertical),
      text = "Please add number 'or 1 will be use'"
    )
  req(input$splitVertical)
  if (is.na(input$splitVertical) || as.numeric(input$splitVertical) <= 0){
    updateNumericInput(session,"splitVertical", value = 1)
    rv$splitVertical <- 1
  }
  else{
    rv$splitVertical <- as.numeric(input$splitVertical)
  }
})
###### splitHorizontal
observeEvent(input$splitHorizontal,{
  feedbackDanger(
      inputId = "splitHorizontal",
      condition = is.na(input$splitHorizontal),
      text = "Please add number 'or 1 will be use'"
    )
  req(input$splitHorizontal)
  if (is.na(input$splitHorizontal) || as.numeric(input$splitHorizontal) <= 0){
    updateNumericInput(session,"splitHorizontal", value = 1)
    rv$splitHorizontal <- 1
  }
  else{
    rv$splitHorizontal <- as.numeric(input$splitHorizontal)
  }
})
###### outputNumber BOTTUM or RIGHT active or not (checkbox)
observeEvent(input$outputNumber,{
  if ( input$outputNumber == FALSE){
    rv$outputNumber <- "bottum"
  }else{
    rv$outputNumber <- "right"
  }
})
