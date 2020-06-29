#####################################################################################################
#
# Copyright 2019 CIRAD-INRA
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, see <http://www.gnu.org/licenses/> or
# write to the Free Software Foundation, Inc.,
# 51 Franklin Street, Fifth Floor, Boston,
# MA 02110-1301, USA.
#
# You should have received a copy of the CeCILL-C license with this program.
#If not see <http://www.cecill.info/licences/Licence_CeCILL-C_V1-en.txt>
#
# Intellectual property belongs to CIRAD and South Green developpement plateform
# Version 0.1.0 written by Sebastien RAVEL, François BONNOT, Sajid ALI, FOURNIER Elisabeth
#####################################################################################################

###############################################
## CODE of function for analysis
###############################################

############################################
## If folder already open keep the working folder
############################################
ui_volumes <- function() {

#  allVolumesAvail2 <- getOwnVolume()
#  sel_path <- dirname(parseDirPath(allVolumesAvail2, input$dirInSamples))
#  print(sel_path)
#  if (length(sel_path) > 0 && !sel_path %in% allVolumesAvail2) {
#    vnames <- c(basename(sel_path), names(allVolumesAvail2))
#    setNames(c(sel_path, allVolumesAvail2), vnames)
#  } else {
    allVolumesAvail
#  }
}

resetRun <- function() {
  # if reload after first run, reset value
  rv$exitStatusAna <- -1
}

############################################
## Input Directory path with images to analysis
############################################
shinyDirChoose(
  input,'dirInSamples',
  filetypes = c('', "png", "PNG","jpg","JPG","jpeg","JPEG", "TIF", "tif"),
  roots = allVolumesAvail,
  session = session,
  restrictions = system.file(package = 'base')
)

updateDirAnalysis <- observeEvent(input$dirInSamples,{
  if (!is.integer(input$dirInSamples))
  {
    resetRun()
    rv$dirSamples <- normalizePath(parseDirPath(allVolumesAvail, input$dirInSamples))
    # render to UI
    output$dirSamples <- renderText({
      rv$dirSamples
    })

  }
})

############################################
## Output Directory path of analysis
############################################
shinyDirChoose(
  input,'dirOut',
  filetypes = c('', "png", "PNG","jpg","JPG","jpeg","JPEG", "TIF", "tif"),
  roots = allVolumesAvail,
  session = session,
  restrictions = system.file(package = 'base')
)

updateDirOutAnalysis <- observeEvent(input$dirOut,{
  if (!is.integer(input$dirOut))
  {
    rv$dirSamplesOut <- normalizePath(parseDirPath(allVolumesAvail, input$dirOut))
    rv$logfilename <- paste0(rv$dirSamplesOut,"/log.txt")
    # render to UI
    output$dirOutAnalysis <- renderText({
      rv$dirSamplesOut
    })
  }
})

############################################
## Load Training folder
############################################
shinyDirChoose(
  input, 'dirTrainingIn',
  roots=allVolumesAvail,
  filetypes=c('', 'rdata' , 'RData',"png", "PNG","jpg","JPG","jpeg","JPEG", "TIF", "tif"),
  session = session,
  restrictions = system.file(package = 'base')
)

# Use observe out the event open dirTraining to auto load when Training step before
observe({
  if (!is.null(rv$dirTraining)) {
    output$dirTrainingIn <- renderText({
      rv$dirTraining
    })
  }
})

observeEvent(input$dirTrainingIn,{
  if (!is.integer(input$dirTrainingIn))
  {
    resetRun()
    rv$dirTraining <- normalizePath(parseDirPath(allVolumesAvail, input$dirTrainingIn))
  }
})


############################################
## Validate value for options
############################################

######## Image
###### Position TOP or BOTTUM active or not (checkbox)
observeEvent(input$outputPositionBottum,{
  if ( input$outputPositionBottum == FALSE){
    rv$position <- "right"
  }else{
    rv$position <- "bottum"
  }
})

###### Blur image
###### active or not (checkbox)
observeEvent(input$active_blur,{
  rv$active_blur <- input$active_blur
})

observeEvent(input$blur_value,{
  feedbackDanger(
      inputId = "blur_value",
      show = is.na(input$blur_value),
      text = "Please add number 'or 1 will be use'"
    )
  req(input$blur_value)
  if (is.na(input$blur_value) || as.numeric(input$blur_value) < 0){
    updateNumericInput(session,"blur_value", value = 1)
    rv$blur_value <- 1
  }
  else if (as.numeric(input$blur_value) > 21){
    updateNumericInput(session,"blur_value", value = 21)
    rv$blur_value <- 21
  }
  else{
    rv$blur_value <- returnOdd(as.numeric(input$blur_value))
    updateNumericInput(session,"blur_value", value = returnOdd(as.numeric(input$blur_value)))
  }
})

######## LEAF
###### watershed_leaf image
###### active or not (checkbox)
observeEvent(input$watershed_leaf,{
  rv$active_watershed_leaf <- input$watershed_leaf
})

observeEvent(input$watershed_leaf_ext,{
  feedbackDanger(
    inputId = "watershed_leaf_ext",
    show = is.na(input$watershed_leaf_ext),
    text = "Please add number 'or 1 will be use'"
  )
  req(input$watershed_leaf_ext)
  if (is.na(input$watershed_leaf_ext) || as.numeric(input$watershed_leaf_ext) <= 0){
    updateNumericInput(session,"watershed_leaf_ext", value = 1)
    rv$watershed_leaf_ext <- 1
  }
  else if (as.numeric(input$watershed_leaf_ext) > 1000){
    updateNumericInput(session,"watershed_leaf_ext", value = 1)
    rv$watershed_leaf_ext <- 1
  }
  else{
    rv$watershed_leaf_ext <- returnOdd(as.numeric(input$watershed_leaf_ext))
    updateNumericInput(session,"watershed_leaf_ext", value = returnOdd(as.numeric(input$watershed_leaf_ext)))
  }
})
###### leaf_min_size
observeEvent(input$leaf_min_size,{
  feedbackDanger(
      inputId = "leaf_min_size",
      show = is.na(input$leaf_min_size),
      text = "Please add number 'or 1000 will be use'"
    )
  req(input$leaf_min_size)
  if (is.na(input$leaf_min_size) || as.numeric(input$leaf_min_size) <= 0){
    updateNumericInput(session,"leaf_min_size", value = 1000)
    rv$leaf_min_size <- 1000
  }
  else{
    rv$leaf_min_size <- as.numeric(input$leaf_min_size)
  }
})

###### leaf_border_size
observeEvent(input$leaf_border_size,{
  feedbackDanger(
      inputId = "leaf_border_size",
      show = is.na(input$leaf_border_size),
      text = "Please add number 'or 5 will be use'"
    )
  req(input$leaf_border_size)
  if (is.na(input$leaf_border_size) || as.numeric(input$leaf_border_size) <= 0){
    updateNumericInput(session,"leaf_border_size", value = 5)
    rv$leaf_border_size <- 5
  }
  else{
    updateNumericInput(session,"leaf_border_size", value = returnOdd(as.numeric(input$leaf_border_size)))
    rv$leaf_border_size <- returnOdd(as.numeric(input$leaf_border_size))
  }
})

######## LESIONS
###### watershed_lesion image
###### active or not (checkbox)
observeEvent(input$watershed_lesion,{
  rv$active_watershed_lesion <- input$watershed_lesion
})

observeEvent(input$watershed_lesion_ext,{
  feedbackDanger(
    inputId = "watershed_lesion_ext",
    show = is.na(input$watershed_lesion_ext),
    text = "Please add number 'or 1 will be use'"
  )
  req(input$watershed_lesion_ext)
  if (is.na(input$watershed_lesion_ext) || as.numeric(input$watershed_lesion_ext) <= 0){
    updateNumericInput(session,"watershed_lesion_ext", value = 1)
    rv$watershed_lesion_ext <- 1
  }
  else if (as.numeric(input$watershed_lesion_ext) > 1000){
    updateNumericInput(session,"watershed_lesion_ext", value = 1)
    rv$watershed_lesion_ext <- 1
  }
  else{
    rv$watershed_lesion_ext <- returnOdd(as.numeric(input$watershed_lesion_ext))
    updateNumericInput(session,"watershed_lesion_ext", value = returnOdd(as.numeric(input$watershed_lesion_ext)))
  }
})
###### lesion_min_size
observeEvent(input$lesion_min_size,{
  feedbackDanger(
      inputId = "lesion_min_size",
      show = is.na(input$lesion_min_size),
      text = "Please add number 'or 10 will be use'"
    )
  req(input$lesion_min_size)
  if (is.na(input$lesion_min_size) || as.numeric(input$lesion_min_size) <= 0){
    updateNumericInput(session,"lesion_min_size", value = 10)
    rv$lesion_min_size <- 10
  }
  else{
    rv$lesion_min_size <- as.numeric(input$lesion_min_size)
  }
})

###### lesion_max_size
observeEvent(input$lesion_max_size,{
  feedbackDanger(
      inputId = "lesion_max_size",
      show = is.na(input$lesion_max_size),
      text = "Please add number 'or 120000 will be use'"
    )
  req(input$lesion_max_size)
  if (is.na(input$lesion_max_size) || as.numeric(input$lesion_max_size) <= 0 || input$lesion_min_size > input$lesion_max_size){
    updateNumericInput(session,"lesion_max_size", value = 120000)
    rv$lesion_max_size <- 120000
  }else{
    rv$lesion_max_size <- as.numeric(input$lesion_max_size)
  }
})

###### lesion_border_size
observeEvent(input$lesion_border_size,{
  feedbackDanger(
      inputId = "lesion_border_size",
      show = is.na(input$lesion_border_size),
      text = "Please add number 'or 3 will be use'"
    )
  req(input$lesion_border_size)
  if ( is.na(input$lesion_border_size) || as.numeric(input$lesion_border_size) <= 0){
    updateNumericInput(session,"lesion_border_size", value = 3)
    rv$lesion_border_size <- 3
  }else{
  updateNumericInput(session,"lesion_border_size", value = returnOdd(as.numeric(input$lesion_border_size)))
  rv$lesion_border_size <- returnOdd(as.numeric(input$lesion_border_size))
  }
})

######### parallel mode
observeEvent(c(input$parallelMode,input$parallelThreadsNum),{
  rv$parallelMode <- input$parallelMode
  max_no_cores <- as.numeric(max(1, parallel::detectCores() - 2))
  feedbackDanger(
      inputId = "parallelThreadsNum",
      show = is.na(input$parallelThreadsNum),
      text = paste("Please add number 'or ",max_no_cores," will be use'")
    )
  req(input$parallelThreadsNum)
  if (is.na(input$parallelThreadsNum)){
    updateNumericInput(session,"parallelThreadsNum", value = max_no_cores)
    rv$parallelThreadsNum <- max_no_cores
  }else if (as.numeric(input$parallelThreadsNum) > max_no_cores){
    updateNumericInput(session,"parallelThreadsNum", value = max_no_cores)
    rv$parallelThreadsNum <- max_no_cores
  }else if (as.numeric(input$parallelThreadsNum) < 1){
    updateNumericInput(session,"parallelThreadsNum", value = 1)
    rv$parallelThreadsNum <- 1
  }else{
    rv$parallelThreadsNum <- as.numeric(input$parallelThreadsNum)
  }
})


######### rm edge lesion
observeEvent(input$rmEdge,{
  rv$rmEdge <- input$rmEdge
})
######### rm Scan Line
observeEvent(input$rmScanLine,{
  rv$rmScanLine <- input$rmScanLine
})

######### Eccentricity
observeEvent(input$rmEccentric,{
  rv$rmEccentric <- input$rmEccentric
})

######### Eccentricity Range
observeEvent(c(input$rmEccentric,input$lesion_eccentric_slider),{
  rv$lesion_eccentricMin <- input$lesion_eccentric_slider[1]
  rv$lesion_eccentricMax <- input$lesion_eccentric_slider[2]
})

############################################
## run analysis
############################################
resultAnalysis <- observeEvent(input$runButtonAnalysis,{
  rv$runActif <- TRUE
  #source("/media/sebastien/Bayer/ScriptsSEB/LEAFTOOL/images/R/analysis_functions_v6.r")
  isolate({
    tryObserve({
      if (rv$runActif == TRUE){
        ## load values and add loading frame
        disable("runButtonAnalysis")
        rv$exitStatusAna <- 0
        rv$lesion_color_border <- input$lesion_color_border
        rv$lesion_color_bodies <- input$lesion_color_bodies
        rv$leaf_color_border <- input$leaf_color_border
        rv$leaf_color_bodies <- input$leaf_color_bodies

        displayableData <- DT::datatable(data = NULL)
        rv$dirInResult <- rv$dirSamplesOut

        rv$responseDataFilter <- NULL

        # reset to 1 if not select parallel mode
        if (rv$parallelMode == FALSE){
          rv$parallelThreadsNum <- 1
        }
        # if no blur change to 0
        if (rv$active_blur == FALSE){
          rv$blur_value <- 0
        }
        # if no active_watershed_leaf change to 0
        if (rv$active_watershed_leaf == FALSE){
          rv$watershed_leaf_ext <- 0
        }
        # if no active_watershed_lesion change to 0
        if (rv$active_watershed_lesion == FALSE){
          rv$watershed_lesion_ext <- 0
        }
        # create log file
        rv$logfilename <- file.path(rv$dirSamplesOut,"log.txt")

        output$logfileANA <- renderText(rv$logfilename)

        shinyjs::show("loading-content")
        rv$dfStatus <- analyseImages(pathTraining = rv$dirTraining,
                              pathResult = rv$dirSamplesOut,
                              pathImages = rv$dirSamples,
                              leafAreaMin = rv$leaf_min_size,
                              leafBorder = rv$leaf_border_size,
                              lesionBorder = rv$lesion_border_size,
                              lesionAreaMin = rv$lesion_min_size,
                              lesionAreaMax = rv$lesion_max_size,
                              lesionEccentricityMin = rv$lesion_eccentricMin,
                              lesionEccentricityMax = rv$lesion_eccentricMax,
                              lesionColorBorder = rv$lesion_color_border,
                              lesionColorBodies = rv$lesion_color_bodies,
                              leafColorBorder = rv$leaf_color_border,
                              leafColorBodies = rv$leaf_color_bodies,
                              blurDiameter = rv$blur_value,
                              watershedLeafExt = rv$watershed_leaf_ext,
                              watershedLesionExt = rv$watershed_lesion_ext,
                              outPosition = rv$position,
                              parallelThreadsNum = rv$parallelThreadsNum,
                              mode="GUI")
        rv$exitStatusAna <- 1

        enable("runButtonAnalysis")
        rv$dirInResult <- rv$dirSamplesOut
      #  ########################### END ANALYSIS
        shinyjs::hide(id = "loading-content", anim = TRUE, animType = "fade")
        shinyjs::reset("runButtonAnalysis")
        rv$runActif <- FALSE
      }
    }, test="analysis")
  })

}, ignoreNULL = FALSE, ignoreInit = TRUE)


#########################################
##  OUTPUT
#########################################

output$analysisFinish <- renderText({
  rv$exitStatusAna
})

img_uri1 <- function(x) {
  ext <- unlist(strsplit(x, "[.]"))[2]
  leafName <- gsub(paste0(".",ext), "", x)
  if ( ext == "tif"){
    img <- "Tiff not support by browser"
  }else{
  img <- sprintf("<img src='%s/%s' height='60'></img>", rv$randomNameOriginal, x)
  }
  return(data.frame(LeafName=leafName,Original=img, stringsAsFactors = FALSE))
}

img_uri2 <- function(x) {
  img <- sprintf("<img src='%s/%s' height='60'></img>", rv$randomNameLesionColor, x)
  leafName <- gsub("_lesion.jpeg", "", x)
  return(data.frame(LeafName=leafName,LesionColor=img, stringsAsFactors = FALSE))
}



output$contents <- DT::renderDataTable({

  if (rv$exitStatusAna == 1){

    # add temporary directory for browser refresh
    rv$randomNameOriginal <- paste0("Original",runif(1, min=0, max=999))
    rv$randomNameLesionColor <- paste0("LesionColor",runif(1, min=0, max=999))

    # list of original samples
    leafNameOriginal <- list.files(rv$dirSamples, full.names=FALSE)

    # dataframe of orignal samples with Leaf Name and image encode URL to visualise on browser (ex: LeafName   Original 163Madu210818_dpi49_6.tif Tiff not support by browser)
    dfLeafOriginal <- do.call(rbind,lapply(leafNameOriginal,img_uri1))

    # list of output file with lesion
    leafNameLesion <- list.files(rv$dirSamplesOut, full.names=FALSE, pattern = "*_lesion.jpeg")

    # dataframe of lesion samples with Leaf Name and image encode URL to visualise on browser (ex: LeafName   Original toto_1.tif <img src='LesionColor128.981319006532/toto_1_lesion.jpeg' height='60'></img>)
    dfLeafLesion <-   do.call(rbind,lapply(leafNameLesion ,img_uri2))

    mergedf <- merge(dfLeafOriginal, dfLeafLesion, all = TRUE)

    if (leafNameOriginal != '' && leafNameLesion != '' ){  #&& length(LeafName) == length(LeafName2)

      rv$responseDataFilter <- merge(mergedf, rv$dfStatus)
#      print(rv$responseDataFilter)

      addResourcePath(rv$randomNameOriginal,rv$dirSamples) # Images are located outside shiny App
      addResourcePath(rv$randomNameLesionColor,rv$dirSamplesOut) # Images are located outside shiny App

      displayableData <- DT::datatable(data = as.data.frame(rv$responseDataFilter),
                                      rownames=FALSE,
                                      escape=FALSE,
                                      selection="single",
                                      colnames=c("FileName","Original","LesionColor", "Status", "Message"),
                                     style = 'bootstrap', class = 'table-condensed ',
                                     filter = list(position = 'top', clear = FALSE, plain = TRUE),
                                     options = list(
                                        order = list(4, 'asc'),
                                        paging=TRUE,searching = TRUE,ordering=TRUE,scrollY = 750,scrollCollapse=TRUE,server = FALSE, autoWidth = TRUE
                                     )
      ) %>%
      formatStyle(
        'Status',
        target = 'row',
        backgroundColor = styleEqual("ERROR", 'red')
      )

    ifelse (!is.null(displayableData),return(displayableData),return(NULL))
    }
  }

})


currentImage <- reactive({

    imIndex <- input$contents_rows_selected
    if (is.null(imIndex))
      return("")

    #Load image for plot
    lesionImgPath <- strsplit(rv$responseDataFilter[imIndex,"LeafName"], ".", fixed = TRUE)[[1]][1]
    rv$responseDataFilter[imIndex,"LeafName"]


})

observeEvent(input$contents_rows_selected,{
  lesionImg <- strsplit(currentImage(), ".", fixed = TRUE)[[1]][1]
  imIndex <- input$contents_rows_selected

  showModal(      # Information Dialog Box
    modalDialog(
      title = paste(lesionImg, "info", sep = " "),
      size = "l",
      easyClose = TRUE,
      fade = FALSE,
      fluidRow(
        column(width = 1, offset = 0, style = "padding-top: 35vh;",
          actionButton("actionPrevious", "", icon = icon("backward"), width = "50px")
        ),
       column(width = 5, offset = 0, style = "height: 80vh;",
          HTML(gsub("height='60'","class='img-responsive'",rv$responseDataFilter[imIndex,"Original"]))
       ),
        column(width = 5, offset = 0, style = "height: 80vh;",
           HTML(gsub("height='60'","class='img-responsive'",rv$responseDataFilter[imIndex,"LesionColor"]))
        ),
        column(width = 1, offset = 0, style = "padding-top: 35vh;",
          actionButton("actionNext", "", icon = icon("forward"), width = "50px")
        )
      )
    )
  )
})

observeEvent(input$actionNext,{
  rowSelected <- input$contents_rows_selected
  nbImage <- nrow(rv$responseDataFilter)
  if(!is.null(rowSelected) && rowSelected < nbImage)
    newRow <- rowSelected + 1
  else newRow <- 1

  dataTableProxy("contents") %>%
    selectRows(newRow)
})

observeEvent(input$actionPrevious,{
  rowSelected <- input$contents_rows_selected
  nbImage <- nrow(rv$responseDataFilter)
  if (!is.null(rowSelected) && rowSelected == 1){
    newRow <- nbImage
  }
  else if(!is.null(rowSelected)){
    newRow <- rowSelected - 1
  }
  dataTableProxy("contents") %>%
  selectRows(newRow)
})

### DISABLE PARALLEL FOR WINDOWS
#observe({
#  osSystem <- Sys.info()["sysname"]
#  if (osSystem == "Windows") {
#     disable("parallelMode")
#     updateCheckboxInput(session, "parallelMode", label = "Active Parallel mode not avail for Windows", value = FALSE)
#  }
#})

outputOptions(output, 'analysisFinish', suspendWhenHidden = FALSE)
outputOptions(output, 'contents', suspendWhenHidden = FALSE)
