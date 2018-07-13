#####################################################################################################
#
# Copyright 2018 CIRAD-INRA
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
## returns the extreme indices of the object value in the vector x
## (called by boundingRectangle)
rangeNA <<- function(x, object) {
  w <- which(x == object)
  if (length(w) == 0)
    return(c(NA, NA))
  return(range(w))
}

boundingRectangle <<- function(mask, object) {
  m <- imageData(mask)
  range.x <- range(apply(m, 1, rangeNA, object), na.rm = TRUE)
  range.y <- range(apply(m, 2, rangeNA, object), na.rm = TRUE)
  list(x = range.x[1]:range.x[2], y = range.y[1]:range.y[2])
}

extractLeaf <<- function(i, mask, imageBackgroundBlack, surfaceLeaves) {
  b <- boundingRectangle(mask, i)
  leaf <- imageBackgroundBlack[b$y, b$x,]
  mask.leaf <- mask[b$y, b$x]
  leaf[mask.leaf != i] <- 0
  xCoord <- list(min = min(b$x),max = max(b$x))
  yCoord <- list(min = min(b$y),max = max(b$y))
  XYcoord <- list(x = xCoord, y = yCoord)
  list(b = b, XYcoord = XYcoord, leaf = leaf, leaf.surface = surfaceLeaves[i,], leafNum = i)
}

## analysis lesion for one leaf
analyseLeaf <<- function(x, lda1, lesion, filename) {

  if (rv$blur_value == 0){
    f <- x$leaf
  }else{
    f <- gblur(x$leaf,rv$blur_value)
  }

  df6 <-
    data.frame(
      red = as.numeric(imageData(f)[, , 1]),
      green = as.numeric(imageData(f)[, , 2]),
      blue = as.numeric(imageData(f)[, , 3])
    )
  df6$predict <- predict(lda1, df6)$class
  df6$tache <- as.numeric(df6$predict == lesion)
  df6$tache[df6$red + df6$green + df6$blue == 0] <- 0
  mask <- channel(f, "gray")
  tache <- matrix(df6$tache, nrow = nrow(imageData(mask)))
  imageData(mask) <- tache

  ## dilation
  brush <- makeBrush(rv$lesion_border_size, shape = 'disc')
  mask <- dilate(mask, brush)

  ## empty fill
  mask <- fillHull(mask)

  ## erosion
  mask <- erode(mask, brush)

  ## segmentation
  mask[mask < 0] <- 0
  mask <- bwlabel(mask)
  featuresLesion <- computeFeatures.shape(mask)

  ## search for small objects
  w.small <- which(featuresLesion[,"s.area"] <= rv$lesion_min_size)
  ## search for great objects
  w.great <- which(featuresLesion[,"s.area"] >= rv$lesion_max_size)

  if (rv$rmEdge == TRUE){
    ## search objets on the edge of the image
    top <- unique(imageData(mask)[1,])
    left <- unique(imageData(mask)[,1])
    bottom <- unique(imageData(mask)[dim(mask)[1],])
    right <- unique(imageData(mask)[,dim(mask)[2]])
    w.edge <- unique(c(top, left, bottom, right))

    w <- unique(c(w.small, w.edge, w.great)) ## values of objects to delete
  }else{
    w <- unique(c(w.small, w.great)) ## values of objects to delete
  }
  # apply to maskLesion
  maskLesion <- rmObjects(mask, w)
  ## renumber objects
  featuresLesion <- computeFeatures.shape(maskLesion)

  featuresLesionClean <- as.data.frame(featuresLesion)

  featuresLesionClean$leaf.surface <- rep(as.numeric(x$leaf.surface),nrow(featuresLesionClean))

  featuresLesionClean$lesion.number <- as.numeric(row.names(featuresLesionClean))

  colnames(featuresLesionClean)[which(colnames(featuresLesionClean) %in%
      c("s.area", "s.perimeter", "s.radius.mean", "s.radius.sd", "s.radius.min", "s.radius.max") )] <-
      c("lesion.surface", "lesion.perimeter", "lesion.radius.mean", "lesion.radius.sd", "lesion.radius.min", "lesion.radius.max")

  moments <- as.data.frame(computeFeatures.moment(maskLesion))
#  print(moments)
#  print(x$XYcoord$x$min)
#  print(x$XYcoord$x$max)
#  print(x$XYcoord$y$min)
#  print(x$XYcoord$y$max)
  moments$m.cx <- moments$m.cx + x$XYcoord$x$min - 1
  moments$m.cy <- moments$m.cy + x$XYcoord$y$min - 1


  moments$lesion.number <- as.numeric(row.names(moments))
  featuresLesionCleanPos <- merge(featuresLesionClean, moments)

  if (nrow(featuresLesionCleanPos) == 0){
    featuresLesionCleanPos <- data.frame("lesion.number" = NA, "lesion.surface"= NA, "lesion.perimeter"= NA, "lesion.radius.mean"= NA, "lesion.radius.sd"= NA, "lesion.radius.min"= NA, "lesion.radius.max"= NA, "leaf.surface"= NA, "m.cx"= NA, "m.cy"= NA, "m.majoraxis"= NA, "m.eccentricity"= NA, "m.theta"= NA)
  }

  outputDF <- data.frame(image=filename,leaf.number = x$leafNum, lesion.status="keep", featuresLesionCleanPos, stringsAsFactors=FALSE)

  list(featuresLesion = featuresLesion, maskLesion = maskLesion, outputDF = outputDF)
}

# analysis One scan image
analyseUniqueFile <<- function(pathResult, pathImages, imageFile) {

  print("RUNNING")
  if (!file.exists(pathResult))
    dir.create(pathResult)

  filename <- strsplit(imageFile, ".", fixed = TRUE)[[1]][1]
  jpegfile <- paste(pathResult, '/', filename, "_both.jpeg", sep = '')
  jpegfileOnly <- paste(pathResult, '/', filename, "_lesion.jpeg", sep = '')

  csv_Merge_lesionsFile <- paste(pathResult, '/', filename, "_Merge_lesions.csv", sep = '')
  csv_All_lesionsFile <- paste(pathResult, '/', filename, "_All_lesions.csv", sep = '')


  background <- names(lda1$prior)[1]
  limb <- names(lda1$prior)[2]
  lesion <- names(lda1$prior)[3]

  ## reading the source image
  sourceImage <- paste(pathImages, '/', imageFile, sep = '')
  image <- readImage(sourceImage)
  widthSize = dim(image)[1]
  heightSize = dim(image)[2]

  ## prediction on the image (not blurred)
  df5 <-
    data.frame(
      red = as.numeric(imageData(image)[, , 1]),
      green = as.numeric(imageData(image)[, , 2]),
      blue = as.numeric(imageData(image)[, , 3])
    )
  df5$predict <- predict(lda1, df5)$class

  ## creating lesions and leafs identifiers
  df5$lesion <- as.numeric(df5$predict == lesion)
  df5$leaf <- as.numeric(df5$predict != background)

  ## mask of leafs
  maskLeaf <- channel(image, "gray")
  leaf <- matrix(df5$leaf, nrow = nrow(imageData(maskLeaf)))
  imageData(maskLeaf) <- leaf

  ## empty fill
  maskLeaf <- fillHull(maskLeaf)

  ## erosion border removal
  ## note: tests and calculations are performed on eroded objects
  brush <- makeBrush(rv$leaf_border_size,  shape = 'disc')
  ## next line added to remove parasitic lines due to scan (delete for normal scan)
#  if (rv$rmScanLine == TRUE){
#    brush2 <- makeBrush(rv$leaf_border_size*2+1,  shape = 'disc') ; maskLeaf <- dilate(maskLeaf, brush2) ; maskLeaf <- erode(maskLeaf,  brush2)
#  }
  maskLeaf <- erode(maskLeaf,  brush)

  ## segmentation
  maskLeaf <- bwlabel(maskLeaf)
  featuresLeaf <- data.frame(computeFeatures.shape(maskLeaf))

  ## removing objects smaller than the minimum area of a leaf
  w <- which(featuresLeaf[, "s.area"] < rv$leaf_min_size)
  if (length(w) > 0) {
    maskLeaf[maskLeaf %in% w] <- 0
    featuresLeaf <- featuresLeaf[-w, ]
  }
  # size of leafs
  surfaceLeaves <- featuresLeaf["s.area"]


  ## removal of the background
  imageBackgroundBlack <- image
  imageBackgroundBlack[maskLeaf == 0] <- 0

  ## separation of leafs
  li <- lapply(as.numeric(row.names(featuresLeaf)), extractLeaf, maskLeaf, imageBackgroundBlack, surfaceLeaves)

  ## analyse des leafs
  analyse.li <- lapply(li,  analyseLeaf,  lda1 = lda1,  lesion = lesion, filename = filename)

  # print both sample and lesion images
  jpeg(jpegfile,
       width = widthSize,
       height = heightSize*2,
       units = "px")
  par( mfrow = c(2,1) )
  display(image, method="raster")

  ## sortie des résultats et coloration des lésions

  for (i in 1:length(li)){
    if (i == 1){
      result <- analyse.li[[i]]$outputDF
    }else{
      result <- rbind( result, analyse.li[[i]]$outputDF )
    }

    maskLesion <- analyse.li[[i]][["maskLesion"]]
    tmpimage <- image[li[[i]]$b$y, li[[i]]$b$x,]
    tmpimage <- paintObjects(maskLesion  ,tmpimage, thick=TRUE, col=c(rv$lesion_color_border, rv$lesion_color_bodies), opac=c(1, 1))
    image[li[[i]]$b$y, li[[i]]$b$x,] <- tmpimage

  }

  display(image, method = "raster")
  dev.off()

  # print only output file image with lesion
  jpeg(jpegfileOnly,
       width = widthSize,
       height = heightSize,
       units = "px")
  par( mfrow = c(1,1) )
  display(image, method = "raster")
  dev.off()

  write.table(
    result,
    file = csv_All_lesionsFile,
    quote = FALSE,
    row.names = FALSE,
    sep = '\t'
  )

  ag.count <- aggregate(result$lesion.surface, result[c("image", "leaf.number", "leaf.surface")], length)
  names(ag.count)[4] <- "lesion.nb"
#  print(ag.count)

  ag.surface <- aggregate(result$lesion.surface, result[c("image", "leaf.number", "leaf.surface")], sum)
  names(ag.surface)[4] <- "lesion.surface"
#  print(ag.surface)

 ag <- merge(ag.count, ag.surface)
  ag$pourcent.lesions <- ag$lesion.surface / ag$leaf.surface * 100
  ag$nbLesions[ag$lesion.surface == 0] <- 0
#  print(ag)

  write.table(
    ag[order(ag$leaf.number),],
    file = csv_Merge_lesionsFile,
    quote = FALSE,
    row.names = FALSE,
    sep = '\t'
  )

  return(paste0("FINISH: ",sourceImage,"\n",jpegfileOnly))
}

############################################
## Input Directory path with images to analysis
############################################
shinyDirChoose(
  input,'dirInSamples',
  filetypes = c('', "png", "PNG","jpg","JPG","jpeg","JPEG"),
  roots = allVolumesAvail,
  session = session,
  restrictions = system.file(package = 'base')
)

output$dirSamples <- renderText({
  rv$dirSamples
})

updateDirAnalysis <- observeEvent(input$dirInSamples,{
  home <- normalizePath(allVolumesAvail[input$dirInSamples$root])
  rv$dirSamples <- file.path(home, paste(unlist(input$dirInSamples$path[-1]), collapse = .Platform$file.sep))
  rv$exitStatusAna -1
  rv$messAna <- NULL
  rv$errAna <- NULL
})

############################################
## Output Directory path of analysis
############################################
shinyDirChoose(
  input,'dirOut',
  filetypes = c('', "png", "PNG","jpg","JPG","jpeg","JPEG"),
  roots = allVolumesAvail,
  session = session,
  restrictions = system.file(package = 'base')
)

output$dirOutAnalysis <- renderText({
  rv$dirSamplesOut
})

updateDirOutAnalysis <- observeEvent(input$dirOut,{
  home <- normalizePath(allVolumesAvail[input$dirOut$root])
  rv$dirSamplesOut <- file.path(home, paste(unlist(input$dirOut$path[-1],"/"), collapse = .Platform$file.sep))
  rv$exitStatusAna <- -1
  rv$messAna <- NULL
  rv$errAna <- NULL
})

############################################
## Load RData file
############################################
shinyFileChoose(input, 'fileRDataIn',
                roots=allVolumesAvail,
                filetypes=c('', 'rdata' , 'RData'))

output$fileRData <- renderText({
  rv$fileRData
})

observeEvent(input$fileRDataIn,{
    rv$fileRData <-  normalizePath(as.character(parseFilePaths(roots=allVolumesAvail, input$fileRDataIn)$datapath))
})

observe({
  if (is.null(rv$fileRData)) {
    # User has not uploaded correct file yet
    return(NULL)
  }else{
    load(file = rv$fileRData, envir = .GlobalEnv)
    rv$exitStatusAna <- -1
    rv$messAna <- NULL
    rv$errAna <- NULL
  }
})

############################################
## Validate value for options
############################################

validate_INT <- function(inputValue,name) {
  if(!is.numeric(inputValue) || (inputValue <= 0) || is.null(inputValue)){
    rv$codeValidationInt <- 0
    rv$warning <- HTML(paste0("Please input a number >= 0 for <b>",name,"</b> !"))
    feedbackWarning(
      inputId = name,
      condition = inputValue < 0,
      text = "Warning please enter 0 < value"
    )
  }else{
    rv$codeValidationInt <- 1
  }
  feedbackDanger(
      inputId = name,
      condition = is.na(inputValue),
      text = "Danger please enter number"
    )
}
######## Image
###### Blur image
observeEvent(input$blur_value,{
  rv$blur_value <- as.numeric(input$blur_value)
})


######## LEAF
###### leaf_min_size
observeEvent(input$leaf_min_size,{
  validate_INT(input$leaf_min_size, "leaf_min_size")
  rv$leaf_min_size <- as.numeric(input$leaf_min_size)
})

###### leaf_border_size
observeEvent(input$leaf_border_size,{
  validate_INT(input$leaf_border_size, "leaf_border_size")
  rv$leaf_border_size <- as.numeric(input$leaf_border_size)
})

######## LESIONS
###### lesion_min_size
observeEvent(input$lesion_min_size,{
  validate_INT(input$lesion_min_size, "lesion_min_size")
  rv$lesion_min_size <- as.numeric(input$lesion_min_size)
})

###### lesion_max_size
observeEvent(input$lesion_max_size,{
  validate_INT(input$lesion_max_size, "lesion_max_size")
  rv$lesion_max_size <- as.numeric(input$lesion_max_size)
})

###### lesion_border_size
observeEvent(input$lesion_border_size,{
  validate_INT(input$lesion_border_size, "lesion_border_size")
  rv$lesion_border_size <- as.numeric(input$lesion_border_size)
})


output$codeValidationInt <- renderText({
  rv$codeValidationInt
})
output$warning <- renderUI({
  rv$warning
})

#output$value <- renderPrint({
#  values = values()
#  values()
#})


############################################
## run analysis
############################################

resultAnalysis <- observeEvent(input$runButtonAnalysis,{
  ## load values and add loading frame
#  show("loading-content")
  rv$exitStatusAna <- 0
  rv$lesion_color_border <- input$lesion_color_border
  rv$lesion_color_bodies <- input$lesion_color_bodies


  ############################ RUN ANALYSIS
  # count number of files on input directory
  listFiles <-list.files(rv$dirSamples)

  nbfiles <<- length(listFiles)
  c <- 1
  show("loading-content")

  progress <<- shiny::Progress$new()
  on.exit(progress$close())
  progress$set(message = 'Making Analysis, please wait\n', value = 0)


  if (rv$parallelMode == TRUE){

  # Start parallel session
  progress$inc(c/nbfiles, detail = paste("start parallel analysis with ", rv$parallelThreadsNum, " cores"))

  cl <- makeCluster(rv$parallelThreadsNum, outfile = logfilename, type = "FORK")
  registerDoParallel(cl)

  # Add an entry to the log file
  cat(as.character(Sys.time()), '\n', file = logfilename,
    append = TRUE)

  res <- foreach(imageFile = listFiles,

          .export = c(".GlobalEnv"),
          .combine = c)  %dopar%
          {
              progress$set(message = 'toto\n', value = c)
              progress$inc(c/nbfiles, detail = paste("analysis leaf ", c, "/", nbfiles))
              print(imageFile)
              analyseUniqueFile(rv$dirSamplesOut,  rv$dirSamples, imageFile)
              c <- c + 1
          }
  print(res)
#  # Close cluster mode
  stopCluster(cl)
  closeAllConnections();
  registerDoSEQ()
  }else{
    c <- 1
    # if not cluster mode do sample by sample on one core
    progress$inc(c/nbfiles, detail = paste("start one sample analysis with 1 cores"))
    for (imageFile in listFiles){
      progress$inc(c / nbfiles, detail = paste("analysis leaf ", c, "/", nbfiles))
      c <- c + 1
      analyseUniqueFile(rv$dirSamplesOut,rv$dirSamples,imageFile)

    }
  }

  tmpCmd  <- paste0("awk '{if($1 == \"image\"){} else{ print $0}}'  ", rv$dirSamplesOut, "/*_Merge_lesions.csv | sort -k1 |sort -k1,1 -k2n,2n > ",rv$dirSamplesOut,"/merge_ResumeCount.csv")
  returnvalue  <- system(tmpCmd, intern = TRUE)
  rv$exitStatusAna <- 1


#  ########################### END ANALYSIS
  hide(id = "loading-content", anim = TRUE, animType = "fade")
})

#########################################
##  OUTPUT
#########################################

output$analysisFinish <- renderText({
  rv$exitStatusAna
})

output$contents <- DT::renderDataTable({
#  # resultAnalysis()
#   rv$dirSamples <- "~/Bayer/AnalyseImagesV4/Exemple1/Images/"
#   rv$dirSamplesOut <- "~/Bayer/AnalyseImagesV4/Exemple1/"
  if (rv$exitStatusAna == 1){

    LeafNames <- list.files(rv$dirSamples, full.names=FALSE)
    LeafNames2 <- list.files(rv$dirSamplesOut, full.names=FALSE, pattern = "*_lesion.jpeg")

    if (LeafNames != '' && LeafNames2 != '' && length(LeafNames) == length(LeafNames2)){
      addResourcePath("Original",rv$dirSamples) # Images are located outside shiny App
      addResourcePath("LesionColor",rv$dirSamplesOut) # Images are located outside shiny App
      LeafTable <- data.frame(LeafNames,LeafNames2,stringsAsFactors = FALSE)
      LeafTable <- within(LeafTable, thumbnail <- paste0("<img src='","Original/",LeafTable$LeafNames,"' height='60'></img>"))
      LeafTable <- within(LeafTable, thumbnail2 <- paste0("<img src='","LesionColor/",LeafTable$LeafNames2 ,"' height='60'></img>"))

      rv$responseDataFilter2 <- LeafTable[,c(1,3,4)]

      rv$displayableData<-DT::datatable(data = as.data.frame(rv$responseDataFilter2, stringAsFactors = FALSE, row.names = NULL),

                                     escape=FALSE,selection="single",rownames=FALSE,colnames=c("FileName","Original","LesionColor"),
                                     style = "bootstrap",
                                     options = list(
                                       paging=TRUE,searching = TRUE,ordering=TRUE,scrollY = 750,scrollCollapse=TRUE,server = FALSE
                                     ))
      ifelse(!is.null(rv$displayableData),return(rv$displayableData),return(NULL))
    }
  }

})


output$plotcurrentImage <- renderPlot({
  if ( is.null(rv$plotcurrentImage)) return(NULL)
  plot(rv$plotcurrentImage)
#  color <- ifelse(rv$loadCSVcurrentImage$keepLesion == "keep", "green", "red")
#  points(rv$loadCSVcurrentImage$m.cx, rv$loadCSVcurrentImage$m.cy, pch='+', cex=2, col=color)
})


currentImage <- reactive({

    imIndex <- input$contents_rows_selected
    if (is.null(imIndex))
      return("")

    #Load image for plot
    lesionImg <- strsplit(rv$responseDataFilter2[imIndex,"LeafNames"], ".", fixed = TRUE)[[1]][1]
    rv$plotcurrentImage <- readImage(paste0(rv$dirSamplesOut,"/",lesionImg,"_lesion.jpeg"))
    rv$loadCSVcurrentImage <- read.csv(paste0(rv$dirSamplesOut, "/",lesionImg,"_All_lesions.csv"),header = TRUE, sep = "\t", stringsAsFactors = FALSE)
    rv$responseDataFilter2[imIndex,"LeafNames"]


})

observeEvent(input$contents_rows_selected,{
  lesionImg <- strsplit(currentImage(), ".", fixed = TRUE)[[1]][1]

  showModal(      # Information Dialog Box
    modalDialog(
      title = paste(lesionImg, "info", sep = " "),
      size = "l",
      easyClose = TRUE,
      fade = FALSE,
      fluidRow(
        column(width = 1, offset = 0,
          actionButton("actionPrevious", "", icon = icon("backward"), width = "50px")
        ),
       column(width = 5, offset = 0,
         img(src= paste0("Original/",currentImage()),width='100%',height='100%')
       ),
        column(width = 5, offset = 0,
          img(src= paste0("LesionColor/", lesionImg, "_lesion.jpeg"),width='100%',height='100%')
#          plotOutput("plotcurrentImage") #,click = "plot_click",dblclick = "plot_dbclick", brush = "plot_brush"
        ),
        column(width = 1, offset = 0,
          actionButton("actionNext", "", icon = icon("forward"), width = "50px")
        )
      )
    )
  )
})

observeEvent(input$actionNext,{
  rowSelected <- input$contents_rows_selected
  nbImage <- nrow(rv$responseDataFilter2)
  if(!is.null(rowSelected) && rowSelected < nbImage)
    newRow <- rowSelected + 1
  else newRow <- 1

  dataTableProxy("contents") %>%
    selectRows(newRow)
})

observeEvent(input$actionPrevious,{
  rowSelected <- input$contents_rows_selected
  nbImage <- nrow(rv$responseDataFilter2)
  if (!is.null(rowSelected) && rowSelected == 1){
    newRow <- nbImage
  }
  else if(!is.null(rowSelected)){
    newRow <- rowSelected - 1
  }
  dataTableProxy("contents") %>%
  selectRows(newRow)
})

######### parallel mode
observeEvent(c(input$parallelMode,input$parallelThreadsNum),{
  rv$parallelMode <- input$parallelMode
  max_no_cores <- as.numeric(max(1, detectCores() - 2))
  if (as.numeric(input$parallelThreadsNum) > max_no_cores){
    updateNumericInput(session,"parallelThreadsNum", value = max_no_cores, max = max_no_cores)
    rv$parallelThreadsNum <- max_no_cores
  }else if (as.numeric(input$parallelThreadsNum) < 1){
    updateNumericInput(session,"parallelThreadsNum", value = 1, max = max_no_cores)
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


outputOptions(output, 'codeValidationInt', suspendWhenHidden = FALSE)
outputOptions(output, 'analysisFinish', suspendWhenHidden = FALSE)
outputOptions(output, 'warning', suspendWhenHidden = FALSE)
outputOptions(output, 'contents', suspendWhenHidden = FALSE)
