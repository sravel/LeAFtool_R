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
rangeNA <- function(x, object) {
  w <- which(x == object)
  if (length(w) == 0)
    return(c(NA, NA))
  return(range(w))
}

boundingRectangle <- function(mask, object) {
  m <- imageData(mask)
  range.x <- range(apply(m, 1, rangeNA, object), na.rm = TRUE)
  range.y <- range(apply(m, 2, rangeNA, object), na.rm = TRUE)
  list(x = range.x[1]:range.x[2], y = range.y[1]:range.y[2])
}

extractLeaf <- function(i, mask, image.fond.noir) {
  b <- boundingRectangle(mask, i)
  leaf <- image.fond.noir[b$y, b$x,]
  mask.leaf <- mask[b$y, b$x]
  leaf[mask.leaf != i] <- 0
  list(b = b, leaf = leaf)
}

## analysis lesion for one leaf
analyseLeaf <- function(x, lda1, lesion, csvLesionFile, filename) {
  f <- x$leaf
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
  w.small <- which(featuresLesion[,"s.area"] < rv$lesion_min_size)
  ## search for great objects
  w.great <- which(featuresLesion[,"s.area"] > rv$lesion_max_size)

  rv$rmEdge <- TRUE
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
  featuresLesion <- computeFeatures.shape(mask)
  list(featuresLesion = featuresLesion, mask = mask)

  featuresLesion.surfmin <- as.data.frame(featuresLesion[-w,]) # delete objects in the table
  featuresLesion.surfmin$object <- as.numeric(row.names(featuresLesion.surfmin))
  moments <- as.data.frame(computeFeatures.moment(mask))
  moments$object <- as.numeric(row.names(moments))
  featuresLesion.surfmin <- merge(featuresLesion.surfmin, moments)

  w1 <- as.numeric(featuresLesion.surfmin$object[featuresLesion.surfmin[,"s.area"]<=100000000]) ## values of objects smaller than surfmax

  keepLesion <- ifelse(featuresLesion.surfmin$object %in% w1, "keep", "remove")

  outputDF <- data.frame(image=filename, keepLesion=keepLesion, featuresLesion.surfmin, stringsAsFactors=FALSE)

  write.table(
    outputDF,
    file = csvLesionFile,
    quote = FALSE,
    row.names = FALSE,
    sep = '\t'
  )


  list(features = featuresLesion, maskLesion = maskLesion)
}

# analysis One scan image
analyseUniqueFile <- function(pathResult, pathImages, imageFile) {

  filename <- strsplit(imageFile, ".", fixed = TRUE)[[1]][1]
  jpegname <- paste(filename, "_both.jpeg", sep = '')
  jpegnameOnly <- paste(filename, "_lesion.jpeg", sep = '')
  txtname1 <- paste(filename, "_1.txt", sep = '')
  txtname2 <- paste(filename, "_2.txt", sep = '')
  csvLesionName <- paste(filename, "_All_lesions.csv", sep = '')
  if (!file.exists(pathResult))
    dir.create(pathResult)
  jpegfile <- paste(pathResult, '/', jpegname, sep = '')
  jpegfileOnly <- paste(pathResult, '/', jpegnameOnly, sep = '')
  txtfile1 <- paste(pathResult, '/', txtname1, sep = '')
  txtfile2 <- paste(pathResult, '/', txtname2, sep = '')
  csvLesionFile <- paste(pathResult, '/', csvLesionName, sep = '')



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
  mask <- channel(image, "gray")
  leaf <- matrix(df5$leaf, nrow = nrow(imageData(mask)))
  imageData(mask) <- leaf

  ## empty fill
  mask <- fillHull(mask)

  ## erosion border removal
  ## note: tests and calculations are performed on eroded objects
  brush <- makeBrush(rv$leaf_border_size,  shape = 'disc')
  ## next line added to remove parasitic lines due to scan (delete for normal scan)
  if (rv$rmScanLine == TRUE){
    brush2 <- makeBrush(rv$leaf_border_size*2+1,  shape = 'disc') ; mask <- dilate(mask, brush2) ; mask <- erode(mask,  brush2)
  }
  mask <- erode(mask,  brush)

  ## segmentation
  mask <- bwlabel(mask)
  features <- data.frame(computeFeatures.shape(mask))

  ## removing objects smaller than the minimum area of a leaf
  w <- which(features[, "s.area"] < rv$leaf_min_size)
  if (length(w) > 0) {
    mask[mask %in% w] <- 0
    features <- features[-w, ]
  }

  ## removal of the background
  imageBackgroundBlack <- image
  imageBackgroundBlack[mask == 0] <- 0

  ## separation of leafs
  li <- lapply(as.numeric(row.names(features)), extractLeaf, mask, imageBackgroundBlack)

  ## analyse des leafs
  analyse.li <<- lapply(li,  analyseLeaf,  lda1 = lda1,  lesion = lesion, csvLesionFile = csvLesionFile, filename = filename)

  # print both sample and lesion images
  jpeg(jpegfile,
       width = widthSize,
       height = heightSize*2,
       units = "px")
  par( mfrow = c(2,1) )
  display(image, method="raster")

  ## sortie des résultats et coloration des lésions
  result <- NULL
  for (i in 1:length(li)) {
    result <-
      rbind(
        result,
        data.frame(
          fichier = filename,
          leaf = i,
          surfaceLeaf = features[i, "s.area"],
          surfaceLesion = if (is.null(analyse.li[[i]]$features))
            0
          else
            analyse.li[[i]]$features[, "s.area"]
        )
      )
    ## la ligne suivante a été remplacée par 3 lignes suite à une erreur apparue sur certaines versions de R
    ## image[li[[i]]$b$y,li[[i]]$b$x,][analyse.li[[i]]$mask>0] <- colorLesion
    maskLesion <- analyse.li[[1]][["maskLesion"]]
    imageLesionColor <- image[li[[i]]$b$y, li[[i]]$b$x,]
    imageLesionColor <- paintObjects(maskLesion  ,imageLesionColor, thick=TRUE, col=c(rv$lesion_color_boundaries, rv$lesion_color_bodies), opac=c(1, 1))
  }

  row.names(result) <- NULL
  display(imageLesionColor, method = "raster")
  dev.off()

  # print only output file image with lesion
  jpeg(jpegfileOnly,
       width = widthSize,
       height = heightSize,
       units = "px")
  par( mfrow = c(1,1) )
  display(imageLesionColor, method = "raster")
  dev.off()

  write.table(
    result,
    file = txtfile1,
    quote = FALSE,
    row.names = FALSE,
    sep = '\t'
  )

  ag.count <-
    aggregate(result$surfaceLesion, result[c("fichier", "leaf", "surfaceLeaf")], length)
  names(ag.count)[4] <- "nbLesions"
  ag.surface <-
    aggregate(result$surfaceLesion, result[c("fichier", "leaf", "surfaceLeaf")], sum)
  names(ag.surface)[4] <- "surfaceLesions"
  ag <- merge(ag.count, ag.surface)
  ag$pourcent.lesions <- ag$surfaceLesions / ag$surfaceLeaf * 100
  ag$nbLesions[ag$surfaceLesions == 0] <- 0

  write.table(
    ag[order(ag$leaf),],
    file = txtfile2,
    quote = FALSE,
    row.names = FALSE,
    sep = '\t'
  )
  print(paste0("FINISH: ",sourceImage,"\n",jpegfileOnly))
  rm(list = ls())
  gc()
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
  rv$dirSamplesOut <- file.path(home, paste(unlist(input$dirOut$path[-1]), collapse = .Platform$file.sep))
  rv$exitStatusAna <- -1
  rv$messAna <- NULL
  rv$errAna <- NULL
})

############################################
## Load RData file
############################################
shinyFileChoose(input, 'files',
                roots=allVolumesAvail,
                filetypes=c('', 'rdata' , 'RData'))

output$fileRData <- renderText({
  rv$fileRData
})

observeEvent(input$files,{
    rv$fileRData <-  normalizePath(as.character(parseFilePaths(roots=allVolumesAvail, input$files)$datapath))
})

observe({
  if (is.null(rv$fileRData)) {
    # User has not uploaded correct file yet
    return(NULL)
  }else{
    load(file = rv$fileRData, envir = .GlobalEnv)
#    rv$exitStatusAna <- -1
    rv$messAna <- NULL
    rv$errAna <- NULL
  }
})

############################################
## Validate value for options
############################################

validate_INT <- function(inputValue,name,reactiveVal) {
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
    rv$name <- as.numeric(inputValue)
  }
  feedbackDanger(
      inputId = name,
      condition = is.na(inputValue),
      text = "Danger please enter number"
    )
}

######## LEAF
###### leaf_min_size
observeEvent(input$leaf_min_size,{
  validate_INT(input$leaf_min_size, "leaf_min_size",rv$leaf_min_size)
})

###### leaf_border_size
observeEvent(input$leaf_border_size,{
  validate_INT(input$leaf_border_size, "leaf_border_size",rv$leaf_border_size)
})

######## LESIONS
###### lesion_min_size
observeEvent(input$lesion_min_size,{
  validate_INT(input$lesion_min_size, "lesion_min_size",rv$lesion_min_size)
})

###### lesion_max_size
observeEvent(input$lesion_max_size,{
  validate_INT(input$lesion_max_size, "lesion_max_size",rv$lesion_max_size)
})

###### lesion_border_size
observeEvent(input$lesion_border_size,{
  validate_INT(input$lesion_border_size, "lesion_border_size",rv$lesion_border_size)
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
  rv$lesion_color_boundaries <- input$lesion_color_boundaries
  rv$lesion_color_bodies <- input$lesion_color_bodies


  ############################ RUN ANALYSIS
  # count number of files on input directory
  listFiles <-list.files(rv$dirSamples)
  print(listFiles)
  nbfiles <- length(listFiles)
  c <- 1
  show("loading-content")

  progress <- shiny::Progress$new()
  on.exit(progress$close())
  progress$set(message = 'Making Analysis, please wait\n', value = 0)
  progress$inc(c/nbfiles, detail = paste("start parallel analysis with ", rv$no_cores, " cores"))

#  updateProgress <- function(c) progress$inc(c/nbfiles, detail = paste("Image", c))


#  # Start parallel session
#  plan(multiprocess, .cleanup = TRUE)
#  cl <- makeCluster(no_cores, outfile = logfilename)
#  registerDoSNOW(cl)
#  clusterExport(cl, c("nbfiles")) # Export max number of iteration to workers

#  opts <- list(progress=updateProgress)

#  foreach(imageFile = listFiles,
#          .options.snow=opts,
#          .combine = c)  %dopar%
#          {
#            tryCatch({
#              # incProgress(c / nbfiles, detail = paste("analysis leaf ", c, "/", nbfiles))
#              analyseUniqueFile(rv$dirSamplesOut,  rv$dirSamples, imageFile)
#              gc()
#            }, error = function(e){return(paste0("The file '", imageFile, "'"," caused the error: '", e, "'"))},
#            warning = function(e){return(paste0("The file '", imageFile, "'"," caused warning: '", e, "'"))}
#            # finally = {
#            # print(image)
#            # addLog(image)
#            # closeAllConnections();
#            # stopCluster(cl)
#            # registerDoSEQ()
#            # }
#            )

#          }
#  # Close cluster mode
#  stopCluster(cl)
#  closeAllConnections();
#  registerDoSEQ()

  # if not cluster mode do sample by sample on one core
   for (imageFile in listFiles){
      progress$inc(c / nbfiles, detail = paste("analysis leaf ", c, "/", nbfiles))
     analyseUniqueFile(rv$dirSamplesOut,rv$dirSamples,imageFile)
     c <- c + 1
#     break
   }

  tmpCmd  <- paste0("awk '{if($1 == \"fichier\"){} else{ print $0}}'  ", rv$dirSamplesOut, "/*_2.txt | sort -k1 |sort -k1,1 -k2n,2n > ",rv$dirSamplesOut,"/merge_output_2.txt")
  returnvalue  <- system(tmpCmd, intern = TRUE)
  rv$exitStatusAna <- 1


#  ########################### END ANALYSIS
  hide(id = "loading-content", anim = TRUE, animType = "fade")
#  if (code == 0){
#    errorMessAnalysis <-tags$div("Error !!!!:")
#    exitStatusAnalysis <<-list(codeAnalysis=0, err=errorMess)
#  }
#  if (code == 1){
#    errorMessAnalysis <-tags$div("Error !!!!:")
#    exitStatusAnalysis <<-list(codeAnalysis=1, mess="good")
#  }
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

  LeafNames <- list.files(rv$dirSamples, full.names=FALSE)
  LeafNames2 <- list.files(rv$dirSamplesOut, full.names=FALSE, pattern = "*_lesion.jpeg")
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
  return(rv$displayableData)

})


output$plotcurrentImage <- renderPlot({
  if ( is.null(rv$plotcurrentImage)) return(NULL)
  nx <- dim(rv$plotcurrentImage)[1]
  ny <- dim(rv$plotcurrentImage)[2]
  plot(rv$plotcurrentImage)
  color <- if (rv$loadCSVcurrentImage$keepLesion == "keep") "green" else "red"
  points(rv$loadCSVcurrentImage$m.cx, rv$loadCSVcurrentImage$m.cy, pch='+', cex=2, col=color)
})


currentImage <- reactive({

    imIndex <- input$contents_rows_selected
    if (is.null(imIndex))
      return("")

    #Load image for plot
    lesionImg <- strsplit(rv$responseDataFilter2[imIndex,"LeafNames"], ".", fixed = TRUE)[[1]][1]
    rv$plotcurrentImage <- readImage(paste0(rv$dirSamplesOut,lesionImg,"_lesion.jpeg"))
    rv$loadCSVcurrentImage <- read.csv(paste0(rv$dirSamplesOut,lesionImg,"_All_lesions.csv"))



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
      actionButton("actionPrevious", "", icon = icon("backward"), width = "50px"),
        img(src= paste0("Original/",currentImage()),width='44%',height='44%'),
        imageOutput("plotcurrentImage",click = "plot_click",dblclick = "plot_dbclick", brush = "plot_brush"),
#        img(src= paste0("LesionColor/",lesionImg,"_lesion.jpeg"),width='44%',height='44%'),
      actionButton("actionNext", "", icon = icon("forward"), width = "50px")
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

outputOptions(output, 'codeValidationInt', suspendWhenHidden = FALSE)
outputOptions(output, 'analysisFinish', suspendWhenHidden = FALSE)
outputOptions(output, 'warning', suspendWhenHidden = FALSE)
