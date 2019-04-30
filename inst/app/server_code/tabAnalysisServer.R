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

extractLeaf <<- function(i, mask, imageBackgroundBlack, featuresLeaf) {
  ##### FOR BANANA LEAF #########################################
#  kern = makeBrush(size = 201)
#  opening(imageBackgroundBlack, kern)
  ## DEBUG:  display(imageBackgroundBlack, method = "raster", all = TRUE)
#  imageBackgroundBlack <- fillHull(imageBackgroundBlack)
 ################################################################

  # size of leaf
  surfaceLeaf <- featuresLeaf[row.names(featuresLeaf) %in% c(i),"s.area"]
  leafNum <- featuresLeaf[row.names(featuresLeaf) %in% c(i),"leaf.number"]
  b <- boundingRectangle(mask, i)
  leaf <- imageBackgroundBlack[b$y, b$x,]
  mask.leaf <- mask[b$y, b$x]
  leaf[mask.leaf != i] <- 0
  xCoord <- list(min = min(b$x),max = max(b$x))
  yCoord <- list(min = min(b$y),max = max(b$y))
  XYcoord <- list(x = xCoord, y = yCoord)

  list(b = b, XYcoord = XYcoord, leaf = leaf, leaf.surface = surfaceLeaf, leafNum = leafNum)
}

## analysis lesion for one leaf
analyseLeaf <<- function(x, lda1, lesion, limb, filename) {

#  print(paste("leafNum : ", x$leafNum, "    surface leaf : ",x$leaf.surface))
  f <- x$leaf

  ## DEBUG: display(f, method = "raster", all = TRUE)

  # replacement of the background by limb
  df6 <-
    data.frame(
      red = as.numeric(imageData(f)[, , 1]),
      green = as.numeric(imageData(f)[, , 2]),
      blue = as.numeric(imageData(f)[, , 3])
    )
  df6$predict <- predict(lda1, df6)$class
  df.limb <- df6[df6$predict %in% limb,]
  black.background <- df6$red+df6$green+df6$blue==0
  mean.limb <- apply(df.limb[1:3],2,mean)
  df6[black.background,1] <- mean.limb[1]
  df6[black.background,2] <- mean.limb[2]
  df6[black.background,3] <- mean.limb[3]
  imageData(f)[,,1] <- df6$red
  imageData(f)[,,2] <- df6$green
  imageData(f)[,,3] <- df6$blue
  # end replace

  # blur image or not
  if (rv$active_blur == TRUE){
    flo <- makeBrush(rv$blur_value, shape='disc', step=FALSE)^2
    flo <- flo/sum(flo)
    f <- filter2(f, flo)
  }

  # analysis
  df6 <- data.frame(red=as.numeric(imageData(f)[,,1]), green=as.numeric(imageData(f)[,,2]), blue=as.numeric(imageData(f)[,,3]))
  df6$predict <- predict(lda1, df6)$class
  df6$tache <- as.numeric(df6$predict %in% lesion)
  mask <- channel(f, "gray")
  tache <- matrix(df6$tache, nrow=nrow(imageData(mask)))
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
  momentsLesion <- computeFeatures.moment(mask)

  ## Remove objects
  w.small <- w.great <- w.eccentricMin <- w.eccentricMax <- w.edge <- NULL

  ## search for small objects
  w.small <- which(featuresLesion[,"s.area"] < rv$lesion_min_size)
  ## search for great objects
  w.great <- which(featuresLesion[,"s.area"] > rv$lesion_max_size)

  if (rv$rmEdge == TRUE){
    ## search objets on the edge of the image
    top <- unique(imageData(mask)[1,])
    left <- unique(imageData(mask)[,1])
    bottom <- unique(imageData(mask)[dim(mask)[1],])
    right <- unique(imageData(mask)[,dim(mask)[2]])
    w.edge <- unique(c(top, left, bottom, right))
  }

  # remove eccentric lesions
  if (rv$rmEccentric == TRUE){
    w.eccentricMin <- which(momentsLesion[,"m.eccentricity"] < rv$rmEccentricMin)
    w.eccentricMax <- which(momentsLesion[,"m.eccentricity"] > rv$rmEccentricMax)
  }

  # apply to maskLesion
  mask[mask %in% c(w.small, w.great, w.edge, w.eccentricMin, w.eccentricMax)] <- 0
  maskLesion <- mask
   ## DEBUG:display(maskLesion, method = "raster", all = TRUE)

  ## renumber objects
  featuresLesion <- computeFeatures.shape(maskLesion)

  featuresLesionClean <- as.data.frame(featuresLesion)
  featuresLesionClean$leaf.surface <- rep(as.numeric(x$leaf.surface),nrow(featuresLesionClean))
  featuresLesionClean$lesion.number <- as.numeric(row.names(featuresLesionClean))

  colnames(featuresLesionClean)[which(colnames(featuresLesionClean) %in%
      c("s.area", "s.perimeter", "s.radius.mean", "s.radius.sd", "s.radius.min", "s.radius.max") )] <-
      c("lesion.surface", "lesion.perimeter", "lesion.radius.mean", "lesion.radius.sd", "lesion.radius.min", "lesion.radius.max")

  # correct coord if multi leaves to edit lesion
  moments <- as.data.frame(computeFeatures.moment(maskLesion))
  moments$m.cy <- moments$m.cy + x$XYcoord$x$min - 1
  moments$m.cx <- moments$m.cx + x$XYcoord$y$min - 1

  moments$lesion.number <- as.numeric(row.names(moments))
  featuresLesionCleanPos <- merge(featuresLesionClean, moments)
#  featuresLesionCleanPos$lesion.number <- seq(1,nrow(featuresLesionCleanPos))
#  row.names(maskLesion) <- seq(1,nrow(maskLesion))

  if (nrow(featuresLesionCleanPos) == 0){
    featuresLesionCleanPos <- data.frame("lesion.number" = 0, "lesion.surface"= 0, "lesion.perimeter"= 0, "lesion.radius.mean"= 0, "lesion.radius.sd"= 0, "lesion.radius.min"= 0, "lesion.radius.max"= 0, "leaf.surface"= 0, "m.cx"= 0, "m.cy"= 0, "m.majoraxis"= 0, "m.eccentricity"= 0, "m.theta"= 0)
  }

  outputDF <- data.frame(image=filename,leaf.number = x$leafNum, lesion.status="keep", featuresLesionCleanPos, stringsAsFactors=FALSE)

  list(featuresLesion = featuresLesion, maskLesion = maskLesion, outputDF = outputDF)
}

# analysis One scan image
analyseUniqueFile <<- function(pathResult, pathImages, imageSamples, classes) {

  if (rv$parallelMode == TRUE){
    print(paste0("RUNNING: ", imageSamples) )
  }else{
    progress$set(value = rv$c, message = paste("Analysis leaf ", rv$nbSamplesAnalysis, " / ", nbSamples," : ", imageSamples), detail = "Step 2/7 : Reading image sample and apply calibration")
  }
  if (!file.exists(pathResult))
    dir.create(pathResult)

  # build filename output
  filename <- strsplit(imageSamples, ".", fixed = TRUE)[[1]][1]
  fileRData <- paste(pathResult, '/', filename, ".RData", sep = '')
  jpegfile <- paste(pathResult, '/', filename, "_both.jpeg", sep = '')
  jpegfileOnly <- paste(pathResult, '/', filename, "_lesion.jpeg", sep = '')
  csv_Merge_lesionsFile <- paste(pathResult, '/', filename, "_Merge_lesions.csv", sep = '')
  csv_All_lesionsFile <- paste(pathResult, '/', filename, "_All_lesions.csv", sep = '')

  background <- classes$subclass[classes$class=="background"]
  limb <- classes$subclass[classes$class=="limb"]
  lesion <- classes$subclass[classes$class=="lesion"]

#  print(classes)
#  print(background)
#  print(limb)
#  print(lesion)

  ## reading the source image
  sourceImage <- paste(pathImages, '/', imageSamples, sep = '')
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
  df5$lesion <- as.numeric(df5$predict %in% lesion)
  df5$leaf <- as.numeric(!(df5$predict %in% background))

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
  if (rv$rmScanLine == TRUE){
    brush2 <- makeBrush(rv$leaf_border_size*2+1,  shape = 'disc')
    maskLeaf <- dilate(maskLeaf, brush2) ; maskLeaf <- erode(maskLeaf,  brush2)
  }else{
    maskLeaf <- erode(maskLeaf,  brush)
  }

  ## segmentation
  maskLeaf <- bwlabel(maskLeaf)
  featuresLeaf <- data.frame(computeFeatures.shape(maskLeaf))

  ## removing objects smaller than the minimum area of a leaf
  w <- which(featuresLeaf[, "s.area"] < rv$leaf_min_size)
  if (length(w) > 0) {
    maskLeaf[maskLeaf %in% w] <- 0
    featuresLeaf <- featuresLeaf[-w, ]
  }
  featuresLeaf$leaf.number <- seq(1, nrow(featuresLeaf))
  ## DEBUG:  display(maskLeaf, method = "raster", all = TRUE)

  # size of leafs
  surfaceLeaves <- featuresLeaf["s.area"]

#  print(paste("surface leaves: ", surfaceLeaves))
#  print(paste("as.numeric(row.names(featuresLeaf)): ", as.numeric(row.names(featuresLeaf))))
#  print(featuresLeaf)


  ## removal of the background
  imageBackgroundBlack <- image
  imageBackgroundBlack[maskLeaf == 0] <- 0

  ## separation of leafs
  progress$set(value = rv$c, message = paste("Analysis leaf ", rv$nbSamplesAnalysis, " / ", nbSamples," : ", imageSamples), detail = "Step 3/7 : Extract leaves")
  li <- lapply(as.numeric(row.names(featuresLeaf)), extractLeaf, maskLeaf, imageBackgroundBlack, featuresLeaf)

  ## analyse des leafs
  progress$set(value = rv$c, message = paste("Analysis leaf ", rv$nbSamplesAnalysis, " / ", nbSamples," : ", imageSamples), detail = "Step 4/7 : Extract lesion on leaves")
  analyse.li <- lapply(li,  analyseLeaf,  lda1 = lda1,  lesion = lesion, limb = limb, filename = filename)

  # print both sample and lesion images
  progress$set(value = rv$c, message = paste("Analysis leaf ", rv$nbSamplesAnalysis, " / ", nbSamples," : ", imageSamples), detail = "Step 5/7 : Generate output images")
  rv$position <- "right"
  if (rv$position == "right"){
    jpegfile <- paste(pathResult, '/', filename, "_right_both.jpeg", sep = '')
    jpeg(jpegfile,
         width = widthSize*2,
         height = heightSize,
         units = "px")
    par( mfrow = c(1,2) )
  }else if (rv$position == "bottum"){
    jpeg(jpegfile,
         width = widthSize,
         height = heightSize*2,
         units = "px")
    par( mfrow = c(2,1) )
  }
  display(image, method="raster")

  # save Analysis to RData file
  save(analyse.li,li ,file=fileRData)

  ## sortie des résultats et coloration des lésions
  for (i in 1:length(li)){
    if (i == 1){
      result <- analyse.li[[i]]$outputDF
    }else{
      result <- rbind( result, analyse.li[[i]]$outputDF )
    }

    maskLesion <- analyse.li[[i]][["maskLesion"]]
    tmpimage <- image[li[[i]]$b$y, li[[i]]$b$x,]
    tmpimage <- paintObjects(maskLesion  ,tmpimage, thick=TRUE, col=c(rv$lesion_color_border, rv$lesion_color_bodies), opac=c(rv$lesion_color_borderAlpha, rv$lesion_color_bodiesAlpha))
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

  progress$set(value = rv$c, message = paste("Analysis leaf ", rv$nbSamplesAnalysis, " / ", nbSamples," : ", imageSamples), detail = "Step 6/7 : Generate ouput tables")
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
  ag$lesion.nb[ag$lesion.surface == 0] <- 0
  ag$lesion.surface[ag$lesion.surface == 0] <- 0
#  print(ag)

  write.table(
    ag[order(ag$leaf.number),],
    file = csv_Merge_lesionsFile,
    quote = FALSE,
    row.names = FALSE,
    sep = '\t'
  )

  if (rv$parallelMode == TRUE){
    print(paste0("FINISH: ", imageSamples) )
  }else{
    progress$set(value = rv$c, message = paste("Analysis leaf ", rv$nbSamplesAnalysis, " / ", nbSamples," : ", imageSamples), detail = "Step 7/7 : Finish sample")
  }

#  return(cat("FINISH: ",sourceImage,"\t",jpegfileOnly,"\n"))
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

output$dirSamples <- renderText({
  rv$dirSamples
})

updateDirAnalysis <- observeEvent(input$dirInSamples,{
  if (!is.integer(input$dirInSamples))
  {
    home <- normalizePath(allVolumesAvail[input$dirInSamples$root], winslash = "\\")
    rv$dirSamples <- file.path(home, paste(unlist(input$dirInSamples$path[-1]), collapse = .Platform$file.sep))
    rv$exitStatusAna -1
    rv$messAna <- NULL
    rv$errAna <- NULL
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

output$dirOutAnalysis <- renderText({
  rv$dirSamplesOut
})

updateDirOutAnalysis <- observeEvent(input$dirOut,{
  if (!is.integer(input$dirOut))
  {
    home <- normalizePath(allVolumesAvail[input$dirOut$root], winslash = "\\")
    rv$dirSamplesOut <- file.path(home, paste(unlist(input$dirOut$path[-1]), collapse = .Platform$file.sep))
    rv$exitStatusAna <- -1
    rv$messAna <- NULL
    rv$errAna <- NULL
  }
})

############################################
## Load RData file
############################################
shinyFileChoose(input, 'fileRDataIn',
                roots=allVolumesAvail,
                filetypes=c('', 'rdata' , 'RData')
                )

output$fileRData <- renderText({
  rv$fileRData
})

observeEvent(input$fileRDataIn,{
  if (!is.integer(input$fileRDataIn))
  {
    rv$fileRData <-  normalizePath(as.character(parseFilePaths(roots=allVolumesAvail, input$fileRDataIn)$datapath), winslash = "\\")
    filename <-  tools::file_path_sans_ext(normalizePath(as.character(parseFilePaths(roots=allVolumesAvail, input$fileRDataIn)$datapath), winslash = "\\"))
#    print(filename)
    rv$fileClass <- paste0(filename,"_classes.txt")
#    print(rv$fileClass)
  }
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
#
  }
if (!is.null(rv$fileClass) && file.exists(rv$fileClass)) {
    # User has not uploaded correct file yet
    rv$classes <- read.table(rv$fileClass,header=TRUE,sep='\t')
  }
})

############################################
## Validate value for options
############################################

#validate_INT <- function(inputValue,name) {
#  if(!is.numeric(inputValue) || (inputValue <= 0) || is.na(inputValue)){
#    rv$codeValidationInt <- 0
#    rv$warning <- HTML(paste0("Please input a number >= 0 for <b>",name,"</b> !"))
#    feedbackWarning(
#      inputId = name,
#      condition = all(c(
#        inputValue <= 0,
#        inputValue  %% 1 == 0
#      )),
#      text = "Warning please enter 0 < value"
#    )
#    updateNumericInput(session,name, value = 1)
#  }else{
#    rv$codeValidationInt <- 1
#  }
#  feedbackDanger(
#      inputId = name,
#      condition = is.na(inputValue),
#      text = "Danger please enter number"
#    )
#}

returnInpair <- function(value){
  if(value%%2==0){
    return(value+1)
  }else{
    return(value)
  }
}

######## Image
###### Blur image

###### active or not (checkbox)
observeEvent(input$active_blur,{
  rv$active_blur <- input$active_blur
})

observeEvent(input$blur_value,{
  feedbackDanger(
      inputId = "blur_value",
      condition = is.na(input$blur_value),
      text = "Please add number 'or 1 will be use'"
    )
  req(input$blur_value)
  if (is.na(input$blur_value) || as.numeric(input$blur_value) == 0){
    updateNumericInput(session,"blur_value", value = 1)
    rv$blur_value <- 1
  }
  else if (as.numeric(input$blur_value) > 21){
    updateNumericInput(session,"blur_value", value = 21)
    rv$blur_value <- 21
  }
  else{
    rv$blur_value <- returnInpair(as.numeric(input$blur_value))
    updateNumericInput(session,"blur_value", value = returnInpair(as.numeric(input$blur_value)))
  }
})

######## LEAF
###### leaf_min_size
observeEvent(input$leaf_min_size,{
  feedbackDanger(
      inputId = "leaf_min_size",
      condition = is.na(input$leaf_min_size),
      text = "Please add number 'or 1000 will be use'"
    )
  req(input$leaf_min_size)
  if (is.na(input$leaf_min_size) || as.numeric(input$leaf_min_size) == 0){
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
      condition = is.na(input$leaf_border_size),
      text = "Please add number 'or 5 will be use'"
    )
  req(input$leaf_border_size)
  if (is.na(input$leaf_border_size) || as.numeric(input$leaf_border_size) == 0){
    updateNumericInput(session,"leaf_border_size", value = 5)
    rv$leaf_border_size <- 5
  }
  else{
    updateNumericInput(session,"leaf_border_size", value = returnInpair(as.numeric(input$leaf_border_size)))
    rv$leaf_border_size <- returnInpair(as.numeric(input$leaf_border_size))
  }
})

######## LESIONS
###### lesion_min_size
observeEvent(input$lesion_min_size,{
  feedbackDanger(
      inputId = "lesion_min_size",
      condition = is.na(input$lesion_min_size),
      text = "Please add number 'or 10 will be use'"
    )
  req(input$lesion_min_size)
  if (is.na(input$lesion_min_size) || as.numeric(input$lesion_min_size) == 0){
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
      condition = is.na(input$lesion_max_size),
      text = "Please add number 'or 120000 will be use'"
    )
  req(input$lesion_max_size)
  if (is.na(input$lesion_max_size) || as.numeric(input$lesion_max_size) == 0 || input$lesion_min_size > input$lesion_max_size){
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
      condition = is.na(input$lesion_border_size),
      text = "Please add number 'or 3 will be use'"
    )
  req(input$lesion_border_size)
  if ( is.na(input$lesion_border_size) || as.numeric(input$lesion_border_size) == 0){
    updateNumericInput(session,"lesion_border_size", value = 3)
    rv$lesion_border_size <- 3
  }else{
  updateNumericInput(session,"lesion_border_size", value = returnInpair(as.numeric(input$lesion_border_size)))
  rv$lesion_border_size <- returnInpair(as.numeric(input$lesion_border_size))
  }
})


output$codeValidationInt <- renderText({
  rv$codeValidationInt
})
output$warning <- renderUI({
  rv$warning
})


######### parallel mode
observeEvent(c(input$parallelMode,input$parallelThreadsNum),{
  rv$parallelMode <- input$parallelMode
  max_no_cores <- as.numeric(max(1, detectCores() - 2))
  feedbackDanger(
      inputId = "parallelThreadsNum",
      condition = is.na(input$parallelThreadsNum),
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
  rv$rmEccentricMin <- input$lesion_eccentric_slider[1]
  rv$rmEccentricMax <- input$lesion_eccentric_slider[2]
})

############################################
## run analysis
############################################

saveParameters <- function(){
      # create config file to save input values
    paramfilename <- paste0(rv$dirSamplesOut,"/LeAFtool-parameters-input.txt")
    parameters <- paste0(
              "Samples folder: ",rv$dirSamples,"\n",
              "Output folder: ",rv$dirSamplesOut,"\n",
              "file RData: ",rv$fileRData,"\n",
              "Blur: ",rv$active_blur, " ",rv$blur_value,"\n",
              "Leaf min size: ",rv$leaf_min_size,"\n",
              "Leaf border size: ",rv$leaf_border_size,"\n",
              "rmEdge: ",rv$rmEdge,"\n",
              "rmEccentric: ",rv$rmEccentric, " ",rv$rmEccentricMin, " ",rv$rmEccentricMax,"\n",
              "lesion_min_size: ", rv$lesion_min_size,"\n",
              "lesion_max_size: ", rv$lesion_max_size,"\n",
              "lesion_border_size: ", rv$lesion_border_size,"\n",
              "lesion_color_border: ", rv$lesion_color_border,"\n",
              "lesion_color_bodies: ", rv$lesion_color_bodies,"\n",
              "parallelMode: ", rv$parallelMode, " ",rv$parallelThreadsNum,"\n"
               )
    cat(parameters, '\n', file = paramfilename)

}

resultAnalysis <- observeEvent(input$runButtonAnalysis,{
  ## load values and add loading frame
  rv$exitStatusAna <- 0
  rv$lesion_color_border <- input$lesion_color_border
  rv$lesion_color_bodies <- input$lesion_color_bodies
  rv$lesion_color_borderAlpha <-col2rgb(input$lesion_color_border, alpha=TRUE)[4]/255
  rv$lesion_color_bodiesAlpha <-col2rgb(input$lesion_color_bodies, alpha=TRUE)[4]/255

  displayableData <- DT::datatable(data = NULL)
  rv$dirInResult <- rv$dirSamplesOut

  rv$responseDataFilter <- NULL

  saveParameters()

  ############################ RUN ANALYSIS
  # count number of Samples on input directory
  listSamples <-list.files(rv$dirSamples)

  nbSamples <<- length(listSamples)
  rv$nbSamplesAnalysis <- 1
  show("loading-content")

  progress <<- shiny::Progress$new(session, min = 1, max = nbSamples+1)
  on.exit(progress$close())

  if (rv$parallelMode == TRUE){

    # if less samples than threads, update number of threads to use only max samples
    if ( rv$parallelThreadsNum > nbSamples){
      updateNumericInput(session,"parallelThreadsNum", value = nbSamples)
      rv$parallelThreadsNum <- nbSamples
    }
    # remove previous log (not working if multiple instance on same path)
    unlink(logfilename)
    # create log file
    logfilename <<- paste0(rv$dirSamplesOut,"/debug.txt")


    cat(as.character(Sys.time()), '\n', file = logfilename)

    progress$set(rv$nbSamplesAnalysis/nbSamples, message = "Analysis run ", detail = paste("Start parallel analysis with ", rv$parallelThreadsNum, " cores, log file is: ", logfilename," open to see progress"))


    # Start parallel session
    osSystem <- Sys.info()["sysname"]
    if (osSystem == "Darwin" || osSystem == "Linux") {
      cl <- makeCluster(rv$parallelThreadsNum, outfile = logfilename, type = "FORK")
    }
    else if (osSystem == "Windows") {
      cl <- makeCluster(rv$parallelThreadsNum, outfile = logfilename, type = "SOCK")
    }
    registerDoParallel(cl)

    # Add an entry to the log file
    cat(as.character(Sys.time()), '\n', file = logfilename,
      append = TRUE)

#    reactive({
#      progress$set(value = rv$nbSamplesAnalysis, message = paste("Analysis leaves ", rv$nbSamplesAnalysis, " / ", nbSamples), detail = output$log)
#      progress$set(value = rv$nbSamplesAnalysis, message = paste("Analysis leaves ", rv$nbSamplesAnalysis, " / ", nbSamples), detail = verbatimTextOutput('log', placeholder = FALSE))
#    })
    res <- foreach(imageSamples = listSamples,
            .export = c(".GlobalEnv"),
            .combine = c,
            .packages = 'shiny')  %dopar%
            {
                cat(paste0(imageSamples,"\n") , file = logfilename, append = TRUE) # write file to log
#                f <- future({
                isolate({
                  analyseUniqueFile(rv$dirSamplesOut,  rv$dirSamples, imageSamples, rv$classes)
                }) # use isolate to prevente error with reactive values
            }
#    print(res)
  #  # Close cluster mode
    stopCluster(cl)
    closeAllConnections();
    registerDoSEQ()

  }else{
    # if not cluster mode do sample by sample on one core
    progress$set(value = 0 , detail = paste("Start samples analysis with 1 cores"))
    for (imageSamples in listSamples){
      progress$set(value = rv$nbSamplesAnalysis, message = paste("Analysis leaf ", rv$nbSamplesAnalysis, " / ", nbSamples, " : ",imageSamples), detail = "Step 1/7 Start function")
      cat(paste0(imageSamples,"\n")  , file = logfilename, append = TRUE) # write file to log
      analyseUniqueFile(rv$dirSamplesOut,rv$dirSamples,imageSamples, rv$classes)
      rv$nbSamplesAnalysis <- rv$nbSamplesAnalysis + 1
    }
    progress$set( value = rv$nbSamplesAnalysis, message = "Analysis Finish ")
  }

  mymergeddata = multmerge(rv$dirSamplesOut, "*_Merge_lesions.csv")

  write.table(
    mymergeddata,
    file = paste0(rv$dirSamplesOut,"/merge_ResumeCount.csv"),
    quote = FALSE,
    row.names = FALSE,
    sep = '\t'
  )
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

img_uri1 <- function(x) {

  ext <- unlist(strsplit(x, "[.]"))[2]
  if ( ext == "tif"){
    sprintf("Tiff not support by browser", x)
  }else{
    sprintf("<img src='Original/%s' height='60'></img>", x)
  }
}
img_uri2 <- function(x) {
#  sprintf("<img src='%s' height='60'></img>", knitr::image_uri(x))
  sprintf("<img src='LesionColor/%s' height='60'></img>", x)
}



output$contents <- DT::renderDataTable({

  if (rv$exitStatusAna == 1){

    LeafNames <- list.files(rv$dirSamples, full.names=FALSE)

    rv$LeafNamesFull <-  unlist(lapply(list.files(rv$dirSamples, full.names=FALSE),img_uri1), use.names=FALSE)
    LeafNames2 <- list.files(rv$dirSamplesOut, full.names=FALSE, pattern = "*_lesion.jpeg")
    rv$LeafNames2Full <-  unlist(lapply(list.files(rv$dirSamplesOut, full.names=FALSE, pattern = "*_lesion.jpeg"),img_uri2), use.names=FALSE)


    if (LeafNames != '' && LeafNames2 != '' && length(LeafNames) == length(LeafNames2)){
      addResourcePath("Original",rv$dirSamples) # Images are located outside shiny App
      addResourcePath("LesionColor",rv$dirSamplesOut) # Images are located outside shiny App

      rv$responseDataFilter <- data.frame(LeafNames = LeafNames,
                              Original = rv$LeafNamesFull,
                              LesionColor = rv$LeafNames2Full,stringsAsFactors = FALSE)

      displayableData<-DT::datatable(data = as.data.frame(rv$responseDataFilter, stringAsFactors = FALSE, row.names = NULL),

                                     escape=FALSE,selection="single",rownames=FALSE,colnames=c("FileName","Original","LesionColor"),
                                     style = "bootstrap",
                                     options = list(
                                       paging=TRUE,searching = TRUE,ordering=TRUE,scrollY = 750,scrollCollapse=TRUE,server = FALSE
                                     ))

    ifelse (!is.null(displayableData),return(displayableData),return(NULL))
    }
  }

})


currentImage <- reactive({

    imIndex <- input$contents_rows_selected
    if (is.null(imIndex))
      return("")

    #Load image for plot
#    lesionImg <- strsplit(rv$responseDataFilter[imIndex,"LeafNames"], ".", fixed = TRUE)[[1]][1]
    lesionImgPath <- strsplit(rv$responseDataFilter[imIndex,"LeafNames"], ".", fixed = TRUE)[[1]][1]
#    rv$plotcurrentImage <- readImage(paste0(rv$dirSamplesOut,"/",lesionImg,"_lesion.jpeg"))
#    rv$loadCSVcurrentImage <- read.csv(paste0(rv$dirSamplesOut, "/",lesionImg,"_All_lesions.csv"),header = TRUE, sep = "\t", stringsAsFactors = FALSE)
    rv$responseDataFilter[imIndex,"LeafNames"]


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
       column(width = 5, offset = 0,
          HTML(gsub("height='60'","height='95%'",rv$responseDataFilter[imIndex,"Original"]))
#         img(src= paste0(rv$dirSamples,"/",currentImage()),width='100%',height='100%')
       ),
        column(width = 5, offset = 0,
           HTML(gsub("height='60'","height='95%'",rv$responseDataFilter[imIndex,"LesionColor"]))
#          img(src= paste0(rv$dirSamplesOut,"/", lesionImg, "_lesion.jpeg"),width='100%',height='100%')
#          displayOutput("plotcurrentImage") #,click = "plot_click",dblclick = "plot_dbclick", brush = "plot_brush"
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


outputOptions(output, 'codeValidationInt', suspendWhenHidden = FALSE)
outputOptions(output, 'analysisFinish', suspendWhenHidden = FALSE)
outputOptions(output, 'warning', suspendWhenHidden = FALSE)
outputOptions(output, 'contents', suspendWhenHidden = FALSE)
