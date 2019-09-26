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

## load packages
library(EBImage)
library(MASS)
library(lattice)
library(ParallelLogger)
library(shinyjs)
## library(e1071) only for svm (not implemented)

##############################################
##### FUNCTION clusterApply2 to run parallel mode with progress bar both on commande line and shiny (base on ParallelLogger function)
##############################################
clusterApply2 <- function(cluster, x, fun, ..., stopOnError = FALSE, progressBar = TRUE, mode="CMD") {
  if (class(cluster)[1] == "noCluster") {
    lapply(x, fun, ...)
  } else {
    n <- length(x)
    p <- length(cluster)
    if (n > 0 && p > 0) {
      if (progressBar && mode == "CMD") {pb <- txtProgressBar(style = 3)}
      if (progressBar && mode == "GUI" && p > 1 )
      {
        progressNODE <- shiny::Progress$new(min = 0, max = n)
        progressNODE$set(message = 'Analysis in progress', detail = paste0('Initiating cluster with ',p,' threads and starting on the first ',p,' files of ',n), value = 0)
        on.exit(progressNODE$close())
        }

      for (i in 1:min(n, p)) {
        snow::sendCall(cluster[[i]], fun, c(list(x[[i]]), list(...)), tag = i)
      }

      val <- vector("list", n)
      hasError <- FALSE
      formatError <- function(threadNumber, error, args) {
        sprintf("Thread %s returns error: \"%s\" when using argument(s): %s",
                threadNumber,
                gsub("\n", "\\n", gsub("\t", "\\t", error)),
                gsub("\n", "\\n", gsub("\t", "\\t", paste(args, collapse = ","))))
      }
      for (i in 1:n) {
        d <- snow::recvOneResult(cluster)
        if (inherits(d$value, "try-error")) {
          val[d$tag] <- NULL
          errorMessage <- formatError(d$node, d$value, c(list(x[[d$tag]]), list(...)))
          if (stopOnError) {
            stop(errorMessage)
          } else {
            ParallelLogger::logError(errorMessage)
            hasError <- TRUE
          }
        }
        if (progressBar && mode == "CMD")  setTxtProgressBar(pb, i/n)
        if (progressBar && mode == "GUI" && p > 1 )
        {
          progressNODE$set(value = i, detail = paste0("Number of image analyze: ",i,"/",n))
        }
        j <- i + min(n, p)
        if (j <= n) {
          snow::sendCall(cluster[[d$node]], fun, c(list(x[[j]]), list(...)), tag = j)
        }
        val[d$tag] <- list(d$value)
      }
      if (progressBar && mode == "CMD") {
        close(pb)
      }
      if (progressBar && mode == "GUI" && p > 1 ) {
        progressNODE$close()
      }
#      if (hasError) {
#        message <- paste0("Error(s) when calling function '",
#                          substitute(fun, parent.frame(1)),
#                          "', see earlier messages for details")
#        stop(message)
#      }
      df <- do.call(rbind,val)
      return(df)
    }
  }
}

##############################################
##### FUNCTION writeLOGAnalysis to To write in log file or show progress if not in parallel mode
##############################################
writeLOGAnalysis <- function(path = NULL, create = FALSE, message = NULL, detail = NULL, mode = NULL, value = NULL, progress = NULL, parallelThreadsNum = NULL) {
    # if not GUI mode write to log file
    if (!is.null(path)){
      if (create == TRUE) {
        # create log file
        logfilename <- base::normalizePath(paste0(path, "/log.txt"), winslash = "/")
        unlink(logfilename)# Clean up log file from the previous example
        ParallelLogger::clearLoggers()# Clean up the loggers from the previous example
        ParallelLogger::addDefaultFileLogger(logfilename)
      }
      ParallelLogger::logInfo(paste0(message, detail))
    if (mode == "GUI" && parallelThreadsNum == 1){
      progress$set(value = value, message = message, detail = detail)
    }
  }
}

##############################################
##### FUNCTION checkDirRead to test if path is valid (e=exist, r=read, w=write)
##############################################
checkDirRead <- function(path,namePath, test = c("e","r","w")){
#  readable = mode 4 and writeable mode 3 existence mode 0
  if (!is.null(path)){
    if (file.access(path, mode = 0)[[1]] == -1 && ("e" %in% test)){
      errorMessage <- paste0("Path '",path,"' is not a valid path for parameter '",namePath,"', because it don't exist")
      stop(errorMessage, call. = FALSE)
    }
    if (file.access(path, mode = 4)[[1]] == -1 && ("r" %in% test)){
      errorMessage <- paste0("Path '",path,"' is not a valid path for parameter '",namePath,"', because you don't have read permission")
      stop(errorMessage, call. = FALSE)
    }
    if (file.access(path, mode = 3)[[1]] == -1 && ("w" %in% test)) {
      errorMessage <- paste0("Path '",path,"' is not a valid path for parameter '",namePath,"', because you don't have write permission")
      stop(errorMessage, call. = FALSE)
    }
  }
}

##############################################
##### FUNCTION is.wholenumber check integer value
##############################################
is.wholenumber <- function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol

##############################################
##### FUNCTION checkValue test for all numeric parameters
##############################################
checkValue <- function(value, nameParam, odd=FALSE, max=NULL){

  if (!is.numeric(value) || !is.wholenumber(value)){
    errorMessage <- paste0("'",value,"' is invalid value for '",nameParam,"', please add integer number.")
    stop(errorMessage, call. = FALSE)
  }
  else if (is.na(value) || as.numeric(value) < 0){
    errorMessage <- paste0("'",value,"' is invalid value for '",nameParam,"', please add positive integer number.")
    stop(errorMessage, call. = FALSE)
  }
  else if(odd == TRUE && value !=0 && value%%2==0){
    warningMessage <- paste0("'",value,"' is not odd value for '",nameParam,"', auto-adjust to ",value+1,".")
    warning(warningMessage)
    return(value+1)
  }
  if(!is.null(max) && max < value){
    errorMessage <- paste0("'",value,"' for parameter '",nameParam,"' is greater than max value '",max,"' for '",gsub('.{3}$', '', nameParam),"Max', please change value.")
    stop(errorMessage, call. = FALSE)
  }
  return(value)
}

##############################################
##### FUNCTION checkValuelesionEccentricity
##############################################
checkValuelesionEccentricity <- function(value, nameParam, max=NULL){

  if (!is.numeric(value)){
    errorMessage <- paste0("'",value,"' is invalid numeric value for '",nameParam,"', please add  0 < float number < 1.")
    stop(errorMessage, call. = FALSE)
  }
  else if (value < 0 || value > 1){
    errorMessage <- paste0("'",value,"' is invalid value for '",nameParam,"', please add  0 < float number < 1.")
    stop(errorMessage, call. = FALSE)
  }
  if(!is.null(max) && max <= value){
    errorMessage <- paste0("'",value,"' for parameter '",nameParam,"' is greater or equal than max value '",max,"' for '",gsub('.{3}$', '', nameParam),"Max', please change value.")
    stop(errorMessage, call. = FALSE)
  }
  return(value)
}

##############################################
##### FUNCTION checkColorHexacode
##############################################
checkColorHexacode <- function(color, nameParam){
  tryCatch({ grDevices::col2rgb(color, alpha=TRUE)[4]/255},
        error=function(cond) {
            stop(paste("Erreur : '",color," is not a valid hexadécimal number color for parameter '",nameParam,"', you can try '#0000FF11' for blue or '#0000FF00' for transparent."), call. = FALSE)
        })
}

##############################################
##### FUNCTION checkParameters, check all parameter and return if need adjust parameters
##############################################
checkParameters <- function(pathTraining,pathImages,fileImage,leafAreaMin,leafBorder,lesionBorder,lesionAreaMin,lesionAreaMax,lesionEccentricityMin, lesionEccentricityMax,lesionColorBorder,lesionColorBodies,leafColorBorder, leafColorBodies,blurDiameter,outPosition){

  # test input training path is readable = mode 4 and/or writeable mode 3 and if file Rdata exist
  basename <- utils::tail(strsplit(pathTraining, .Platform$file.sep)[[1]], 1)
  fileTrain <-  paste0(pathTraining, .Platform$file.sep, basename,".RData")
  checkDirRead(pathTraining,"pathTraining")
  if (file.access(fileTrain, mode = 0)[[1]] == -1) {
    errorMessage <- paste0("Path '",pathTraining,"' is not a valid path for parameter 'pathTraining', can't find the training file: '",fileTrain,"', please run training function before.")
    stop(errorMessage, call. = FALSE)
  }

  # test input pathImages path is readable = mode 4 and writeable mode 3
  checkDirRead(pathImages,"pathImages")
  if (!is.null(pathImages)){
    listSamples <- list.files(pathImages, full.names = FALSE, include.dirs = FALSE, pattern = "\\.jpg$|\\.jpeg$|\\.PNG$|\\.tif$", ignore.case = TRUE)
    if (length(listSamples) == 0){
      errorMessage <- paste0("Path '",pathImages,"' is not a valid path for parameter 'pathImages', can't find the jpg, jpeg, png or tif files.")
      stop(errorMessage, call. = FALSE)
    }
  }
  # test input fileImage path is readable = mode 4 and writeable mode 3
  checkDirRead(fileImage,"fileImage",test=c("e","r"))

  # check all numeric values
  checkValue(leafAreaMin,"leafAreaMin")
  checkValue(lesionAreaMin,"lesionAreaMin",max=lesionAreaMax)
  checkValue(lesionAreaMax,"lesionAreaMax")
  leafBorder <- checkValue(leafBorder,"leafBorder",odd=TRUE)
  lesionBorder <- checkValue(lesionBorder,"lesionBorder", odd=TRUE)
  blurDiameter <- checkValue(blurDiameter,"blurDiameter", odd=TRUE)
  checkValuelesionEccentricity(lesionEccentricityMin,"lesionEccentricityMin", max=lesionEccentricityMax)
  checkValuelesionEccentricity(lesionEccentricityMax,"lesionEccentricityMax")

  # check color:
  checkColorHexacode(lesionColorBorder,"lesionColorBorder")
  checkColorHexacode(lesionColorBodies,"lesionColorBodies")
  checkColorHexacode(leafColorBorder,"leafColorBorder")
  checkColorHexacode(leafColorBodies,"leafColorBodies")

  # outPosition
  if (!(outPosition %in% c("right", "bottum"))){
    stop(paste("'",outPosition,"' is not valid value for outPosition, please only use 'right' or 'bottum'"), call. = FALSE)
  }

  return(list("leafBorder"=leafBorder, "lesionBorder"=lesionBorder, "blurDiameter"=blurDiameter))
}


##############################################
##### FUNCTIONS  for analysis leaves
##############################################
## returns the range of indexes of objet in vector x
## (called by boundingRectangle)
rangeNA <- function(x, object) {
    w <- which(x == object)
    if (length(w) == 0) return(c(NA, NA))
    return(range(w))
}

boundingRectangle <- function(mask, object) {
    m <- EBImage::imageData(mask)
    range.x <- range(apply(m,1,rangeNA,object),na.rm=TRUE)
    range.y <- range(apply(m,2,rangeNA,object),na.rm=TRUE)
    list(x=range.x[1]:range.x[2],y=range.y[1]:range.y[2])
}

extractLeaf <- function(i,mask,imageBlackBackground, featuresLeaf) {
    # size of leaf
    surfaceLeaf <- featuresLeaf[row.names(featuresLeaf) %in% c(i),"s.area"]
    leafNum <- featuresLeaf[row.names(featuresLeaf) %in% c(i),"leaf.number"]
    b <- boundingRectangle(mask,i)
    leaf <- imageBlackBackground[b$y,b$x,]
    mask.leaf <- mask[b$y,b$x]
    leaf[mask.leaf!=i] <- 0
    xCoord <- list(min = min(b$x),max = max(b$x))
    yCoord <- list(min = min(b$y),max = max(b$y))
    XYcoord <- list(x = xCoord, y = yCoord)
    list(b = b, XYcoord = XYcoord, leaf = leaf, leaf.surface = surfaceLeaf, leafNum = leafNum)
}

predict2 <- function(image,train) { ## returns predicted values according to train
    df <- data.frame(red=as.numeric(EBImage::imageData(image)[,,1]), green=as.numeric(EBImage::imageData(image)[,,2]), blue=as.numeric(EBImage::imageData(image)[,,3]))
    if (train$colormodel=="hsv") df <- rgb2hsv2(df)
    if (!is.null(train$transform)) df[1:3] <- lapply(df,train$transform)
    if (train$method=="lda" || train$method=="qda")
        return(stats::predict(train$lda1,df)$class)
    else if (train$method=="svm")
        return(stats::predict(train$svm,df))
}

##############################################
##### FUNCTION analyseLeaf to analyse a single leaf if multiple on image
##############################################
analyseLeaf <- function(x,train,lesionBorder,lesionAreaMin,lesionAreaMax,lesionEccentricityMin,lesionEccentricityMax,blurDiameter,filename) {

  ## DEBUG: print(paste("leafNum : ", x$leafNum, "    surface leaf : ",x$leaf.surface))
  f <- x$leaf
  ## DEBUG: EBImage::display(f, method = "raster", all = TRUE)

  ## remplace background with limb
  limb <- train$classes$subclass[train$classes$class=="limb"]
  lesion <- train$classes$subclass[train$classes$class=="lesion"]
  df6 <- data.frame(red=as.numeric(EBImage::imageData(f)[,,1]), green=as.numeric(EBImage::imageData(f)[,,2]), blue=as.numeric(EBImage::imageData(f)[,,3]))
  black.background <- df6$red+df6$green+df6$blue==0
  prediction <- predict2(f,train)
  df.limb <- df6[prediction %in% limb & !black.background,]
  mean.limb <- apply(df.limb[1:3],2,mean)
  df6[black.background,1] <- mean.limb[1]
  df6[black.background,2] <- mean.limb[2]
  df6[black.background,3] <- mean.limb[3]
  EBImage::imageData(f)[,,1] <- df6$red
  EBImage::imageData(f)[,,2] <- df6$green
  EBImage::imageData(f)[,,3] <- df6$blue
  # end replace

  # blur image or not
  if (blurDiameter >= 1) { ## blurring
    ##x11() ; EBImage::display(f,method="raster")
    flo <- EBImage::makeBrush(blurDiameter, shape='disc', step=FALSE)^2
    flo <- flo/sum(flo)
    f <- EBImage::filter2(f, flo)
    f[f<0] <- 0
    ##x11() ; EBImage::display(f,method="raster")
  }

  ## analyse prediction
  prediction <- predict2(f, train)
  patch <- as.numeric(prediction %in% lesion)
  mask <- EBImage::channel(f, "gray")
  patch.mat <- matrix(patch, nrow=nrow(EBImage::imageData(mask)))
  EBImage::imageData(mask) <- patch.mat

  ## dilate
  brush <- EBImage::makeBrush(lesionBorder, shape='disc')
  mask <- EBImage::dilate(mask, brush)

  ## fill empty areas
  mask <- EBImage::fillHull(mask)

  ## erode
  mask <- EBImage::erode(mask, brush)

  ## segmentation
  mask[mask<0] <- 0
  mask <- EBImage::bwlabel(mask)
  featuresLesion <- EBImage::computeFeatures.shape(mask)
  momentsLesion <- EBImage::computeFeatures.moment(mask)

  ## Remove objects
  w.small <- w.great <- w.eccentricMin <- w.eccentricMax <- w.edge <- NULL

  ## search for small objects
  w.small <- which(featuresLesion[,"s.area"] <= lesionAreaMin)
  ## search for great objects
  w.great <- which(featuresLesion[,"s.area"] >= lesionAreaMax)

#    if (rmEdge == TRUE){
#    ## search objets on the edge of the image
#    top <- unique(EBImage::imageData(mask)[1,])
#    left <- unique(EBImage::imageData(mask)[,1])
#    bottom <- unique(EBImage::imageData(mask)[dim(mask)[1],])
#    right <- unique(EBImage::imageData(mask)[,dim(mask)[2]])
#    w.edge <- unique(c(top, left, bottom, right))
#    }
  # remove eccentric lesions
  w.eccentricMin <- which(momentsLesion[,"m.eccentricity"] <= lesionEccentricityMin)
  w.eccentricMax <- which(momentsLesion[,"m.eccentricity"] >= lesionEccentricityMax)

  # apply to maskLesion
  mask[mask %in% c(w.small, w.great, w.edge, w.eccentricMin, w.eccentricMax)] <- 0
  maskLesion <- mask
  ## DEBUG:EBImage::display(maskLesion, method = "raster", all = TRUE)

  ## renumber objects
  featuresLesion <- EBImage::computeFeatures.shape(maskLesion)

  featuresLesionClean <- as.data.frame(featuresLesion)
  featuresLesionClean$lesion.number <- as.numeric(row.names(featuresLesionClean))

  colnames(featuresLesionClean)[which(colnames(featuresLesionClean) %in%
    c("s.area", "s.perimeter", "s.radius.mean", "s.radius.sd", "s.radius.min", "s.radius.max") )] <-
    c("lesion.surface", "lesion.perimeter", "lesion.radius.mean", "lesion.radius.sd", "lesion.radius.min", "lesion.radius.max")

  # correct coord if multi leaves to edit lesion
  moments <- as.data.frame(EBImage::computeFeatures.moment(maskLesion))
  moments$m.cy <- moments$m.cy + x$XYcoord$x$min - 1
  moments$m.cx <- moments$m.cx + x$XYcoord$y$min - 1

  moments$lesion.number <- as.numeric(row.names(moments))
  featuresLesionCleanPos <- merge(featuresLesionClean, moments)
  #  featuresLesionCleanPos$lesion.number <- seq(1,nrow(featuresLesionCleanPos))
  #  row.names(maskLesion) <- seq(1,nrow(maskLesion))

  if (nrow(featuresLesionCleanPos) == 0){
    featuresLesionCleanPos <- data.frame("lesion.number" = 0, "lesion.surface"= 0, "lesion.perimeter"= 0, "lesion.radius.mean"= 0, "lesion.radius.sd"= 0, "lesion.radius.min"= 0, "lesion.radius.max"= 0, "m.cx"= 0, "m.cy"= 0, "m.majoraxis"= 0, "m.eccentricity"= 0, "m.theta"= 0)
  }

  outputDF <- data.frame(image=filename,leaf.number = x$leafNum, leaf.surface = x$leaf.surface, lesion.status="keep", featuresLesionCleanPos, stringsAsFactors=FALSE)

  list(featuresLesion = featuresLesion, maskLesion = maskLesion, outputDF = outputDF)
}

# function to merge dataframe
multmerge = function(mypath, pattern){
  filenames=list.files(path=mypath, full.names=TRUE, pattern = pattern)
  datalist = lapply(filenames, function(x){utils::read.csv(file=x,header=T, sep="\t")})
  Reduce(function(x,y) {rbind(x,y)}, datalist)
}

#' Analyse an image or a set of images
#'
#' Analysis step can use many ram on parallel mode.
#' The function return a dataframe with file name, exit status and message if error.
#'
#' @param pathTraining The path of the directory containing the sampled images used for training. After the training step, this directory contains the parameters of the training set.
#' @param pathResult The path of the directory where to store the result files (created by the function if it does not exist).
#' @param pathImages The path of the directory containing the images to analyse.
#' @param fileImage A character vector containg the fils names of the images to analyse in pathImages (NULL to analyse all the images in pathImages).
#' @param leafAreaMin The minimum area of a leaf (in pixels) Default: 1000.
#' @param leafBorder The diameter of the brush (in pixels) used to erode the leafBorder Default: 5.
#' @param lesionBorder The diameter of the brush (in pixels) used to erode the lesionBorder Default: 3.
#' @param lesionAreaMin The minimum area of a lesion (in pixels) Default: 10.
#' @param lesionAreaMax The maximum area of a lesion (in pixels) Default: 120000.
#' @param lesionEccentricityMin The minimum eccentricity of a lesion Default: 0.
#' @param lesionEccentricityMax The maximum eccentricity of a lesion Default: 1.
#' @param lesionColorBorder hexadecimal code for output fill color for lesion in the output image Default: #0000FF11 (blue).
#' @param lesionColorBodies hexadecimal code for output bodies color for lesion in the output image Default: #FE8E0000 (transparent).
#' @param leafColorBorder hexadecimal code for output fill color for leaf in the output image Default: #FF000000 (transparent).
#' @param leafColorBodies hexadecimal code for output bodies color for leaf in the output image Default: #FF000000 (transparent).
#' @param blurDiameter The diameter of the brush (in pixels) used to blur the image (0 for no blur) Default: 0'.
#' @param outPosition join origale and color lesion image at right or buttom Default: right'.
#' @param parallelThreadsNum number of thread use, 1 thread analysis 1 image if >= 2 Default: 1'.
#' @param mode auto selection to switch between GUI or CMD mode Default:"CMD")'.
#'
#' @examples
#' dataframeExitStatus <- analyseImages(pathTraining = "../exemple1/learning",
#' pathResult = "../exemple1/results",
#' pathImages = "../exemple1/samples", parallelThreadsNum=4)
analyseImages <- function(pathTraining,pathResult,pathImages=NULL,fileImage=NULL,leafAreaMin=1000,leafBorder=5,lesionBorder=3,lesionAreaMin=10,lesionAreaMax=120000,lesionEccentricityMin=0,lesionEccentricityMax=1,lesionColorBorder="#0000FF11",lesionColorBodies="#FE8E0000",leafColorBorder="FF000000", leafColorBodies="FF000000",blurDiameter=0, outPosition="right", parallelThreadsNum=1, mode="CMD") {

  # transforme to full path with slash for window or linux
  pathTraining <- base::normalizePath(pathTraining, winslash = "/")
  pathResult <- base::normalizePath(pathResult, winslash = "/")
  if (!is.null(pathImages)){ pathImages <- base::normalizePath(pathImages, winslash = "/") }
  progress <- NULL
  # auto-detect number of core on computer
  max_no_cores <- as.numeric(max(1, parallel::detectCores() - 2))

  if (!is.null(mode) && mode == "GUI" && parallelThreadsNum == 1){
      # add progress bar
      progress <- shiny::Progress$new(min=0, max=7)
      on.exit(progress$close())
    }

  ############################ Check parameters
  # test input pathResult path is readable = mode 4 and writeable mode 3
  checkDirRead(pathResult,"pathResult")
  # if possible add log file for all error after
  writeLOGAnalysis(path = pathResult, create = TRUE, message = "Analysis run, please wait", detail = " VERSION: 1.0", mode = mode, progress = progress, parallelThreadsNum = parallelThreadsNum)

  autoAdjustList <- checkParameters(pathTraining,pathImages,fileImage,leafAreaMin,leafBorder,lesionBorder,lesionAreaMin,lesionAreaMax,lesionEccentricityMin, lesionEccentricityMax,lesionColorBorder,lesionColorBodies, leafColorBorder, leafColorBodies,blurDiameter,outPosition)
  leafBorder <- autoAdjustList$leafBorder
  lesionBorder <- autoAdjustList$lesionBorder
  blurDiameter <- autoAdjustList$blurDiameter

  ############################ add to parameters file
  # create config file to save input values
  paramfilename <- file(paste0(pathResult,"/LeAFtool-parameters-input.txt"))
  parameters <- paste0(
            "Training folder: ",pathTraining,"\n",
            "Samples folder: ",pathImages,"\n",
            "Output folder: ",pathResult,"\n",
            "file image: ",fileImage,"\n",
            "leaf area min: ",leafAreaMin,"\n",
            "Leaf border size: ",leafBorder,"\n",
            "lesion border size: ",lesionBorder,"\n",
            "lesion area min: ", lesionAreaMin,"\n",
            "lesion area max: ", lesionAreaMax,"\n",
            "lesion eccentricity: min ",lesionEccentricityMin, "\tmax ",lesionEccentricityMax,"\n",
            "lesion color border: ", lesionColorBorder,"\n",
            "lesion color bodies: ", lesionColorBodies,"\n",
            "leaf color border: ", leafColorBorder,"\n",
            "leaf color bodies: ", leafColorBodies,"\n",
            "blur diameter: ",blurDiameter,"\n",
            "out position: ",outPosition,"\n",
            "parallelMode: ", parallelThreadsNum,"\n"
             )
    cmd <- paste0("analyseImages(pathTraining = '",pathTraining,"', pathResult = '",pathResult,"', pathImages = '",pathImages,"', fileImage = ",fileImage,
                ", leafAreaMin = ",leafAreaMin,
                ", leafBorder = ",leafBorder,
                ", lesionBorder = ",lesionBorder,
                ", lesionAreaMin = ",lesionAreaMin,
                ", lesionAreaMax = ",lesionAreaMax,
                ", lesionEccentricityMin = ",lesionEccentricityMin,
                ", lesionEccentricityMax = ",lesionEccentricityMax,
                ", lesionColorBorder = '",lesionColorBorder,
                "', lesionColorBodies = '",lesionColorBodies,
                ", leafColorBorder = '",leafColorBorder,
                "', leafColorBodies = '",leafColorBodies,
                "', blurDiameter = ",blurDiameter,
                ", outPosition = '",outPosition,
                "', parallelThreadsNum = ",parallelThreadsNum,")")
  cat(paste0(parameters,"\n",cmd), '\n', file = paramfilename)
  close(paramfilename)

  ############################ RUN ANALYSIS
  # count number of Samples on input directory
  if (!is.null(fileImage)){
    listSamples <- fileImage
    writeLOGAnalysis(path = pathResult, message = NULL, detail = paste0("Start Analysis on image: ",fileImage), mode = mode, value = 1, progress = progress, parallelThreadsNum = parallelThreadsNum)
  }
  else{
#    listSamples <-list.files(pathImages)
    listSamples <- list.files( pathImages, full.names = FALSE, pattern = "\\.jpg$|\\.jpeg$|\\.PNG$|\\.tif$", include.dirs = FALSE, ignore.case = TRUE)
    writeLOGAnalysis(path = pathResult, message = NULL, detail = paste0("Start Analysis on folder: ",pathImages), mode = mode, value = 1, progress = progress, parallelThreadsNum = parallelThreadsNum)
  }

  nbSamples <- length(listSamples)
  nbSamplesAnalysis <- 1
  if (parallelThreadsNum >= 1){

    # if less samples than threads or computer can, update number of threads to use only max samples
    if ( parallelThreadsNum > max_no_cores){
      warning(paste("You select ",parallelThreadsNum,", it is more use threads than computer can. auto-adjust to", max_no_cores, "threads (avail:",max_no_cores+2,")"))
      parallelThreadsNum <- max_no_cores
    }
    if ( parallelThreadsNum > nbSamples){
      warning(paste("You select ",parallelThreadsNum,", it is more use threads than samples images. auto-adjust to", nbSamples, "threads"))
      parallelThreadsNum <- nbSamples
    }

    # Start parallel session
    osSystem <- Sys.info()["sysname"]
    if (osSystem == "Darwin" || osSystem == "Linux" || osSystem == "Windows") {
      cl <- ParallelLogger::makeCluster(numberOfThreads = parallelThreadsNum, singleThreadToMain = TRUE, divideFfMemory = FALSE, setFfTempDir = FALSE)
    }
    if (parallelThreadsNum > 1)
    {
      ## load libraries on workers
      ParallelLogger::clusterRequire(cl, "shiny")
      ParallelLogger::clusterRequire(cl, "EBImage")
      ParallelLogger::clusterRequire(cl, "MASS")
      ParallelLogger::clusterRequire(cl, "lattice")
      ParallelLogger::clusterRequire(cl, "ParallelLogger")
#      ParallelLogger::clusterRequire(cl, "shinyjs")
#     ParallelLogger::clusterRequire(cl, "LeAFtool")
      parallel::clusterExport(cl, varlist=c("analyseLeaf", "analyseImageUnique", "predict2", "boundingRectangle","extractLeaf", "rangeNA", "nbSamples", "nbSamplesAnalysis", "writeLOGAnalysis", "mode", "parallelThreadsNum"), envir=environment())
    }

    res <- clusterApply2(cl, listSamples, analyseImageUnique, pathTraining, pathResult,pathImages,leafAreaMin,leafBorder,lesionBorder,lesionAreaMin,lesionAreaMax,lesionEccentricityMin, lesionEccentricityMax,lesionColorBorder,lesionColorBodies, leafColorBorder, leafColorBodies, blurDiameter,outPosition, nbSamplesAnalysis, nbSamples, mode, progress, parallelThreadsNum, stopOnError = FALSE, progressBar = TRUE, mode = mode)

    # Close cluster mode
    ParallelLogger::stopCluster(cl)
    closeAllConnections(); # for kill all process, use to add button for stop work
    parallelThreadsNum <- 1
  }

  # merge all tables
  mymergeddata = multmerge(pathResult, "*_Merge_lesions.csv")
  utils::write.table(
    mymergeddata,
    file = paste0(pathResult,"/merge_ResumeCount.csv"),
    quote = FALSE,
    row.names = FALSE,
    sep = '\t'
  )

  if (class(res) == "list")
  {
    return(do.call(rbind.data.frame, res))
  }
  return(res)
}

##############################################
##### FUNCTION analyseImageUnique to analyse only one file image
##############################################
analyseImageUnique <- function(fileImage, pathTraining,pathResult,pathImages,leafAreaMin,leafBorder,lesionBorder,lesionAreaMin,lesionAreaMax,lesionEccentricityMin,lesionEccentricityMax,lesionColorBorder,lesionColorBodies, leafColorBorder, leafColorBodies, blurDiameter,outPosition, nbSamplesAnalysis, nbSamples, mode, progress, parallelThreadsNum) {

  # check params:
  lesionColorBorderAlpha <-grDevices::col2rgb(lesionColorBorder, alpha=TRUE)[4]/255
  lesionColorBodiesAlpha <-grDevices::col2rgb(lesionColorBodies, alpha=TRUE)[4]/255

  leafColorBorderAlpha <-grDevices::col2rgb(leafColorBorder, alpha=TRUE)[4]/255
  leafColorBodiesAlpha <-grDevices::col2rgb(leafColorBodies, alpha=TRUE)[4]/255
  print(leafColorBorder)
  print(leafColorBorderAlpha)
  print(leafColorBodies)
  print(leafColorBodiesAlpha)

  if (!file.exists(pathResult)) dir.create(pathResult)

  message <- paste("Analysis leaf ",nbSamplesAnalysis,"/",nbSamples,": ", fileImage, "    ")
  detail <- " Step 1/6 : Reading image sample and apply calibration"
  writeLOGAnalysis(path = pathResult, message = message, detail = detail, mode = mode, value = 1, progress = progress, parallelThreadsNum = parallelThreadsNum)

  ## load trainig results
  basename <- utils::tail(strsplit(pathTraining, .Platform$file.sep)[[1]], 1)
  file.train <-  base::normalizePath(paste0(pathTraining, .Platform$file.sep, basename,".RData"),winslash = "/")
  load(file.train)

  # LOAD class
  background <- train$classes$subclass[train$classes$class=="background"]
  limb <- train$classes$subclass[train$classes$class=="limb"]
  lesion <- train$classes$subclass[train$classes$class=="lesion"]

  # build filename output
  ext <- unlist(strsplit(basename(fileImage), "[.]"))[2]
  leafName <- gsub(paste0(".",ext), "", basename(fileImage))
  filename <- strsplit(basename(fileImage), ".", fixed = TRUE)[[1]][1]
  fileRData <-  paste0(pathResult, .Platform$file.sep, '.', filename, ".RData")
  jpegfile <-  paste0(pathResult, .Platform$file.sep, filename, "_both.jpeg")
  jpegfileOnly <-  paste0(pathResult, .Platform$file.sep, filename, "_lesion.jpeg", sep = '')
  csv_Merge_lesionsFile <- paste0(pathResult, .Platform$file.sep, filename, "_Merge_lesions.csv")
  csv_All_lesionsFile <- paste0(pathResult, .Platform$file.sep, filename, "_All_lesions.csv")

  ## reading the source image
  sourceImage <- paste0(pathImages, .Platform$file.sep, fileImage, sep = '')
  image <- EBImage::readImage(sourceImage)
  widthSize = dim(image)[1]
  heightSize = dim(image)[2]

  ## prediction on the image (not blurred)
  prediction <- predict2(image,train)

  ## create patch and leaf identifiers
  patch <- as.numeric(prediction %in% lesion)
  leaf <- as.numeric(!(prediction %in% background))

  ## create leaf mask
  maskLeaf <- EBImage::channel(image, "gray")
  leafMat <- matrix(leaf, nrow=nrow(EBImage::imageData(maskLeaf)))
  EBImage::imageData(maskLeaf) <- leafMat

  ## fill empty areas
  maskLeaf <- EBImage::fillHull(maskLeaf)

  ## delete border by erosion
  ## remark: tests and computations are made with eroded objects
  brush <- EBImage::makeBrush(leafBorder, shape='disc')
  ## uncomment the following line to remove abnormal stripes from the scan
  ## brush2 <- EBImage::makeBrush(leafBorder*2+1, shape='disc') ; mask <- EBImage::dilate(mask,brush2) ; mask <- EBImage::erode(mask, brush2)
  maskLeaf <- EBImage::erode(maskLeaf, brush)

  detail <- " Step 2/6 : Extract leaves"
  writeLOGAnalysis(path = pathResult, message = message, detail = detail, mode = mode, value = 1, progress = progress, parallelThreadsNum = parallelThreadsNum)

  ## segmentation
  maskLeaf <- EBImage::bwlabel(maskLeaf)
  featuresLeaf <- data.frame(EBImage::computeFeatures.shape(maskLeaf))

  ## remove objects smaller than the minimum leaf area
  w <- which(featuresLeaf[,"s.area"] <= leafAreaMin)
  if (length(w) > 0) {
      maskLeaf[maskLeaf %in% w] <- 0
      featuresLeaf <- featuresLeaf[-w,]
  }

  if (nrow(featuresLeaf) == 0){
    logError( paste0("no leaf found for image ", leafName))
    return(data.frame(LeafName = leafName, Status = "ERROR", Message = "No leaf found for image"))
    stop(paste0("no leaf found for image ", leafName), call. = FALSE)
  }

  # renumber leaves
  featuresLeaf$leaf.number <- seq(1, nrow(featuresLeaf))
  ## DEBUG:  EBImage::display(maskLeaf, method = "raster", all = TRUE)

  ## delete background
  imageBlackBackground <- image
  imageBlackBackground[maskLeaf==0] <- 0

  ## separate the leaves
  li <- lapply(as.numeric(row.names(featuresLeaf)),extractLeaf,maskLeaf,imageBlackBackground, featuresLeaf)

  ## analyse the leaves
  detail <- " Step 3/6 : Extract lesion on leaves"
  writeLOGAnalysis(path = pathResult, message = message, detail = detail, mode = mode, value = 1, progress = progress, parallelThreadsNum = parallelThreadsNum)
  analyse.li <- lapply(li,analyseLeaf,train,lesionBorder,lesionAreaMin,lesionAreaMax,lesionEccentricityMin,lesionEccentricityMax,blurDiameter,filename)

  # print both sample and lesion images to right or bottum
  filePosition <- outPosition

  # save Analysis to RData file
  save(analyse.li,li,filePosition ,file=fileRData)

  detail <- " Step 4/6 : Generate output images"
  writeLOGAnalysis(path = pathResult, message = message, detail = detail, mode = mode, value = 1, progress = progress, parallelThreadsNum = parallelThreadsNum)
  if (outPosition == "right"){
    grDevices::jpeg(jpegfile,
         width = widthSize*2,
         height = heightSize,
         units = "px")
    graphics::par( mfrow = c(1,2) )
  }else if (outPosition == "bottum"){
    grDevices::jpeg(jpegfile,
         width = widthSize,
         height = heightSize*2,
         units = "px")
    graphics::par( mfrow = c(2,1) )
  }
  EBImage::display(image, method="raster")

  # leaf coloration if not transparent
  image <- EBImage::paintObjects(maskLeaf  ,image, thick=TRUE, col=c(leafColorBorder, leafColorBodies), opac=c(leafColorBorderAlpha, leafColorBodiesAlpha))

  ## sortie des résultats et coloration des lésions
  for (i in 1:length(li)){
    if (i == 1){
      result <- analyse.li[[i]]$outputDF
    }else{
      result <- rbind( result, analyse.li[[i]]$outputDF )
    }

    maskLesion <- analyse.li[[i]][["maskLesion"]]
    tmpimage <- image[li[[i]]$b$y, li[[i]]$b$x,]
    tmpimage <- EBImage::paintObjects(maskLesion  ,tmpimage, thick=TRUE, col=c(lesionColorBorder, lesionColorBodies), opac=c(lesionColorBorderAlpha, lesionColorBodiesAlpha))
    image[li[[i]]$b$y, li[[i]]$b$x,] <- tmpimage

  }

  EBImage::display(image, method = "raster")
  grDevices::dev.off()

  # print only output file image with lesion
  grDevices::jpeg(jpegfileOnly,
       width = widthSize,
       height = heightSize,
       units = "px")
  graphics::par( mfrow = c(1,1) )
  EBImage::display(image, method = "raster")
  grDevices::dev.off()


  detail <- " Step 5/6 : Generate ouput tables"
  writeLOGAnalysis(path = pathResult, message = message, detail = detail, mode = mode, value = 1, progress = progress, parallelThreadsNum = parallelThreadsNum)
  utils::write.table(
    result,
    file = csv_All_lesionsFile,
    quote = FALSE,
    row.names = FALSE,
    sep = '\t'
  )

  ag.count <- stats::aggregate(result$lesion.surface, result[c("image", "leaf.number", "leaf.surface")], length)
  names(ag.count)[4] <- "lesion.nb"
#  print(ag.count)

  ag.surface <- stats::aggregate(result$lesion.surface, result[c("image", "leaf.number", "leaf.surface")], sum)
  names(ag.surface)[4] <- "lesion.surface"
#  print(ag.surface)

  ag <- merge(ag.count, ag.surface)
  ag$pourcent.lesions <- ag$lesion.surface / ag$leaf.surface * 100
  ag$lesion.nb[ag$lesion.surface == 0] <- 0
  ag$lesion.surface[ag$lesion.surface == 0] <- 0
#  print(ag)

  utils::write.table(
    ag[order(ag$leaf.number),],
    file = csv_Merge_lesionsFile,
    quote = FALSE,
    row.names = FALSE,
    sep = '\t'
  )
  detail <- " Step 6/6 : Finish sample"
  writeLOGAnalysis(path = pathResult, message = message, detail = detail, mode = mode, value = 1, progress = progress, parallelThreadsNum = parallelThreadsNum)
  return(data.frame(LeafName = leafName, Status = "finish", Message = ""))
}
