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
## library(e1071) only for svm (not implemented)

# To write in log file or show progress if not in parallel mode
writeLOGAnalysis <- function(path = NULL, create = FALSE, message = NULL, detail = NULL, mode = NULL, value = NULL, progress = NULL, parallelThreadsNum = NULL) {
    # if not GUI mode write to log file
    if (!is.null(path)){
      if (create == TRUE) {
        # create log file
        logfilename <- paste0(path, "/log.txt")
        unlink(logfilename)# Clean up log file from the previous example
        ParallelLogger::clearLoggers()# Clean up the loggers from the previous example
        ParallelLogger::addDefaultFileLogger(logfilename)
      }
      ParallelLogger::logInfo(paste0(message, detail))
    if (mode == "GUI" && parallelThreadsNum == 1){
      progress$set(value = value, message = message, detail = detail)
    }
#    if (mode == "GUI" && parallelThreadsNum > 1){
#      progress$set(value = value, message = message, detail = detail)
#    }
  }
}

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

## analyse a single leaf
analyseLeaf <- function(x,train,lesionBorder,lesionAreaMin,lesionAreaMax,lesionEccentricityMin,lesionEccentricityMax,blurDiameter,filename) {

  #  print(paste("leafNum : ", x$leafNum, "    surface leaf : ",x$leaf.surface))

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
#  featuresLesionClean$leaf.surface <- rep(as.numeric(x$leaf.surface),nrow(featuresLesionClean))
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
#' Analysis step on multi-thread is able only for Linux and Mac system:
#' Please use 1 for Windows
#'
#' @param pathTraining The path of the directory containing the sampled images used for training. After the training step, this directory contains the parameters of the training set.
#' @param pathResult The path of the directory where to store the result files (created by the function if it does not exist).
#' @param pathImages The path of the directory containing the images to analyse.
#' @param fileImage A character vector containg the fils names of the images to analyse in pathImages (NA to analyse all the images in pathImages).
#' @param leafAreaMin The minimum area of a leaf (in pixels) Default:1000.
#' @param leafBorder The diameter of the brush (in pixels) used to erode the leafBorder Default:5.
#' @param lesionBorder The diameter of the brush (in pixels) used to erode the lesionBorder Default:3.
#' @param lesionAreaMin The minimum area of a lesion (in pixels) Default:10.
#' @param lesionAreaMax The maximum area of a lesion (in pixels) Default:120000.
#' @param lesionEccentricityMin The minimum eccentricity of a lesion Default:0.
#' @param lesionEccentricityMax The maximum eccentricity of a lesion Default:1.
#' @param lesionColorBorder hexadecimal code for output fill color for lesion in the output image Default:#0000FF (blue).
#' @param lesionColorBodies hexadecimal code for output bodies color for lesion in the output image Default:#FE8E0000 (transparent).
#' @param blurDiameter The diameter of the brush (in pixels) used to blur the image (0 for no blur) Default:0)'.
#' @param outPosition join origale and color lesion image at right or buttom Default:right)'.
#' @param parallelThreadsNum number of thread use, 1 thread analysis 1 image if >= 2 Default:1)'.
#' @param mode auto selection to switch between GUI or CMD mode Default:"CMD")'.
#'
#' @examples
#' analyseImages(pathTraining = "/media/sebastien/Bayer/ScriptsSEB/exemples/exemple1/learning",
#' pathResult = "/media/sebastien/Bayer/ScriptsSEB/exemples/exemple1/results",
#' pathImages = "/media/sebastien/Bayer/ScriptsSEB/exemples/exemple1/samples", parallelThreadsNum=4)
analyseImages <- function(pathTraining,pathResult,pathImages,fileImage=NA,leafAreaMin=1000,leafBorder=5,lesionBorder=3,lesionAreaMin=10,lesionAreaMax=120000,lesionEccentricityMin=0,lesionEccentricityMax=1,lesionColorBorder="#0000FF11",lesionColorBodies="#FE8E0000",blurDiameter=0, outPosition="right", parallelThreadsNum=1, mode="CMD") {

  progress <- NULL
  # auto-detect number of core on computer
  max_no_cores <- as.numeric(max(1, parallel::detectCores() - 2))

  if (!is.null(mode) && mode == "GUI" && parallelThreadsNum == 1){
      # add progress bar
      progress <- shiny::Progress$new(min=0, max=7)
      on.exit(progress$close())
    }
  writeLOGAnalysis(path = pathResult, create = TRUE, message = "Analysis run, please wait", detail = "VERSION: 1.0", mode = mode, progress = progress, parallelThreadsNum = parallelThreadsNum)

  # create config file to save input values
  paramfilename <- file(paste0(pathResult,"/LeAFtool-parameters-input.txt"))
  parameters <- paste0(
            "Training folder: ",pathTraining,"\n",
            "Samples folder: ",pathImages,"\n",
            "Output folder: ",pathResult,"\n",
            "leaf area min: ",leafAreaMin,"\n",
            "Leaf border size: ",leafBorder,"\n",
            "lesion border size: ",lesionBorder,"\n",
            "lesion area min: ", lesionAreaMin,"\n",
            "lesion area max: ", lesionAreaMax,"\n",
            "lesion eccentricity: min ",lesionEccentricityMin, "\tmax ",lesionEccentricityMax,"\n",
            "lesion color border: ", lesionColorBorder,"\n",
            "lesion color bodies: ", lesionColorBodies,"\n",
            "blur diameter: ",blurDiameter,"\n",
            "out position: ",outPosition,"\n",
            "parallelMode: ", parallelThreadsNum,"\n"
             )
    cmd <- paste0("pathTraining=",pathTraining,"pathResult="pathResult,"pathImages=",pathImages,
                "leafAreaMin=",leafAreaMin,
                "leafBorder=",leafBorder,
                "lesionBorder=",lesionBorder,
                "lesionAreaMin=",lesionAreaMin,
                "lesionAreaMax=",lesionAreaMax,
                "lesionEccentricityMin=",lesionEccentricityMin,
                "lesionEccentricityMax=",lesionEccentricityMax,
                "lesionColorBorder=",lesionColorBorder,
                "lesionColorBodies=",lesionColorBodies,
                "blurDiameter=",blurDiameter,
                "outPosition=",outPosition,
                "parallelThreadsNum=",parallelThreadsNum))
  cat(parameters, '\n', file = paramfilename)
  cat(cmd, '\n', file = paramfilename)
  close(paramfilename)

  ############################ RUN ANALYSIS
  # count number of Samples on input directory
  if (!is.na(fileImage)){
    listSamples <- fileImage
    writeLOGAnalysis(path = pathResult, message = NULL, detail = paste0("Start Analysis on image: ",fileImage), mode = mode, value = 1, progress = progress, parallelThreadsNum = parallelThreadsNum)
  }
  else{
    listSamples <-list.files(pathImages)
    writeLOGAnalysis(path = pathResult, message = NULL, detail = paste0("Start Analysis on folder: ",pathImages), mode = mode, value = 1, progress = progress, parallelThreadsNum = parallelThreadsNum)
  }

  nbSamples <- length(listSamples)
  nbSamplesAnalysis <- 1
  if (parallelThreadsNum > 1){

    # if less samples than threads or computer can, update number of threads to use only max samples
    if ( parallelThreadsNum > max_no_cores){
      parallelThreadsNum <- max_no_cores
      warning(paste("You select more use thread than computer can. auto-ajust to", max_no_cores, "threads (avail:",max_no_cores+2,")"))
    }
    if ( parallelThreadsNum > nbSamples){
      parallelThreadsNum <- nbSamples
      warning(paste("You select more use thread than samples images. auto-ajust to", nbSamples, "threads"))
    }


    # Start parallel session
    osSystem <- Sys.info()["sysname"]
    if (osSystem == "Darwin" || osSystem == "Linux") {
      cl <- parallel::makeForkCluster(parallelThreadsNum)
    }
    else if (osSystem == "Windows") {
      warning(paste("You run parallel mode but on windows you can't', so run with 1 thread"))
      parallelThreadsNum <- 1
      for (fileImage in listSamples){
        analyseImageUnique(pathTraining,pathResult,pathImages,fileImage,leafAreaMin,leafBorder,lesionBorder,lesionAreaMin,lesionAreaMax,lesionEccentricityMin, lesionEccentricityMax,lesionColorBorder,lesionColorBodies,blurDiameter,outPosition, nbSamplesAnalysis, nbSamples, mode, progress, parallelThreadsNum)
        nbSamplesAnalysis <- nbSamplesAnalysis + 1
      }
    }
    ## load libraries on workers
#    parallel::clusterEvalQ(cl, library(shiny))
#    parallel::clusterEvalQ(cl, library(EBImage))
#    parallel::clusterEvalQ(cl, library(MASS))
#    parallel::clusterEvalQ(cl, library(lattice))
#    parallel::clusterExport(cl, varlist=c(".GlobalEnv", "analyseLeaf", "analyseImageUnique", "predict2", "boundingRectangle","extractLeaf", "rangeNA", "nbSamples", "nbSamplesAnalysis", "writeLOGAnalysis", "mode", "parallelThreadsNum"), envir=environment())
    doParallel::registerDoParallel(cl)

    res <- foreach::foreach(fileImage = listSamples,
#            .export = c(".GlobalEnv"),
            .combine = c)  %dopar%
            {
              analyseImageUnique(pathTraining,pathResult,pathImages,fileImage,leafAreaMin,leafBorder,lesionBorder,lesionAreaMin,lesionAreaMax,lesionEccentricityMin, lesionEccentricityMax,lesionColorBorder,lesionColorBodies,blurDiameter,outPosition, nbSamplesAnalysis, nbSamples, mode, progress, parallelThreadsNum)
            }
  #  # Close cluster mode
    parallel::stopCluster(cl)
    closeAllConnections(); # for kill all process, use to add button for stop work
    foreach::registerDoSEQ()
    parallelThreadsNum <- 1
  }else{
    # if not cluster mode do sample by sample on one core
    for (fileImage in listSamples){
      analyseImageUnique(pathTraining,pathResult,pathImages,fileImage,leafAreaMin,leafBorder,lesionBorder,lesionAreaMin,lesionAreaMax,lesionEccentricityMin, lesionEccentricityMax,lesionColorBorder,lesionColorBodies,blurDiameter,outPosition, nbSamplesAnalysis, nbSamples, mode, progress, parallelThreadsNum)
      nbSamplesAnalysis <- nbSamplesAnalysis + 1
    }
  }

  mymergeddata = multmerge(pathResult, "*_Merge_lesions.csv")
  utils::write.table(
    mymergeddata,
    file = paste0(pathResult,"/merge_ResumeCount.csv"),
    quote = FALSE,
    row.names = FALSE,
    sep = '\t'
  )
}

analyseImageUnique <- function(pathTraining,pathResult,pathImages,fileImage,leafAreaMin,leafBorder,lesionBorder,lesionAreaMin,lesionAreaMax,lesionEccentricityMin,lesionEccentricityMax,lesionColorBorder,lesionColorBodies,blurDiameter,outPosition, nbSamplesAnalysis, nbSamples, mode, progress, parallelThreadsNum) {

  # check params:
  lesionColorBorderAlpha <-grDevices::col2rgb(lesionColorBorder, alpha=TRUE)[4]/255
  lesionColorBodiesAlpha <-grDevices::col2rgb(lesionColorBodies, alpha=TRUE)[4]/255

  if (!file.exists(pathResult)) dir.create(pathResult)

  message <- paste("Analysis leaf ",nbSamplesAnalysis,"/",nbSamples,": ", fileImage, "    ")
  detail <- " Step 1/6 : Reading image sample and apply calibration"
  writeLOGAnalysis(path = pathResult, message = message, detail = detail, mode = mode, value = 1, progress = progress, parallelThreadsNum = parallelThreadsNum)

  ## load trainig results
  basename <- utils::tail(strsplit(pathTraining, '/')[[1]], 1)
  file.train <- paste(pathTraining,'/',basename,".RData",sep='')
  load(file.train)

  # LOAD class
  background <- train$classes$subclass[train$classes$class=="background"]
  limb <- train$classes$subclass[train$classes$class=="limb"]
  lesion <- train$classes$subclass[train$classes$class=="lesion"]

  # build filename output
  filename <- strsplit(fileImage, ".", fixed = TRUE)[[1]][1]
  fileRData <- paste0(pathResult, '/.', filename, ".RData", sep = '')
  jpegfile <- paste0(pathResult, '/', filename, "_both.jpeg", sep = '')
  jpegfileOnly <- paste0(pathResult, '/', filename, "_lesion.jpeg", sep = '')
  csv_Merge_lesionsFile <- paste0(pathResult, '/', filename, "_Merge_lesions.csv", sep = '')
  csv_All_lesionsFile <- paste0(pathResult, '/', filename, "_All_lesions.csv", sep = '')

  ## reading the source image
  sourceImage <- paste0(pathImages, '/', fileImage, sep = '')
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
}
