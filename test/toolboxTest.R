library(EBImage)
library(tools)

is.wholenumber <- function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol


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

splitImage <- function(path, splitVertical, splitHorizontal, numOrder = "bottum", mode = "CMD"){
  if (!file.exists(path)) stop(paste0("Directory '",path,"' not found."))
  # numOrder
  if (!(numOrder %in% c("right", "bottum"))){
    stop(paste("'",numOrder,"' is not valid value for numOrder, please only use 'right' or 'bottum'"), call. = FALSE)
  }
  checkValue(splitVertical,"splitVertical")
  checkValue(splitHorizontal,"splitHorizontal")
  basename(path)
  newpath <- file.path(path,paste0(basename(path),"_split"))
  if (!file.exists(newpath)) dir.create(newpath)
  
  files <- list.files(path, full.names = FALSE, include.dirs = FALSE, pattern = "\\.jpg$|\\.jpeg$|\\.PNG$|\\.tif$", ignore.case = TRUE)
  nbSamples <- length(files)
  print(nbSamples)
  if (mode == "CMD") {pb <- txtProgressBar(min = 0, max = nbSamples, initial = 0, style = 3)}
  if (mode == "GUI")
  {
    progressTOOLBOX <- shiny::Progress$new(min = 0, max = nbSamples)
    progressTOOLBOX$set(message = 'Resize in progress', detail = paste0('start on ',nbSamples,'files'), value = 0)
    on.exit(progressTOOLBOX$close())
  }
  nb <- 1
  for (file in files) {
    image <- EBImage::readImage(file.path(path,file))
    name <- tools::file_path_sans_ext(file)
    ext <-  tools::file_ext(file)
    widthSize = dim(image)[1]
    heightSize = dim(image)[2]
    sizePartVertical <- widthSize/splitVertical
    startVerticalx <- 0
    startVerticaly <- sizePartVertical
    sizePartHorizontal <- heightSize/splitHorizontal
    startHorizontalx <- 0
    startHorizontaly <- sizePartHorizontal
    count <- 1
    if (numOrder == "right"){
      for (i in 1:splitHorizontal){
        for (j in 1:splitVertical){
          newfile <- base::file.path(newpath,paste0(name,'_',count,'.',ext))
          EBImage::writeImage(image[startVerticalx:startVerticaly, startHorizontalx:startHorizontaly,],newfile)
          startVerticalx <- startVerticaly
          startVerticaly <- startVerticaly + sizePartVertical
          count <- count + 1
        }
        startVerticalx <- 0
        startVerticaly <- sizePartVertical
        startHorizontalx <- startHorizontaly
        startHorizontaly <- startHorizontaly + sizePartHorizontal
      }
    }
    
    else if (numOrder == "bottum"){
      for (i in 1:splitVertical){
        for (j in 1:splitHorizontal){
          newfile <- base::file.path(newpath,paste0(name,'_',count,'.',ext))
          EBImage::writeImage(image[startVerticalx:startVerticaly, startHorizontalx:startHorizontaly,],newfile)
          startHorizontalx <- startHorizontaly
          startHorizontaly <- startHorizontaly + sizePartHorizontal
          count <- count + 1
        }
        startHorizontalx <- 0
        startHorizontaly <- sizePartHorizontal
        startVerticalx <- startVerticaly
        startVerticaly <- startVerticaly + sizePartVertical
      }
    }
    nb <- nb + 1
    if (mode == "CMD")  setTxtProgressBar(pb, nb)
    if (mode == "GUI")  progressTOOLBOX$set(value = nb, detail = paste0("Number of image resize: ",nb,"/",nbSamples))
  }
  if (mode == "CMD") close(pb)
  if (mode == "GUI") progressTOOLBOX$close()
  return(newpath)
}

#splitImage(path = "/media/sebastien/Bayer/ScriptsSEB/exemples/musaBrut/", splitVertical = 2, splitHorizontal = 3)

splitImage(path = "/media/sebastien/Bayer/ScriptsSEB/exemples/musaBrut/", splitVertical = 2, splitHorizontal = 1)



