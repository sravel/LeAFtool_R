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
library(tools)

#' Resize all images on directory in order to reduce size
#'
#' The function build new folder with images resize.
#'
#' @param path The path of the directory containing the sampled images to resize.
#' @param factor The factor of reduce size Default: 2.
#'
#' @examples
#' pathImages <- '../exemple1/samples'
#' resizeImageDirectory(pathImages, 1.5)
resizeImageDirectory <- function(path,factor=2, mode="CMD") {
  if (!file.exists(path)) stop(paste0("Directory '",path,"' not found."), call. = FALSE)
  factor2 <- paste0('_',gsub('\\.','_',factor)) ## to remove '.' if factor is not odd
  newpath <- paste0(path,factor2)
  if (!file.exists(newpath)) dir.create(newpath)

  files <- list.files(path, full.names = FALSE, include.dirs = FALSE, pattern = "\\.jpg$|\\.jpeg$|\\.PNG$|\\.tif$", ignore.case = TRUE)
  nbSamples <- length(files)
  if (mode == "CMD") {pb <- txtProgressBar(min = 0, max = nbSamples, style = 3)}
  if (mode == "GUI")
  {
    progressTOOLBOX <- shiny::Progress$new(min = 0, max = nbSamples)
    progressTOOLBOX$set(message = 'Resize in progress', detail = paste0('start on ',nbSamples,'files'), value = 0)
    on.exit(progressTOOLBOX$close())
  }

  i <- 1
  for (file in files) {
    name <- tools::file_path_sans_ext(file)
    ext <-  tools::file_ext(file)
    image <- EBImage::readImage(file.path(path,file))
    newfile <- base::file.path(newpath,paste0(name,factor2,'.',ext))
    #print(paste0("resize file:",file," to ",newfile, " with factor: ",factor))
    EBImage::writeImage(resize(image,dim(image)[1]/factor),newfile)
    i <- i + 1
    if (mode == "CMD")  setTxtProgressBar(pb, i)
    if (mode == "GUI")  progressTOOLBOX$set(value = i, detail = paste0("Number of image resize: ",i,"/",nbSamples))
  }
  if (mode == "CMD") close(pb)
  if (mode == "GUI") progressTOOLBOX$close()
  return(newpath)
}


#' Split all images on directory
#'
#' The function split image on n horizontal / m vertical.
#' For exemple if you want to split on 2 part
#'
#' @param path The path of the directory containing the sampled images to split.
#' @param splitVertical The number of part split vertical Default: 1.
#' @param splitHorizontal The number of part split horizontal Default: 1.
#' @param numOrder The order to numerote output image left from right or top to buttom. Default:bottum.
#' @param marginTop The crop margin top. Default:0
#' @param marginRight The crop margin right. Default:0
#' @param marginBottom The crop margin bottom. Default:0
#' @param marginLeft The crop margin left. Default:0
#'
#' @examples
#' pathImages <- '../exemple1/samples'
#' splitImages(pathImages, splitVertical = 2, splitHorizontal = 3, marginTop = 10, marginRight = 300, marginBottom = 300, marginLeft = 170,) # split on 6 part (2x3)
splitImages <- function(path, splitVertical = 1, splitHorizontal = 1, marginTop=0, marginRight=0, marginBottom=0, marginLeft=0, numOrder = "bottum", mode = "CMD"){
  if (!file.exists(path)) stop(paste0("Directory '",path,"' not found."), call. = FALSE)
  if (!(numOrder %in% c("right", "bottum"))){
    stop(paste("'",numOrder,"' is not valid value for numOrder, please only use 'right' or 'bottum'"), call. = FALSE)
  }
  checkValue(splitVertical,"splitVertical")
  checkValue(splitHorizontal,"splitHorizontal")
  newpath <- file.path(path,paste0(basename(path),"_split"))
  if (!file.exists(newpath)) dir.create(newpath)

  files <- list.files(path, full.names = FALSE, include.dirs = FALSE, pattern = "\\.jpg$|\\.jpeg$|\\.PNG$|\\.tif$", ignore.case = TRUE)
  nbSamples <- length(files)
  if (mode == "CMD") {pb <- txtProgressBar(min = 0, max = nbSamples, initial = 0, style = 3)}
  if (mode == "GUI")
  {
    progressTOOLBOX <- shiny::Progress$new(min = 0, max = nbSamples)
    progressTOOLBOX$set(message = 'Split in progress', detail = paste0('start on ',nbSamples,' files'), value = 0)
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
          EBImage::writeImage(image[(startVerticalx+marginLeft):(startVerticaly-marginRight), (startHorizontalx+marginTop):(startHorizontaly-marginBottom),],newfile)
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
          EBImage::writeImage(image[(startVerticalx+marginLeft):(startVerticaly-marginRight), (startHorizontalx+marginTop):(startHorizontaly-marginBottom),],newfile)
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
    if (mode == "GUI")  progressTOOLBOX$set(value = nb, detail = paste0("Number of image split: ",nb,"/",nbSamples))
  }
  if (mode == "CMD") close(pb)
  if (mode == "GUI") progressTOOLBOX$close()
  return(newpath)
}
