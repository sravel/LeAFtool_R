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

## packages nécessaires
library(EBImage)
library(MASS)
#library(parallel)
library(foreach)

library(future)

library(doSNOW)
# Calculate the number of cores
# no_cores <- max(1, detectCores() - 1)
no_cores <- 8

## retourne les indices extrêmes de la valeur objet dans le vecteur x
## (appelé par bounding.rectangle)
range.na <- function(x, object) {
  w <- which(x == object)
  if (length(w) == 0)
    return(c(NA, NA))
  return(range(w))
}

bounding.rectangle <- function(mask, object) {
  m <- imageData(mask)
  range.x <- range(apply(m, 1, range.na, object), na.rm = TRUE)
  range.y <- range(apply(m, 2, range.na, object), na.rm = TRUE)
  list(x = range.x[1]:range.x[2], y = range.y[1]:range.y[2])
}

extrait.leaf <- function(i, mask, image.fond.noir) {
  b <- bounding.rectangle(mask, i)
  leaf <- image.fond.noir[b$y, b$x,]
  mask.leaf <- mask[b$y, b$x]
  leaf[mask.leaf != i] <- 0
  list(b = b, leaf = leaf)
}

## analyse d'une leaf
analyse.leaf <- function(x, lda1, lesion) {
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

  ## dilatation
  brush <- makeBrush(lesionBorderSize, shape = 'disc')
  ##    mask <- dilateGreyScale(mask, brush)
  mask <- dilate(mask, brush)

  ## remplissage vides
  mask <- fillHull(mask)

  ## erosion
  mask <- erode(mask, brush)

  ## segmentation
  mask[mask < 0] <- 0
  mask <- bwlabel(mask)
  features <- computeFeatures.shape(mask)

  ## suppression des petits objets
  w.petit <- which(features[, "s.area"] < lesionMinSize)
  mask <- rmObjects(mask, w.petit)

  features <- computeFeatures.shape(mask)
  list(features = features, mask = mask)
}

analyseFiles <- function(fileRdata = NA, pathResult, pathImages, onefileImage = NA, leafMinSize, leafBorderSize, lesionBorderSize, lesionMinSize, colorLesion) {
  ## chargement du résultat de l'analyse discriminante
  if (!is.na(fileRdata))
    load(fileRdata)

  # DEBUG print(list(fileRdata,pathResult,pathImages,onefileImage,leafMinSize,leafBorderSize,lesionBorderSize,lesionMinSize,colorLesion))

  #ASSIGNE TO GLOBAL ENV
  fileRdata <<- fileRdata
  pathResult <<- pathResult
  pathImages <<- pathImages
  onefileImage <<- onefileImage
  leafMinSize <<- leafMinSize
  leafBorderSize <<- leafBorderSize
  lesionBorderSize <<- lesionBorderSize
  lesionMinSize <<- lesionMinSize
  colorLesion <<- colorLesion

  if (is.na(onefileImage)){
    onefileImage <- list.files(pathImages)
    # DEBUG print(onefileImage)
  }

  nbfiles <- length(onefileImage)
  c <- 1

  listFiles <<-as.list(unlist(onefileImage, recursive = FALSE, use.names = FALSE))

  plan(multiprocess, .cleanup = TRUE)
  cl <<- makeCluster(no_cores, outfile = logfilename)
  # registerDoParallel(cl, cores = no_cores)
  registerDoSNOW(cl)


  show("loading-content")

  progress <- shiny::Progress$new()
  on.exit(progress$close())
  progress$set(message = 'Making Analysis, please wait\n', value = 0)
  progress$inc(c/nbfiles, detail = paste("start parallel analysis with ", no_cores, " cores"))

  # upadateProgress <- function(c) progress$inc(c/nbfiles, detail = paste("Image", c))

  clusterExport(cl, c("nbfiles")) # Export max number of iteration to workers

  opts <- list(progress=upadateProgress)

  foreach(imageFile = listFiles,
          .options.snow=opts,
          .combine = c)  %dopar%
          {
            tryCatch({
              # incProgress(c / nbfiles, detail = paste("analysis leaf ", c, "/", nbfiles))
              analyseUniqueFile(pathResult, pathImages, imageFile, leafMinSize, leafBorderSize, lesionBorderSize, lesionMinSize, colorLesion)
              gc()
            }, error = function(e){return(paste0("The file '", image, "'"," caused the error: '", e, "'"))},
            warning = function(e){return(paste0("The file '", image, "'"," caused warning: '", e, "'"))}
            # finally = {
            # print(image)
            # addLog(image)
            # closeAllConnections();
            # stopCluster(cl)
            # registerDoSEQ()
            # }
            )

          }

  # parSapply(cl, X = listFiles, function(image) {
  #   analyseUniqueFile(pathResult,pathImages,image,leafMinSize,leafBorderSize,lesionBorderSize,lesionMinSize,colorLesion)
  #   incProgress(c / nbfiles, detail = paste("analysis leaf ", c, "/", nbfiles))
  #   c <- c + 1
  #   gc()
  # })
  # print('after foreach')
  # showConnections(all = TRUE)
  # print('after show')
  closeAllConnections();

  hide(id = "loading-content", anim = TRUE, animType = "fade")


  # print('after close')
  # for (image in onefileImage){
  # #   incProgress(c/nbfiles, detail = paste("analysis leaf ", c, "/", nbfiles))
  #   analyseUniqueFile(pathResult,pathImages,image,leafMinSize,leafBorderSize,lesionBorderSize,lesionMinSize,colorLesion)
  #   break
  # }
  # stopCluster(cl)
  # registerDoSEQ()
  # print('after stop')

  tmpCmd  <- paste0("awk '{if($1 == \"fichier\"){} else{ print $0}}'  ", pathResult, "/*_2.txt | sort -k1 |sort -k1,1 -k2n,2n > ",pathResult,"/merge_output_2.txt")
  return  <- system(tmpCmd, intern = TRUE)


  #awk '{if($1 == "fichier"){} else{ print $0}}'  *_2.txt | sort -k1 |sort -k1,1 -k2n,2n > output
  return(1)
}

analyseUniqueFile <- function(pathResult, pathImages, imageFile, leafMinSize, leafBorderSize, lesionBorderSize, lesionMinSize, colorLesion) {

  background <- names(lda1$prior)[1]
  limb <- names(lda1$prior)[2]
  lesion <- names(lda1$prior)[3]

  ## lecture de l'image source
  source.image <- paste(pathImages, '/', imageFile, sep = '')
  image <- readImage(source.image)
  widthSize = dim(image)[1]
  heightSize = dim(image)[2]

  ## prédiction sur l'image (non floutée)
  df5 <-
    data.frame(
      red = as.numeric(imageData(image)[, , 1]),
      green = as.numeric(imageData(image)[, , 2]),
      blue = as.numeric(imageData(image)[, , 3])
    )
  df5$predict <- predict(lda1, df5)$class

  ## création des identificateurs des taches et des leafs
  df5$tache <- as.numeric(df5$predict == lesion)
  df5$leaf <- as.numeric(df5$predict != background)

  ## masque des leafs
  mask <- channel(image, "gray")
  leaf <- matrix(df5$leaf, nrow = nrow(imageData(mask)))
  imageData(mask) <- leaf

  ## suppression des vides
  mask <- fillHull(mask)

  ## suppression de la bordure par érosion
  ## remarque : les tests et les calculs sont effectués sur les objets érodés
  brush <- makeBrush(leafBorderSize,  shape = 'disc')
  ## ligne suivante ajoutée pour supprimer les lignes parasites dues au scan (supprimer pour scan normal)
  ## brush2 <- makeBrush(leafBorderSize*2+1,  shape = 'disc') ; mask <- dilate(mask, brush2) ; mask <- erode(mask,  brush2)
  mask <- erode(mask,  brush)

  ## segmentation
  mask <- bwlabel(mask)
  features <- data.frame(computeFeatures.shape(mask))

  ## suppression des objets plus petits que la surface minimum d'une leaf
  w <- which(features[, "s.area"]<leafMinSize)
  if (length(w) > 0) {
    mask[mask %in% w] <- 0
    features <- features[-w, ]
  }

  ## suppression du fond
  image.fond.noir <- image
  image.fond.noir[mask == 0] <- 0

  ## séparation des leafs
  li <- lapply(as.numeric(row.names(features)), extrait.leaf, mask, image.fond.noir)

  ## analyse des leafs
  analyse.li <- lapply(li,  analyse.leaf,  lda1,  lesion)

  filename <- strsplit(imageFile, ".", fixed = TRUE)[[1]][1]
  jpegname <- paste(filename, "_both.jpeg", sep = '')
  jpegnameOnly <- paste(filename, "_lesion.jpeg", sep = '')
  txtname1 <- paste(filename, "_1.txt", sep = '')
  txtname2 <- paste(filename, "_2.txt", sep = '')
  if (!file.exists(pathResult))
    dir.create(pathResult)
  jpegfile <- paste(pathResult, '/', jpegname, sep = '')
  jpegfileOnly <- paste(pathResult, '/', jpegnameOnly, sep = '')
  txtfile1 <- paste(pathResult, '/', txtname1, sep = '')
  txtfile2 <- paste(pathResult, '/', txtname2, sep = '')

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
    tmpimage <- image[li[[i]]$b$y, li[[i]]$b$x,]
    tmpimage[analyse.li[[i]]$mask > 0] <- colorLesion
    image[li[[i]]$b$y, li[[i]]$b$x,] <- tmpimage
  }
  row.names(result) <- NULL
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
  print(paste0("DEBUG6"))
  write.table(
    ag[order(ag$leaf),],
    file = txtfile2,
    quote = FALSE,
    row.names = FALSE,
    sep = '\t'
  )
  print(paste0("FINISH: ",source.image,"\n",jpegfileOnly))
  rm(list = ls())
  gc()

  return(1)
}
