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

## returns the range of indexes of objet in vector x
## (called by bounding.rectangle)
range.na <- function(x, object) {
    w <- which(x==object)
    if (length(w)==0) return(c(NA,NA))
    return(range(w))
}

bounding.rectangle <- function(mask, object) {
    m <- imageData(mask)
    range.x <- range(apply(m,1,range.na,object),na.rm=TRUE)
    range.y <- range(apply(m,2,range.na,object),na.rm=TRUE)
    list(x=range.x[1]:range.x[2],y=range.y[1]:range.y[2])
}

extract.leaf <- function(i,mask,image.black.background) {
    b <- bounding.rectangle(mask,i)
    leaf <- image.black.background[b$y,b$x,]
    mask.leaf <- mask[b$y,b$x]
    leaf[mask.leaf!=i] <- 0
    list(b=b,leaf=leaf)
}

predict2 <- function(image,train) { ## returns predicted values according to train
    df <- data.frame(red=as.numeric(imageData(image)[,,1]), green=as.numeric(imageData(image)[,,2]), blue=as.numeric(imageData(image)[,,3]))
    if (train$colormodel=="hsv") df <- rgb2hsv2(df)
    if (!is.null(train$transform)) df[1:3] <- lapply(df,train$transform)
    if (train$method=="lda" || train$method=="qda")
        return(predict(train$lda1,df)$class)
    else if (train$method=="svm")
        return(predict(train$svm,df))
}

## analyse a single leaf
analyse.leaf <- function(x,train,lesion.border,lesion.area.min,lesion.area.max,lesion.eccentricity.max,blur.diameter) {
    ## remplace background with limb
    f <- x$leaf
    limb <- train$classes$subclass[train$classes$class=="limb"]
    lesion <- train$classes$subclass[train$classes$class=="lesion"]
    df6 <- data.frame(red=as.numeric(imageData(f)[,,1]), green=as.numeric(imageData(f)[,,2]), blue=as.numeric(imageData(f)[,,3]))
    black.background <- df6$red+df6$green+df6$blue==0
    prediction <- predict2(f,train)
    df.limb <- df6[prediction %in% limb & !black.background,]
    mean.limb <- apply(df.limb[1:3],2,mean)
    df6[black.background,1] <- mean.limb[1]
    df6[black.background,2] <- mean.limb[2]
    df6[black.background,3] <- mean.limb[3]
    imageData(f)[,,1] <- df6$red
    imageData(f)[,,2] <- df6$green
    imageData(f)[,,3] <- df6$blue

    ## analyse
    if (blur.diameter>1) { ## blurring
        ##x11() ; display(f,method="raster")
        flo <- makeBrush(blur.diameter, shape='disc', step=FALSE)^2
        flo <- flo/sum(flo)
        f <- filter2(f, flo)
        f[f<0] <- 0
        ##x11() ; display(f,method="raster")
    }
    prediction <- predict2(f, train)
    patch <- as.numeric(prediction %in% lesion)
    mask <- channel(f, "gray")
    patch.mat <- matrix(patch, nrow=nrow(imageData(mask)))
    imageData(mask) <- patch.mat

    ## dilate
    brush <- makeBrush(lesion.border, shape='disc')
    mask <- dilate(mask, brush)

    ## fill empty areas
    mask <- fillHull(mask)

    ## erode
    mask <- erode(mask, brush)

    ## segmentation
    mask[mask<0] <- 0
    mask <- bwlabel(mask)
    features <- computeFeatures.shape(mask)
    moments <- computeFeatures.moment(mask)

    ## remove small objects
    w.small <- w.large <- w.eccentric <- NULL
    w.small <- which(features[,"s.area"]<lesion.area.min)
    w.large <- which(features[,"s.area"]>lesion.area.max)
    w.eccentric <- which(moments[,"m.eccentricity"]>lesion.eccentricity.max)
    mask[mask %in% c(w.small,w.large,w.eccentric)] <- 0

    features <- computeFeatures.shape(mask)
    list(features=features, mask=mask)
}

#' Analyse an image or a set of images
#'
#' @param path.sample The path of the directory containing the sampled images used for training. After the training step, this directory contains the parameters of the training set.
#' @param path.result The path of the directory where to store the result files (created by the function if it does not exist).
#' @param path.image The path of the directory containing the images to analyse.
#' @param file.image A character vector containg the fils names of the images to analyse in path.image (NA to analyse all the images in path.image).
#' @param leaf.area.min The minimum area of a leaf (in pixels).
#' @param leaf.border The diameter of the brush (in pixels) used to erode the leaf border.
#' @param lesion.border The diameter of the brush (in pixels) used to erode the lesion border.
#' @param lesion.area.min The minimum area of a lesion (in pixels).
#' @param lesion.area.max The maximum area of a lesion (in pixels).
#' @param lesion.eccentricity.max The maximum eccentricity of a lesion.
#' @param lesion.color Lesion color in the output image.
#' @param blur.diameter The diameter of the brush (in pixels) used to blur the image (0 for no blur).
analyse.image <- function(path.sample,path.result,path.image,file.image=NA,leaf.area.min,leaf.border,lesion.border,lesion.area.min,lesion.area.max,lesion.eccentricity.max,lesion.color,blur.diameter) {
    if (is.na(file.image)) file.image <- list.files(path.image)
    for (image in file.image)
        analyse.image.unique(path.sample,path.result,path.image,image,leaf.area.min,leaf.border,lesion.border,lesion.area.min,lesion.area.max,lesion.eccentricity.max,lesion.color,blur.diameter)
}

analyse.image.unique <- function(path.sample,path.result,path.image,file.image,leaf.area.min,leaf.border,lesion.border,lesion.area.min,lesion.area.max,lesion.eccentricity.max,lesion.color,blur.diameter) {
    ## load trainig results
    filename <- tail(strsplit(path.sample,'/')[[1]],1)
    file.train <- paste(path.sample,'/',filename,".RData",sep='')
    load(file.train)

    background <- train$classes$subclass[train$classes$class=="background"]
    limb <- train$classes$subclass[train$classes$class=="limb"]
    lesion <- train$classes$subclass[train$classes$class=="lesion"]

    ## load source image
    source.image <- paste(path.image,'/',file.image,sep='')
    image <- readImage(source.image)

    prediction <- predict2(image,train)

    ## create patch and leaf identifiers
    patch <- as.numeric(prediction %in% lesion)
    leaf <- as.numeric(!(prediction %in% background))

    ## create leaf mask
    mask <- channel(image, "gray")
    leaf.mat <- matrix(leaf, nrow=nrow(imageData(mask)))
    imageData(mask) <- leaf.mat

    ## fill empty areas
    mask <- fillHull(mask)

    ## delete border by erosion
    ## remark: tests and computations are made with eroded objects
    brush <- makeBrush(leaf.border, shape='disc')
    ## uncomment the following line to remove abnormal stripes from the scan
    ## brush2 <- makeBrush(leaf.border*2+1, shape='disc') ; mask <- dilate(mask,brush2) ; mask <- erode(mask, brush2)
    mask <- erode(mask, brush)

    ## segmentation
    mask <- bwlabel(mask)
    features <- data.frame(computeFeatures.shape(mask))

    ## remove objects smaller than the minimum leaf area
    w <- which(features[,"s.area"]<leaf.area.min)
    if (length(w) > 0) {
        mask[mask %in% w] <- 0
        features <- features[-w,]
    }

    ## delete background
    image.black.background <- image
    image.black.background[mask==0] <- 0

    ## separate the leaaves
    li <- lapply(as.numeric(row.names(features)),extract.leaf,mask,image.black.background)

    ## analyse the leaves
    analyse.li <- lapply(li,analyse.leaf,train,lesion.border,lesion.area.min,lesion.area.max,lesion.eccentricity.max,blur.diameter)

    ## create output file names
    filename <- strsplit(file.image,'\\.')[[1]][1]
    pdfname <- paste(filename,".pdf",sep='')
    txtname1 <- paste(filename,"_1.txt",sep='')
    txtname2 <- paste(filename,"_2.txt",sep='')
    if (!file.exists(path.result)) dir.create(path.result)
    pdffile <- paste(path.result,'/',pdfname,sep='')
    txtfile1 <- paste(path.result,'/',txtname1,sep='')
    txtfile2 <- paste(path.result,'/',txtname2,sep='')

    ## write results
    pdf(pdffile)
    display(image, method="raster", all=TRUE)

    result <- NULL
    for (i in 1:length(li)) {
        result <- rbind(result,data.frame(file=filename,leaf=i,leaf.area=features[i,"s.area"],lesion.area=if (is.null(analyse.li[[i]]$features)) 0 else analyse.li[[i]]$features[,"s.area"]))
        tmpimage <- image[li[[i]]$b$y,li[[i]]$b$x,]
        tmpimage[analyse.li[[i]]$mask>0] <- lesion.color
        image[li[[i]]$b$y,li[[i]]$b$x,] <- tmpimage
    }
    row.names(result) <- NULL

    display(image, method="raster", all=TRUE)
    dev.off()

    write.table(result,file=txtfile1,quote=FALSE,row.names=FALSE,sep='\t')

    ag.count <- aggregate(result$lesion.area,result[c("file","leaf", "leaf.area")],length)
    names(ag.count)[4] <- "lesion.count"
    ag.area <- aggregate(result$lesion.area,result[c("file","leaf","leaf.area")],sum)
    names(ag.area)[4] <- "lesion.area"
    ag <- merge(ag.count,ag.area)
    ag$percent.lesion <- ag$lesion.area/ag$leaf.area*100
    ag$lesion.count[ag$lesion.area==0] <- 0

    write.table(ag[order(ag$leaf),],file=txtfile2,quote=FALSE,row.names=FALSE,sep='\t')
}
