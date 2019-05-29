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
library(lattice)
library(MASS)
## library(e1071) only for svm (not implemented)

table2 <- function(x,y) {
    ta <- table(x,y,dnn=c("group","predict"))
    print(ta)
    paste("\nError rate: ",round((1-sum(diag(ta))/sum(ta))*100,2),'%\n\n')
}

lastname <- function(fullname) { ## extracts directory names from full file name
    li <- strsplit(fullname,'/')
    tail(li[[1]],2)[1]
}

load.subgroup <- function(sg) { ## load images from directory sg
    files.subgroup <- list.files(sg,full.name=TRUE, pattern = "\\.jpg$|\\.jpeg$|\\.PNG$|\\.tif$", include.dirs = FALSE, ignore.case = TRUE)
    li <- lapply(files.subgroup,function(file) {
        im <- readImage(file)
        data.frame(group=lastname(file),red=as.numeric(imageData(im)[,,1]), green=as.numeric(imageData(im)[,,2]), blue=as.numeric(imageData(im)[,,3]))
    })
    do.call(rbind, li)
}

load.group <- function(g) { ## load the images of group g (which can contain subdirectories)
    ## search for sud-directories
    dirs <- list.dirs(g,recursive=FALSE)
    if (length(dirs)==0) { ## no subdirectories (only files)
        return(load.subgroup(g))
    }
    ## load images from subdirectories
    li <- lapply(dirs,load.subgroup)
    do.call(rbind,li)
}

load.class <- function(class,path.sample) { ## load all the training images
    path.class <- paste(path.sample,class,sep='/')
    if (!all(file.exists(path.class))) stop("Directory not found.")
    li <- lapply(path.class,load.group)
    do.call(rbind,li)
}

rgb2hsv2 <- function(rgb) { ## convert a data frame from rgb to hsv
    w <- match(c("red","green","blue"),names(rgb))
    hsv <- t(rgb2hsv(t(rgb[w])))
    rgb[w] <- hsv
    names(rgb)[w] <- colnames(hsv)
    rgb
}


#' Compute and saves on disk the parameters of the training set
#'
#' @param path.sample The path of the directory containing sampled images for training. This directory must contain at least 3 directories (for background, limb, and lesion images).
#' @param background The name of the directory in path.sample containing sampled images of the background. This directory can contain either image files or subdirectories containing different groups of image files.
#' @param limb The name of the directory in path.sample containing sampled images of the limb. This directory can contain either image files or subdirectories containing different groups of image files.
#' @param lesion The name of the directory in path.sample containing sampled images of lesions. This directory can contain either image files or subdirectories containing different groups of image files.
#' @param method Method of discrimainant analysis: "lda" (default) or "qda"
#' @param transform Function for data transformation before analysis (e.g. sqrt)
#' @param colormodel Model of color for the analysis: "rgb" (default) or "hsv"
#'
#' @examples
#' path.sample <- "../Exemple_Dominique/CLFD/Samples/Sup"
#'
#' training(.path.sample,"background","limb","lesion")
#' training(path.sample,"background","limb","lesion",transform=function(x) log1p(x),colormodel="rgb",method="svm")
#' training(path.sample,"background","limb","lesion",colormodel="hsv",method="lda")
#' training(path.sample,"background","limb","lesion",transform=function(x) (sin(pi*(x-0.5))+1)/2,method="qda")
#' training(path.sample,"background","limb","lesion",transform=function(x) asin(2*x-1)/pi+0.5)
#' training(path.sample,"background","limb","lesion",transform=log1p)
(training) <- function(path.sample,background,limb,lesion,method="lda",transform=NULL,colormodel="rgb") {
    version <- "6.0"
    stopifnot(method %in% c("lda","qda","svm"))
    stopifnot(colormodel %in% c("rgb","hsv"))
    ## search for subdirectories in path.sample
    dirs <- list.dirs(path.sample,recursive=FALSE,full.names=FALSE)

    ## check the existence of the subdirectories passed in argument
    class <- c(background,limb,lesion)
    li <- lapply(class,load.class,path.sample)
    groups.li <- lapply(li,function(x) unique(x$group))
    classes <- rbind(data.frame(class="background",subclass=groups.li[[1]]),data.frame(class="limb",subclass=groups.li[[2]]),data.frame(class="lesion",subclass=groups.li[[3]]))
    groups <- unlist(groups.li)
    if (any(duplicated(groups))) stop("Error: duplicated group names.")
    df2 <- do.call(rbind, li)
    if (colormodel=="hsv") df2 <- rgb2hsv2(df2)
    if (!is.null(transform)) df2[2:4] <- lapply(df2[2:4],transform)

    ## split df2 into train and test for cross-validation
    type <- rep("train",nrow(df2))
    type[sample(1:length(type),length(type)/2)] <- "test"
    df.train <- df2[type=="train",]
    df.test <- df2[type=="test",]

    ## discriminant analysis
    ## compute lda1 for graphic output (even if method is not "lda")
    lda1 <- lda(df2[2:4], df2$group, prior=rep(1,length(groups))/length(groups))
    df4 <- cbind(df2, as.data.frame(as.matrix(df2[2:4])%*%lda1$scaling))

    if (method=="lda") {
        lda2 <- lda(df.train[2:4], df.train$group, prior=rep(1,length(groups))/length(groups))
    }
    else if (method=="qda"){
        lda1 <- qda(df2[2:4], df2$group, prior=rep(1,length(groups))/length(groups))
        lda2 <- qda(df.train[2:4], df.train$group, prior=rep(1,length(groups))/length(groups))
    }
    else if (method=="svm") { ## only if library e1071 is installed
        svm1 <- svm(group~red+green+blue, data=df2, kernel ="radial", gamma =10, cost =1.8)
        svm2 <- svm(group~red+green+blue, data=df.train, kernel ="radial", gamma =10, cost =1.8)
   }

    ## common name for the 3 output files, identique to the directory name
    filename <- tail(strsplit(path.sample,'/')[[1]],1)

    ## save results in text file
    file.txt <- paste(path.sample,paste0(filename,".txt"),sep='/') ## text output file
    sink(file.txt)
    cat("Version",version,'\n')
    if (!is.null(transform)) {
        cat("transform:\n")
        print(transform)
    }
    cat("colormodel:",colormodel,'\n')
    cat("method:",method,'\n')
    print(table(df2$group))
    cat('\n')
    if (method=="lda" || method=="qda") {
        df.test$predict <- predict(lda2, df.test[2:4])$class
        print(lda1$scaling)
        train <- list(version=version,lda1=lda1,classes=classes,transform=transform,colormodel=colormodel,method=method)
    }
    else if (method=="svm") { ## only if library e1071 is installed
        df.test$predict <- predict(svm2, df.test)
        print(svm1)
        train <- list(version=version,svm11=svm1,classes=classes,transform=transform,colormodel=colormodel,method=method)
    }
    cat('\n')
    cat(table2(df.test$group, df.test$predict))
    sink()

    ## graph of the groups in axes 1 and 2 of discriminant analysis
    file.png <- paste(path.sample,paste0(filename,".png"),sep='/') ## fichier de sortie png
    png(file.png,width=480*6,height=480*6,res=72*6)
    print(xyplot(LD2~LD1, group=group, cex=0.8, alpha=1, pch=1, asp=1, auto.key=TRUE, data=df4))
    dev.off()

    ## save results
    file.train <- paste(path.sample,paste0(filename,".RData"),sep='/')
    save(train,file=file.train)
}
