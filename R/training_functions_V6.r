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

## library(e1071) only for svm (not implemented)

table2 <- function(x,y) {
    ta <- table(x,y,dnn=c("group","predict"))
    print(ta)
    paste("\nError rate: ",round((1-sum(diag(ta))/sum(ta))*100,2),'%\n\n')
}

load_group <- function(g, path.training) {
  path_group <- paste(path.training,g,sep='/')
  files_group <- list.files(path_group, full.name=TRUE, pattern = "\\.jpg$|\\.jpeg$|\\.PNG$|\\.tif$", include.dirs = FALSE, ignore.case = TRUE)
  sample <- lapply(files_group,readImage)
  ## creation of the data frame of the sampled pixels
  li <- lapply(sample, function(im) {
    data.frame(group=g,red=as.numeric(imageData(im)[,,1]), green=as.numeric(imageData(im)[,,2]), blue=as.numeric(imageData(im)[,,3]))
  })
  do.call(rbind, li)
}

rgb2hsv2 <- function(rgb) { ## convert a data frame from rgb to hsv
    w <- match(c("red","green","blue"),names(rgb))
    hsv <- t(rgb2hsv(t(rgb[w])))
    rgb[w] <- hsv
    names(rgb)[w] <- colnames(hsv)
    rgb
}

# function to test if directory pass contain sub-directory limb, background lesion
existDirTraining <- function(dirTraining){
  list(
    dirlimb = dir.exists(paste(dirTraining,"/limb", sep = .Platform$file.sep)),
    dirBackground = dir.exists(paste(dirTraining,"/background", sep = .Platform$file.sep)),
    dirLesion = dir.exists(paste(dirTraining,"/lesion", sep = .Platform$file.sep))
  )
}

#' Compute and saves on disk the parameters of the training set
#'
#' @param path.training The path of the directory containing sampled images for training. This directory must contain at least 3 directories (for background, limb, and lesion images).
#' @param background The name of the directory in path.training containing sampled images of the background. This directory can contain either image files or subdirectories containing different groups of image files.
#' @param limb The name of the directory in path.training containing sampled images of the limb. This directory can contain either image files or subdirectories containing different groups of image files.
#' @param lesion The name of the directory in path.training containing sampled images of lesions. This directory can contain either image files or subdirectories containing different groups of image files.
#' @param method Method of discrimainant analysis: "lda" (default) or "qda"
#' @param transform Function for data transformation before analysis (e.g. sqrt)
#' @param colormodel Model of color for the analysis: "rgb" (default) or "hsv"
#'
#' @examples
#' path.training <- "../Exemple_Dominique/CLFD/Samples/Sup"
#'
#' training(.path.training,"background","limb","lesion")
#' training(path.training,"background","limb","lesion",transform=function(x) log1p(x),colormodel="rgb",method="svm")
#' training(path.training,"background","limb","lesion",colormodel="hsv",method="lda")
#' training(path.training,"background","limb","lesion",transform=function(x) (sin(pi*(x-0.5))+1)/2,method="qda")
#' training(path.training,"background","limb","lesion",transform=function(x) asin(2*x-1)/pi+0.5)
#' training(path.training,"background","limb","lesion",transform=log1p)
training <- function(path.training,background,limb,lesion,method="lda",transform=NULL,colormodel="rgb") {
    version <- "6.0"
    stopifnot(method %in% c("lda","qda","svm"))
    stopifnot(colormodel %in% c("rgb","hsv"))
    listdirTraining <- existDirTraining(path.training)
    # if all subfolder exist run analysis
    if(listdirTraining$dirlimb == FALSE || listdirTraining$dirBackground == FALSE || listdirTraining$dirLesion == FALSE){
        errorMess <-paste("Error not find all sub-folders !!!!:"
                     paste0("limb: ", listdirTraining$dirlimb),
                     paste0("background: ", listdirTraining$dirBackground),
                    paste0("lesion: ", listdirTraining$dirLesion),
                    sep="\n")
        stop(errorMess)
    }


    ## search for subdirectories in path.training
    dirs <- list.dirs(path.training,recursive=FALSE,full.names=FALSE)

    ## check the existence of the subdirectories passed in argument
    limbDir <- list.dirs(paste0(path.training,"/limb"),full.names=FALSE)[-1]
    if (length(limbDir)==0){limbDir = "limb"}
    else { limbDir <- paste0("limb/",limbDir)}

    lesionDir <- list.dirs(paste0(path.training,"/lesion"),full.names=FALSE)[-1]
    if (length(lesionDir)==0){lesionDir = "lesion"}
    else { lesionDir <- paste0("lesion/",lesionDir)}

    backgroundDir <- list.dirs(paste0(path.training,"/background"),full.names=FALSE)[-1]
    if (length(backgroundDir)==0){backgroundDir = "background"}
    else { backgroundDir <- paste0("background/",backgroundDir)}

    groups <- c(backgroundDir,limbDir,lesionDir)
    if (any(duplicated(groups))) stop("Error: duplicated group names.")
    nbGroups <- length(groups)
    classes <- rbind(data.frame(class="background",subclass=backgroundDir),data.frame(class="limb",subclass=limbDir),data.frame(class="lesion",subclass=lesionDir))

    ## constitution of the data.frame of the pixels of the samples
    li <- lapply(groups,load_group, path.training = path.training)
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
    basename <- tail(strsplit(path.training,'/')[[1]],1)

    ## save results in text file
    file.txt <- paste(path.training,paste0(basename,".txt"),sep='/') ## text output file
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

    df4 <- data.frame(df4, classes=do.call(rbind, strsplit(as.character(df4$group),'/',1))) ## add classes column
    print(head(df4))

    ## graph of groups in the discriminant plane
    plotFileTraining1_2 <- paste(path.training,paste0(basename,"1_2.jpeg"),sep='/') ## output file jpeg
    plotFileTraining1_3 <- paste(path.training,paste0(basename,"1_3.jpeg"),sep='/') ## output file jpeg
    plotFileTraining2_3 <- paste(path.training,paste0(basename,"2_3.jpeg"),sep='/') ## output file jpeg

    # Palette color for graph
    colBackPalette <- c("#0000FF","#74D0F1","#26C4EC","#0F9DE8","#1560BD","#0095B6","#00CCCB","#1034A6","#0ABAB5","#1E7FCB")
    colLimbPalette <- c("#32CD32","#9ACD32","#00FA9A","#008000","#ADFF2F","#6B8E23","#3CB371","#006400","#2E8B57","#00FF00")
    colLesionPalette <- c("#FF0000","#DB0073","#91283B","#B82010","#FF4901","#AE4A34","#FF0921","#BC2001","#FF5E4D","#E73E01")

    colBack <- colBackPalette[1:length(backgroundDir)]
    colLimb <- colLimbPalette[1:length(limbDir)]
    colLesion <- colLesionPalette[1:length(lesionDir)]

    # Save picture of Discriminent analysis
    jpeg(plotFileTraining1_2,
      width = 800,
      height = 800,
      quality = 100,
      units = "px")

    if ( nbGroups <= 3){
      g <- ggplot( data = df4, aes(x = LD1, y = LD2, colour = group, shape = classes)) +
                  geom_point() +
                  scale_color_manual(values = c(colBack,colLimb,colLesion)) +
                  labs( x = "LD1", y = "LD2",
                          title = "Add a title above the plot",
                          caption="Source: LeAFtool", colour = "Groups"
                      ) +
                  theme( legend.position = "right",
                          panel.grid.major = element_blank(),
                          panel.grid.minor = element_blank()
                      ) +
                  guides(colour = guide_legend(override.aes = list(shape = c(rep(16,length(backgroundDir)),rep(15,length(limbDir)),rep(17,length(lesionDir))))), shape = FALSE, size = FALSE)
      print(g)
      dev.off()
      rv$plotALL <- FALSE
    }else{
      g <- ggplot( data = df4, aes(x = LD1, y = LD2, colour = group, shape = classes.1)) +
                  geom_point() +
                  scale_color_manual(values = c(colBack,colLimb,colLesion)) +
                  labs( x = "LD1", y = "LD2",
                          title = "Add a title above the plot",
                          caption="Source: LeAFtool", colour = "Groups"
                      ) +
                  theme( legend.position = "right",
                          panel.grid.major = element_blank(),
                          panel.grid.minor = element_blank()
                      ) +
                  guides(colour = guide_legend(override.aes = list(shape = c(rep(16,length(backgroundDir)),rep(15,length(limbDir)),rep(17,length(lesionDir))))), shape = FALSE, size = FALSE)
      print(g)
      dev.off()
      # Save picture of Discriminent analysis
      jpeg(plotFileTraining1_3,
        width = 800,
        height = 800,
        quality = 100,
        units = "px")
      g <- ggplot( data = df4, aes(x = LD1, y = LD3, colour = group, shape = classes.1)) +
                  geom_point() +
                  scale_color_manual(values = c(colBack,colLimb,colLesion)) +
                  labs( x = "LD1", y = "LD3",
                          title = "Add a title above the plot",
                          caption="Source: LeAFtool", colour = "Groups"
                      ) +
                  theme( legend.position = "right",
                          panel.grid.major = element_blank(),
                          panel.grid.minor = element_blank()
                      ) +
                  guides(colour = guide_legend(override.aes = list(shape = c(rep(16,length(backgroundDir)),rep(15,length(limbDir)),rep(17,length(lesionDir))))), shape = FALSE, size = FALSE)
      print(g)
      dev.off()
      # Save picture of Discriminent analysis
      jpeg(plotFileTraining2_3,
        width = 800,
        height = 800,
        quality = 100,
        units = "px")
      g <- ggplot( data = df4, aes(x = LD2, y = LD3, colour = group, shape = classes.1)) +
                  geom_point() +
                  scale_color_manual(values = c(colBack,colLimb,colLesion)) +
                  labs( x = "LD2", y = "LD3",
                          title = "Add a title above the plot",
                          caption="Source: LeAFtool", colour = "Groups"
                      ) +
                  theme( legend.position = "right",
                          panel.grid.major = element_blank(),
                          panel.grid.minor = element_blank()
                      ) +
                  guides(colour = guide_legend(override.aes = list(shape = c(rep(16,length(backgroundDir)),rep(15,length(limbDir)),rep(17,length(lesionDir))))), shape = FALSE, size = FALSE)
      print(g)
      dev.off()
    }

    ## save results
    file.train <- paste(path.training,paste0(basename,".RData"),sep='/')
    save(train,file=file.train)
}
