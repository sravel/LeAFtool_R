options(width=160)

## packages n?cessaires

## retourne les indices extr?mes de la valeur objet dans le vecteur x
## (appel? par bounding.rectangle)
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

extrait.feuille <- function(i,mask,image.fond.noir) {
    kern = makeBrush(size = 200)
    opening(image.fond.noir, kern)
    b <- bounding.rectangle(mask,i)
    feuille <- image.fond.noir[b$y,b$x,]
    mask.feuille <- mask[b$y,b$x]
    feuille[mask.feuille!=i] <- 0
    display(feuille,method="raster")
    list(b=b,feuille=feuille)
}

## analyse d'une feuille
analyse.feuille <- function(x,lda1,lesion,limb) {

   f <- x$feuille

   ## remplacement du fond par du limbe
   df6 <- data.frame(red=as.numeric(imageData(f)[,,1]), green=as.numeric(imageData(f)[,,2]), blue=as.numeric(imageData(f)[,,3]))
   df6$predict <- predict(lda1, df6)$class
   df.limbe <- df6[df6$predict %in% limb,]
   fond.noir <- df6$red+df6$green+df6$blue==0
   mean.limbe <- apply(df.limbe[1:3],2,mean)
   df6[fond.noir,1] <- mean.limbe[1]
   df6[fond.noir,2] <- mean.limbe[2]
   df6[fond.noir,3] <- mean.limbe[3]
   imageData(f)[,,1] <- df6$red
   imageData(f)[,,2] <- df6$green
   imageData(f)[,,3] <- df6$blue

    ## analyse
    if (diametre.flou>1) { ## floutage
        ##windows() ; display(f,method="raster")
        flo <- makeBrush(diametre.flou, shape='disc', step=FALSE)^2
        flo <- flo/sum(flo)
        f <- filter2(f, flo)
        ##windows() ; display(f,method="raster")
    }
    df6 <- data.frame(red=as.numeric(imageData(f)[,,1]), green=as.numeric(imageData(f)[,,2]), blue=as.numeric(imageData(f)[,,3]))
    df6$predict <- predict(lda1, df6)$class
    df6$tache <- as.numeric(df6$predict %in% lesion)
    mask <- channel(f, "gray")
    tache <- matrix(df6$tache, nrow=nrow(imageData(mask)))
    imageData(mask) <- tache

    ## dilatation
    brush <- makeBrush(bordure.lesion, shape='disc')
    mask <- dilate(mask, brush)

    ## remplissage vides
    mask <- fillHull(mask)

    ## erosion
    mask <- erode(mask, brush)

    ## segmentation
    mask[mask<0] <- 0
    mask <- bwlabel(mask)
    features <- computeFeatures.shape(mask)
    moments <- computeFeatures.moment(mask)

    ## suppression des petits objets
    w.petit <- w.grand <- w.eccentric <- NULL
    w.petit <- which(features[,"s.area"]<surface.lesion.mini)
    w.grand <- which(features[,"s.area"]>surface.lesion.maxi)
    w.eccentric <- which(moments[,"m.eccentricity"]>max.eccentricity)
    mask[mask %in% c(w.petit,w.grand,w.eccentric)] <- 0

    features <- computeFeatures.shape(mask)
    list(features=features, mask=mask)
}

##analyse.feuille <- function(x,lda1, lesion) {
##    f <- x$feuille
##    ##### modif
##    fond <- (as.numeric(imageData(f)[,,1]) + as.numeric(imageData(f)[,,2]) + as.numeric(imageData(f)[,,3]))==0
##    if (diametre.flou>1) {
##        windows() ; display(f,method="raster")
##        flo <- makeBrush(diametre.flou, shape='disc', step=FALSE)^2
##        flo <- flo/sum(flo)
##        f <- filter2(f, flo)
##        windows() ; display(f,method="raster")
##    }
##    ##### FIN modif
##
##    df6 <- data.frame(red=as.numeric(imageData(f)[,,1]), green=as.numeric(imageData(f)[,,2]), blue=as.numeric(imageData(f)[,,3]))
##    df6$red[fond] <- 0 ; df6$green[fond] <- 0 ; df6$blue[fond] <- 0
##    df6$predict <- predict(lda1, df6)$class
##    df6$tache <- as.numeric(df6$predict %in% lesion)
##    df6$tache[df6$red+df6$green+df6$blue==0] <- 0
##    mask <- channel(f, "gray")
##    tache <- matrix(df6$tache, nrow=nrow(imageData(mask)))
##    imageData(mask) <- tache
##
##    ## dilatation
##    brush <- makeBrush(bordure.lesion, shape='disc')
####    mask <- dilateGreyScale(mask, brush)
##    mask <- dilate(mask, brush)
##
##    ## remplissage vides
##    mask <- fillHull(mask)
##
##    ## erosion
##    mask <- erode(mask, brush)
##
##    ## segmentation
##    mask[mask<0] <- 0
##    mask <- bwlabel(mask)
##    features <- computeFeatures.shape(mask)
##
##    ## suppression des petits objets
##    w.petit <- which(features[,"s.area"]<surface.lesion.mini)
##    mask <- rmObjects(mask,w.petit)
##
##    features <- computeFeatures.shape(mask)
##    list(features=features, mask=mask)
##}
##








#' Launch runAnalysisSamples on folder
#'
#' Description of the function
#'
#' detail of the function ...
#' Training input folder must include sub-folders:
#' \itemize{
#'   \item limb
#'   \item background
#'   \item lesion
#' }
#'
#' @param pathImages  Path to folder with all images
#' @param pathResult Path to folder for output
#' @param pathTraining Path to folder training

#' @param leafMinSize  This is the minimum number of pixels (that is, the size) of a leaf that the program should consider.
#' @param borderLeaf  When zooming in on the edge of the leaf, you might observe shadows or other anomalies.
#'    This parameter we applies a brush that removes pixels from the edge of the leaf.
#'    Thus, this makes it possible to remove the shadow that might be classified as a lesion during an analysis.
#'
#' @return
#'
#' @examples
#' surface.feuille.mini <- 100 ## surface minimum d'une feuille
#' bordure.feuille <- 3 ## ?paisseur de bordure de feuille ? supprimer
#' bordure.lesion <- 3 ## ?paisseur de bordure de l?sion ? dilater / ?roder
#' surface.lesion.mini <- 2 ## surface minimum d'une l?sion
#' surface.lesion.maxi <- 120000 ## surface maximum d'une l?sion
#' max.eccentricity <- 0 ## excentricit? maximum d'une l?sion
#' couleur.lesion <-  1 ## couleur des l?sions dans l'image analys?e (0=noir, 1=blanc)
#' diametre.flou <- 0 ## flou si valeur > 1 (valeur impaire)
#' path.sample <- "/media/sebastien/Bayer/ScriptsSEB/images/exemples/mathias/Apprentissage/"
#' path.result <- "/media/sebastien/Bayer/ScriptsSEB/images/exemples/mathias/MF_A_01/results"
#' runAnalysisSamples(path.sample=path.sample,
#'               path.result=path.result,
#'               path.image=path.image,
#'               surface.feuille.mini=surface.feuille.mini,
#'               bordure.feuille=bordure.feuille,
#'               bordure.lesion=bordure.lesion,
#'               surface.lesion.mini=surface.lesion.mini,
#'               couleur.lesion=couleur.lesion)
runAnalysisSamples <- function(pathTraining,path.result,path.image,leafMinSize,bordure.feuille,bordure.lesion,surface.lesion.mini,couleur.lesion) {
    file.image <- list.files(path.image)
    for (image in file.image)
        analyse.image.unique(path.sample,path.result,path.image,image,surface.feuille.mini,bordure.feuille,bordure.lesion,surface.lesion.mini,couleur.lesion)
}

analyse.image.unique <- function(path.sample,path.result,path.image,file.image,surface.feuille.mini,bordure.feuille,bordure.lesion,surface.lesion.mini,couleur.lesion) {
    ## chargement du r?sultat de l'analyse discriminante
    filename <- tail(strsplit(path.sample,'/')[[1]],1)
##    filename <- paste0(tail(strsplit(path.sample,'/')[[1]],1),".RData")
    file.lda <- paste(path.sample,'/',filename,".RData",sep='')
    load(file.lda)
    print("toto")

    file.classes <- paste(path.sample,'/',filename,"_classes.txt",sep='')
    classes <- read.table(file.classes,header=TRUE,sep='\t')

    background <- classes$subclass[classes$class=="background"]
    limb <- classes$subclass[classes$class=="limb"]
    lesion <- classes$subclass[classes$class=="lesion"]

    ## lecture de l'image source
    source.image <- paste(path.image,'/',file.image,sep='')
    image <- readImage(source.image)

    ## pr?diction sur l'image (non flout?e)
    df5 <- data.frame(red=as.numeric(imageData(image)[,,1]), green=as.numeric(imageData(image)[,,2]), blue=as.numeric(imageData(image)[,,3]))
    df5$predict <- predict(lda1, df5)$class

    ## cr?ation des identificateurs des taches et des feuilles
    df5$tache <- as.numeric(df5$predict %in% lesion)
    df5$feuille <- as.numeric(!(df5$predict %in% background))

    ## masque des feuilles
    mask <- channel(image, "gray")
    feuille <- matrix(df5$feuille, nrow=nrow(imageData(mask)))
    imageData(mask) <- feuille

    ## suppression des vides
    mask <- fillHull(mask)

    ## suppression de la bordure par ?rosion
    ## remarque : les tests et les calculs sont effectu?s sur les objets ?rod?s
    brush <- makeBrush(bordure.feuille, shape='disc')
    ## ligne suivante ajout?e pour supprimer les lignes parasites dues au scan (supprimer pour scan normal)
    ## brush2 <- makeBrush(bordure.feuille*2+1, shape='disc') ; mask <- dilate(mask,brush2) ; mask <- erode(mask, brush2)
    mask <- erode(mask, brush)

    ## segmentation
    mask <- bwlabel(mask)
    features <- data.frame(computeFeatures.shape(mask))

    ## suppression des objets plus petits que la surface minimum d'une feuille
    w <- which(features[,"s.area"]<surface.feuille.mini)
    if (length(w) > 0) {
        mask[mask %in% w] <- 0
        features <- features[-w,]
    }

    ## suppression du fond
    image.fond.noir <- image
    image.fond.noir[mask==0] <- 0

    ## s?paration des feuilles
    li <- lapply(as.numeric(row.names(features)),extrait.feuille,mask,image.fond.noir)

    ## analyse des feuilles
    analyse.li <- lapply(li, analyse.feuille, lda1, lesion, limb)
    ## analyse.li <- lapply(li, analyse.feuille, lda1, lda2, lesion, limb)

    filename <- strsplit(file.image,'\\.')[[1]][1]
    pdfname <- paste(filename,".pdf",sep='')
    txtname1 <- paste(filename,"_1.txt",sep='')
    txtname2 <- paste(filename,"_2.txt",sep='')
    if (!file.exists(path.result)) dir.create(path.result)
    pdffile <- paste(path.result,'/',pdfname,sep='')
    txtfile1 <- paste(path.result,'/',txtname1,sep='')
    txtfile2 <- paste(path.result,'/',txtname2,sep='')

    pdf(pdffile)
    display(image, method="raster")

    ## sortie des r?sultats et coloration des l?sions
    result <- NULL
    for (i in 1:length(li)) {
        result <- rbind(result,data.frame(fichier=filename,feuille=i,surface.feuille=features[i,"s.area"],surface.lesion=if (is.null(analyse.li[[i]]$features)) 0 else analyse.li[[i]]$features[,"s.area"]))
## la ligne suivante a ?t? remplac?e par 3 lignes suite ? une erreur apparue sur certaines versions de R
## image[li[[i]]$b$y,li[[i]]$b$x,][analyse.li[[i]]$mask>0] <- couleur.lesion
        tmpimage <- image[li[[i]]$b$y,li[[i]]$b$x,]
        tmpimage[analyse.li[[i]]$mask>0] <- couleur.lesion
        image[li[[i]]$b$y,li[[i]]$b$x,] <- tmpimage
    }
    row.names(result) <- NULL

    display(image, method="raster")
    dev.off()

    write.table(result,file=txtfile1,quote=FALSE,row.names=FALSE,sep='\t')

    ag.count <- aggregate(result$surface.lesion,result[c("fichier","feuille", "surface.feuille")],length)
    names(ag.count)[4] <- "nb.lesions"
    ag.surface <- aggregate(result$surface.lesion,result[c("fichier","feuille", "surface.feuille")],sum)
    names(ag.surface)[4] <- "surface.lesions"
    ag <- merge(ag.count,ag.surface)
    ag$pourcent.lesions <- ag$surface.lesions/ag$surface.feuille*100
    ag$nb.lesions[ag$surface.lesions==0] <- 0

    write.table(ag[order(ag$feuille),],file=txtfile2,quote=FALSE,row.names=FALSE,sep='\t')
}
