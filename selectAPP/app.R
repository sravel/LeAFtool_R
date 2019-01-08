library(shiny)
library(EBImage)
library(MASS)



# pathData <- "/Users/054-h163/Documents/images/exemples/musa/sample"
# fileRdata = "/Users/054-h163/Documents/images/exemples/musa/calibration.RData"

# fileRdata = "/media/sebastien/Bayer/ScriptsSEB/scripts/GUI/countLesionTools/exemples/Images/Apprentissage/Apprentissage.RData"
# pathData = "/media/sebastien/Bayer/ScriptsSEB/scripts/GUI/countLesionTools/exemples/Images/samples1/"

pathData <- "/media/sebastien/Bayer/ScriptsSEB/images/exemples/musa/samples"
fileRdata = "/media/sebastien/Bayer/ScriptsSEB/images/exemples/musa/learning/learning.RData"
onefileImage = NA
leafMinSize = 1000
leafBorderSize = 3
lesionBorderSize = 3
lesionMinSize = 3
lesionMaxSize = 200000
colorLesion = 1
rmBorder <- TRUE

###############################################################
#################################################################
## retourne les indices extrêmes de la valeur objet dans le vecteur x
## (appelé par bounding.rectangle)
range.na <- function(x, object) {
  w <- which(x == object)
  if (length(w) == 0)
    return(c(NA, NA))
  return(range(w))
}

bounding.rectangle <- function(mask, object) {
  m <<- imageData(mask)
  range.x <- range(apply(m, 1, range.na, object), na.rm = TRUE)
  range.y <- range(apply(m, 2, range.na, object), na.rm = TRUE)
  list(x = range.x[1]:range.x[2], y = range.y[1]:range.y[2])
}

extrait.leaf <- function(i, mask, image.fond.noir) {
  b <<- bounding.rectangle(mask, i)
  leaf <<- image.fond.noir[b$y, b$x,]
  mask.leaf <- mask[b$y, b$x]
  leaf[mask.leaf != i] <- 0
  xCoord <- list(min = min(b$x),max = max(b$x))
  yCoord <- list(min = min(b$y),max = max(b$y))
  XYcoord <- list(x = xCoord, y = yCoord)
  list(b = b, leaf = leaf, XYcoord = XYcoord)
}



#################################################################
#################################################################

ui <- fluidPage(

  # Application title
  titlePanel("Image display"),

  # Sidebar with a select input for the image
  sidebarLayout(
    sidebarPanel(width = 2, 
      selectInput("image", "Sample image:", list.files(pathData)),
      actionButton("brightness", "brightness")
    ),

    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel("Static raster", width = 10,
          fluidRow(
            column(4,
                 plotOutput("raster")
            ),
            column(6,

                   imageOutput("raster2",click = "plot_click",dblclick = "plot_dbclick", brush = "plot_brush"),
                 verbatimTextOutput("position"),
                 verbatimTextOutput("brushPos"),
                 tableOutput("plot_brushedpoints")
            )
          ),
          fluidRow(
            tableOutput("results"),
            verbatimTextOutput("infos")
          )
        ),

        tabPanel("Interactive browser",
                 displayOutput("widget", width = "100%", height = "800px")
        )
      )
    )
  )

)

server <- function(input, output) {

  rv <<- reactiveValues(imgBlack = NULL, results=NULL, position = NULL, idmin = NULL)

  img <- reactive({
    source.image <- paste(pathData, '/', input$image, sep = '')
    readImage(source.image)
  })

  output$widget <- renderDisplay({
    display(rv$imgBlack)
    # display(img(), all=TRUE)
    # rv$position <- locator()
  })

  output$raster <- renderPlot({
    plot(img(), all=TRUE)
  })
  output$raster2 <- renderPlot({
    if ( is.null(rv$imgBlack)) return(NULL)
    nx <- dim(rv$imgBlack)[1]
    ny <- dim(rv$imgBlack)[2]
    plot(rv$imgBlack, all=TRUE)
    points(rv$sortie$m.cx, rv$sortie$m.cy, pch='+', cex=0.5, col=rv$sortie$couleur)
  })

  output$position <- renderText({
    paste0("x=", input$plot_click$x, "\ny=", input$plot_click$y)

  })

  output$brushPos <- renderPrint({
    paste0("x=", input$plot_brush)

  })

  output$results <- renderTable({
    # rv$results
    rv$sortie
    })

  observeEvent(input$image,{

    load(fileRdata)
    background <- names(lda1$prior)[1]
    limb <- names(lda1$prior)[2]
    lesion <- names(lda1$prior)[3]

    image <-img()

    widthSize = dim(img())[1]
    heightSize = dim(img())[2]

    ## prédiction sur l'image (non floutée)
    df5 <-
      data.frame(
        red = as.numeric(imageData(img())[, , 1]),
        green = as.numeric(imageData(img())[, , 2]),
        blue = as.numeric(imageData(img())[, , 3])
      )
    df5$predict <- predict(lda1, df5)$class

    ## création des identificateurs des taches et des leafs
    df5$tache <- as.numeric(df5$predict == lesion)
    df5$leaf <- as.numeric(df5$predict != background)

    ## masque des leafs
    mask <- channel(img(), "gray")
    leaf <- matrix(df5$leaf, nrow = nrow(imageData(mask)))
    imageData(mask) <- leaf

    toto <<- imageData(mask)

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
    image.fond.noir <- img()
    image.fond.noir[mask == 0] <- 0

    ## séparation des leafs
    li <<- lapply(as.numeric(row.names(features)), extrait.leaf, mask, image.fond.noir)

    ## analyse des leafs
    analyse.li <<- lapply(li,  analyse.leaf,  lda1,  lesion)


    filename <- strsplit(input$image, ".", fixed = TRUE)[[1]][1]
    ## sortie des résultats et coloration des lésions
    result <- NULL
    resultLesion <- NULL
    imageLesionColor <- image
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
      # rename leaf number
      analyse.li[[i]][["leafLesionDT"]]$image <- paste(filename, "leaf",i,sep = "_")
      
      resultLesion <-
        rbind(
          resultLesion,
          analyse.li[[i]][["leafLesionDT"]]
        )

        # tmpimage <- image[li[[i]]$b$y, li[[i]]$b$x,]
        # tmpimage[analyse.li[[i]]$mask > 0] <- colorLesion
        # image[li[[i]]$b$y, li[[i]]$b$x,] <- tmpimage
        
        maskLesion <<- analyse.li[[i]][["maskLesion"]]
        analyse.li[[i]][["leafLesionDT"]]$image <- paste(filename,i,sep = "_")
        initialImage <- image[li[[i]]$b$y, li[[i]]$b$x,]
        initialImage <- paintObjects(maskLesion  ,initialImage, thick=TRUE, col=c(rv$lesion_color_border, rv$lesion_color_bodies), opac=c(1, 1))
        ## combine images horizontally
        if (is.null(imageLesionColor)){
          print("toto")
          imageLesionColor <- initialImage
        }
        # }else{
        #   imageLesionColor <- combine(imageLesionColor, initialImage)
        # }
        ## stack images one on top of the other
        # z = abind(x, x, along=2)

    }
    rv$sortie <<-resultLesion
    rv$imgBlack <-  imageLesionColor

    ag.count <-
      aggregate(result$surfaceLesion, result[c("fichier", "leaf", "surfaceLeaf")], length)
    names(ag.count)[4] <- "nbLesions"
    ag.surface <-
      aggregate(result$surfaceLesion, result[c("fichier", "leaf", "surfaceLeaf")], sum)
    names(ag.surface)[4] <- "surfaceLesions"
    ag <- merge(ag.count, ag.surface)
    ag$pourcent.lesions <- ag$surfaceLesions / ag$surfaceLeaf * 100
    ag$nbLesions[ag$surfaceLesions == 0] <- 0

    datas <- as.data.frame(ag[order(ag$leaf),])

    rv$results <- as.data.frame(ag[order(ag$leaf),])

  })

  output$infos <- renderPrint({
    nearPoints(rv$sortie, input$plot_click, xvar = "m.cx", yvar = "m.cy", threshold = 10, maxpoints = 1)
  })

  observeEvent(input$plot_click$x,{
  # invalidateLater(1000, session = getDefaultReactiveDomain())
  if(is.null(input$plot_click$x)) return(NULL)
    click <- c(input$plot_click$x, input$plot_click$y)


    idmin <- nearPoints(rv$sortie, input$plot_click, xvar = "m.cx", yvar = "m.cy", threshold = 10, maxpoints = 1)
    if(length(idmin$object) != 0){

      rv$sortie$couleur[rv$sortie$object == idmin$object] <- if (rv$sortie$couleur[rv$sortie$object == idmin$object]=="blue") "red" else "blue"
      maskLesionRed <- maskLesion
      w1 <-  rv$sortie$object[ rv$sortie$couleur=="red"]
      maskLesion[maskLesion %in% w1] <- 0 ## suppression dans mask des objets rouges
      maskLesionRed[!(maskLesionRed %in% w1)] <- 0 ## suppression dans mask2 des objets non rouges

      ## coloration des objets selon leur surface
      rv$imgBlack <- paintObjects(maskLesion  ,rv$imgBlack, thick=TRUE, col=c("blue", "blue"), opac=c(1, 0.15))
      rv$imgBlack <- paintObjects(maskLesionRed ,rv$imgBlack, thick=TRUE, col=c("red", "red"), opac=c(1, 0.15))
    }
    # print(nearPoints(sortie,input$plot_click,xvar = sortie$m.cx, yvar = sortie$m.cy, threshold = 5, maxpoints = 1))
  })



  ################## BRUSH

  observeEvent(input$plot_brush,{
    res <- brushedPoints(rv$sortie, input$plot_brush, xvar = "m.cx", yvar = "m.cy")
    if (nrow(res) == 0)
      return()
    for (i in res$object) {
      rv$sortie$couleur[rv$sortie$object == i] <- if (rv$sortie$couleur[rv$sortie$object == i]=="blue") "red" else "blue"
      maskLesionRed <- maskLesion
      w1 <-  rv$sortie$object[ rv$sortie$couleur=="red"]
    }

  })
  output$plot_brushedpoints <- renderTable({
    res <- brushedPoints(rv$sortie, input$plot_brush, xvar = "m.cx", yvar = "m.cy")
    if (nrow(res) == 0)
      return()
    res
  })


  ## analyse d'une leaf
  analyse.leaf <- function(x, lda1, lesion) {
    f <- x$leaf
    XYcoord = x$XYcoord
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
    featuresLesion <- computeFeatures.shape(mask)

    ## recherche des petits objets
    w.petit <- which(featuresLesion[,"s.area"] < lesionMinSize)

    w.grand <- which(featuresLesion[,"s.area"] > lesionMaxSize)

    ## recherche des objets touchant les bords
    haut <- unique(imageData(mask)[1,])
    gauche <- unique(imageData(mask)[,1])
    bas <- unique(imageData(mask)[dim(mask)[1],])
    droite <- unique(imageData(mask)[,dim(mask)[2]])
    w.bord <- unique(c(haut, gauche, bas, droite))



    if (rmBorder == TRUE){
      w <- unique(c(w.petit, w.bord, w.grand)) ## valeurs des objets à supprimer
    }else{
      w <- unique(c(w.petit, w.grand)) ## valeurs des objets à supprimer
    }

    maskLesion <<- rmObjects(mask, w)


    ## renumérote les objets
    featuresLesion <- computeFeatures.shape(mask)
    list(featuresLesion = featuresLesion, mask = mask)

    featuresLesion.surfmin <- as.data.frame(featuresLesion[-w,]) ## suppression des objets dans le tableau
    featuresLesion.surfmin$object <- as.numeric(row.names(featuresLesion.surfmin))
    moments <- as.data.frame(computeFeatures.moment(mask))
    moments$object <- as.numeric(row.names(moments))
    featuresLesion.surfmin <- merge(featuresLesion.surfmin, moments)

    w1 <- as.numeric(featuresLesion.surfmin$object[featuresLesion.surfmin[,"s.area"]<=100000000]) ## valeurs des objets plus petits que  surfmax
    couleur <- ifelse(featuresLesion.surfmin$object %in% w1, "blue", "red")



    leafLesionDT <- data.frame(image="toto", couleur=couleur, featuresLesion.surfmin, stringsAsFactors=FALSE)
    
    # print(leafLesionDT$m.cx)
    leafLesionDT$m.cx <- leafLesionDT$m.cx + XYcoord$y$min
    print(leafLesionDT$m.cx)
    
    # print(leafLesionDT$m.cy)
    leafLesionDT$m.cy <- leafLesionDT$m.cy + XYcoord$x$min
    print(leafLesionDT$m.cy)
    # print(XYcoord)
    
    list(features = featuresLesion, maskLesion = maskLesion, leafLesionDT = leafLesionDT)
  }




}

# Run the application
shinyApp(ui = ui, server = server)
