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



############################################
## Output Directory path of analysis
############################################
shinyDirChoose(
  input,'dirInResult',
  filetypes = c('', "png", "PNG","jpg","JPG","jpeg","JPEG"),
  roots = allVolumesAvail,
  session = session,
  restrictions = system.file(package = 'base')
)

# To add file path in UI
output$dirInResult <- renderText({
  rv$dirInResult
})

# extract file Path on input and update list of images
observeEvent(input$dirInResult,{
  if (!is.integer(input$dirInResult))
  {
    home <- normalizePath(allVolumesAvail[input$dirInResult$root])
    rv$dirInResult <- file.path(home, paste(unlist(input$dirInResult$path[-1],"/"), collapse = .Platform$file.sep))
    loadImageEdit <- list.files(rv$dirInResult, full.names=FALSE, pattern = "*_lesion.jpeg")
    updateSelectInput(session, "imageEdit", label = NULL, choices = loadImageEdit)
  }
})

############################################
## OBSERVE
############################################
observeEvent(c(input$lesion_color_borderEdit,input$lesion_color_bodiesEdit), {

  #### Update color of lesion
  if (!is.null(rv$dirInResult))# && input$imageEdit != "")
  {
    rv$lesion_color_borderEdit <- input$lesion_color_borderEdit
    rv$lesion_color_bodiesEdit <- input$lesion_color_bodiesEdit
    rv$lesion_color_borderAlphaEdit <-col2rgb(input$lesion_color_borderEdit, alpha=TRUE)[4]/255
    rv$lesion_color_bodiesAlphaEdit <-col2rgb(input$lesion_color_bodiesEdit, alpha=TRUE)[4]/255
    leaves <- rv$loadCSVcurrentImage$leaf.number
    updateLesionColor(leaves)
  }
  #### SAVE IMAGE IF CHANGE COLOR
  if (!is.null(rv$loadcurrentImageEdit) && !is.null(rv$loadcurrentImageOriginaleEdit) && !is.null(rv$originalFileNameBoth) && !is.null(rv$originalFileName))
  {
    isolate({
      # print both sample and lesion images
      jpeg(rv$originalFileNameBoth,
           width = rv$widthSize,
           height = rv$heightSize*2,
           units = "px")
      par( mfrow = c(2,1) )
      display(rv$loadcurrentImageOriginaleEdit, method="raster")
      display(rv$loadcurrentImageEdit, method = "raster")
      dev.off()

      # print only output file image with lesion
      jpeg(rv$originalFileName,
           width = rv$widthSize,
           height = rv$heightSize,
           units = "px")
      par( mfrow = c(1,1) )
      display(rv$loadcurrentImageEdit, method = "raster")
      dev.off()
    })
  }
})

#### LOAD all infos
observeEvent(input$imageEdit, {
  #Load image for plot
  lesionImg <- strsplit(input$imageEdit, ".", fixed = TRUE)[[1]][1]
  pathImg <- paste(rv$dirInResult, input$imageEdit, sep = '/')

  if(file.exists(pathImg) && !is.null(rv$dirInResult) && input$imageEdit != ""){
    baseName <- gsub("_lesion", "", lesionImg)
    rv$originalFileName <- paste0(rv$dirInResult, "/",baseName,"_lesion.jpeg")
    rv$originalFileNameBoth <- paste0(rv$dirInResult, "/",baseName,"_both.jpeg")
    rv$loadcurrentImageEdit <- readImage(pathImg)
    img <- readImage(rv$originalFileNameBoth)
    rv$widthSize = dim(img)[1]
    rv$heightSize = dim(img)[2]/2
    rv$loadcurrentImageOriginaleEdit <- img[1:rv$widthSize,1:rv$heightSize,]
    rv$loadCSVcurrentImageName <- paste0(rv$dirInResult, "/",baseName,"_All_lesions.csv")
    rv$loadCSVcurrentImage <- read.csv(rv$loadCSVcurrentImageName ,header = TRUE, sep = "\t", stringsAsFactors = FALSE)
    rv$csv_Merge_lesionsFile <- paste(rv$dirInResult, "/",baseName,"_Merge_lesions.csv", sep = '')
    rv$AG <- read.csv(rv$csv_Merge_lesionsFile ,header = TRUE, sep = "\t", stringsAsFactors = FALSE)
    rv$MERGE <- read.csv(paste0(rv$dirInResult,"/merge_ResumeCount.csv") ,header = TRUE, sep = "\t", stringsAsFactors = FALSE)

    load(file = paste0(rv$dirInResult, "/",baseName,".RData"), envir = .GlobalEnv)
  }
})

############################################
## DISPLAY IMAGES
############################################

#### Display image original
output$plotcurrentImageOriginalEdit <- renderPlot({

 ### PLOT ALL FILE if checkbox uncheck
  if (rv$zoomOriginalCheck == FALSE){
    if ( is.null(rv$loadcurrentImageOriginaleEdit)) return(NULL)

    plot(rv$loadcurrentImageOriginaleEdit)

    if (rv$pchOriginal == TRUE){
      text(rv$loadCSVcurrentImage$m.cx, rv$loadCSVcurrentImage$m.cy, labels=rv$loadCSVcurrentImage$lesion.number, cex=1, col=rv$color)
    }
    points(input$plot_hover$x, input$plot_hover$y, pch=4, cex=3, col="tomato")
  }
  #  else Plot zoom original File
  if (rv$zoomOriginalCheck == TRUE){
    if (is.null(rv$zoomInitial)) return(NULL)

    plot(rv$zoomInitial)
    if (rv$pchOriginal == TRUE){
      text(rv$loadCSVcurrentImage$m.cx-rv$addleft, rv$loadCSVcurrentImage$m.cy-rv$addbottom+rv$zoomValue, labels=rv$loadCSVcurrentImage$lesion.number, cex=1, col=rv$color)
    }
    points(rv$pointx, rv$pointy, pch=4, cex=3, col="tomato")
  }
})
#### Display image lesion color
output$plotcurrentImageEdit <- renderPlot({
  if ( is.null(rv$loadcurrentImageEdit)) return(NULL)
  plot(rv$loadcurrentImageEdit)
  points(rv$loadCSVcurrentImage$m.cx, rv$loadCSVcurrentImage$m.cy, pch='+', cex=2, col=rv$color)
})

#### Display image lesion color ZOOM
output$zoomcurrentImageEdit <- renderPlot({
  if (is.null(rv$zoom)) return(NULL)
  plot(rv$zoom)
  points(rv$pointx, rv$pointy, pch=4, cex=3, col="tomato")
})


############################################
## DISPLAY TABLES
############################################

#### ALL lesion tables for selected image
output$results <- DT::renderDataTable({

  if (is.null(rv$loadCSVcurrentImage)) return(NULL)
  DT::datatable(data = as.data.frame(rv$loadCSVcurrentImage, stringAsFactors = FALSE),
                                     rownames = NULL,
                                     escape=FALSE,
                                     selection="multiple",
                                     style = "bootstrap",
                                     filter = list(position = 'top', clear = TRUE, plain = FALSE),
                                     options = list(
                                       paging=TRUE,searching = TRUE,ordering=TRUE,scrollCollapse=FALSE,server = FALSE, autoWidth = TRUE
                                     )
  ) %>%
  formatStyle(
    'lesion.status',
    backgroundColor = styleEqual(c("keep","remove"), c('green', 'red'))
  ) %>%
  formatRound(c("lesion.radius.mean", "lesion.radius.sd", "lesion.radius.min", "lesion.radius.max", "m.cx", "m.cy", "m.majoraxis", "m.eccentricity", "m.theta"), 2)
})

#### ALL lesion merge by leaf for selected image
output$AG <- DT::renderDataTable({

  if (is.null(rv$AG)) return(NULL)
  DT::datatable(data = as.data.frame(rv$AG , stringAsFactors = FALSE),
                 rownames = NULL,
                 escape=FALSE,
                 selection="single",
                 style = "bootstrap",
                 filter = "none",
                 options = list(
                   paging=TRUE,searching = FALSE,ordering=TRUE,scrollCollapse=FALSE,server = FALSE, autoWidth = TRUE
                 )
  )
})

#### ALL lesion merge by leaf for selected all images
output$MERGE <- DT::renderDataTable({

  if (is.null(rv$MERGE)) return(NULL)
  DT::datatable(data = as.data.frame(rv$MERGE , stringAsFactors = FALSE),
                                     rownames = NULL,
                                     escape=FALSE,
                                     selection="single",
                                     style = "bootstrap",
                                     filter = "none",
                                     options = list(
                                       paging=TRUE,searching = FALSE,ordering=TRUE,scrollCollapse=FALSE,server = FALSE, autoWidth = TRUE
                                     )
  )
})

############################################
## EVENTS on plot
############################################

################## CLICK
observeEvent(input$plot_click,{

  if(is.null(input$plot_click$x)) return(NULL)
  res <- nearPoints(rv$loadCSVcurrentImage, input$plot_click, xvar = "m.cx", yvar = "m.cy", threshold = 10, maxpoints = 1)
  updateAll(res)
})

################## BRUSH
observeEvent(input$plot_brush,{
  res <- brushedPoints(rv$loadCSVcurrentImage, input$plot_brush, xvar = "m.cx", yvar = "m.cy")
  updateAll(res)
})

######### zoom Value
observeEvent(input$zoomValueSlider,{
#  rv$zoomValue <- input$zoomValueSlider
  zoomVector <- c(500,400,300,200,100,50,20,10)
  rv$zoomValue <- zoomVector[input$zoomValueSlider-1]
})

 observeEvent(input$plot_hover, {
  if (!is.null(input$plot_hover$x) &&!is.null(input$plot_hover$y)){
    ## If image less than 200px prenvent crash on zoom plot
    rv$sizeZoomX <- if (rv$widthSize < rv$zoomValue ) rv$widthSize/2 else  rv$zoomValue/2
    rv$sizeZoomX2 <- rv$sizeZoomX*2
    rv$sizeZoomY <- if (rv$heightSize < rv$zoomValue ) rv$heightSize/2 else  rv$zoomValue/2
    rv$sizeZoomY2 <- rv$sizeZoomY*2

    xleft <- input$plot_hover$x - rv$sizeZoomX
    xright <- input$plot_hover$x + rv$sizeZoomX
    ytop <- input$plot_hover$y - rv$sizeZoomY
    ybottom <- input$plot_hover$y + rv$sizeZoomY
    rv$pointy <- (ybottom-ytop)/2
    rv$pointx <- (xright-xleft)/2
    rv$addleft <- xleft
    rv$addbottom <- ybottom

    if (xleft <= 0 ){
     xleft <- 1
     xright <- rv$sizeZoomX2
     rv$addleft <- xleft
     rv$pointx <- if (input$plot_hover$x <= 0 ) 1 else  input$plot_hover$x
    }
    else if (xright >= rv$widthSize ){
     xright <- rv$widthSize
     xleft <- rv$widthSize - rv$sizeZoomX2
     rv$pointx <- if (input$plot_hover$x >= rv$widthSize) rv$widthSize else  input$plot_hover$x - xleft
    }
    if (ytop <= 0 ){
     ytop <- 1
     ybottom <- rv$sizeZoomY2
     rv$addbottom <- 1
     rv$pointy <- input$plot_hover$y
    }
    else if (ybottom >= rv$heightSize ){
     ybottom <- rv$heightSize
     rv$addbottom <- 1
     ytop <- rv$heightSize - rv$sizeZoomY2
     rv$pointy <- input$plot_hover$y - ytop
    }

    if (is.null(rv$pointy)) rv$pointy <- (ybottom-ytop)/2
    if (is.null(rv$pointx)) rv$pointx <- (xright-xleft)/2


      rv$addleft <- xleft
      rv$addbottom <- if (ybottom == rv$heightSize) rv$zoomValue else ybottom

    output$coor <-  renderText(
              (paste("xleft",xleft,"\n",
               "xright",xright,"\n",
               "ytop",ytop,"\n",
               "ybottom",ybottom,"\n",
               "input$plot_hover$x",input$plot_hover$x,"\n",
               "input$plot_hover$y",input$plot_hover$y,"\n",
               "rv$pointx",rv$pointx,"\n",
               "rv$pointy",rv$pointy,"\n",
               "LEFT:", rv$addleft,"\n",
               "BOTTOM:",  rv$addbottom,"\n"
               ))
    )
      rv$zoom <- rv$loadcurrentImageEdit[xleft:xright, ytop:ybottom,]
      rv$zoomInitial <- rv$loadcurrentImageOriginaleEdit[xleft:xright, ytop:ybottom,]
   }

 })

updateLesionColor <- function(leaves){

  ## Update lesion color on image
  for (leaf in unique(leaves))
  {
    maskLesion <- analyse.li[[leaf]][["maskLesion"]]
    maskLesionRed <- maskLesion

    remove <-  rv$loadCSVcurrentImage$lesion.number[ (rv$loadCSVcurrentImage$lesion.status=="remove") & (rv$loadCSVcurrentImage$leaf.number==leaf) ]
    keep <-  rv$loadCSVcurrentImage$lesion.number[ (rv$loadCSVcurrentImage$lesion.status=="keep") & (rv$loadCSVcurrentImage$leaf.number==leaf) ]

    maskLesion[!(maskLesion %in% keep)] <- 0 ## suppression dans mask des objets rouges
    maskLesionRed[!(maskLesionRed %in% remove)] <- 0 ## suppression dans mask2 des objets non rouges

    # open original file
    tmpimage <- rv$loadcurrentImageOriginaleEdit[li[[leaf]]$b$y, li[[leaf]]$b$x,]

    tmpimage <- paintObjects(maskLesion  ,tmpimage, thick=TRUE, col=c(rv$lesion_color_borderEdit, rv$lesion_color_bodiesEdit), opac=c(rv$lesion_color_borderAlphaEdit, rv$lesion_color_bodiesAlphaEdit))
    tmpimage <- paintObjects(maskLesionRed ,tmpimage, thick=TRUE, col=c("red", "red"), opac=c(0.3, 0))

    rv$loadcurrentImageEdit[li[[leaf]]$b$y, li[[leaf]]$b$x,] <- tmpimage
  }

}


updateAll <- function(res){
  if (nrow(res) == 0) return(NULL)  # if no lesion

  ## If lesion near click or in brush, update status
  for (i in row.names(res)) {
    row <- as.integer(i)
    rv$loadCSVcurrentImage$lesion.status[row] <- if (rv$loadCSVcurrentImage$lesion.status[row]=="keep") "remove" else if (rv$loadCSVcurrentImage$lesion.status[row]=="remove") "keep"
  }
  ## Update csv file
  write.table(
    rv$loadCSVcurrentImage,
    file = rv$loadCSVcurrentImageName,
    quote = FALSE,
    row.names = FALSE,
    sep = '\t'
  )

  ## Update lesion color on image
  leaves <- res$leaf.number
  updateLesionColor(leaves)

  # update aggregate table
  result <- rv$loadCSVcurrentImage[rv$loadCSVcurrentImage$lesion.status != "remove", ]

  if ( nrow(result) != 0 )
  {
    # Update merge leaves stats
    ag.count <- aggregate(result$lesion.surface, result[c("image", "leaf.number", "leaf.surface")], length)
    names(ag.count)[4] <- "lesion.nb"
  #  print(ag.count)

    ag.surface <- aggregate(result$lesion.surface, result[c("image", "leaf.number", "leaf.surface")], sum)
    names(ag.surface)[4] <- "lesion.surface"
  #  print(ag.surface)

    ag <- merge(ag.count, ag.surface)
    ag$pourcent.lesions <- ag$lesion.surface / ag$leaf.surface * 100
    ag$lesion.nb[ag$lesion.surface == 0] <- 0
    ag$lesion.surface[ag$lesion.surface == 0] <- 0

    # if leaves and one or more no lesion
    for (leafNum in unique(rv$loadCSVcurrentImage$leaf.number))
    {
      if (! (leafNum %in% unique(ag$leaf.number) ))
      {
        leafSurf <- rv$loadCSVcurrentImage$leaf.surface[rv$loadCSVcurrentImage$leaf.number == leafNum][1]
        line <- data.frame(image = rv$loadCSVcurrentImage$image[1], leaf.number = leafNum, leaf.surface = leafSurf, lesion.nb = 0, lesion.surface = 0, pourcent.lesions = 0)
        ag <- rbind(ag,line)
      }
    }
  }

  if ( nrow(result) == 0 ) # if no lesions in one or more leaves
  {
    for (leafNum in unique(rv$loadCSVcurrentImage$leaf.number))
    {
      leafSurf <- rv$loadCSVcurrentImage$leaf.surface[rv$loadCSVcurrentImage$leaf.number == leafNum][1]
      ag <- data.frame(image = rv$loadCSVcurrentImage$image[1], leaf.number = leafNum, leaf.surface = leafSurf, lesion.nb = 0, lesion.surface = 0, pourcent.lesions = 0)
    }
  }
#  print(ag)

  rv$AG <- ag[order(ag$leaf.number),]

  write.table(
    rv$AG,
    file = rv$csv_Merge_lesionsFile,
    quote = FALSE,
    row.names = FALSE,
    sep = '\t'
  )

  rv$MERGE = multmerge(rv$dirInResult, "*_Merge_lesions.csv")

  write.table(
    rv$MERGE,
    file = paste0(rv$dirInResult,"/merge_ResumeCount.csv"),
    quote = FALSE,
    row.names = FALSE,
    sep = '\t'
  )

}

######### add pch to original image
observeEvent(input$pchOriginal,{
  rv$pchOriginal <- input$pchOriginal
})
observeEvent(input$zoomOriginalCheck,{
  rv$zoomOriginalCheck <- input$zoomOriginalCheck
})


########## If table filter on values, color lesion in darkorange1 on plot

observeEvent(c(input$results_rows_all,input$results_rows_selected), {
  rv$filtered_data <- rv$loadCSVcurrentImage[input$results_rows_all,]
  rv$missing <- rv$loadCSVcurrentImage[-(input$results_rows_all),]

  #### update color of lesion base on filter
  rv$color <- ifelse(rv$loadCSVcurrentImage$lesion.status == "keep", "green", "red")
  for (i in input$results_rows_selected) {
    rv$color[i] <- "cyan"
  }
  for (i in row.names(rv$missing)) {
    row <- as.integer(i)
    if (rv$color[row] != "red") rv$color[row] <- "darkorange1"
  }
})


#  output$plot_brushedpoints <- renderTable({
#    res <- brushedPoints(rv$loadCSVcurrentImage, input$plot_brush, xvar = "m.cx", yvar = "m.cy")
#    if (nrow(res) == 0)
#      return()
#    res
#  })

#  output$infos <- renderPrint({
#    nearPoints(rv$loadCSVcurrentImage, input$plotcurrentImageEdit, xvar = "m.cx", yvar = "m.cy", threshold = 10, maxpoints = 1)
#  })
#output$position <- renderText({
#  paste0("x=", input$plotcurrentImageEdit$x, "\ny=", input$plotcurrentImageEdit$y)

#})

#output$brushPos <- renderPrint({
#  paste0("x=", input$plot_brush)

#})
